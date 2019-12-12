/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import java.awt.MouseInfo
import java.awt.event.{ComponentAdapter, ComponentEvent}
import java.awt.geom.Rectangle2D.{Double => Rect2DDouble}

import client.comm.ClientQueryManager
import client.dataviewer.TitlePopupMenu
import client.dialog._
import client.spreadsheet.{CellTuple, SpreadSheetTransferable}
import client.ui.ClientApp
import definition.data._
import definition.expression._
import definition.typ.{AnswerDefinition, DataType, DialogQuestion}
import javax.swing.{JComponent, JLayeredPane, SwingUtilities}
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scala.swing.event._
import scala.util.control.NonFatal


/**
 * 
 */

case class MatchingScreenPoints(hitBoth:Option[Point],hitX:Option[Point],hitY:Option[Point],hitWorld:Option[VectorConstant])

class LPane extends Component {
    lazy val panel=new JLayeredPane
  override lazy val peer: JLayeredPane = panel
  }

class GraphViewController extends AbstractViewController[(AbstractLayer, Iterable[GraphElem]),GraphElem] {
  val scaleModel=new ScaleModel
  val selectModel=new ViewSelectModel(this)
  val layerModel=new LayerTableModel(this)
	val canvas=new GraphViewCanvas(this)
  val theCanvas: JComponent = canvas.peer
  val ddListener=new GraphViewDDListener(this)
  val transferHandler=new CommonTransferHandler(ddListener)
  theCanvas.setTransferHandler(transferHandler)
	val scalePanel=new ScalePanel(scaleModel,this)  
	
	var shuttingDown=false
	var inplaceTextElement:Option[TextElement]=None
	var inplaceEditFinishListener:Option[String => Unit]=None

	//var jumpUpCallback:Option[()=>Unit]=None
	
  scaleModel.registerScaleListener(()=>{
  	refreshCanvas()
  })
  
  val layerPane=new LPane
  private var ipePane: InplaceEditPanel = _
  
  layerPane.panel.add(canvas.peer,Integer.valueOf(1))
  
  def createIPEPane():Unit=ClientQueryManager.runInPool{
     ipePane=new InplaceEditPanel(this)
     Swing.onEDT{
       ipePane.setup()       
       Log.w("ipe:"+ipePane+" peer:"+ipePane.peer)
       ipePane.visible=false
       layerPane.panel.add(ipePane.peer,Integer.valueOf(2))
       //System.out.println("setup IPE "+layerPane.peer.getSize())
       ipePane.peer.setSize(layerPane.size)
       ipePane.setPaneSize(layerPane.size)
			 ipePane.pause()
     }
  }

	createIPEPane()

	
	val canvasPanel: BorderPanel =new BorderPanel {
		add(layerPane,BorderPanel.Position.Center)
		add(scalePanel,BorderPanel.Position.North)
	} 
	
	layerPane.peer.addComponentListener(new ComponentAdapter(){
    override def componentResized(e: ComponentEvent): Unit = {
	    //System.out.println("Resized: canvas:" +canvas.size+" pane:"+layerPane.size+" ipe:"+ipePane)
	    val si=layerPane.size
	    canvas.peer.setSize(si)
      if(ipePane!=null) {
				//Log.w("IPE Set site")
				ipePane.resume()
				ipePane.updatePaneSize(si)
      }
			scaleModel.viewSize=canvas.size		
	  } 
	})


  def layerChanged(lay: AbstractLayer, updateZoom: Boolean): Unit = if (!shuttingDown) {
	  if(updateZoom) zoomAll()
	  else refreshCanvas()
	  selectModel.deselect(true)
	  //layerModel.fireTableDataChanged()
	}

  def graphElemAdded(lay: AbstractLayer, elem: GraphElem): Unit = {
	  //println("graphElem added:"+elem+" numcr:"+numCreatedElements+" cas:"+hasCreateActionStarted)
	  if(hasCreateActionStarted) {
	    numCreatedElements-= 1
      selectModel.addSelection(Seq((lay,List(elem))),toggle = false)
	    if(numCreatedElements<1)   createdDataReceived()
	  }	    
		refreshCanvas()
	}

  def graphElemRemoved(lay: AbstractLayer, elem: GraphElem): Unit = {
    //println("graphElem removed :"+elem)
		selectModel.elemRemoved((lay,Seq(elem)))
		if(lastHittedElements.exists(entry=> entry._2.exists(el=>el.ref==elem.ref) ))
		{  
		  lastHittedElements=lastHittedElements.map(entry=> if(entry._2.exists(el=>el.ref==elem.ref)){
		    (entry._1,entry._2.filter(_.ref!=elem.ref))
      } else entry).filter(_._2.nonEmpty)
		}    
		//lastHittedElements=Nil
		refreshCanvas()
	}


  def graphElementsChanged(lay: AbstractLayer, newStates: Iterable[GraphElem], repaint: Boolean = true): Unit = {
    //println("graphElemsChanged "+lay.ref+" newStates:"+newStates.mkString(","))
		selectModel.elementChanged((lay,newStates))
		if(lastHittedElements.exists(entry=>  entry._2.exists(el=> newStates.exists(nse=>nse.ref==el.ref)))){
		  lastHittedElements=lastHittedElements.map(entry=>if(entry._2.exists(el=>newStates.exists(nse=>nse.ref==el.ref)) ) {
		    (entry._1,entry._2 flatMap(el => newStates.find(_.ref==el.ref)))
		  } else entry )   
		}		
		//lastHittedElements=Nil
		if(repaint)refreshCanvas()
	}
	
	// will be called when the DataViewController has another selection	
  def getAllBounds: Rect2DDouble = layerModel.calcAllLayerBounds()

	
	/** finds the elements that are in the given rectangle and selects them 
	 * in the select model
	 *
	 */
	def checkSelection(minX:Double,minY:Double,maxX:Double,maxY:Double,onlyInside:Boolean,control:Boolean):Unit= {
	  lastHittedElements=Nil		
	  val elemList= if(onlyInside) layerModel.filterLayersSelection(onlyEdible = true, e=>{
	  		val eb=e.getBounds(this)
	  		eb.x>=minX && eb.width<=maxX && eb.y>=minY && eb.height<=maxY
	  	})
	  	else {
        val rect = new Rect2DDouble(minX, minY, maxX - minX, maxY - minY)
	  	  layerModel.filterLayersSelection(onlyEdible = true,_.intersectsRect(this, rect))
	  	}	
	  //System.out.println("check selection result="+elemList)
	  if(control) {	  	
	  	selectModel.addSelection(elemList,toggle = false)
	  } else {
	  	if (elemList.isEmpty)selectModel.deselect(true)
	  	else //for(it<-elemList)	  		
	  		selectModel.setSelection(elemList)
	  }	  
	}	
	
	def doubleClick(where:Point,control:Boolean,shift:Boolean):Unit = {
	  //resetCAS()	  
		viewportState match {
			case ViewportState.SelectState =>  editSelectedElemIPE()			
			case _=>
	  }
	}
	
	override def findCrossPoint(clickPosX:Double,clickPosY:Double):Option[VectorConstant]= {
    val lcd=getCurrentLineCatchDistance
    val lines: Iterable[GraphElem] =layerModel.filterLayersSelection(onlyEdible = false, el ⇒ el.ref.typ==GraphElemConst.lineClassID &&
      el.hits(this,clickPosX,clickPosY,lcd)).flatMap(_._2)
    if (lines.size>1) {
      val firstLine=lines.head.asInstanceOf[LineElement]
      val firstLineDelta=firstLine.delta
      val iter=lines.iterator
      iter.next()
      iter.find{case el:LineElement ⇒ !el.delta.isLinearyDependentFrom(firstLineDelta);case _ ⇒ false } match {
        case Some(secondLine:LineElement)⇒ Some(firstLine.toLine3D.intersectionWith(secondLine.toLine3D))
        case _ ⇒ None
      }
    }
    else None
	}
	
	
	def getChoosableElements(onlyEdible:Boolean,clickPosX:Double,clickPosY:Double):Iterable[GraphElem]={
	  val lcd=getCurrentLineCatchDistance
	  if(objSelectMode==ObjectSelectMode.TempObject) 
	     layerModel.newElemLayer.filterSelection(onlyEdible = false,filterFunc = _.hits(this, clickPosX, clickPosY, lcd))
	  else if(objSelectClassConstraints==null) {
      util.Log.e("ObjSelectClassConstraints == null")
			    Nil
			  }
	  else { layerModel.filterLayersSelection(onlyEdible,el=>el.hits(this,clickPosX,clickPosY,lcd) ).
	    flatMap(_._2).filter(a=>(a.ref!=null)&& objSelectClassConstraints. contains(a.ref.typ)&&
	        (if(objSelectMode==ObjectSelectMode.SingleObjectNotSelected) !selectModel.selectionList.contains(a) else true))
	  } 
	}

  def filterSelection(clickPosX: Double, clickPosY: Double, lcd: Double): Iterable[(AbstractLayer, Iterable[GraphElem])] =
    layerModel.filterLayersSelection(onlyEdible = true, el => el.hits(this, clickPosX, clickPosY, lcd))
	
	def processElementClick(clickPosX:Double,clickPosY:Double,hittedElements:Iterable[GraphElem],editable:Boolean):Unit =
	  //println("processElementClick "+hittedElements.mkString(", "))
    try
      objSelectMode match {
        case ObjectSelectMode.TempObject =>
          for (o <- objSelectListener) o.tempObjectSelected(hittedElements.head.ref.instance)
          clearNewElements()
        case ObjectSelectMode.EdgeAndPoint =>
          val hPoint = new VectorConstant(clickPosX, clickPosY, 0)
          for (o <- objSelectListener) o.objectsSelectedWithPoint(ObjectReference(hittedElements.head.ref), hPoint, editable)
        //Swing.onEDT{changeViewportState(ViewportState.SelectState)} // execute after seconnd question is loaded
        case ObjectSelectMode.SegmentPart => if (editable) {
          //println("segment part :"+editable)
          val hPoint = new VectorConstant(clickPosX, clickPosY, 0)
          layerModel.getSegmentPart(hittedElements.head, hPoint) match {
            case Some((p1, p2)) => for (o <- objSelectListener)
              o.segmentPartSelected(ObjectReference(hittedElements.head.ref), p1, p2)
            case None => //DialogManager.reset
          }
        }
        case ObjectSelectMode.SingleObject | ObjectSelectMode.SingleObjectNotSelected =>
          for (o <- objSelectListener) o.objectsSelected(ObjectReference(hittedElements.head.ref), editable)
      }
    catch {
      case NonFatal(e) => ClientQueryManager.printErrorMessage(e.toString)
      case other: Throwable => println(other); System.exit(0)
    }

	
	
	def getFirstHittedElement(hittedElements:Iterable[(AbstractLayer,Iterable[GraphElem])]):(AbstractLayer,Iterable[GraphElem])={
	  val he=hittedElements.head
	  //System.err.println("get first hitted Elements "+he)
	  (he._1,Seq(he._2.head))
	}
	
	private def getHittedElementNr(hittedElements:Iterable[(AbstractLayer,Iterable[GraphElem])],nr:Int):(AbstractLayer,GraphElem)={
	  var counter=0
	  var layer=0
	  val hittedSeq=hittedElements.toSeq
	  while(nr>=counter+hittedSeq(layer)._2.size){
	    counter+=hittedSeq(layer)._2.size
	    layer+=1
	  }
	  val currentSet=hittedSeq(layer)
	  (currentSet._1,currentSet._2.toSeq(nr-counter))
	  
	}
	
	def getNextHittedElementNr(hittedElements:Iterable[(AbstractLayer,Iterable[GraphElem])],lastNr:Int):(Int,(AbstractLayer,Iterable[GraphElem]))= {
	  val numElements=hittedElements.foldLeft(0)(_ + _._2.size)
	  if(lastNr>=numElements-1) (0,getFirstHittedElement(hittedElements))
	  else {
	    val nextNr=lastNr+1
	    val (lay,elems)=getHittedElementNr(hittedElements,nextNr)
	    (nextNr,(lay,Seq(elems)))
	  }
	}

  override def getElementByRef(ref: Reference): Option[GraphElem] = layerModel.getElementByRef(ref)


  def deselectZoomInBut(): Unit = {
	  scalePanel.zoomInBut.selected=false	
	}
	
	def addTempElem( newElem:GraphElem):Unit = {
	  showTempElements=true
	  layerModel.newElemLayer.addTempElement(newElem)
	}
	
	def keyPressed(e:KeyPressed):Unit = {
		//System.out.println("key typed "+e)
	  //resetCAS()
	  /*if(measureMode!=MeasureMode.NoMeasure){ 
	    setMeasureMode(MeasureMode.NoMeasure) 
	  	return     
	  }*/
	  
		_viewportState match {
			case ViewportState.SelectState =>
				e.key match {
					case Key.Left => scaleModel.moveLeft()
					case Key.Right => scaleModel.moveRight()
					case Key.Up => scaleModel.moveUp()
					case Key.Down => scaleModel.moveDown()
					case Key.Escape => DialogManager.reset()
					case Key.F2 => editSelectedElemIPE()
					//case Key.Add=> showCreatePopup
					case _ => //System.out.println(e)
				}
			case ViewportState.AskPoint |  ViewportState.AskPointOrObject => 
			e.key match {		
			  case Key.Left => scaleModel.moveLeft()
				case Key.Right => scaleModel.moveRight()
				case Key.Up => scaleModel.moveUp()
				case Key.Down => scaleModel.moveDown()
				case Key.Escape => DialogManager.reset() //DialogManager.reset()
				case _ => //System.out.println(e)
			}
			case o =>  e.key match {
			  case Key.Escape => DialogManager.reset()
			  case Key.Left => scaleModel.moveLeft()
					case Key.Right => scaleModel.moveRight()
					case Key.Up => scaleModel.moveUp()
					case Key.Down => scaleModel.moveDown()
			  case _=>
			}// System.out.println(o)
		}		
	}

  def showCreatePopup(): Unit = {
    val mousePos = MouseInfo.getPointerInfo.getLocation
    val canvasPos = theCanvas.getLocationOnScreen
		requestFocus()
    //println("Show Create "+NewButtonsList.actionButtons.map(_.commandName).mkString(", ")+" mousePos:"+mousePos+" canvasPos:"+canvasPos)
    val buttons = layerModel.getActiveLayer match {
      case Some(_: MeasureLayer) => CreateActionList.actionButtons.takeRight(3)
      case _ => CreateActionList.actionButtons.dropRight(3)
    }
    if (buttons.size == 1) {
      buttons.head.strokeHit()
    } else {
      val popup = new TitlePopupMenu("Neues Element:")
      popup.addButtons(buttons)
      popup.peer.show(theCanvas, mousePos.x - canvasPos.x - 20, mousePos.y - canvasPos.y - 40)
    }
	}
	
	
	/** in AskPoint modus: check if there is a fitting point next to the mouse cursor 
	 * 
	 * @param screenPos current screen pos of the mouse cursor
	 * @return screen pos of a hitting element point, or null if no points arround
	 */
	def checkPointHit(screenPos:Point):MatchingScreenPoints = {
		val worldPoints: MatchingPoints =getNearestPoint(scaleModel.xToWorld(screenPos.x),scaleModel.yToWorld(screenPos.y))
		MatchingScreenPoints(optionToScreen(worldPoints.hitBoth),optionToScreen(worldPoints.hitX),
			optionToScreen(worldPoints.hitY),worldPoints.hitBoth)			  	
	}
	 
	 
	 def deselect():Unit = {
	  // println("Controller deselect "+Thread.currentThread.getStackTrace().take(5).mkString("\n")+"\n")	   
		 selectModel.deselect(false)
		 //pointSelectModel.deselect
	 }
	 
	 // FocusContainer
	 def containerName="Graph2DEdit"

  def ownerRef: Option[AbstractLayer] = layerModel.getActiveLayer
	  
	def shutDown(): Unit = {
    shuttingDown=true
    layerModel.removeAllLayers(true)
  }

  def clearNewElements(): Unit = {
    layerModel.newElemLayer.shutDown()
    showTempElements=false   
  }

  def setRelativeScale(scaleID: Int): Unit = scaleModel.setRelativeScaleID(scaleID)
	 
	 def scaleRatio:Double= scaleModel.relativeScaleValue

  def setActiveLayerScale(newRelativeScaleID: Int): Unit = layerModel.setActiveLayerScale(newRelativeScaleID)

  def editSelectedElemIPE(): Unit = {
	   if (lastHittedElements.size==1&& _viewportState==ViewportState.SelectState) {
	     val lastGroup=lastHittedElements.head._2
	     if(lastGroup.size==1)
	     lastGroup.head match {
	       case gel:GraphElem => startIPEMode(gel,None)
	     }
	   }
	 }

  def startIPEMode(el: GraphElem, listener: Option[String => Unit]): Unit =  {
	   Log.w("start IPE Mode:"+el+" listener:"+listener+" vps:"+viewportState)
	   if(_viewportState==ViewportState.InPlaceEdit) stopIPEMode()
	   else  el match {
	  	 case tel:TextElement=>
				 inplaceTextElement=Some(tel)
				 inplaceEditFinishListener=listener
				 if (ipePane != null) {
					 ipePane.resume()
					 ipePane.runLater{
						 ipePane.visible = true
						 ipePane.setValues(tel, scaleModel)
					 }
				 }
				 changeViewportState(ViewportState.InPlaceEdit)
			 case _ =>
				 inplaceTextElement=None
				 inplaceEditFinishListener=None
		 }
	 }

  override def stopIPEMode(): Unit = if (_viewportState == ViewportState.InPlaceEdit) {
	   val text=if(ipePane==null)"" else ipePane.getEditText
	   //println("Stop IPE Mode "+ Thread.currentThread().getStackTrace().take(4).mkString("\n"))
	   inplaceEditFinishListener match {
	     case Some(listener) =>
				 super.stopIPEMode()
				 //println("super Stop:"+_viewportState)
				 listener(text)
			 case None =>
				 val el= inplaceTextElement
				 super.stopIPEMode()
				 el match {
          case Some(te)=>
						if(te.ref==null) { // new element
              util.Log.e("ERROR: change empty text element")
            }
            else ClientQueryManager.writeInstanceField(te.ref, 1.toByte, StringConstant(text))
					case _ =>
        }
		 }
	   
	 }

  override def cleanUpIPEMode(): Unit = {
	   //println("Cleanup IPE")
	   ipePane.visible=false
		 ipePane.pause()
	   inplaceTextElement=None
	   inplaceEditFinishListener=None
	   super.cleanUpIPEMode()	   
	 }

  override def focusGained(): Unit = {
	   ColorFieldEditor.showEditor= !scaleModel.colorsFixed
	   super.focusGained()
	   scalePanel.activateKeyStrokes()
	 }

  def setupColorsFixed(newValue: Boolean): Unit = {
	  scaleModel.colorsFixed=newValue
	  scalePanel.colorsFixedBut.selected=newValue
	 }
	 
	 /** opens the Filter Dialog	  
	  */
   def filterButClicked(filterBut: ToggleButton): Unit = {
	   	if(filterBut.selected){
	   	  openFilterDialog()	   	  
	   	} else {
	   	  if(layerModel.viewFilter.isDefined) layerModel.viewFilter=None
	   	  canvas.repaint()
	   	}  
	 }

  def openFilterDialog(): Unit = {
	   val dialog=new SelectionFilterDialog(ClientApp.top,this)
		 SwingUtilities.updateComponentTreeUI(dialog.peer)
	   dialog.setLocationRelativeTo(scalePanel.filterBut)
	   dialog.visible=true
	 }

  def setSelectionFilter(info: SelectionFilterInfo): Unit = {
		 println("Filter: "+info)
	   if(selectModel.hasSelection){
	   	 selectModel.filter(info.elementMatch)
	   	 canvas.repaint()
	   	 scalePanel.filterBut.selected=false
	   }else {
	   	 layerModel.viewFilter=Some(info) 
	   	 canvas.repaint()
	   	 selectModel.notifyListeners()
	   }
	 }

  def cancelSelectionFilter(): Unit = {
	   if(layerModel.viewFilter.isDefined)layerModel.viewFilter=None
	   scalePanel.filterBut.selected=false
	   canvas.repaint()
	 }

  def checkElemBounds(elem: GraphElem, bounds: Rect2DDouble): Unit = {
    val eb = elem.getBounds(this)
	  if (eb.x<bounds.x)bounds.x=eb.x
		if (eb.y<bounds.y)bounds.y=eb.y
		// use the width fields as maxX and height as maxY
		if (eb.width> bounds.width)bounds.width=eb.width
		//print (" e.maxY:"+elem.maxY+" b.y:"+bounds.y+" b.h:"+bounds.height)
		if (eb.height> bounds.height)bounds.height=eb.height
	}
	 
	def importLayers(instData: client.dataviewer.InstanceSelection): Boolean = {
    var success=false
    for(elRef <-instData.selection;if Layer.allowedDisplayListTypes.contains(elRef.typ) &&
      !layerModel.containsRef(elRef)){
      val newLayer = Layer.createLayer(this, elRef, visible = true, edible = true, instData.pathFromRoot)
	    newLayer.load(Some(()=>Swing.onEDT{
	      layerModel.addLayer( newLayer)
	      layerModel.setActiveLayerIx(layerModel.getRowCount-1) 
	      zoomAll()
	    }),alwaysNotify = true)
	    success=true
	  }
    success
  }

	private def valueFromTuple(cell:Option[CellTuple]):Double= cell match {
		case Some(tuple)=>tuple.value.getValue.toDouble
			case None=> 0d
	}

  def importSpreadSheetData(spreadData: SpreadSheetTransferable): Unit = {
		 for (layer<-layerModel.getActiveLayer) {
			 val ownerRefArray=Array(layer.ownerRef)
       DialogManager.startIntermediateQuestion(DialogQuestion("Projektion", List(new AnswerDefinition("Projektion", DataType.EnumTyp, None, "Draufsicht,Ansicht X,Ansicht Y"))),
				 resList =>{
					 val proj=resList.head.result.toString match {
						 case "Draufsicht" => 0
						 case "Ansicht X" => 1
						 case "Ansicht Y" => 2
					 }
					 DialogManager.reset()
						 val elemBuffer=ArrayBuffer[(Int,Array[Expression])]()
					 for(row<-0 until spreadData.numRows){
						 val pointNr=valueFromTuple(spreadData.cellValues(row*4)).toInt
						 var xValue=valueFromTuple(spreadData.cellValues(row*4+1))
						 var yValue=valueFromTuple(spreadData.cellValues(row*4+2))* -1
						 val zValue=valueFromTuple(spreadData.cellValues(row*4+3))
						 if(proj==1) yValue = zValue else if(proj==2) {xValue=yValue;yValue=zValue}

						 val vector=new VectorConstant(xValue,yValue,0d)
						 elemBuffer+= ((41,Array(EMPTY_EX,EMPTY_EX,EMPTY_EX,vector,DoubleConstant(0.02d),EMPTY_EX,DoubleConstant(360d))))
						 elemBuffer+= ((44,Array(EMPTY_EX,StringConstant(pointNr.toString),vector + VectorConstant(0.04d,-0.04d,0d),EMPTY_EX,DoubleConstant(2d),
							 EMPTY_EX,EMPTY_EX,EMPTY_EX,EMPTY_EX,EMPTY_EX)))
					 }
					 ClientQueryManager.createInstances(ownerRefArray,elemBuffer)
				 })
		 }
	}
}