/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import java.awt.MouseInfo
import java.awt.event.{ComponentAdapter, ComponentEvent}
import java.awt.geom.Rectangle2D
import javax.swing.JLayeredPane

import client.comm.ClientQueryManager
import client.dataviewer.TitlePopupMenu
import client.dialog._
import client.spreadsheet.{CellTuple, SpreadSheetTransferable}
import client.ui.ClientApp
import definition.data._
import definition.expression._
import definition.typ.{DataType, AnswerDefinition, DialogQuestion}

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
    override lazy val peer=panel
  }

class GraphViewController extends AbstractViewController[(AbstractLayer, Iterable[GraphElem]),GraphElem] {
  val scaleModel=new ScaleModel
  val selectModel=new ViewSelectModel(this)
  val layerModel=new LayerTableModel(this)
	val canvas=new GraphViewCanvas(this)
  val theCanvas=canvas.peer
  val ddListener=new GraphViewDDListener(this)
  val transferHandler=new CommonTransferHandler(ddListener)
  theCanvas.setTransferHandler(transferHandler)
	val scalePanel=new ScalePanel(scaleModel,this)  
	
	var shuttingDown=false
	var inplaceTextElement:Option[TextElement]=None
	var inplaceEditFinishListener:Option[(String) => Unit]=None
	
  scaleModel.registerScaleListener(()=>{
  	refreshCanvas()
  })
  
  val layerPane=new LPane  
  private var ipePane:InplaceEditPanel=null
  
  layerPane.panel.add(canvas.peer,new java.lang.Integer(1))  
  
  ClientQueryManager.runInPool{
   {
     
     ipePane=new InplaceEditPanel(this)
     Swing.onEDT{
       ipePane.setup()       
       //println("ipe:"+ipePane+" peer:"+ipePane.peer)
       ipePane.visible=false
       layerPane.panel.add(ipePane.peer,new java.lang.Integer(2))
       //System.out.println("setup IPE "+layerPane.peer.getSize())
       ipePane.peer.setSize(layerPane.size)
       ipePane.setPaneSize(layerPane.size)
     }
     
   } 
  }  
	
	val canvasPanel=new BorderPanel {
		add(layerPane,BorderPanel.Position.Center)
		add(scalePanel,BorderPanel.Position.North)
	} 
	
	layerPane.peer.addComponentListener(new ComponentAdapter(){
	  override def componentResized(e:ComponentEvent) = {
	    //System.out.println("Resized: canvas:" +canvas.size+" pane:"+layerPane.size+" ipe:"+ipePane)
	    val si=layerPane.size
	    canvas.peer.setSize(si)
      if(ipePane!=null) {
  	    ipePane.peer.setSize(si)
  	    ipePane.setPaneSize(si)
      }
			scaleModel.viewSize=canvas.size		
	  } 
	})	  
  
	
	def layerChanged(lay:AbstractLayer,updateZoom:Boolean) =  if(!shuttingDown){
	  if(updateZoom) zoomAll()
	  else refreshCanvas()
	  selectModel.deselect(true)
	  //layerModel.fireTableDataChanged()
	}
	
	def graphElemAdded(lay:AbstractLayer,elem:GraphElem) = {
	  //println("graphElem added:"+elem+" numcr:"+numCreatedElements+" cas:"+hasCreateActionStarted)
	  if(hasCreateActionStarted) {
	    numCreatedElements-= 1
      selectModel.addSelection(Seq((lay,List(elem))),false)
	    if(numCreatedElements<1){
	      casReceived()	    	
	    }
	    //System.out.println("CAS graphElem Added "+elem)	    
	  }	    
		refreshCanvas()
	}
	
	def graphElemRemoved(lay:AbstractLayer,elem:GraphElem) = {
    //println("graphElem removed :"+elem)
		selectModel.elemRemoved((lay,Seq(elem)))
		if(lastHittedElements.exists(entry=> entry._2.exists(el=>el.ref==elem.ref) ))
		{  
		  lastHittedElements=lastHittedElements.map(entry=> if(entry._2.exists(el=>el.ref==elem.ref)){
		    (entry._1,entry._2.filter(_.ref!=elem.ref))
		  } else entry).filter(_._2.size>0)
		}    
		//lastHittedElements=Nil
		refreshCanvas()
	}	
	
	
	def graphElementsChanged(lay:AbstractLayer,newStates:Iterable[GraphElem],repaint:Boolean=true) = {
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
	def getAllBounds=	layerModel.calcAllLayerBounds()
	
	
	/*private def pointsInRect(points:TraversableOnce[VectorConstant],minX:Double,minY:Double,maxX:Double,maxY:Double):Boolean=
	  points.exists(p=> p.x>=minX && p.x<=maxX && p.y>=minY && p.y<=maxY)*/

	
	/** finds the elements that are in the given rectangle and selects them 
	 * in the select model
	 * 
	 * @param startPoint
	 * @param endPoint
	 * @param add
	 */
	def checkSelection(minX:Double,minY:Double,maxX:Double,maxY:Double,onlyInside:Boolean,control:Boolean):Unit= {
	  lastHittedElements=Nil		
	  val elemList= if(onlyInside) layerModel.filterLayersSelection(true,(e)=>{
	  		val eb=e.getBounds(this)
	  		eb.x>=minX && eb.width<=maxX && eb.y>=minY && eb.height<=maxY
	  	})
	  	else {
	  	  val rect=new Rectangle2D.Double(minX,minY,maxX-minX,maxY-minY)
	  	  layerModel.filterLayersSelection(true,_.intersectsRect(this, rect))
	  	}	
	  //System.out.println("check selection result="+elemList)
	  if(control) {	  	
	  	selectModel.addSelection(elemList,false)
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
	
	
	
	
	def getChoosableElements(onlyEdible:Boolean,clickPosX:Double,clickPosY:Double):Iterable[GraphElem]={
	  val lcd=getCurrentLineCatchDistance
	  if(objSelectMode==ObjectSelectMode.TempObject) 
	     layerModel.newElemLayer.filterSelection(false,_.hits(/*layerModel.newElemLayer*/this,clickPosX,clickPosY,lcd)) 
	  else if(objSelectClassConstraints==null) {
      util.Log.e("ObjSelectClassConstraints == null")
			    Nil
			  }
	  else { layerModel.filterLayersSelection(onlyEdible,(el)=>el.hits(this,clickPosX,clickPosY,lcd) ).
	    flatMap(_._2).filter(a=>(a.ref!=null)&& objSelectClassConstraints. contains(a.ref.typ)&&
	        (if(objSelectMode==ObjectSelectMode.SingleObjectNotSelected) !selectModel.selectionList.contains(a) else true))
	  } 
	}
	
	def filterSelection(clickPosX:Double,clickPosY:Double,lcd:Double)= layerModel.filterLayersSelection(true,(el)=>el.hits(this,clickPosX,clickPosY,lcd) )
	
	def processElementClick(clickPosX:Double,clickPosY:Double,hittedElements:Iterable[GraphElem],editable:Boolean):Unit = {
	  //println("processElementClick "+hittedElements.mkString(", "))
	  try {
	  objSelectMode match {
	    case ObjectSelectMode.TempObject =>
				for(o<-objSelectListener) o.tempObjectSelected(hittedElements.head.ref.instance)
				clearNewElements()
			case ObjectSelectMode.EdgeAndPoint =>
				val hPoint=new VectorConstant(clickPosX,clickPosY,0)
				for(o<-objSelectListener) o.objectsSelectedWithPoint(ObjectReference(hittedElements.head.ref),hPoint,editable)
				//Swing.onEDT{changeViewportState(ViewportState.SelectState)} // execute after seconnd question is loaded
			case ObjectSelectMode.SegmentPart => if(editable){
	  	  //println("segment part :"+editable)
	  		val hPoint=new VectorConstant(clickPosX,clickPosY,0)
	  		layerModel.getSegmentPart(hittedElements.head,hPoint) match {
	  			case Some((p1,p2))=>for(o<-objSelectListener)
						o.segmentPartSelected(ObjectReference(hittedElements.head.ref),p1,p2)
					case None => //DialogManager.reset
	  		}
	  	}
	  	case ObjectSelectMode.SingleObject|ObjectSelectMode.SingleObjectNotSelected =>
				for(o<-objSelectListener) o.objectsSelected(ObjectReference(hittedElements.head.ref),editable)
		}
	  } catch {case NonFatal(e)=> ClientQueryManager.printErrorMessage(e.toString())
		case other:Throwable =>println(other);System.exit(0)}
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

	override def getElementByRef(ref:Reference)= layerModel.getElementByRef(ref)

	
	def deselectZoomInBut()= {
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
	
	def showCreatePopup()= {
    val mousePos = MouseInfo.getPointerInfo().getLocation()
    val canvasPos = theCanvas.getLocationOnScreen()
		requestFocus()
    //println("Show Create "+NewButtonsList.actionButtons.map(_.commandName).mkString(", ")+" mousePos:"+mousePos+" canvasPos:"+canvasPos)
    val buttons = layerModel.getActiveLayer match {
      case Some(m: MeasureLayer) => NewButtonsList.actionButtons.takeRight(1)
      case _ => NewButtonsList.actionButtons.dropRight(1)
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
		val worldPoints=getNearestPoint(scaleModel.xToWorld(screenPos.x),scaleModel.yToWorld(screenPos.y))
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
	 def containerRef=layerModel.getActiveLayer	 
	  
	 /** Format Field Values to give to a new created Object
   * @param forType class type of the object to create
   * @return list of (formatfieldNr,FieldValue) 
   */
  
  
  def shutDown() = {
    shuttingDown=true
    layerModel.removeAllLayers(true)
  }
	 
	 def clearNewElements()= {
    layerModel.newElemLayer.shutDown
    showTempElements=false   
  }
	 
	 def setRelativeScale(scaleID:Int )= scaleModel.setRelativeScaleID(scaleID)
	 
	 def scaleRatio:Double= scaleModel.relativeScaleValue
	 
	 def setActiveLayerScale(newRelativeScaleID:Int)= layerModel.setActiveLayerScale(newRelativeScaleID)
	 
	 def editSelectedElemIPE()= {	   
	   if (lastHittedElements.size==1&& _viewportState==ViewportState.SelectState) {
	     val lastGroup=lastHittedElements.head._2
	     if(lastGroup.size==1)
	     lastGroup.head match {
	       case gel:GraphElem => startIPEMode(gel,None)
	     }
	   }
	 }
	 
	 def startIPEMode(el:GraphElem,listener:Option[(String)=>Unit]) = if(ipePane!=null){
	   //println("start IPE Mode:"+el+" listener:"+listener)
	   if(_viewportState==ViewportState.InPlaceEdit) stopIPEMode()
	   else  el match {
	  	 case tel:TextElement=>
				 inplaceTextElement=Some(tel)
				 inplaceEditFinishListener=listener
				 ipePane.visible=true
				 ipePane.setValues(tel,scaleModel)
				 changeViewportState(ViewportState.InPlaceEdit)
			 case _ =>
				 inplaceTextElement=None
				 inplaceEditFinishListener=None
		 }
	 }
	 
	 override def stopIPEMode() = if(_viewportState==ViewportState.InPlaceEdit){	   
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
            else ClientQueryManager.writeInstanceField(te.ref,1.toByte,new StringConstant(text))
					case _ =>
        }
		 }
	   
	 }
	 
	 override def cleanUpIPEMode() = {
	   //println("Cleanup IPE")
	   ipePane.visible=false
	   inplaceTextElement=None
	   inplaceEditFinishListener=None
	   super.cleanUpIPEMode()	   
	 }
	 
	 override def focusGained()= {
	   ColorFieldEditor.showEditor= !scaleModel.colorsFixed
	   super.focusGained()
	   scalePanel.activateKeyStrokes()
	 }
	 
	 def setupColorsFixed(newValue:Boolean)= {
	  scaleModel.colorsFixed=newValue
	  scalePanel.colorsFixedBut.selected=newValue
	 }
	 
	 /** opens the Filter Dialog	  
	  */
	 def filterButClicked(filterBut:ToggleButton)= {
	   	if(filterBut.selected){
	   	  openFilterDialog()	   	  
	   	} else {
	   	  if(layerModel.viewFilter.isDefined) layerModel.viewFilter=None
	   	  canvas.repaint()
	   	}  
	 }
	 
	 def openFilterDialog()={
	   val dialog=new SelectionFilterDialog(ClientApp.top,this)
	   dialog.setLocationRelativeTo(scalePanel.filterBut)
	   dialog.visible=true
	 }	
	 
	 def setSelectionFilter(info:SelectionFilterInfo)= {
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
	 
	 def cancelSelectionFilter()={
	   if(layerModel.viewFilter.isDefined)layerModel.viewFilter=None
	   scalePanel.filterBut.selected=false
	   canvas.repaint()
	 }
	 
	 def checkElemBounds(elem:GraphElem,bounds:Rectangle2D.Double):Unit = {
		val eb=elem.getBounds(this)		
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
	    val newLayer=Layer.createLayer(this,elRef,true,true)
	    newLayer.load(Some(()=>Swing.onEDT{
	      layerModel.addLayer( newLayer)
	      layerModel.setActiveLayerIx(layerModel.getRowCount-1) 
	      zoomAll()
	    }),true)
	    success=true
	  }
    success
  }

	private def valueFromTuple(cell:Option[CellTuple]):Double= cell match {
		case Some(tuple)=>tuple.value.getValue.toDouble
			case None=> 0d
	}

	def importSpreadSheetData(spreadData:SpreadSheetTransferable)= {
		 for (layer<-layerModel.getActiveLayer) {
			 val ownerRefArray=Array(layer.ownerRef)
			 DialogManager.startInterQuestion(new DialogQuestion("Projektion",List(new AnswerDefinition("Projektion",DataType.EnumTyp,None,"Draufsicht,Ansicht X,Ansicht Y"))),
				 (resList)=>{
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