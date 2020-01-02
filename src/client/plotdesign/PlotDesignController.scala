package client.plotdesign
import java.awt.datatransfer.{DataFlavor, Transferable}
import java.awt.event.{ComponentAdapter, ComponentEvent}
import java.awt.geom.{Point2D, Rectangle2D}
import java.awt.{Color, Graphics2D, Point}

import client.comm.ClientQueryManager
import client.dataviewer.InstanceSelection
import client.dialog._
import client.graphicsView.{GraphElem, MatchingScreenPoints, ScaleModel, ViewportState}
import client.print.{APageable, PreviewWindow, PrintReceiver}
import client.ui.ClientApp
import definition.data.{EMPTY_REFERENCE, OwnerReference, Reference}
import definition.expression.{DoubleConstant, ObjectReference}
import definition.typ.SystemSettings
import javax.print.attribute.standard.PageRanges
import javax.swing.{JComponent, TransferHandler}
import util.Log

import scala.swing.Swing
import scala.swing.event.{Key, KeyPressed}




class PlotDesignController(editor:PlotDesignEditor) extends AbstractViewController[LayerRef,LayerRef] with DragDropListener[InstanceSelection] with PrintReceiver {
  
  val scaleModel=new ScaleModel
  val pageModel=new PageModel(this)
  val headerPanel=new PDHeaderPanel(this)
  var loadedRef:Option[Reference]=None
  val selectModel=new PDSelectModel(this)
  var doneListener: ()=>Unit = null
  var dotPitch:Double=0.25  
  val layerRefList=new LayerRefList(this)
  val extraGraphicsList=new ExtraGraphicsList(this)
  var layerTypes:Seq[Int]= Seq.empty
  var layerRefType:Int = -1
  def layerModel: LayerRefList =layerRefList
  val canvas=new PDCanvas(this)
  def theCanvas: JComponent =canvas.peer
  //var createdRef:Option[Reference]=None // latest created LayerRef
	val createDropPos=new Point2D.Double	
	val allScreenBounds=new Rectangle2D.Double  
	val canvasTransferHandler=new CommonTransferHandler(this)
	lazy val previewDialog=new PreviewWindow(ClientApp.top)
  
  var dropLayers:Seq[DropInfo]=Nil // dropped Layers
  var dropRefs:Seq[Reference]=Nil
  var dropMousePos:Option[Point]=None
  
  canvas.peer.setTransferHandler(canvasTransferHandler)
  canvas.peer.addComponentListener(new ComponentAdapter(){
  	override def componentResized(e:ComponentEvent): Unit = {
  		scaleModel.viewSize=canvas.size
  		//zoomAll()
  	}
  })
  ClientQueryManager.registerSetupListener(()=>{
    println("Plot setup")
  	layerTypes=Seq(SystemSettings().systemTypes("Layer"),SystemSettings().systemTypes("MeasureLayer"))
  	layerRefType=SystemSettings().systemTypes("LayerPlotRef")
		println("Plot Setup done")
  })
  scaleModel.registerScaleListener(()=>{
  	canvas.repaint()
  })
  
  selectModel.registerSelectListener(SelectEventDispatcher)


  def scaleRatio=1d
  
  
  def load(ref:Reference,loadDoneListener: ()=>Unit): Unit = {
    //println("PlotDesign load:"+ref)
    for(ref<-loadedRef) shutDown()
    loadedRef=Some(ref)
    selectModel.setOwner(ref)
    pageModel.load(ref,pageModelLoaded _)
    
    def pageModelLoaded():Unit = Swing.onEDT{
      //println("pageModel loaded")
      layerRefList.load(ref,layerRefsLoaded _)
    }
    
    def layerRefsLoaded():Unit = {
      //println("LayersLoaded")
      layerRefList.calcBounds()
      //println("layerBounds "+layerRefList.bounds)
      zoomAll()
      loadDoneListener()
    }
  }   
  
  
  def shutDown(): Unit = for(ref<-loadedRef){
    //println("Controller shutdown ")
    pageModel.shutDown()
    layerRefList.shutDown()
    loadedRef=None
  }
  
  
   def checkSelection(minX:Double,minY:Double,maxX:Double,maxY:Double,onlyInside:Boolean,control:Boolean):Unit= {
	  lastHittedElements=Nil		
	  val elemList=	if(onlyInside) layerRefList.filterSelection(e=>{
	  		val eb=e.screenBounds
	  		eb.x>=minX && eb.width<=maxX && eb.y>=minY && eb.height<=maxY
	  	})
	  	else layerRefList.filterSelection(e=>{
	  		val eb=e.screenBounds
	  		eb.width>=minX && eb.x<=maxX && eb.height>=minY && eb.y<=maxY	  	
	  	})	  
	  if(control) {	      
	  	  selectModel.addSelection(elemList,toggle = false)
	  } else {
	  	if (elemList.isEmpty)selectModel.deselect(true)
	  	else selectModel.setSelection(elemList)
	  }	  
	}  
   
   
  def updatePage(): Unit = {
    layerRefList.calcBounds()
    zoomAll()
    canvas.repaint()
  } 
  
  def layerChanged(layerRef:LayerRef): Unit = {
    selectModel.elementChanged(layerRef)
    
    layersChanged()
  }
  
  def extraGraphicsChanged(): Unit = {
    calcAllBounds()
    canvas.repaint()
  }
  
  def layersChanged(): Unit = {
    //println("LayersChanged pageBounds:"+pageModel.getPageBounds+" clipRect:"+pageModel.clipRect)
    layerRefList.calcBounds()
    
    //println("layerBounds "+layerRefList.bounds)
    canvas.repaint()
  }
  
  def getAllBounds: Rectangle2D.Double =allScreenBounds
  
  def calcAllBounds():Unit = {
    allScreenBounds.setRect(layerRefList.bounds)
    if(extraGraphicsList.screenBounds.x<allScreenBounds.x) {
      allScreenBounds.width+=allScreenBounds.x- extraGraphicsList.screenBounds.x
      allScreenBounds.x=extraGraphicsList.screenBounds.x
    }
    if(extraGraphicsList.screenBounds.y<allScreenBounds.y) {
      allScreenBounds.height+=allScreenBounds.y- extraGraphicsList.screenBounds.y
      allScreenBounds.y=extraGraphicsList.screenBounds.y
    }
    if(extraGraphicsList.screenBounds.x+extraGraphicsList.screenBounds.width>allScreenBounds.x+allScreenBounds.width) {
      allScreenBounds.width+=extraGraphicsList.screenBounds.x+extraGraphicsList.screenBounds.width-(allScreenBounds.x+allScreenBounds.width)
    }
    if(extraGraphicsList.screenBounds.y+extraGraphicsList.screenBounds.height>allScreenBounds.y+allScreenBounds.height) {
      allScreenBounds.height+=extraGraphicsList.screenBounds.y+extraGraphicsList.screenBounds.height-(allScreenBounds.y+allScreenBounds.height)
    }
  }
  
  def layerCreatedAndLoaded(ncrRef:Reference,middleX:Double,middleY:Double): Unit = /*for(crRef <-createdRef)*/{
    //if(ncrRef != crRef) System.out.println("ncrRef:"+ncrRef +" not equal crRef:"+crRef) 
    //else {
    	//System.out.println("Layer created and loaded crRef:"+crRef+" mx:"+middleX+" my:"+middleY)
    	ClientQueryManager.writeInstanceField(ncrRef,7,new DoubleConstant(createDropPos.x-middleX))
    	ClientQueryManager.writeInstanceField(ncrRef,8,new DoubleConstant(createDropPos.y-middleY))
    //}
    //createdRef=None
   
  }
  
  def layerRemoved(layerRef:LayerRef): Unit = {
    selectModel.elemRemoved(layerRef)
    canvas.repaint()
  }
  
  def addLayers(layerRefs:Iterable[Reference],pos:Point): Unit = if(layerRefs.nonEmpty){
    createDropPos.x=scaleModel.xToWorld(pos.x)
    createDropPos.y=scaleModel.yToWorld(pos.y)
    
    //System.out.println("Dropped "+layerRefs.mkString(", ")+" at Pos:" +createDropPos)
    var inst= -1
    for(ref<-loadedRef;newRef<-layerRefs) {
    	inst = ClientQueryManager.createInstance(layerRefType,Array(new OwnerReference(1.toByte,ref)))
    	val createdRef=/*Some*/ new Reference(layerRefType, inst)
    	//println("Dropped layer:"+layerRefs.head+" layerRef:"+createdRef)
    	ClientQueryManager.writeInstanceField(createdRef,0,new ObjectReference(newRef.typ,newRef.instance))
    }
  }
  
  
  def deselectZoomInBut(): Unit =headerPanel.zoomInBut.selected=false
	
	  
  def processElementClick(clickPosX:Double,clickPosY:Double,hittedElements:Iterable[LayerRef],editable:Boolean):Unit = {
    for(o<-objSelectListener) o.objectsSelected(ObjectReference(hittedElements.head.ref),editable)	  
  } 
  
  def getChoosableElements(onlyEdible:Boolean,clickPosX:Double,clickPosY:Double):Seq[LayerRef]= {
    val lcd=getCurrentLineCatchDistance
    layerRefList.filterSelection(_.hits(clickPosX,clickPosY,lcd) ).
		      filter(a=>objSelectClassConstraints.iterator.contains(a.ref.typ))
  }
  
  def filterSelection(clickPosX:Double,clickPosY:Double,lcd:Double):Seq[LayerRef]= {
    layerRefList.filterSelection(_.hits(clickPosX,clickPosY,lcd) )
  }
  
  def getFirstHittedElement(hittedElements:Iterable[LayerRef]):LayerRef= hittedElements.head
	
	def keyPressed(e:KeyPressed): Unit = {
		//System.out.println("key typed "+e)
	  //resetCAS()
		_viewportState match {
			case ViewportState.SelectState =>
			e.key match {
				case Key.Left => scaleModel.moveLeft()
				case Key.Right => scaleModel.moveRight()
				case Key.Up => scaleModel.moveUp()
				case Key.Down => scaleModel.moveDown()
				case Key.Escape => cancelModus()
				case _ => //System.out.println(e)
		  }
			case ViewportState.AskPoint |  ViewportState.AskPointOrObject => 
			e.key match {				
				case Key.Escape => DialogManager.reset()
				case _ => //System.out.println(e)
			}
			case o => // System.out.println(o)
		}		
	}
	
		
	
	def containerName="Graph2DEdit"
	 def ownerRef: Option[Reference] =loadedRef//.getOrElse(EMPTY_REFERENCE)
	 
	 
	 def addTempElem( newElem:GraphElem):Unit= {
	  extraGraphicsList.addTempElem(newElem)
	}
	 
	
	
	 def deselect():Unit = {
	   lastHittedElements=Nil
		 selectModel.deselect(false)
		 resetCustomDragger()
		 dropLayers=Nil
		 dropRefs=Nil
		 //pointSelectModel.deselect
	 }
	 
	def clearNewElements(): Unit = {
	  extraGraphicsList.clearTempList()
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
	
		
	def getNextHittedElementNr(hittedElements:Iterable[LayerRef],lastNr:Int):(Int,LayerRef)= {
	  val numElements=hittedElements.size
	  if(lastNr>=numElements-1) (0,hittedElements.head)
	  else {
	    val nextNr=lastNr+1
	    (nextNr,hittedElements.toSeq(nextNr))
	  }
	}
	
	def print(): Unit = for(ref<-loadedRef){
	  ClientQueryManager.executeAction(new OwnerReference(0,EMPTY_REFERENCE),Seq(ref),"Ausgabe",Seq())
	}
	
	def showArchive(): Unit = {
	  for(r<-loadedRef) {
	    val odefRef=ClientQueryManager.queryInstance(r,0)
	    //rintln("show Archive DesignRef:"+r+" odefs:"+odefRef.mkString("| "))
	    if(odefRef.nonEmpty)
	    previewDialog.showArchive("Plot-Archiv",odefRef.head.ref,this)  
	  }
	  
	}

	def showVersions(): Unit =
		for(r<-loadedRef; if editor.pathController!=null)
			 editor.pathController.addPathElement(r,withCustomEditor = false)

	
	
	///  ******************************************    DRAG & DROP
	
	
	def sourceActions:Int = TransferHandler.NONE 
  def DDfactory() =null
  def canImport(action:Int,data:Transferable,pos:TransferHandler.DropLocation):Boolean = {
	  data match {
	    case instData:InstanceSelection=>
				if(! instData.selection. exists(aref=>layerTypes.contains(aref.typ))) {
          util.Log.e("no layer type ")
					false
        }
        else {
					showDrop(instData,pos.getDropPoint)
					true
				}

			case other=> ClientQueryManager.printErrorMessage("Falscher DragDrop-Typ "+other);Log.e("falscher Drop-Typ "+other);dropRefs=Nil; false
	  }	  
	}
	
	
	def showDrop(instData:InstanceSelection,mousePos:Point): Unit = {
	  val layerRefs=instData.selection. filter(aref=>layerTypes.contains(aref.typ)).toSeq	  
	  dropLayers= if(dropRefs==layerRefs) dropLayers else {
	    dropRefs=layerRefs	    
	    dropMousePos=None	    
	    dropRefs.map(dref=>new DropInfo(ClientQueryManager.queryInstance(dref, -1).head,this))
	  }	  
	  for(pos<-dropMousePos) drawDropRects(pos)	  
	  drawDropRects(mousePos)
	  dropMousePos=Some(mousePos)
	}
	
	def drawDropRects(pos:Point): Unit ={
	  val g:Graphics2D=canvas.peer.getGraphics.asInstanceOf[Graphics2D]
	  g.setXORMode(Color.white)
		g.setPaint(Color.black)
	  for(dl<-dropLayers) {		    
	    val screenWidth=dl.paperWidth*scaleModel.scale/2
	    val screenHeight=dl.paperHeight*scaleModel.scale/2
	    val mx= pos.x
	    val my= pos.y
	    g.drawLine((mx-screenWidth).toInt,(my-screenHeight).toInt,(mx+screenWidth).toInt, (my-screenHeight).toInt)
	    g.drawLine((mx-screenWidth).toInt,(my+screenHeight).toInt,(mx+screenWidth).toInt, (my+screenHeight).toInt)
	    g.drawLine((mx-screenWidth).toInt,(my-screenHeight).toInt,(mx-screenWidth).toInt, (my+screenHeight).toInt)
	    g.drawLine((mx+screenWidth).toInt,(my-screenHeight).toInt,(mx+screenWidth).toInt, (my+screenHeight).toInt)
	  }  
	}
	
	
  def importData(action:Int, data:Transferable,pos:TransferHandler.DropLocation):Boolean = {
    dropLayers=Nil
    dropRefs=Nil
    resetCustomDragger()
    data match {
	    case instData:InstanceSelection=> action match {
  			case TransferHandler.MOVE |
  			 TransferHandler.COPY | TransferHandler.LINK =>
					addLayers(instData.selection.filter(aref=>layerTypes.contains(aref.typ)),pos.getDropPoint)
					true
				case _ => false
  		}  
	  }
  
  }
  lazy val flavors: Array[DataFlavor] =Array(InstanceSelection.flavor)
  
  
  // interface Print Receiver
  
  def printOut(pageable:APageable,copies:Int,pages:PageRanges):Unit = {
    println("print out Plot Design not implemented")
  }
  def printerName:String=pageModel.printer
  def mediaName:String=pageModel.selectedMedia.toString
  
}