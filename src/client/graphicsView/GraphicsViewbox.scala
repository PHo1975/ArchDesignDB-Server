/**
 * Author: Peter Started:10.11.2010
 */
package client.graphicsView

import client.layout._
import definition.comm.{IntValue, PropertyGroup}
import definition.data.Referencable
import definition.typ.SelectGroup
import scala.swing._
import client.dialog.{SelectEventDispatcher,NewButtonsList}
import scala.swing.event.ButtonClicked
import javax.swing.BorderFactory
import javax.swing.border._
import javax.swing.event.{TableModelListener,TableModelEvent}
import client.comm.ClientQueryManager
import client.dialog.NewButtonsList
import scala.util.control.NonFatal



/** a viewbox for graphic content
 * 
 */
class GraphicsViewbox extends BorderPanel with ViewboxContent {
  //val time=System.currentTimeMillis()
  val storeFactor=10000
	
	val graphViewController:GraphViewController=new GraphViewController
  //println("controller created "+(System.currentTimeMillis()-time))
	val layerPanController:LayerPanelController=new LayerPanelController(graphViewController)
  
	var boxOpen:Boolean=true
	//val layerLabel:Label=new Label()
	val emptyBox=Swing.HStrut(0)
	var viewbox:Viewbox=null	
	
	
	val layerScrollPane:ScrollPane = new ScrollPane() {
			//preferredSize=new Dimension(200,200)
			viewportView=layerPanController.layerTable			
			
			graphViewController.layerModel.registerSizeChangeListener(callback)
			def callback(nsize:Int) = {
				preferredSize=new Dimension(10,Math.round(((if(nsize==0)1d else nsize.toDouble)+1.5)*LayerPanelController.lineHeight).toInt)
				maximumSize=preferredSize
				GraphicsViewbox.this.revalidate()
				GraphicsViewbox.this.peer.invalidate()
			}
		}  
	border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)
  graphViewController.layerModel.addTableModelListener(new TableModelListener (){
		def tableChanged(e:TableModelEvent) = {
			if(viewbox!=null)viewbox.setTitle(graphViewController.layerModel.getLabelText)
		}
	})
	
	val switchLayerButton:Button=new Button("\u02c4")
	switchLayerButton.tooltip="Layerliste zusammenklappen"
	
	val layerBox=new BorderPanel{
		add(layerScrollPane,BorderPanel.Position.Center)
		add(new BoxPanel(Orientation.Vertical) {
			contents+= Swing.VGlue += switchLayerButton
		},BorderPanel.Position.West)
		listenTo(switchLayerButton)
		reactions += {
			case ButtonClicked(`switchLayerButton`) =>
				switchBox()
		}		
	}
	
	add(graphViewController.canvasPanel,BorderPanel.Position.Center)
	add(layerBox,BorderPanel.Position.North)  
	graphViewController.selectModel.registerSelectListener(SelectEventDispatcher)
	graphViewController.registerContainerListener(NewButtonsList)
	switchLayerButton.margin=new Insets(0,0,0,0)
	switchLayerButton.focusable=false
	
  def open(readyListener:()=>Unit,sourceBoxSelection:AnyRef): Unit = {
		sourceBoxSelection match {
			case e:Iterable[_]=>e.headOption match {
				case Some(selGroup:SelectGroup[_])=>{
					val layerList=selGroup.children.filter(a=>Layer.allowedDisplayListTypes.contains(a.ref.typ)).toSeq
					if(layerList.nonEmpty) graphViewController.layerModel.openLayers(layerList,Some(readyListener))
				}
				case _=>
			}
			case _=>
		}
	}

  def close(): Unit = 	shutDown()
  	
  
  def storeSettings(pgroup:PropertyGroup) = {
    graphViewController.layerModel.storeSettings(pgroup)
    pgroup.addProperty(new IntValue("WX", (graphViewController.scaleModel.world_X*storeFactor).toInt))
    pgroup.addProperty(new IntValue("WY", (graphViewController.scaleModel.world_Y*storeFactor).toInt))
    pgroup.addProperty(new IntValue("WW", (graphViewController.scaleModel.world_Width*storeFactor).toInt))
    pgroup.addProperty(new IntValue("WH", (graphViewController.scaleModel.world_Height*storeFactor).toInt))
    val (sc1,sc2)=graphViewController.scaleModel.relativeScale
    pgroup.addProperty(new IntValue("s1", (sc1 *100d).toInt))
    pgroup.addProperty(new IntValue("s2", (sc2 *100d).toInt))
    pgroup.addProperty(new IntValue("fs",if(graphViewController.scaleModel.colorsFixed) 1 else 0))
  }
  
  
  def restoreSettings(pgroup:PropertyGroup,doneListener:()=>Unit)= try{
    //println( "Graphviewbox start restore "+(System.currentTimeMillis()-time))
    val WX=pgroup.getIntProperty("WX").toDouble/storeFactor
    val WY=pgroup.getIntProperty("WY").toDouble/storeFactor
    val WW=pgroup.getIntProperty("WW").toDouble/storeFactor
    val WH=pgroup.getIntProperty("WH").toDouble/storeFactor
    val s1=pgroup.getIntProperty("s1")
    val s2=pgroup.getIntProperty("s2")
    val fs=pgroup.getIntProperty("fs")
    
    def layersLoaded():Unit = Swing.onEDT{
      graphViewController.setupColorsFixed(fs==1)
      graphViewController.scaleModel.relativeScale=(s1.toDouble/100d,s2.toDouble/100d)
      graphViewController.scaleModel.setWorldBounds(WX,WY,WW,WH)        
      if(doneListener!=null)
        doneListener() 
      layerPanController.layerTable.repaint()
      //val now=System.currentTimeMillis()
      //println("restore GraphicsView "+ (now-time))
    }
    graphViewController.layerModel.restoreSettings(pgroup,Some(layersLoaded _))
    
  } catch {case NonFatal(e)=>util.Log.e("restoreSetting pgroup:"+pgroup,e);doneListener()
	case other:Throwable =>println(other);System.exit(0)}


  def setViewbox(box: Viewbox): Unit = { viewbox=box }

  
  def shutDown() = {
  	graphViewController.shutDown()
    layerPanController.shutDown()
  	//SelectEventDispatcher.removeSelectListener(layerPanController)
  }
  
  
  def switchBox():Unit = {
  	if(boxOpen) { // close  	  		
  		viewbox.setTitle(graphViewController.layerModel.getLabelText)
			//switchLayerButton.tooltip="Layerliste zusammenklappen"
  		add(emptyBox,BorderPanel.Position.North)
  		viewbox.minimizeHeaderPanel(switchBox _)
  		revalidate()
  	}else {  		
  		add(layerBox,BorderPanel.Position.North)
			//switchLayerButton.tooltip="Layerliste aufklappen"
  		revalidate()
  	}
  	boxOpen= !boxOpen
  }
  def typeID:String = "graphics"

	def selectedItems=graphViewController.selectModel.list
}