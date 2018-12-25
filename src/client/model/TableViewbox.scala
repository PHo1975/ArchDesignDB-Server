/**
 * Author: Peter Started:10.11.2010
 */
package client.model

import java.awt.Color
import java.awt.event.{ActionEvent, KeyEvent}

import client.dataviewer.DataViewController
import client.dialog.{NewButtonsList, SelectEventDispatcher}
import client.layout.{Viewbox, ViewboxContent}
import client.ui.ClientApp
import definition.comm.{ListValue, PropertyGroup}
import definition.data.{InstanceData, Reference}
import definition.typ.SelectGroup
import javax.swing._
import util.MyListView

import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, Component, Container, Dimension, Insets, Orientation, Panel, SequentialContainer, Swing}
import scala.util.control.NonFatal


class AdaptedScroller(aView:Component) extends Component with Container {
   override lazy val peer: JScrollPane = new JScrollPane with SuperMixin {
		 override def getMaximumSize(): Dimension = new Dimension(Short.MaxValue, aView.peer.getPreferredSize.height + 1)

		 override def getPreferredSize(): Dimension = aView.peer.getPreferredSize

		 override def getMinimumSize(): Dimension = new Dimension(20, aView.peer.getPreferredSize.height + 1)
    }    
    //yLayoutAlignment=1d     
    peer.setViewportView(aView.peer)
    def contents=List(aView)
    border=null
}

trait AbstractTableViewbox extends ViewboxContent {
	def pathController: PathController

	def goUp(): Unit
}

/**
 * 
 */
class TableViewbox extends BoxPanel(Orientation.Vertical) with AbstractTableViewbox {
	//println("new view box "+Thread.currentThread().getName())
	val dataviewController=new DataViewController(this)
	
	val pathMod=new PathModel()
	val pathView=new MyListView[InstanceData]()		
	pathView.xLayoutAlignment=0d
	pathView.yLayoutAlignment=1d
	var viewbox: Viewbox = _
	
	val pathController:PathController=new PathController(pathMod,pathView,dataviewController)
	pathController.registerSizeChangeListener((a) => {viewbox.setTitle(pathMod.getTitleText)})
	var pathBoxOpen=true
	val bookmarkDialog=new BookmarkDialog(ClientApp.top)
	preferredSize=new Dimension(200,200)
	border=BorderFactory.createEmptyBorder(2,2,2,2)//createEtchedBorder(EtchedBorder.LOWERED);
  
  val switchPathButton=new Button("\u02c4")
  switchPathButton.peer.putClientProperty("JComponent.sizeVariant", "small")
  switchPathButton.peer.updateUI()
	switchPathButton.xLayoutAlignment=1d
	switchPathButton.yLayoutAlignment=1d	
	switchPathButton.maximumSize=new Dimension(30,30)
	val glue=Swing.VGlue
	glue.opaque=true
	glue.background=Color.green
	glue.xLayoutAlignment=0d
	
	listenTo(switchPathButton)
	reactions += {
		case ButtonClicked(`switchPathButton`) =>
			switchPathBox()
	}
	
	val scr=new AdaptedScroller(pathView)
	
	val pathBox=new Panel with SequentialContainer.Wrapper{
		override lazy val peer: javax.swing.JPanel = {
	    val p = new javax.swing.JPanel with SuperMixin {
				override def getPreferredSize: Dimension = new Dimension(100, pathView.peer.getPreferredSize.height + 1)
	    }
	    val l = new javax.swing.BoxLayout(p, Orientation.Horizontal.id)
	    p.setLayout(l)
	    p
	  }	 
	  xLayoutAlignment=0d	  	  
	  contents+= switchPathButton += scr				
	}
	
  switchPathButton.margin=new Insets(0,0,0,0)
	switchPathButton.focusable=false 
  dataviewController.registerSelectListener(SelectEventDispatcher)
  dataviewController.registerContainerListener(NewButtonsList)
  contents+=pathBox+=dataviewController.splitBox//+=glue  
  dataviewController.formPanel.bookmarkButListener=showBookmarkDialog _
	val goUpActionString="Go Up"	
	peer.getInputMap( JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_UP,0 ), goUpActionString)
	peer.getActionMap().put(goUpActionString,new AbstractAction{
		def actionPerformed(e: ActionEvent): Unit = goUp()
	})

	def goUp(): Unit = if (pathMod.getSize > 0)
	     pathController.selectionChanged(pathMod.getSize-1)	
  
  /** opens a new box */
	def open(readyListener:()=>Unit,sourceBoxSelection:AnyRef): Unit ={
    if(BookmarkFactory.standardPath.isEmpty)util.Log.e("StandardPath is empty !")
    pathController.loadPath(BookmarkFactory.standardPath,readyListener)
  } 	    

  def close(): Unit =  shutDown()

	def storeSettings(pgroup: PropertyGroup): Unit = {
  	if(pathMod.dataList.nonEmpty)
  	//PathFactory.releasePathEntry(pathFactoryIndex, pathController.model.dataList.get.map(_.ref))
  	pgroup.addProperty(new ListValue[Reference]("path",pathMod.dataList.map(a=>a.ref)))
  }  
  
  def restoreSettings(pgroup:PropertyGroup,listener:()=>Unit): Unit = {
		println("Table restore settings")
    val pathList=if(pgroup.containsProperty("path")) pgroup.getListProperty[Reference]("path") else BookmarkFactory.standardPath
		println("Path list "+pathList.mkString(","))
    try {
      pathController.loadPath(pathList,listener)
		} catch {
			case NonFatal(e) => util.Log.e("restore", e); listener()
			case other: Throwable => util.Log.e("Error restore Settings", other); listener()
		}
  }

	def shutDown(): Unit = {
  	pathMod.shutDown()
  	dataviewController.shutDown()
  }

	def setViewbox(newbox: Viewbox): Unit = viewbox = newbox
  
  def switchPathBox():Unit = {
  	if(pathBoxOpen) { // close
  	  contents-=pathBox  		
  		viewbox.setTitle(	pathMod.getTitleText)//(pathMod.dataList.get.last,pathMod.dataList.get.size-1)else "empty")
  		viewbox.minimizeHeaderPanel(switchPathBox _)
  		revalidate()
  	} 
  	else {
  	  contents.clear()
  	  contents+=pathBox+=dataviewController.splitBox+=glue
  	}
  	pathBoxOpen= !pathBoxOpen
  }  
  
  def typeID:String = "table"    
    
  private def showBookmarkDialog():Unit= {
    //println("show BookmarkDialog ")
    val pos=dataviewController.formPanel.peer.getLocationOnScreen()
    bookmarkDialog.bounds=new java.awt.Rectangle(pos.x,pos.y,400,400)
    //for(path<-pathMod.dataList)
    	bookmarkDialog.showBookmarks(pathMod.dataList.map(_.ref),openBookmarks)
  }
  
  private def openBookmarks(path:Seq[Reference])= Swing.onEDT{
    if(path.nonEmpty){
      //println("open Bookmarks "+path)
      pathController.shutDown()
      pathController.loadPath(path,()=>{
        TableViewbox.this.revalidate()
      })
    }
    	
  }

	def selectedItems: List[SelectGroup[InstanceData]] = dataviewController.selGroupList
}

