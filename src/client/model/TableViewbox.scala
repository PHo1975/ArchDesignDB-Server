/**
 * Author: Peter Started:10.11.2010
 */
package client.model

import java.awt.Color
import definition.comm.{PropertyGroup, ListValue}

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Dimension
import scala.swing.Insets
import scala.swing.Label
import util.MyListView
import scala.swing.Orientation
import scala.swing.Swing
import scala.swing.event.ButtonClicked
import client.dataviewer.DataViewController
import client.dialog.NewButtonsList
import client.dialog.SelectEventDispatcher
import client.layout.Viewbox
import client.layout.ViewboxContent
import client.ui.ClientApp
import definition.data.InstanceData
import definition.data.Reference
import javax.swing.BorderFactory
import javax.swing.border.EtchedBorder
import scala.swing.ScrollPane
import scala.swing.Component
import scala.swing.Container
import javax.swing.JScrollPane
import scala.swing.Panel
import scala.swing.SequentialContainer
import javax.swing.JComponent
import javax.swing.AbstractAction
import javax.swing.KeyStroke
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import java.awt.event.InputEvent
import scala.util.control.NonFatal


class AdaptedScroller(aView:Component) extends Component with Container {
   override lazy val peer: JScrollPane = new JScrollPane with SuperMixin {
      override def getMaximumSize()=new Dimension(Short.MaxValue,aView.peer.getPreferredSize.height+1)
      override def getPreferredSize()=aView.peer.getPreferredSize
      override def getMinimumSize()=new Dimension(20,aView.peer.getPreferredSize.height+1)       
    }    
    //yLayoutAlignment=1d     
    peer.setViewportView(aView.peer)
    def contents=List(aView)
    border=null
}

/**
 * 
 */
class TableViewbox extends BoxPanel(Orientation.Vertical) with ViewboxContent  {
	//println("new view box "+Thread.currentThread().getName())
	val dataviewController=new DataViewController(this)
	
	val pathMod=new PathModel()
	val pathView=new MyListView[InstanceData]()		
	pathView.xLayoutAlignment=0d
	pathView.yLayoutAlignment=1d
	var viewbox:Viewbox=null	
	
	val pathController:PathController=new PathController(pathMod,pathView,dataviewController)
	//pathController.registerSizeChangeListener(pathView.callback)
	pathController.registerSizeChangeListener((a) => {viewbox.setTitle(pathMod.getTitleText);/*dataviewController.updateHeight*/})
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
		  override lazy val peer = {
	    val p = new javax.swing.JPanel with SuperMixin {
	      override def getPreferredSize()=new Dimension(100,pathView.peer.getPreferredSize.height+1)
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
	  def actionPerformed(e:ActionEvent) =goUp()	  
	})
  
  def goUp()= if(pathMod.getSize>0)
	     pathController.selectionChanged(pathMod.getSize-1)	
  
  /** opens a new box */
	def open(readyListener:()=>Unit,sourceBoxSelection:AnyRef): Unit ={
    if(BookmarkFactory.standardPath.isEmpty)util.Log.e("StandardPath is empty !")
    pathController.loadPath(BookmarkFactory.standardPath,readyListener)
  } 	    

  def close(): Unit =  shutDown()
  
  def storeSettings(pgroup:PropertyGroup) = {
  	if(pathMod.dataList.nonEmpty)
  	//PathFactory.releasePathEntry(pathFactoryIndex, pathController.model.dataList.get.map(_.ref))
  	pgroup.addProperty(new ListValue[Reference]("path",pathMod.dataList.map(a=>a.ref)))
  }  
  
  def restoreSettings(pgroup:PropertyGroup,listener:()=>Unit): Unit = {
    val pathList=if(pgroup.containsProperty("path")) pgroup.getListProperty[Reference]("path") else BookmarkFactory.standardPath    
    try {
      pathController.loadPath(pathList,listener)   
    }catch {case NonFatal(e)=> util.Log.e(e.toString);listener()
		case other:Throwable =>println(other);System.exit(0)}
  }  
  
  def shutDown() = {
  	pathMod.shutDown()
  	dataviewController.shutDown()
  }
    
  def setViewbox(newbox:Viewbox)= viewbox=newbox  
  
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

	def selectedItems=dataviewController.selGroupList
}

