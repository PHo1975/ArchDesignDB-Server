/**
 * Author: Peter Started:07.11.2010
 */
package client.layout

import definition.comm.{PropertyGroup, ListValue, PropertyValue}

import scala.collection.mutable
import scala.swing._
import java.awt.Color
import javax.swing.UIManager
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/**
 * 
 */
class MainBox extends Panel with ViewboxHolder {
	
	def layoutManager = peer.getLayout.asInstanceOf[ViewboxLayout]  
	var _centerBox:Viewbox=null	
	def centerBox:Viewbox=_centerBox
	
	
	
	def centerBox_=(newB:Viewbox)={
		//if(_centerBox!=null) remove(_centerBox)		
		_centerBox=newB
		//add(_centerBox)
	}
	
	def add(comp:Component)= peer.add(comp.peer)
	def remove(comp:Component)= peer.remove(comp.peer)
	
	opaque=true
	background=Color.lightGray
	
	def close():Unit = {
		if(_centerBox!=null) centerBox.foreach(_.close())
	}
	
	def shutDown():Unit = {
	  if(_centerBox!=null) centerBox.foreach(_.shutDown())
	  centerBox=null
	}
	
	def maximizeBox(mBox:Viewbox)={
	  layoutManager.maximizedBox match {
	    case Some(oldBox) =>
				oldBox.doUnMaximize()
				layoutManager.maximizedBox=None
				showAllBoxes()
			case None =>
				layoutManager.maximizedBox=Some(mBox)
				mBox.doMaximize()
				hideAllBoxes(mBox)
		}
	  revalidate()
	  repaint()
	}
	
	def hideAllBoxes(besidesBox:Viewbox)= {
	  if(_centerBox!=null) centerBox.foreach(b=> if(b!=besidesBox) b.visible=false)
	}
	def showAllBoxes() = 
	  if(_centerBox!=null) centerBox.foreach(b=> b.visible=true)
	
	def storeSettings(pGroup:PropertyGroup):Unit = {
	  val groupList=new ArrayBuffer[PropertyGroup] () 
	  var counter=0
		if(_centerBox!=null) centerBox.foreach(box => {
		  val newGroup=new PropertyGroup(counter.toString,new mutable.HashMap[String,PropertyValue]())
		  groupList +=newGroup
		  box.storeSettings(newGroup)
		  counter+=1
		})
		pGroup.addProperty(new ListValue[PropertyGroup]("boxes",groupList.toIndexedSeq))
	}
	
	def restoreSettings(prGroup:Option[PropertyGroup],readyListener:()=>Unit):Unit = {
    //val now=System.currentTimeMillis()
	  val groupStack= prGroup match {
	    case Some(pGroup)=>
				val groupList=pGroup.getListProperty[PropertyGroup]("boxes")
				new collection.mutable.Stack[PropertyGroup]()++groupList
			case None => collection.mutable.Stack[PropertyGroup]()
	  }
	  
	  centerBox=new Viewbox(this,false,this)
    //println("restoreSettings "+(System.currentTimeMillis()-now))
	  centerBox.restoreSettings(groupStack,readyListener)
	  revalidate()
	  repaint()
	  //System.out.println("Box List:"+groupList.map(_.name))
	}
	
	
	override lazy val peer = new MainboxPeer 
		
	
  def replaceBox(oldBox:Viewbox,newBox:Viewbox) = {
	  //System.out.println("replace "+newBox)
	  centerBox=newBox
	  revalidate()
	  repaint()
	}
	
	def deleteMe(oldBox:Viewbox):Boolean = false
	
	class MainboxPeer extends javax.swing.JPanel(new ViewboxLayout) with SuperMixin {
		def mainBox= MainBox.this
	}
	
	
}