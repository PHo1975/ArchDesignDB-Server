/**
 * Author: Peter Started:07.11.2010
 */
package client.layout

import java.awt.Color

import client.ui.ClientApp
import definition.comm.{ListValue, PropertyGroup, PropertyValue}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing._

/**
 * 
 */
class MainBox extends Panel with ViewboxHolder {

	def layoutManager: ViewboxLayout = peer.getLayout.asInstanceOf[ViewboxLayout]

	var _centerBox: Viewbox = _
	def centerBox:Viewbox=_centerBox


	def centerBox_=(newB: Viewbox): Unit = {
		//if(_centerBox!=null) remove(_centerBox)		
		_centerBox=newB
		//add(_centerBox)
	}

	def add(comp: Component): Unit = peer.add(comp.peer)

	def remove(comp: Component): Unit = peer.remove(comp.peer)
	
	opaque=true
	background=Color.lightGray
	
	def close():Unit = {
		if(_centerBox!=null) centerBox.foreach(_.close())
	}
	
	def shutDown():Unit = {
	  if(_centerBox!=null) centerBox.foreach(_.shutDown())
	  centerBox=null
	}

	def maximizeBox(mBox: Viewbox): Unit = {
	  layoutManager.maximizedBox match {
	    case Some(oldBox) =>
				oldBox.doUnMaximize()
				layoutManager.maximizedBox=None
				showAllBoxes()
				ClientApp.top.decorate()
			case None =>
				layoutManager.maximizedBox=Some(mBox)
				mBox.doMaximize()
				hideAllBoxes(mBox)
				ClientApp.top.undecorate()
		}
	  revalidate()
	  repaint()
	}

	def hideAllBoxes(besidesBox: Viewbox): Unit = {
	  if(_centerBox!=null) centerBox.foreach(b=> if(b!=besidesBox) b.visible=false)
	}

	def showAllBoxes(): Unit =
	  if(_centerBox!=null) centerBox.foreach(b=> b.visible=true)
	
	def storeSettings(pGroup:PropertyGroup):Unit = {
	  val groupList=new ArrayBuffer[PropertyGroup] () 
	  var counter=0
		if(_centerBox!=null) centerBox.foreach(box => {
			val newGroup = PropertyGroup(counter.toString, new mutable.HashMap[String, PropertyValue]())
		  groupList +=newGroup
		  box.storeSettings(newGroup)
		  counter+=1
		})
		//Log.w("Settings:"+groupList.mkString(" , "))
		pGroup.addProperty(new ListValue[PropertyGroup]("boxes",groupList.toIndexedSeq))
	}
	
	def restoreSettings(prGroup:Option[PropertyGroup],readyListener:()=>Unit):Unit = {
    //val now=System.currentTimeMillis()
	  val groupStack= prGroup match {
	    case Some(pGroup)=>
				pGroup.getListProperty[PropertyGroup]("boxes").toList
			case None => Nil
	  }
	  //Log.w("Groupstack "+groupStack.mkString(" , "))
	  centerBox=new Viewbox(this,false,this)
		//Log.w("Viewbox created")
    //println("restoreSettings "+(System.currentTimeMillis()-now))
		//println("start Restore "+groupStack.mkString("\n"))
	  centerBox.restoreSettings(groupStack,(_)=>readyListener())
		//Log.w("Centerbox loaded")
	  revalidate()
	  repaint()
	  //System.out.println("Box List:"+groupList.map(_.name))
	}
	
	
	override lazy val peer = new MainboxPeer


	def replaceBox(oldBox: Viewbox, newBox: Viewbox): Unit = {
	  //System.out.println("replace "+newBox)
	  centerBox=newBox
	  revalidate()
	  repaint()
	}
	
	def deleteMe(oldBox:Viewbox):Boolean = false
	
	class MainboxPeer extends javax.swing.JPanel(new ViewboxLayout) with SuperMixin {
		def mainBox: MainBox = MainBox.this
	}
	
	
}