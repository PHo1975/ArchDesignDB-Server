/**
 * Author: Peter Started:25.09.2010
 */
package client.dialog

import scala.swing.AbstractButton
import scala.swing.BoxPanel
import scala.swing.Component
import scala.swing.Insets
import scala.swing.MenuItem
import scala.swing.Point
import scala.swing.Swing
import scala.swing._
import scala.swing.event.ButtonClicked

import client.comm.ClientObjectClass
import client.comm.KeyStrokeManager
import client.dataviewer.TitlePopupMenu
import definition.data.Referencable
import definition.typ.ActionDescription
import definition.typ.AllClasses
import definition.typ.SelectGroup
import javax.swing.BorderFactory

	
object ActionPanel extends BoxPanel(scala.swing.Orientation.Vertical) with SelectListener {
  val emptyInsets=new Insets(0,0,0,0)
  var lastSender:Option[SelectSender]= None
  var groupList:Iterable[SelectGroup[_<:Referencable]]= Nil
  //var lastCommonClass= -1
  var buttons:Seq[StrokableButton]=Nil
    val miniInsets=new Insets(0,5,0,5)  
  var lastClassName:String= _
  
  opaque=false
  
  border=BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(8,0,6,0),
      BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(8,0,6,0),"Funktionen:"))
   
  xLayoutAlignment=0.5d
  yLayoutAlignment=0.5d 
  
  def shutDown():Unit = {
    removeButtons()
    contents.clear()          
  } 
  
    
  def hasSelection= groupList.nonEmpty && groupList.head.children.nonEmpty
  
  def getButtonID(but:StrokableButton):Int=but match {
    case asb:ActionStrokeButton=>asb.theAction match {
      case it:ActionDescription=> it.buttonID
      case _=> 0
    }
    case csb:CustomStrokeButton=>csb.buttonID
    case _=>0
  }
  
  def insertButtons(newButtons:Seq[StrokableButton])={
    contents.clear()
      var groupID=0
      for (but<-newButtons){
        if(getButtonID(but)/10!=groupID){
          contents+=Swing.VStrut(10)
          groupID=getButtonID(but)/10
        }
        contents+=but
      }   
  }
  
  
  def selectionChanged [T <: Referencable](sender:SelectSender,groups:Iterable[SelectGroup[T]],alsoSelected:Iterable[T]):Unit = {
   //println("Selection Changed sender:"+sender.getClass+" groups:"+groups+" ")       
  //removeButtons()
    groupList=groups
    for(ls <-lastSender;if ls != sender) ls.deselect(false)
    if( groupList.isEmpty || groupList.head.children.isEmpty) visible=false
    else {
      val allc=AllClasses.get
      val commonClass=allc.getCommonClassForGroups(groups)
      shutDown()
      if(commonClass>0) {
        val theClass = allc.getClassByID(commonClass).asInstanceOf[ClientObjectClass]

        if (hasSelection && groupList.head.children.head.ref.instance != -1) {
          // ignore invalidate dummy instances
          buttons = theClass.actionButtons.sortBy(getButtonID)
          restoreButtonBindings()
          insertButtons(buttons)
          listenTo(buttons: _*)
          visible = true
        } else {
          buttons = Nil
          visible = false
        }
        //lastCommonClass = commonClass
      }
      peer.revalidate()
      repaint()
    }
    lastSender=Some(sender)       
  }   
 
  
  def removeButtons()= deafTo(buttons:_*)
  
    
  def addCustomButtons(custBut:Seq[CustomStrokeButton]):Unit= {
     buttons=buttons ++ custBut
     if(!visible) visible=true
     for(b<-custBut) KeyStrokeManager.registerReceiver(b)
     insertButtons(buttons)  
     revalidate()
     repaint()
     listenTo(custBut:_*)
  }
  
  def restoreButtonBindings():Unit= for(b<-buttons) KeyStrokeManager.registerReceiver(b)
  
  reactions += {
    case ButtonClicked(but:ActionStrokeButton) => if(groupList!=null)
      DialogManager.startActionDialog(but.theAction,groupList)      
      
    case ButtonClicked(but:CustomStrokeButton)=> but.callBack()   
  }
  
  // ******* Support for right click menu
	
	class CustomMenuItem(val ob:CustomStrokeButton) extends MenuItem(ob.text)
  class ActionMenuItem(val ob:ActionStrokeButton) extends MenuItem(ob.text)
	
	def showRightMenu(point:Point,component:Component):Unit= if(hasSelection){  
	  val popup=new TitlePopupMenu("Actions")
	  val menuItems=buttons map (el => {
	    val m= el match {
	      case csb:CustomStrokeButton=> new CustomMenuItem(csb)
	      case asb:ActionStrokeButton=> new ActionMenuItem(asb)	      
	    }
	    m.icon=el.icon 
	    m
	  })	  
	  popup.addButtons(menuItems)	  
	  popup.show(component.peer,point.x,point.y)
	  popup.listenTo(menuItems:_*)
	  popup.reactions+={
			case ButtonClicked(but:ActionMenuItem) => if(groupList!=null)
			  DialogManager.startActionDialog(but.ob.theAction,groupList)	
			case ButtonClicked(but:CustomMenuItem)=> but.ob.callBack()				
	  }
	}
	
	def minimizeButton(button:AbstractButton)= {
	  button.peer.putClientProperty("JComponent.sizeVariant", "small")
    button.peer.updateUI()
	}

}	


