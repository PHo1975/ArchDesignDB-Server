package client.dialog

import java.awt.Color
import java.awt.event.HierarchyEvent

import client.comm.{KeyStrokeManager, KeyStrokeReceiver}
import client.dataviewer.ViewConstants
import client.icons.IconManager
import definition.typ.{ActionTrait, _}
import javax.swing.{BorderFactory, KeyStroke}

import scala.swing.event.ButtonClicked
import scala.swing.{AbstractButton, Button, MenuItem, ToggleButton}

trait StrokableButton extends AbstractButton with KeyStrokeReceiver {   
    focusable=false
    //xLayoutAlignment=0.5d
    margin= ActionPanel.emptyInsets
    horizontalAlignment=scala.swing.Alignment.Left
    for(sicon<-IconManager.getIcon(groupName,commandName)) icon=sicon
  font = ViewConstants.tableFont

  def strokeHit(): Unit = publish(ButtonClicked(this))

  def setStroke(stroke: KeyStroke): Unit = tooltip = "( " + KeyStrokeManager.keyStrokeToString(stroke) + " )"

  def initListener(): Unit = peer.addHierarchyListener { e => {
    if ((e.getChangeFlags & HierarchyEvent.SHOWING_CHANGED) > 0) {
      // println("show change "+StrokableButton.this.commandName+" "+StrokableButton.this.peer.isShowing()+" ch:"+e.getChanged().getClass.toString)
      if (!StrokableButton.this.peer.isShowing())
        KeyStrokeManager.unregisterReceiver(StrokableButton.this)
    }
  }
  }

  initListener()
} 


trait AbstractPanelButton extends StrokableButton {
    margin=ActionPanel.miniInsets
    focusable=false
    ActionPanel.minimizeButton(this)    
  }



class IconableButton(val commandName:String,val groupName:String,ntooltipText:String) extends Button with AbstractPanelButton  {
   this.tooltip=ntooltipText
  peer.putClientProperty("Nimbus.Overrides", ViewConstants.buttonDefaults)
  peer.updateUI()
}

class IconableToggleButton(val commandName:String,val groupName:String,ntooltipText:String) extends ToggleButton with AbstractPanelButton  {
   IconManager.getIcon(groupName,commandName) match {
     case Some(aicon)=> icon=aicon
     case _=> text=commandName
   }
   tooltip=ntooltipText
  peer.putClientProperty("Nimbus.Overrides", ViewConstants.toggleButtonDefaults)
  peer.updateUI()
}
  

class ActionStrokeButton(val groupName:String,val theAction:ActionTrait) extends Button(theAction.name) with StrokableButton{
  def commandName: String = theAction.name

  maximumSize = ViewConstants.buttonSize
} 

class CustomStrokeButton(val groupName:String,val commandName:String,val callBack:()=>Unit,val buttonID:Int) extends Button(commandName) with StrokableButton{
  maximumSize = ViewConstants.buttonSize
}


class CreateActionMenuButton(val  groupName:String,val propField:Byte,val ccd:AbstractCCD) extends MenuItem(ccd.getName) with StrokableButton {
  def commandName: String = ccd.getName
  //maximumSize=DialogManager.buttonSize
  // minimumSize=DialogManager.minButtonSize
  override def initListener(): Unit = {} // dont unregister when hide
}


class CreateMenuButton(val  groupName:String,val propField:Byte,val ccd:AbstractCCD) extends MenuItem(ccd.childName) with StrokableButton{
  def commandName: String = ccd.childName
  border=BorderFactory.createLineBorder(Color.gray)
  //preferredSize=DialogManager.minButtonSize
  //maximumSize=DialogManager.buttonSize
}