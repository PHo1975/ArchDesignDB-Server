package client.dialog

import scala.swing.event.ButtonClicked
import client.comm.KeyStrokeReceiver
import java.awt.event.HierarchyEvent
import client.comm.KeyStrokeManager
import javax.swing.KeyStroke
import definition.typ.ActionTrait
import client.icons.IconManager
import scala.swing.AbstractButton
import scala.swing.ToggleButton
import scala.swing.Button
import client.dataviewer.ViewConstants
import definition.typ._
import scala.swing.MenuItem
import javax.swing.BorderFactory
import java.awt.Color

trait StrokableButton extends AbstractButton with KeyStrokeReceiver {   
    focusable=false
    //xLayoutAlignment=0.5d
    margin= ActionPanel.emptyInsets
    horizontalAlignment=scala.swing.Alignment.Left
    for(sicon<-IconManager.getIcon(groupName,commandName)) icon=sicon
    
    def strokeHit()=publish(new ButtonClicked(this))
    def setStroke(stroke:KeyStroke)= tooltip="( "+KeyStrokeManager.keyStrokeToString(stroke)+" )"
    def initListener()= peer.addHierarchyListener(new java.awt.event.HierarchyListener{
      def hierarchyChanged(e:HierarchyEvent) = {        
        if((e.getChangeFlags & HierarchyEvent.SHOWING_CHANGED)>0) {
         // println("show change "+StrokableButton.this.commandName+" "+StrokableButton.this.peer.isShowing()+" ch:"+e.getChanged().getClass.toString)
          if(!StrokableButton.this.peer.isShowing())          
            KeyStrokeManager.unregisterReceiver(StrokableButton.this)
        }
      }
    })
    
    initListener()
} 


trait AbstractPanelButton extends StrokableButton {
    font= ViewConstants.smallFont
    margin=ActionPanel.miniInsets
    focusable=false
    ActionPanel.minimizeButton(this)    
  }



class IconableButton(val commandName:String,val groupName:String,ntooltipText:String) extends Button with AbstractPanelButton  {
  
   this.tooltip=ntooltipText   
   peer.putClientProperty("Nimbus.Overrides", DialogManager.buttonDefaults)   
}

class IconableToggleButton(val commandName:String,val groupName:String,ntooltipText:String) extends ToggleButton with AbstractPanelButton  {
   IconManager.getIcon(groupName,commandName) match {
     case Some(aicon)=> icon=aicon
     case _=> text=commandName
   }
   tooltip=ntooltipText   
   peer.putClientProperty("Nimbus.Overrides", DialogManager.toggleButtonDefaults)   
}
  

class ActionStrokeButton(val groupName:String,val theAction:ActionTrait) extends Button(theAction.name) with StrokableButton{
    def commandName=theAction.name
    maximumSize=DialogManager.buttonSize
} 

class CustomStrokeButton(val groupName:String,val commandName:String,val callBack:()=>Unit,val buttonID:Int) extends Button(commandName) with StrokableButton{
  maximumSize=DialogManager.buttonSize
}


class CreateActionMenuButton(val  groupName:String,val propField:Byte,val ccd:AbstractCCD) extends MenuItem(ccd.getName) with StrokableButton {
  def commandName=ccd.getName
  //maximumSize=DialogManager.buttonSize
  // minimumSize=DialogManager.minButtonSize
  override def initListener()={} // dont unregister when hide
}


class CreateMenuButton(val  groupName:String,val propField:Byte,val ccd:AbstractCCD) extends MenuItem(ccd.childName) with StrokableButton{
  def commandName=ccd.childName
  border=BorderFactory.createLineBorder(Color.gray)
  //preferredSize=DialogManager.minButtonSize
  //maximumSize=DialogManager.buttonSize
}