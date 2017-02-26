package client.dialog

import scala.swing.Dialog
import scala.swing.Component
import client.ui.ClientApp
import scala.swing.BorderPanel
import scala.swing.Label
import client.dataviewer.ViewConstants
import java.awt.Color
import javax.swing.BorderFactory
import scala.swing.Swing
import javax.swing.JComponent
import java.awt.Toolkit
import scala.swing.Window

class CustomToast(relativeComp:JComponent,window:Window) extends Dialog(window) { 
   peer.setAlwaysOnTop(true)
   peer.setUndecorated(true)
   peer.setFocusableWindowState(false)
   peer.pack()
   
   def setContent(content:Component)= {
  	 contents=content      
  	 size=content.preferredSize  
  	 visible=true
   }
   
   def updatePos(posx:Int,posy:Int) = {
     val relPos=relativeComp.getLocationOnScreen()     
     peer.setLocation(relPos.getX().toInt+posx, relPos.getY().toInt+posy)
   }
}


class Toast(message:String,component:JComponent,window:Window) extends Dialog(window){
  //component.getTopLevelAncestor()
   val panel=new BorderPanel{
     val label=new Label(message)
     label.font=ViewConstants.tableFont
     label.foreground=Color.white     
     add(label,BorderPanel.Position.Center)
     background=Color.gray
     border=BorderFactory.createCompoundBorder(
         BorderFactory.createLineBorder(Color.LIGHT_GRAY,2),
         BorderFactory.createEmptyBorder(5,5,5,5))     
   }   
   peer.setAlwaysOnTop(true)
   peer.setUndecorated(true)
   peer.setFocusableWindowState(false)
   peer.pack()   
   contents=panel      
   size=panel.preferredSize 
   if(component==null) util.Log.e("Toast, comp == null")
   peer.setLocationRelativeTo(component)
   val loc=peer.getLocation()
   peer.setLocation(loc.getX().toInt, loc.getY().toInt+30)

   new Thread(){
      override def run(): Unit = {
          try {
              Thread.sleep(4000)
              Swing.onEDT{ peer.dispose()}
          } catch {case e:InterruptedException=>  util.Log.e("toast ",e);}
      }
    }.start()
}