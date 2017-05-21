package client.dialog

import java.awt.{Color, Point}
import javax.swing.{BorderFactory, JComponent}

import client.dataviewer.ViewConstants

import scala.swing.{BorderPanel, Component, Dialog, Label, Swing, Window}

class CustomToast(relativeComp:JComponent,window:Window) extends Dialog(window) { 
   peer.setAlwaysOnTop(true)
   peer.setUndecorated(true)
   peer.setFocusableWindowState(false)
   peer.pack()

  def setContent(content: Component): Unit = {
  	 contents=content      
  	 size=content.preferredSize  
  	 visible=true
   }

  def updatePos(posx: Int, posy: Int): Unit = {
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
  val loc: Point = peer.getLocation()
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