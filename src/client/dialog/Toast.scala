package client.dialog

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{Color, Point}
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import client.dataviewer.ViewConstants
import client.ui.ClientApp
import javax.swing.{BorderFactory, JComponent, SwingUtilities}

import scala.swing.{BorderPanel, Component, Dialog, Label, Swing, Window}

class CustomToast(relativeComp:JComponent,window:Window) extends Dialog(window) {
  //Log.w("Custom Toast ")
  peer.setAlwaysOnTop(true)
  peer.setUndecorated(true)
  peer.setFocusableWindowState(false)
  peer.pack()

  def setContent(content: Component): Unit = {
    //Log.w("Set Content")
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
  //Log.w("Create Toast "+message)
  val time=4000
  val label=new Label(message)
  val panel=new BorderPanel{
    label.font=ViewConstants.tableFont
    label.foreground=Color.white
    add(label,BorderPanel.Position.Center)
    background=Color.gray
  }
  peer.setAlwaysOnTop(true)
  peer.setUndecorated(true)
  peer.setFocusableWindowState(false)
  peer.pack()
  contents=panel
  startup()
  size=panel.preferredSize

  def startup():Unit = {
    //Log.w("Toast startup")
    if(component==null) util.Log.e("Toast, comp == null")
    panel.border=BorderFactory.createCompoundBorder(
      BorderFactory.createLineBorder(Color.LIGHT_GRAY,2),
      BorderFactory.createEmptyBorder(5,5,5,5))
    peer.setLocationRelativeTo(component)
    val loc: Point = peer.getLocation()
    peer.setLocation(loc.getX().toInt, loc.getY().toInt+30)
  }

  FollowMouseToast.executor.schedule(new Runnable(){
    override def run(): Unit = {
      try {
        Thread.sleep(time)
        Swing.onEDT {
          if(peer.isVisible)peer.dispose()
        }
      } catch {case e:InterruptedException=>  util.Log.e("toast ",e);}
    }
  },4L,TimeUnit.SECONDS)

}


class NFollowMouseToast(message:String,relComponent:JComponent) extends Dialog(ClientApp.top) {
  //Log.w("NfollowMouseToast "+message)
  import client.dialog.FollowMouseToast._
  val label=new Label(message)
  label.font=ViewConstants.tableFont
  peer.setAlwaysOnTop(true)
  peer.setUndecorated(true)
  peer.setFocusableWindowState(false)
  peer.pack()
  contents=label
  size=label.preferredSize
  label.opaque=true
  label.background=Color.white
  currentToast=Some(this)

  val mouseListener=new MouseAdapter{
    override def mouseExited(e: MouseEvent): Unit = peer.setVisible(false)
    override def mouseEntered(e: MouseEvent): Unit =peer.setVisible(true)
    override def mouseMoved(e: MouseEvent): Unit = changePos(e)
  }

  relComponent.addMouseListener(mouseListener)
  relComponent.addMouseMotionListener(mouseListener)

  def changePos(e:MouseEvent):Unit = {
    val loc=relComponent.getLocation()
    SwingUtilities.convertPointToScreen(loc,relComponent)
    peer.setLocation(new Point(loc.x+e.getX+30*ViewConstants.fontScale/100,loc.y+e.getY-40*ViewConstants.fontScale/100))
  }

  def shutDown():Unit= {
    relComponent.removeMouseListener(mouseListener)
    relComponent.removeMouseMotionListener(mouseListener)
    currentToast=None
    peer.dispose()
  }
}

object FollowMouseToast {
  val executor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
  var currentToast:Option[NFollowMouseToast]=None

  def showToast(message:String,relComponent:JComponent): Unit = {
    reset()
    new NFollowMouseToast(message,relComponent).visible=true
  }

  def reset():Unit = for(t <- currentToast) t.shutDown()
}