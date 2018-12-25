package client.ui

import scala.swing.{Dialog, Window}

/**
 * Created by Kathi on 30.05.2015.
 */
class LogWindow(w:Window) extends Dialog {
  val consolePanel=new management.databrowser.ConsolePanel(false)
  contents = consolePanel
  this.peer.setBounds(50,100,1000,600)
}
