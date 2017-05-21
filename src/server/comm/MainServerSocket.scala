/**
 * Author: Peter Started:29.08.2010
 */
package server.comm

import java.io.IOException
import java.net.{BindException, ServerSocket, SocketException}
import javax.swing.JOptionPane

import server.config.FSPaths

import scala.swing.Swing


/**
 * 
 */
object MainServerSocket extends Thread("MainSock") {
  var noBulkAction=false
  var listener: Option[ServerSocket] = None

  def setup(): Unit = try {
    listener = Some(new ServerSocket(FSPaths.serverPort))
    } catch {
      case b:BindException=> Swing.onEDT{JOptionPane.showMessageDialog(null, "Server läuft schon","Datenbank-Server", JOptionPane.ERROR_MESSAGE);System.exit(0)}
    }
  var isRunning=true
  override def run():Unit = {
    try listener match {
      case Some(li) =>
        System.out.println("ServerSocket ready on port " + FSPaths.serverPort)
        while (isRunning)
          new JavaClientSocket(li.accept()).start()
      case None => util.Log.e("listener not defined, run setup()")
    }
    catch {
      case b:BindException=> Swing.onEDT{JOptionPane.showMessageDialog(null, "Server läuft schon","Datenbank-Server", JOptionPane.ERROR_MESSAGE);System.exit(0)}
      case s:SocketException => Swing.onEDT{JOptionPane.showMessageDialog(null,s.getMessage())}
      case e: IOException => Swing.onEDT{
      	JOptionPane.showMessageDialog(null,"Could not listen on port: "+FSPaths.serverPort+" "+e+"\n"+e.getStackTrace().take(10).mkString("\n"))
        //System.exit(-1)
      }
      case o: Exception =>
        System.out.println("Exception:"+o)
        Swing.onEDT{JOptionPane.showMessageDialog(null,"Error on MainSocket: "+o)}
    }
  }
}