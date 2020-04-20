/**
 * Author: Peter Started:14.12.2010
 */
package management.databrowser

import java.io.{OutputStream, PrintStream}
import java.text.SimpleDateFormat
import java.util.Date

import client.ui.ViewConstants
import javax.swing.text.Document

import scala.swing._

/**
 * 
 */
class ConsolePanel(debug: Boolean) extends BorderPanel {
	val textArea=new TextArea
  val doc: Document = textArea.peer.getDocument
  textArea.font = ViewConstants.tableFont
	textArea.editable=false
	val timeFormat=new SimpleDateFormat("dd| HH:mm ")
	add(new Label("Log-Console"),BorderPanel.Position.North)
	add(new ScrollPane {
		viewportView=textArea
	},BorderPanel.Position.Center)
  util.Log.addLogListener((st:String,error:Boolean) => printWithTimeStamp(st+"\n",error))

  protected def printError(errorText: String): Unit = printWithTimeStamp(errorText,error = true)

  protected def printWithTimeStamp(text: String, error: Boolean): Unit =  Swing.onEDT {
    textArea.append( timeFormat.format(new Date()) + (if (error) " Error: " else " ") + text)
  }

	def printWithoutTimeStamp(text:String): Unit = textArea.append(text)


	class MyStream(error:Boolean) extends OutputStream {
    override def write(b: Int): Unit = printError(String.valueOf(b.toChar))

    override def write(b: Array[Byte], off: Int, len: Int): Unit = if (len == 2 && b(1) == '\n') {}
    		else printWithTimeStamp(new String(b, off, len)+"\n",error)

    override def write(b: Array[Byte]): Unit = write(b, 0, b.length)
	}
	
	val out = new PrintStream(new MyStream(false),true)
	val err= new PrintStream(new MyStream(true),true)
  
	if(!debug) {
	  System.setOut(out)
	  System.setErr( err)
  } else println("debug")
}


