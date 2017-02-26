/**
 * Author: Peter Started:14.12.2010
 */
package management.databrowser
import scala.swing._
import java.awt.Font
import client.comm.ErrorListener
import client.comm.ClientQueryManager
import java.io.OutputStream
import java.text.SimpleDateFormat
import java.util.Date
import java.io.PrintStream
/**
 * 
 */
class ConsolePanel(debug:Boolean) extends BorderPanel with ErrorListener{
	val textArea=new TextArea
	val doc=textArea.peer.getDocument()
	//textArea.wordWrap=true
	//textArea.lineWrap=true
	val textFont=new Font("Courier New",0,12)
	textArea.font=textFont
	textArea.editable=false
	val timeFormat=new SimpleDateFormat("dd| HH:mm ")
	add(new Label("Log-Console"),BorderPanel.Position.North)
	add(new ScrollPane {
		viewportView=textArea
	},BorderPanel.Position.Center)

	def printError(errorText:String)=Swing.onEDT{		
		textArea.append(errorText)
	}
	def printWithTimeStamp(text:String,error:Boolean)={
    //if (error) println(text + "::" + Thread.currentThread().getStackTrace.drop(2).mkString("\n "))
    Swing.onEDT {
    textArea.append( timeFormat.format(new Date()) + (if (error) "Error: " else "") + text)
  }
	}
	class MyStream(error:Boolean) extends OutputStream {
		override def write( b:Int)= printError( String.valueOf( b.toChar))

		override def write(b:Array[Byte], off:Int, len:Int) = if(len==2&&b(1)=='\n') printError("\n")
    		else printWithTimeStamp(new String(b, off, len),error)

		override def write(b:Array[Byte]) =  write(b, 0, b.length)
	}
	
	val out = new PrintStream(new MyStream(false),true)
	val err= new PrintStream(new MyStream(true),true)
  
	if(!debug) {
	  System.setOut(out)
	  System.setErr( err)
	}
}


