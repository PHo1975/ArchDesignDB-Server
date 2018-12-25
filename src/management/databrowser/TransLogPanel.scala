/**
 * Author: Peter Started:27.07.2010
 */
package management.databrowser

import client.dataviewer.ViewConstants
import javax.swing.SwingWorker
import server.storage.{TransDetailLogHandler, TransLogHandler}

import scala.swing._
import scala.swing.event._



/** Shows the Transaction Log Table
 * 
 */
object TransLogPanel extends GridPanel(1,2) {
  
  val leftPanel: BorderPanel = new BorderPanel {
    add(ViewConstants.label("Transaction Log File "), BorderPanel.Position.North)
  	
    add( new ScrollPane() {
    	viewportView= new Table() 	{
    		model=LogFileModel  		
    	}
    },BorderPanel.Position.Center)
    
    add ( new GridPanel(1,3){
    	val refBut= new Button("Refresh")
    	val filterBut=new Button("Filter")
      val indexLab: Label = ViewConstants.label("Detail")
    	contents += refBut+=filterBut+=indexLab
    	listenTo(refBut,filterBut)
    	reactions+= {
    	  case ButtonClicked(`refBut`) =>
          new SwingWorker[Unit,Unit] {
            override def doInBackground(): Unit = {
              LogFileModel.refresh()
            }
          }.execute()
          indexLab.text="ins:"+TransLogHandler.getInsertPos.toString+" tr:"+TransLogHandler.getTransID
        case ButtonClicked(`filterBut`) => filter()
        
    	}
    },BorderPanel.Position.South )
  }
  
  val rightPanel: BorderPanel = new BorderPanel {
    add(ViewConstants.label("Trasaction Details Log File "), BorderPanel.Position.North)
    
    add( new ScrollPane() {
      viewportView= new Table()   {
        model=DetailLogFileModel      
      }
    },BorderPanel.Position.Center)
    
    add ( new GridPanel(1,3){
      val refBut= new Button("Refresh")
      val indexLab: Label = ViewConstants.label(" ")
      contents += refBut+=indexLab
      listenTo(refBut)
      reactions+= {
        case ButtonClicked(`refBut`) =>
          new SwingWorker[Unit,Unit] {
            override def doInBackground(): Unit = {
              DetailLogFileModel.refresh()
            }
          }.execute()
          indexLab.text="insertPos:"+TransDetailLogHandler.getInsertPos.toString+" lastLoggedID:"+TransDetailLogHandler.getLastLoggedID
      }
    },BorderPanel.Position.South )
  }
  contents+=leftPanel+=rightPanel


  def filter(): Unit = {
	  Dialog.showInput(this, "Welche Instance Filtern (Typ,Inst)","Instance Filtern", Dialog.Message.Question , Swing.EmptyIcon, Nil, "") match{
	    case Some(text)=>
        val ints=text.trim.split(",").map(_.trim.toInt)
        LogFileModel.filter(ints(0),ints(1))
      case _=>
	  }	  
	}
  
  
}