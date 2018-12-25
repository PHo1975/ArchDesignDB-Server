package management.databrowser

import java.awt.Dimension
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.io.File

import javax.swing.SwingWorker
import server.config.FSPaths
import transaction.handling.BackupThread

import scala.swing.{BoxPanel, Button, Dialog, Orientation, ProgressBar, Window}
import scala.swing.event.ButtonClicked

class BackupDialog(w:Window) extends Dialog(w){
  title="Backup"
  val cancelButton=new Button("Cancel")
  val progressBar=new ProgressBar  
  val mainPanel=new BoxPanel(Orientation.Vertical) {
    contents+=progressBar+=cancelButton    
    preferredSize=new Dimension(300,90) 
    listenTo(cancelButton)
    reactions+= {      
      
      case ButtonClicked(`cancelButton`)=> close
    }
  }
  
  contents=mainPanel
  
  def showDialog()= {	
		cancelButton.text="Abbruch"
		progressBar.max=100
		progressBar.min=0
		progressBar.value=0
		
		
		
		
		val worker=new SwingWorker[Unit,Int] with BackupThread {
		  override def updateProgress(value:Int)=setProgress(value)
		  override def doInBackground():Unit= {
		    val zipFile=createBackupZipFile(new File(getTodayName))
		    try {
			    loopDirectory(FSPaths.configDir,zipFile)
				  loopDirectory(FSPaths.dataDir,zipFile)				  
		    } finally{if(zipFile!=null)zipFile.close()}
		  }
		  override def done(): Unit = {
			  cancelButton.text="Fertig"			  
			}		  
		}
		
		visible=true	
		worker.addPropertyChangeListener(new PropertyChangeListener{
		  def propertyChange(evt:PropertyChangeEvent)= if(evt.getPropertyName()=="progress"){		    
		    evt.getNewValue() match {
		      case i:Integer => progressBar.value=i
		      case o=>println(o.getClass)
		    }		  
		  }
		})	
		worker.execute()
 }
  
}