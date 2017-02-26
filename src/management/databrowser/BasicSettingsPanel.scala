package management.databrowser

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Label
import scala.swing.TextField
import java.awt.Dimension
import scala.swing.event.EditDone
import scala.swing.Button
import scala.swing.Swing
import server.config.FSPaths
import scala.swing.event.ButtonClicked
import javax.swing.JFileChooser
import java.io.File
import java.awt.Color
import definition.data.Reference
import java.text.SimpleDateFormat
import java.util.Date
import transaction.handling.SessionManager

class PathEditBox(labelText:String,getter:()=> String,setter: (String)=>Unit) extends 
		BoxPanel(Orientation.Vertical)	{
    import management.databrowser.PathEditBox._
    xLayoutAlignment=0    
		val label=new Label(labelText+":")
    label.xLayoutAlignment=0    
		val edit=new TextField(getter())    
    edit.xLayoutAlignment=0      
		val but=new Button("...")
    but.yLayoutAlignment=0
		contents+=label+=new BoxPanel(Orientation.Horizontal) {      
      xLayoutAlignment=0
      yLayoutAlignment=0
		  contents+=edit+=Swing.HStrut(10)+=but 		  
      maximumSize=new Dimension(Short.MaxValue,30)      
		}
    revalidate
		def update() = edit.text=getter()
		
		listenTo(edit)
		reactions+= {
			case e:EditDone => setter(edit.text)
		}	
		listenTo(but)
		reactions+= {
		  case ButtonClicked(`but`)=>
        fileChooser.setCurrentDirectory(new File(edit.text))
        fileChooser.setDialogType( JFileChooser.OPEN_DIALOG)
        fileChooser.setDialogTitle("Choose "+labelText)
        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY )
        if(fileChooser.showDialog(this.peer, "Choose")==JFileChooser.APPROVE_OPTION) {
          println(fileChooser.getSelectedFile.getAbsolutePath()+" "+fileChooser.getSelectedFile.getPath())
          setter(fileChooser.getSelectedFile.getAbsolutePath())
          update()
        }
    }
		//maximumSize=new Dimension(Short.MaxValue,60)
	}

object PathEditBox {
  lazy val fileChooser=new JFileChooser
  val textSize=new Dimension(130,30)
}

class BasicSettingsPanel extends BoxPanel(Orientation.Vertical) {
  val backupDateLabel=new Label()
  val dateFormat=new SimpleDateFormat("dd.MM.yyyy HH:mm")
  
  def updateBackupDateLabel()= {
    backupDateLabel.text=dateFormat.format(new Date(FSPaths.lastBackup))
  }
  
  updateBackupDateLabel()
  SessionManager.registerSetupListener(()=>updateBackupDateLabel())
  contents+= new PathEditBox("Config-Directory",()=>FSPaths.configDir,(newDir)=> {
    FSPaths.setConfigDir(newDir)
  })+= new PathEditBox("Data-Directory",()=>FSPaths.dataDir,(newDir)=> {
    FSPaths.setDataDir(newDir)    
  })+= new PathEditBox("Backup-Directory",()=>FSPaths.backupDir,(newDir)=> {
    FSPaths.setBackupDir(newDir)    
  })+= new PathEditBox("Log-Directory",()=>FSPaths.logDir,(newDir)=> {
    FSPaths.setLogDir(newDir)    
  })+= new PathEditBox("Deploy-Directory",()=>FSPaths.deployDir,(newDir)=> {
    FSPaths.setDeployDir(newDir)
  })  +=
    new FormatLine(100,"Server Port",()=>FSPaths.serverPort.toString,(newPort)=>FSPaths.setServerPort(newPort.toInt))+=
    //new FormatLine(100,"Backup Hour",()=>FSPaths.backupHour.toString,(newHour)=>FSPaths.setBackupHour(newHour.toInt))+=
    new FormatLine(100,"Settings Object",()=>FSPaths.settingsObjectRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("SettingsObject",ref)
    })+= 
    new FormatLine(100,"ProjectRoot",()=>FSPaths.projectRootRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("ProjectRoot",ref)
    })+=
      new FormatLine(100,"LibraryRoot",()=>FSPaths.libraryRootRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("LibraryRoot",ref)
    })+=
      new FormatLine(100,"UserRoot",()=>FSPaths.userRootRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("UserRoot",ref)
    })+=Swing.VStrut(10)+= backupDateLabel+=
    Swing.VGlue
}