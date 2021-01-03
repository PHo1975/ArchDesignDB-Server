package management.databrowser

import client.ui.ViewConstants
import definition.data.Reference
import server.config.FSPaths
import transaction.handling.SessionManager

import java.awt.Dimension
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.swing.JFileChooser

class PathEditBox(labelText:String,getter:()=> String,setter: (String)=>Unit) extends 
		BoxPanel(Orientation.Vertical)	{
    import management.databrowser.PathEditBox._
    xLayoutAlignment=0
  val label: Label = ViewConstants.label(labelText + ":")
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
    revalidate()

  def update(): Unit = edit.text = getter()
		
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
  val backupDateLabel: Label = ViewConstants.label()
  val dateFormat=new SimpleDateFormat("dd.MM.yyyy HH:mm")

  def updateBackupDateLabel(): Unit = {
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
  })+= new PathEditBox("Image-Directory",()=>FSPaths.imageDir,(newDir)=> {
    FSPaths.setImageDir(newDir)
  }) +=
    new FormatLine(140,"Server Port",()=>FSPaths.serverPort.toString,(newPort)=>FSPaths.setServerPort(newPort.toInt))+=
    new FormatLine(140, "Web Port NoSSL", () => FSPaths.webServerPortNoSSL.toString, (newPort) => FSPaths.setWebServerPortNoSSL(newPort.toInt)) +=
    new FormatLine(140, "Web Port SSL", () => FSPaths.webServerPortSSL.toString, (newPort) => FSPaths.setWebServerPortSSL(newPort.toInt)) +=
    //new FormatLine(100,"Backup Hour",()=>FSPaths.backupHour.toString,(newHour)=>FSPaths.setBackupHour(newHour.toInt))+=
    new FormatLine(140,"Settings Object",()=>FSPaths.settingsObjectRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("SettingsObject",ref)
    })+= 
    new FormatLine(140,"ProjectRoot",()=>FSPaths.projectRootRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("ProjectRoot",ref)
    })+=
      new FormatLine(140,"LibraryRoot",()=>FSPaths.libraryRootRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("LibraryRoot",ref)
    })+=
      new FormatLine(140,"UserRoot",()=>FSPaths.userRootRef.sToString,(newString)=>{
        val ref=Reference(newString)
        FSPaths.setSettingsRef("UserRoot",ref)
    })+=Swing.VStrut(10)+= backupDateLabel+=
    Swing.VGlue
}