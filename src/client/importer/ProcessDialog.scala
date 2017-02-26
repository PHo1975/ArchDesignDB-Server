package client.importer

import scala.swing.Dialog
import scala.swing.Window
import java.awt.Dimension
import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.BoxPanel
import scala.swing.Swing
import scala.swing.Label
import scala.swing.ProgressBar
import scala.swing.event.ButtonClicked
import java.awt.Point
import java.awt.Rectangle

class ProcessDialog(w:Window,cancelCallBack:()=>Unit) extends Dialog(w) {
  val cancelBut=new Button("Abbrechen")
  val filesLabel=new Label("Datei")
  val currFileLabel=new Label("Akt")
  val filesBar=new ProgressBar
  val currFileBar=new ProgressBar
  
  
  val bottomBox=new BoxPanel(scala.swing.Orientation.Horizontal) {
    contents+=Swing.HGlue+=cancelBut
  }
  
  val topPanel= new BoxPanel(scala.swing.Orientation.Vertical) {
    contents+=filesLabel+=filesBar+=Swing.VStrut(10)+=currFileLabel+=currFileBar
  }
  
  val mainPanel=new BorderPanel(){
    add(topPanel,BorderPanel.Position.Center)
    add(bottomBox,BorderPanel.Position.South)
    listenTo(cancelBut)
    reactions += {
      case ButtonClicked(`cancelBut`) => cancelImport()
    }
  }  
    
  //modal=true    
  contents=mainPanel
 
  def cancelImport() = {
    cancelCallBack()
    close()
  }
  
  def showDialog(importName:String,pos:Point,numFiles:Int)= {
    filesBar.max=numFiles
    filesBar.min=1
    filesBar.value=1
    currFileBar.value=0
    title="Import "+importName
    val cpos=FileImportManager.correctScreenPos(pos,610,360)   
    bounds=new Rectangle(cpos.x,cpos.y,600,350)
    visible=true
  }
  
  def setCurrFile(procData:ProcData)= {
    //println("Set curr File "+procData.file)
    filesBar.value=procData.fileNr+1
    filesLabel.text="Datei "+procData.fileNr+" von "+ filesBar.max+" Größe:"+(procData.file.length()/1024)+" kB"
    currFileBar.value=0
    currFileBar.max=100
    currFileLabel.text="Importiere "+procData.file.getName+ "..."
  }
  
  def setCurrentFileState(state:Int) = {
    currFileBar.value=state
    currFileBar.repaint()
  }
  
}