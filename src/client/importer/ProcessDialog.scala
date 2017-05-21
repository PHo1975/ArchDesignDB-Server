package client.importer

import java.awt.{Point, Rectangle}

import client.dataviewer.ViewConstants

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, BoxPanel, Button, Dialog, Label, ProgressBar, Swing, Window}

class ProcessDialog(w:Window,cancelCallBack:()=>Unit) extends Dialog(w) {
  val cancelBut=new Button("Abbrechen")
  val filesLabel: Label = ViewConstants.label("Datei")
  val currFileLabel: Label = ViewConstants.label("Akt")
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

  def cancelImport(): Unit = {
    cancelCallBack()
    close()
  }

  def showDialog(importName: String, pos: Point, numFiles: Int): Unit = {
    filesBar.max=numFiles
    filesBar.min=1
    filesBar.value=1
    currFileBar.value=0
    title="Import "+importName
    val cpos=FileImportManager.correctScreenPos(pos,610,360)   
    bounds=new Rectangle(cpos.x,cpos.y,600,350)
    visible=true
  }

  def setCurrFile(procData: ProcData): Unit = {
    //println("Set curr File "+procData.file)
    filesBar.value=procData.fileNr+1
    filesLabel.text="Datei "+procData.fileNr+" von "+ filesBar.max+" Größe:"+(procData.file.length()/1024)+" kB"
    currFileBar.value=0
    currFileBar.max=100
    currFileLabel.text="Importiere "+procData.file.getName+ "..."
  }

  def setCurrentFileState(state: Int): Unit = {
    currFileBar.value=state
    currFileBar.repaint()
  }
  
}