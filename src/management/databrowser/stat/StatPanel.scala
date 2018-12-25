package management.databrowser.stat

import java.awt.Dimension

import client.dataviewer.ViewConstants
import management.databrowser.MainWindow
import util.MyListView

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, BoxPanel, Button, Label, Orientation, ScrollPane, Swing}

class StatPanel extends BoxPanel(Orientation.Vertical) {
  val projList=new MyListView[String]
  val projMod=new ProjListModel
  projList.peer.setModel(projMod)
  val projScroller=new ScrollPane{
    viewportView=projList
    preferredSize=new Dimension(200,300)
  }
  val loadBut=new Button("Projekte Laden")
  val showBut=new Button("Daten zeigen:")
  val resultLab: Label = ViewConstants.label("Gesamtstunden:")
  resultLab.font = ViewConstants.labelFont
  val statPanel=new PrjStatPanel
  val prLab: Label = ViewConstants.label("Project:")
  prLab.font = ViewConstants.labelFont
  contents += loadBut += Swing.VStrut(40) += prLab += projScroller += Swing.VStrut(30) +=
    showBut+=resultLab
  
  listenTo(loadBut,showBut)
  
  reactions+={
    case ButtonClicked(`loadBut`)=> loadProjects()
    case ButtonClicked(`showBut`)=> showStatistics() 
  }

  def loadProjects(): Unit = projMod.load()


  def showStatistics(): Unit = {
    for(ix <-projList.selection.indices.headOption) {
      //println("Show "+ix+" "+projMod.projectList(ix)._1)
      MainWindow.rightPanel.addIt(statPanel, BorderPanel.Position.Center)
      MainWindow.rightPanel.peer.invalidate()
      MainWindow.rightPanel.repaint
      Swing.onEDT{
        statPanel.load(projMod.projectList(ix)._1)
        statPanel.revalidate
      }
    } 
  }

}