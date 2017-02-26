package management.databrowser.stat

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.Label
import scala.swing.Button
import scala.swing.Swing
import java.awt.Dimension
import util.MyListView
import scala.swing.event.ButtonClicked
import scala.swing.BorderPanel
import management.databrowser.MainWindow

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
  val resultLab=new Label("Gesamtstunden:")
  val statPanel=new PrjStatPanel
  
  contents+=loadBut+=Swing.VStrut(40)+=new Label("Project:")+=projScroller+=Swing.VStrut(30)+=
    showBut+=resultLab
  
  listenTo(loadBut,showBut)
  
  reactions+={
    case ButtonClicked(`loadBut`)=> loadProjects()
    case ButtonClicked(`showBut`)=> showStatistics() 
  }
  
  def loadProjects()= {
    projMod.load()
  }
  
  def showStatistics()= {
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