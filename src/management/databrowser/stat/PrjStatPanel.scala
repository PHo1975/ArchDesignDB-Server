package management.databrowser.stat

import scala.swing.Table
import client.dataviewer.FieldColumnModel
import java.awt.Color
import scala.swing.BorderPanel
import scala.swing.ScrollPane
import scala.swing.Label
import scala.swing.BoxPanel
import scala.swing.Orientation
import javax.swing.JTable

class PrjStatPanel extends BorderPanel {
   
  val columnMod=new FieldColumnModel{
      createColumn(0,"Tag",70)
      createColumn(1,"Benutzer",80)      
      createColumn(2,"Zeit",70)
      createColumn(3,"Aktivität",700)
  }
  val stModel=new ProjectDataTableModel
  val renderer=new StatRenderer
  
  val statTable=new Table{
     selection.intervalMode=Table.IntervalMode.Single
     selection.elementMode=Table.ElementMode.None    
     peer.setModel(stModel)
     peer.setAutoCreateColumnsFromModel(false)
     peer.setColumnModel(columnMod)
     peer.setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
     peer.setDefaultRenderer(classOf[Array[Byte]],renderer)
     showGrid=true
     gridColor=Color.gray
  }
  
  val scroller = new ScrollPane{
    viewportView=statTable    
  }
  
  val buttonPanel=new BoxPanel(Orientation.Vertical)
  val sumLabel=new Label()
  buttonPanel.contents+=sumLabel
  
  add(new Label("Daten"),BorderPanel.Position.North)
  add(scroller,BorderPanel.Position.Center)
  add(buttonPanel,BorderPanel.Position.South)
  
  def load(prjID:Int)= {
    stModel.readForProject(prjID)
    val sumMins=stModel.getSumMins
    sumLabel.text="Summe: "+sumMins+" min = "+(sumMins/60)+" std"
  }
}