package management.databrowser.stat

import java.awt.Color

import client.dataviewer.FieldColumnModel
import client.ui.ViewConstants
import javax.swing.JTable

import scala.swing.{BorderPanel, BoxPanel, Label, Orientation, ScrollPane, Table}

class PrjStatPanel extends BorderPanel {
   
  val columnMod: FieldColumnModel =new FieldColumnModel{
      createColumn(0,"Tag",70)
      createColumn(1,"Benutzer",80)      
      createColumn(2,"Zeit",70)
      createColumn(3,"Aktivität",700)
  }
  val stModel=new ProjectDataTableModel
  val renderer=new StatisticsRenderer
  
  val statTable: Table =new Table{
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
  
  val scroller: ScrollPane = new ScrollPane{
    viewportView=statTable    
  }
  
  val buttonPanel=new BoxPanel(Orientation.Vertical)
  val sumLabel: Label = ViewConstants.label()
  buttonPanel.contents+=sumLabel
  
  add(new Label("Daten"),BorderPanel.Position.North)
  add(scroller,BorderPanel.Position.Center)
  add(buttonPanel,BorderPanel.Position.South)

  def load(prjID: Int): Unit = {
    stModel.readForProject(prjID)
    val sumMins=stModel.getSumMins
    sumLabel.text="Summe: "+sumMins+" min = "+(sumMins/60)+" std"
  }
}