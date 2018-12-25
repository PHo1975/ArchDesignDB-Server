/**
  * Author: Peter Started:07.03.2011
  */
package client.dataviewer.sidePanel

/** Header Panel for XTab
  *  background=Color.green
  */

import java.awt.event.{ComponentAdapter, ComponentEvent}
import java.awt.{BorderLayout, Dimension, Insets}

import javax.swing.table.{DefaultTableCellRenderer, JTableHeader, TableCellRenderer}
import javax.swing.{JButton, JLabel, JPanel, JTable}

import scala.swing._


class XTabHeaderPanel(header:JTableHeader) extends Component {

  override lazy val peer: JTableHeader = header
  xLayoutAlignment=0d
  yLayoutAlignment = 0d
  header.setReorderingAllowed(false)
  peer.addComponentListener(new ComponentAdapter(){
    override def componentResized(evt: ComponentEvent): Unit = {
      peer.setMaximumSize(new Dimension(peer.getPreferredSize.width,Short.MaxValue))
      peer.invalidate()
    }
  })
}

class XRenderer(contr:XTabSidePanelController) extends JPanel with TableCellRenderer {
  setLayout(new BorderLayout())
  val renderClass: TableCellRenderer = contr.getDefaultHeaderRenderer
  val render: DefaultTableCellRenderer = renderClass.asInstanceOf[DefaultTableCellRenderer]
  val label: JLabel = render.getTableCellRendererComponent(null, " ", false, false, 0, 0).asInstanceOf[JLabel]
  val but=new JButton("x")
  but.setMargin(new Insets(0,0,0,0))
  but.putClientProperty("JComponent.sizeVariant", "mini")
  but.updateUI()
  add(but,BorderLayout.EAST)
  def getTableCellRendererComponent(table:JTable, value:Object ,isSelected:Boolean, hasFocus:Boolean,
                                    rowIndex:Int, ColIndex:Int):java.awt.Component ={
    label.setText(if(value==null) " " else value.toString)
    add(label,BorderLayout.CENTER)
    //label.getTableCellRendererComponent(table,value,isSelected,hasFocus,rowIndex,ColIndex)
    this
  }
  // The following methods override the defaults for performance reasons
  override def validate(): Unit = {}

  override def revalidate(): Unit = {}
  override def firePropertyChange(propertyName:String , oldValue:Object, newValue:Object ): Unit = {}
  override def firePropertyChange(propertyName:String , oldValue:Boolean, newValue:Boolean): Unit = {}
}
