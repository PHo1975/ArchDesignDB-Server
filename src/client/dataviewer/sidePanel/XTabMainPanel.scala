/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel

import java.awt.Dimension
import javax.swing.JViewport

import scala.swing._

/** Main Panel for XTab
 * 
 */
class XTabMainPanel(table:Table) extends Component with Container  {
   override lazy val peer=new JViewport with SuperMixin{
     override def getPreferredSize: Dimension = getView.getPreferredSize

     override def getMaximumSize: Dimension = getView.getPreferredSize
   }
   peer.setView(table.peer)
  xLayoutAlignment = 0d
  yLayoutAlignment = 0d
   def contents=List(table)
}