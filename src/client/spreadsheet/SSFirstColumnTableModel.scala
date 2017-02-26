package client.spreadsheet
import javax.swing.table.AbstractTableModel
import javax.swing.table.DefaultTableColumnModel
import javax.swing.table.TableColumn

class SSFirstColumnTableModel(controller:SpreadSheetController) extends AbstractTableModel  {
  def mainTableModel=controller.tableModel
  
  val columnModel=new DefaultTableColumnModel
  val firstColumn=new TableColumn(0,40)
  firstColumn.setHeaderValue(" ")
  firstColumn.setResizable(false)
  columnModel.addColumn(firstColumn)
  
  def getRowCount(): Int = mainTableModel.getRowCount()

  def getColumnCount(): Int = 1
  
  def getValueAt(row: Int, col: Int): Object = (row+1).toString 
  

}