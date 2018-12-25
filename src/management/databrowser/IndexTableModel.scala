/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import server.storage._


/** Table Model for the Index list for a class
 * 
 */
object IndexTableModel extends AbstractTableModel {
	var handler:ClassIndexHandler=_
	var ixList: Array [IndexRecord]=Array ()	
	
  def setTypeHandler(nhandler:ClassIndexHandler): Unit =  {
		handler=nhandler
		InstFieldTableModel.setClass(handler.theClass)
		InstPropTableModel.setClass(handler.theClass)
		readTheList()					
  }
	
	def readTheList(): Unit = {
		ixList=handler.readFully()
		fireTableStructureChanged()
	}
	
	
  def getRowCount: Int =  ixList.length

  def getColumnCount: Int =  10

  def getValueAt(rowIndex: Int, columnIndex: Int): Object =  {
  	if(rowIndex>=ixList.length) return null
  	val a =ixList(rowIndex) 
  	columnIndex match 	{
			case 0 ⇒ java.lang.Integer.valueOf(rowIndex)
  		case 1 => a.inst.toString
  		case 2 => a.dataPos.toString
  		case 3 => a.dataLength.toString
				case 4 => a.propPos.toString
				case 5 => a.propLength.toString
				case 6 => a.linkPos.toString
				case 7 => a.linkLength.toString
				case 8 => a.collPos.toString
				case 9 => a.collLength.toString

  	}
  }
  
  override def getColumnName(col:Int): String =
  	col match 	{
			case 0 ⇒ "ix"
  		case 1 => "Inst-ID"
  		case 2 => "DPos"
  		case 3 => "DLen"
			case 4 => "PPos"
				case 5 => "PLen"
				case 6 => "LPos"
				case 7 => "LLen"
				case 8 => "CPos"
				case 9 => "CLen"
  	}


}