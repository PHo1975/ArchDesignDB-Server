/**
 * Author: Peter Started:07.09.2010
 */
package management.databrowser

import definition.data._
import javax.swing.table.AbstractTableModel
import server.storage._

/**
 * 
 */
object CollFuncTableModel extends AbstractTableModel{
	var collData:Option[CollFuncResultSet]=None
	
	def setCollData(data:Option[CollFuncResultSet]): Unit = {
		collData=data
		fireTableStructureChanged()
	}
	
	def getRowCount:Int = collData match {
			 case Some(a) => a.callResultList.size
			 case None => 0
		 }
	
	def getColumnCount:Int = 6
	
	def getValueAt(row:Int,column:Int):java.lang.Object =
		collData match {
			case Some(a)=>
			a.callResultList(row) match {
				case s:SingleCollFuncResult =>
					column match {
						case 0 => s.funcName
						case 1 => s.childType.toString
						case 2 => s.childField.toString
						case 3 => s.parentField.toString
						case 4 => s.parentPropField.toString
						case 5 => s.resultValue.toString
					}
				case l:ListCollFuncResult =>
					column match {
						case 0 => l.funcName
						case 1 => l.childType.toString
						case 2 => l.childField.toString
						case 3 => l.parentField.toString
						case 4 => l.parentPropField.toString
						case 5 => l.resultList.mkString(",")
					}
					case _ => null
			}
			case None=> null
		}


	
	override def getColumnName(column:Int): String =   {
		column match {
			case 0=> "funkName"
			case 1=> "childType"
			case 2=> "childField"
			case 3=> "parentField"
			case 4=> "parentPropField" 
			case 5=> "resultValue"
		}
  }

}