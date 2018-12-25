/**
 * Author: Peter Started:17.04.2011
 */
package management.databrowser

import definition.data.ExtFieldRef
import javax.swing.table.AbstractTableModel

/**
 * 
 */
object RefLinksTableModel extends AbstractTableModel {

	
	private var refData:Seq[(Int,List[ExtFieldRef])]=Seq.empty
	
	
	
	
	
	def setRefData(nprop:Seq[(Int,List[ExtFieldRef])]): Unit = {
		refData=nprop
		//System.out.println(" set refData "+refData)
		
  	//System.out.println("Set inst "+theVersion)
  	fireTableStructureChanged()
	}
	
	
	def getRowCount():Int =
  {
     refData.size     
  }
	
	
	def getColumnCount():Int = 2
	
	
	def getValueAt(row:Int,column:Int):java.lang.Object =
  {
  	
    if(column==0)
    {  	
    	  refData(row)._1.toString    	
    }
    else
    {	    	
    	 refData(row)._2 .mkString(", ")
    }
  }
	
	override def getColumnName(column:Int) =
  {
  	column match {
  		case 0 => "Field"
  		case 1 => "ExtFieldRefs"  		
  		case _ => "***"
  	}
  }

}