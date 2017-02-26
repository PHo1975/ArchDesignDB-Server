/**
 * Author: Peter Started:27.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import server.storage._
import definition.data.TransType
import definition.data.LogIndexSet

import scala.swing.Swing

/**
 * 
 */
object LogFileModel extends AbstractTableModel {
	
   var transList:IndexedSeq[LogIndexSet]=IndexedSeq.empty
   
   
   
   def getRowCount:Int = {
       transList.length 	 
   }
   
   override def getColumnClass(col:Int)= col match {
  	   case 0 => classOf[Int]
  		 case 1 => classOf[TransType.Value]
  		 case 2 => classOf[Int]  		 
  		 case 3 => classOf[Int]
  		 case 4 => classOf[Integer]
  		 case 5 => classOf[Long]
  		 case 6 => classOf[Int]
   }		 
   
   def refresh() = {
  	 transList=TransLogHandler.readFullIndex
  	 println("refresh Translog "+transList.size)
		 Swing.onEDT{
			 fireTableStructureChanged()
		 }
   }
   
   def filter(typ:Int,inst:Int)= {
     transList=transList.filter(r=> r.typ ==typ&&r.inst==inst)
     fireTableStructureChanged()
   }
   
   
   
   def getColumnCount:Int = 7
   
   def getValueAt(row:Int,col:Int) = {
  	 val rec=transList(row)
  	 col match {
  	   case 0 => row.asInstanceOf[AnyRef]
  		 case 1 => rec.transTyp
  		 case 2 => rec.trID.asInstanceOf[AnyRef]
  		 //case 2 => rec.userID.asInstanceOf[AnyRef]
  		 case 3 => rec.typ.asInstanceOf[AnyRef]
  		 case 4 => new Integer(rec.inst)
  		 case 5 => rec.dataPos.asInstanceOf[AnyRef]
  		 case 6 => rec.dataLength.asInstanceOf[AnyRef]
  		 case _ => "**"
  	 }
   }
   
   override def getColumnName(col:Int) = col match {
     case 0 => "Pos"
  	 case 1 => "Trans-Type "
  	 case 2 => "TransID"
  	 //case 2 => "UserID"
  	 case 3 => "Obj-Type"
  	 case 4 => "Obj-Inst"
  	 case 5 => "Data pos"
  	 case 6 => "Data length"
  	 case _ => "**"
   }
  	 
}