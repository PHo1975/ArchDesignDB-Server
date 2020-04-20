/**
 * Author: Peter Started:27.07.2010
 */
package management.databrowser

import definition.data.{LogIndexSet, TransType}
import javax.swing.table.AbstractTableModel
import server.storage._
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing
import scala.util.control.NonFatal

/**
 * 
 */
object LogFileModel extends AbstractTableModel {
	
   var transList:ArrayBuffer[LogIndexSet]=ArrayBuffer.empty
   
   def getRowCount:Int = transList.length

   
   override def getColumnClass(col:Int) = col match {
  	   case 0 => classOf[Int]
  		 case 1 => classOf[TransType.Value]
  		 case 2 => classOf[Int]  		 
  		 case 3 => classOf[String]
  		 case 4 => classOf[Integer]
  		 case 5 => classOf[Long]
  		 case 6 => classOf[Int]
   }		 
   
   def refresh(): Unit = try {
  	 transList=TransLogHandler.readFullIndex()
  	 println("refresh Translog "+transList.size)
		 Swing.onEDT{
			 fireTableStructureChanged()
		 }
   } catch {
		 case NonFatal(e)=>Log.e("refresh Translog:",e)
	 }
   
   def filter(typ:Int,inst:Int): Unit = {
     transList=transList.filter(r=> r.typ ==typ&&r.inst==inst)
     fireTableStructureChanged()
   }
   
   
   
   def getColumnCount:Int = 7
   
   def getValueAt(row:Int,col:Int): AnyRef = {
  	 val rec=transList(row)
  	 col match {
  	   case 0 => row.asInstanceOf[AnyRef]
  		 case 1 => rec.transTyp
  		 case 2 => rec.trID.asInstanceOf[AnyRef]
  		 case 3 => if(rec.typ>0) rec.typ.toString else "B"+rec.typ.toString()
  		 case 4 => Integer.valueOf(rec.inst)
  		 case 5 => rec.dataPos.asInstanceOf[AnyRef]
  		 case 6 => rec.dataLength.asInstanceOf[AnyRef]
  		 case _ => "**"
  	 }
   }
   
   override def getColumnName(col:Int): String = col match {
     case 0 => "Pos"
  	 case 1 => "Trans-Type "
  	 case 2 => "TransID"
  	 case 3 => "Obj-Type"
  	 case 4 => "Obj-Inst"
  	 case 5 => "Data pos"
  	 case 6 => "Data length"
  	 case _ => "**"
   }
  	 
}