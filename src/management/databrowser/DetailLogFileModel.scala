package management.databrowser

import java.util.Date

import definition.data.TransStepData
import javax.swing.table.AbstractTableModel
import server.storage.TransDetailLogHandler

import scala.swing.Swing


object DetailLogFileModel extends AbstractTableModel {
   var logList:IndexedSeq[TransStepData]=IndexedSeq.empty
   
   def getRowCount:Int = logList.size
   def getColumnCount:Int = 7
   
   def refresh() = {
     logList=TransDetailLogHandler.readFully
     Swing.onEDT {
       fireTableStructureChanged()
     }
   }
   
   override def getColumnClass(col:Int)= col match {
       case 0  => classOf[Int]
       case 1 => classOf[Int]
       case _ => classOf[String]
   }
   
   def printTime(time:Int)=  util.JavaUtils.shortDateTimeFormat.format(new Date(time*60000L))
   
   
   def getValueAt(row:Int,col:Int) = {
     val rec=logList(row)
     col match {
        case 0 => row.asInstanceOf[AnyRef]
        case 1 => rec.trID.asInstanceOf[AnyRef]
        case 2 => printTime(rec.time)
        case 3 => rec.userID
        case 4 => rec.firstInst.ref.sToString()
        case 5 => rec.multiInst.toString
        case 6 => rec.action
     }
   }

   
   override def getColumnName(col:Int) = 
     col match {
       case 0 => "Ix"
       case 1 => "TransID"
       case 2 => "Time"
       case 3 => "User"
       case 4 => "first Inst"
       case 5 => "Multi"
       case 6 => "Action"
     }    
   
}