package management.databrowser.stat

import definition.expression.DateConstant
import javax.swing.table.AbstractTableModel
import server.storage.UsageStatFileHandler
import scala.collection.mutable.ArrayBuffer
import java.util.Date
import java.text.SimpleDateFormat
import server.comm.UserList


case class UseRecord(day:Date,user:Short,data:Array[Byte],numBlocks:Int){
  
}



class ProjectDataTableModel extends AbstractTableModel {
  val dateFormat=new SimpleDateFormat("dd.MM.yy")
  val theList=ArrayBuffer[UseRecord]()
  
  def readForProject(projID:Int)= {
    theList.clear()
    UsageStatFileHandler.loopAll((time,user,prj,data)=>{
      //println("found prj "+prj+" time:"+(new Date(time))+" user:"+user)
      if(prj==projID) theList+= new UseRecord(new Date(time),user,data,countBits(data)*5)

    })
    //println("ready "+theList.size)
    fireTableRowsInserted(0, theList.size-1)
    fireTableStructureChanged()
  }
  
  def getRowCount()= theList.size
  
  def getColumnCount()=4
  def getValueAt(row:Int, column:Int)=  if(row>=0 && row < theList.size) {
      val record=theList(row)      
      column match {
        case 0 => dateFormat.format(record.day)
        case 1 => UserList.getUserName(record.user)
        case 2 => (record.numBlocks/60)+"h "+(record.numBlocks%60)+"min"
        case 3 => record.data//.asInstanceOf[AnyRef] 
        case _=> "wrong column"
      }
    } else null 
   
    
  def countBits(array:Array[Byte])= {
    var result:Int=0
    for(b<-array;if b > 0) {
      var y =(b & 0x55) + ((b >>> 1) & 0x55)     
      y = (y & 0x33) + ((y >>> 2) & 0x33)  
      result+= (y & 0x07) + (y >>> 4)       
    }    
    result
  }

  def getSumMins=theList.foldLeft(0)((sum,record)=>sum+record.numBlocks)
  
  
  override def getColumnClass(ix:Int)= {
    if(ix==3) classOf[Array[Byte]]
    else classOf[String]
  }
}