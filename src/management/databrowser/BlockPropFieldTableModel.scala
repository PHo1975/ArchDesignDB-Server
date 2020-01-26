package management.databrowser

import definition.typ.BlockPropertyFieldDefinition
import server.storage.ServerObjectClass

class BlockPropFieldTableModel(val showLastLine:Boolean) extends ActivableAbstractTableModel {
  var propFieldList:Seq[BlockPropertyFieldDefinition]=Seq.empty
  def setValues(npList:Seq[BlockPropertyFieldDefinition]): Unit = {
    propFieldList=npList
    fireTableDataChanged()
    isDirty=false
  }

  def updateList(newList:Seq[BlockPropertyFieldDefinition]): Unit = {
    propFieldList=newList
    isDirty=true
  }

  override def isCellEditable(row:Int,col:Int):Boolean = {
    showLastLine
  }

  def getRowCount: Int =  propFieldList.size+(if(showLastLine)1 else 0)

  def getColumnCount: Int =  2

  def getValueAt(row: Int, col: Int): Object = {
    if(row>=propFieldList.size) null
    else {
      val fd=propFieldList(row)
      col match {
        case 0 => fd.name
        case 1 => fd.blockClass.asInstanceOf[AnyRef]
      }
    }
  }

  override def setValueAt(value:Object,row:Int,column:Int):Unit= {
    if(row==propFieldList.size&&showLastLine)  // create
      propFieldList =propFieldList:+ new BlockPropertyFieldDefinition("",0)

    val ov=propFieldList(row)
    propFieldList = propFieldList.updated(row,
      column match {
        case 0 => new BlockPropertyFieldDefinition(value.toString,ov.blockClass)
        case 1 => new BlockPropertyFieldDefinition(ov.name,value.asInstanceOf[java.lang.Integer].intValue)
        case _ => ov
      })
    isDirty=true
    fireTableDataChanged()
  }

  override def getColumnClass(col:Int):Class[_] = {
    col match {
      case 0 => classOf[String]
      case 1 => classOf[Int]
      case _ => classOf[String]
    }
  }

  def getPropField(row:Int): BlockPropertyFieldDefinition =  propFieldList(row)

  def update(theClass:ServerObjectClass): Unit = if(isDirty){
    if(showLastLine)
      theClass.ownBlockPropFields=propFieldList
  }
}
