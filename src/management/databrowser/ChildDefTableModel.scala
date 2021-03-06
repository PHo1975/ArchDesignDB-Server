/**
 * Author: Peter Started:29.11.2010
 */
package management.databrowser

import server.storage.{ServerCCD, ServerObjectClass}

/**
 * 
 */
class ChildDefTableModel extends ActivableAbstractTableModel {
	
  var childDefList:Seq[ServerCCD]=Seq.empty
  var propMod:PropFieldTableModel=_
  var propRow:Int=0
  
	
  def setValues(newList:Seq[ServerCCD],npropMod:PropFieldTableModel,npropRow:Int): Unit = {
  	childDefList=newList
  	propMod=npropMod
  	propRow=npropRow
  	fireTableDataChanged()
  	isDirty=false
  }
  
  
	
  def getRowCount: Int = { childDefList.size+1 }

  def getColumnCount: Int = { 3 }

  def getValueAt(row: Int, col: Int): Object = { 
  	if(row>=childDefList.size) null
  	else {
  		val cd=childDefList(row)
  		col match {
  			case 0 => cd.editorName
  			case 1 => cd.childClassID.asInstanceOf[AnyRef] 
  			case 2 => cd.action match {
  			  case Some(a)=>a.name
  			  case _=> "*"
  			} 
  		}
  	}
  }
  
  override def isCellEditable(row:Int,col:Int):Boolean = {  	
  	 if(propMod==null) false 
  	 else propMod.showLastLine
  }
  
  
  override def setValueAt(value:Object,row:Int,column:Int):Unit= { 
  	if(propMod==null) return
  	if(row==childDefList.size) { // create
  		childDefList=childDefList:+new ServerCCD("",0)
  	}
  	val defField=childDefList(row)
  	childDefList=childDefList.updated(row,
  		column match {
  		case 0 => defField.setEditorName(value.toString)
  		case 1 => defField.setChildClass(value.asInstanceOf[java.lang.Integer].intValue)
  		case 2 => defField.setAction(value.toString)
  		case _ => defField
  		})
  	println("ChilddefList changed:"+childDefList.mkString("\n"))	
  	fireTableDataChanged()
  	isDirty=true
  	val newPropField=propMod.propFieldList(propRow).setChildDefs(childDefList)
  	propMod.updateList(propMod.propFieldList.updated(propRow,newPropField))
  }
  
  
  override def getColumnClass(col:Int):Class[_] = {
  	col match {  		
  		case 0 => classOf[String]
  		case 1 => classOf[Int]
  		case 2 => classOf[String]
  		case _ => classOf[String]
  	}
  }

	override def update(theClass: ServerObjectClass): Unit = {}
}