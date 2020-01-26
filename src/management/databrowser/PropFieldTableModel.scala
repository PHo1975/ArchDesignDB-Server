/**
 * Author: Peter Started:29.11.2010
 */
package management.databrowser

import definition.typ._
import server.storage.ServerObjectClass

/** Table Model for showing and setting up the property fields definition of a class
 * 
 */
class PropFieldTableModel(val showLastLine:Boolean) extends ActivableAbstractTableModel {
  
	var propFieldList:Seq[PropertyFieldDefinition]=Seq.empty
	
	def setValues(npList:Seq[PropertyFieldDefinition]): Unit = {
		propFieldList=npList
		fireTableDataChanged()
		isDirty=false
	}
	
	def updateList(newList:Seq[PropertyFieldDefinition]): Unit = {
		propFieldList=newList
		isDirty=true
	}
	
	override def isCellEditable(row:Int,col:Int):Boolean = {  	
  	 showLastLine  	    	
  }
	
  def getRowCount: Int =  propFieldList.size+(if(showLastLine)1 else 0)

  def getColumnCount: Int =  5

  def getValueAt(row: Int, col: Int): Object = {
  	if(row>=propFieldList.size) null
  	else {  		
  		 val fd=propFieldList(row)
  		 col match {
  			 case 0 => fd.name 
  			 case 1 => fd.single.asInstanceOf[AnyRef]
  			 case 2 => fd.allowedClass.asInstanceOf[AnyRef]
  			 case 3 => fd.hidden .asInstanceOf[AnyRef]
  			 case 4 => fd.volatile .asInstanceOf[AnyRef]
  		 }
  		}
  }
  
  override def setValueAt(value:Object,row:Int,column:Int):Unit= { 
  	if(row==propFieldList.size&&showLastLine)  // create
  		propFieldList =propFieldList:+ new PropertyFieldDefinition("")

  	val ov=propFieldList(row)
  	//System.out.println("Set Value At:"+value+" row:"+row+" col:"+column)
  	propFieldList = propFieldList.updated(row,
  		column match {
  			case 0 => ov.setName(value.toString)
  			case 1 => ov.setSingle(value.asInstanceOf[Boolean].booleanValue)
  			case 2 => ov.setAllowedClass(value.asInstanceOf[java.lang.Integer].intValue)
  			case 3 => ov.setHidden(value.asInstanceOf[Boolean].booleanValue)
  			case 4 => ov.setVolatile(value.asInstanceOf[Boolean].booleanValue)
  			case _ => ov
  		})
  	isDirty=true  	
  	fireTableDataChanged()
  }
  
  override def getColumnClass(col:Int):Class[_] = {
  	col match {  		
  		case 0 => classOf[String]
  		case 1 => classOf[Boolean]
  		case 2 => classOf[Int]
  		case 3 => classOf[Boolean]
  		case 4 => classOf[Boolean]
  		case _ => classOf[String]
  	}
  }

  def getPropField(row:Int): PropertyFieldDefinition =  propFieldList(row)
  
  def update(theClass:ServerObjectClass): Unit = if(isDirty){
  	if(showLastLine)
  		theClass.ownPropFields=propFieldList
  }
}