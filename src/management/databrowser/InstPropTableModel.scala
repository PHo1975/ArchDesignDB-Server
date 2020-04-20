/**
 * Author: Peter Started:01.08.2010
 */
package management.databrowser

import definition.data._
import definition.typ._
import javax.swing.table.AbstractTableModel

/** Table Model for showing the property field values of an instance
 * 
 */
object InstPropTableModel extends AbstractTableModel {

	var theClass:AbstractObjectClass=_
	private var propData:Option[InstanceProperties]=None

	def setClass(newClass:AbstractObjectClass): Unit =	{
		theClass=newClass
		fireTableStructureChanged()
	}

	def setPropData(nprop:Option[InstanceProperties]): Unit = {
		propData=nprop
		fireTableStructureChanged()
	}

	def getRowCount:Int = if(theClass==null) 0
	else theClass.propFields.size+theClass.blockPropFields.size

	def getColumnCount:Int = 2

	def getValueAt(row:Int,column:Int):java.lang.Object = 	if(theClass==null) " "
	else if(column==0) {
		if(row<theClass.propFields.size) theClass.propFields(row).name
		else "B "+theClass.blockPropFields(row-theClass.propFields.size).name
	}
	else propData match{
		case Some(a: InstanceProperties) =>
			if(row<a.propertyFields.length)
				a.propertyFields(row).toString
			else "Psize:"+a.propertyFields.length
		case _ => " "
	}

	override def getColumnName(column:Int): String = column match {
		case 0 => "PropertyField"
		case 1 => "owned Instances"
		case _ => "***"
	}

	override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
}