/**
 * Author: Peter Started:28.11.2010
 */
package management.databrowser


import definition.expression.StringParser
import definition.typ._
import javax.swing.table.AbstractTableModel
import server.storage.ServerObjectClass

/**
 * 
 */
abstract class ActivableAbstractTableModel extends AbstractTableModel {
	var isDirty:Boolean=false
	
	protected var _isCreating:Boolean=false
	def isCreating_=(newValue:Boolean): Unit = {
		_isCreating=newValue
		fireTableDataChanged()
	}
	def isCreating: Boolean =_isCreating
	
	def update(theClass:ServerObjectClass): Unit
}



class FieldDefTableModel(ownfieldTable:Boolean) extends ActivableAbstractTableModel {
	var offset:Int=0
  protected var fieldDefList:Seq[AbstractFieldDefinition]=Seq.empty
  protected var fieldSettingList:Seq[FieldSetting]=Seq.empty
  
  def getRowCount: Int = fieldDefList.size+( if(ownfieldTable)1 else 0)

  def getColumnCount: Int = { 10 }
  
    
  override def getColumnClass(col:Int):Class[_] = {
  	col match { 
  	  case 0 => classOf[String]
  		case 1 => classOf[String]
  		case 2 => classOf[DTWrap]
  		case 3 => classOf[EnumDefinition]
  		case 4|5|6|7 => classOf[Boolean]  		
  		case _ => classOf[String]
  	}
  }
  
  override def isCellEditable(row:Int,col:Int):Boolean =
  	if (col>2) true
  	else if(_isCreating) ownfieldTable
  	else false    	


  def getValueAt(row: Int, column: Int): Object =
  	if(row>=fieldDefList.size) null
  	else {
  		if(column<4) {
  		 val fd=fieldDefList(row)  		 
  		 column match {
  		   case 0 => ( row+offset ).toString
  			 case 1 => fd.name 
  			 case 2 => DataType.wrapMap(fd.typ).asInstanceOf[AnyRef]
  			 case 3 => fd match {
  				 case f:EnumFieldDefinition => SystemSettings().enumByID( f.enumID)
  				 case _ => NOENUM
  			 }
  		 }
  		}
  		else {
  			val fs=fieldSettingList(row)
  			column match {
  				case 4 => fs.readOnly .asInstanceOf[AnyRef]
  				case 5 => fs.showFormula .asInstanceOf[AnyRef]
  				case 6 =>fs.formatField.asInstanceOf[AnyRef]
  				case 7 => fs.visible.asInstanceOf[AnyRef]
  				case 8 => fs.editor
  				case 9 => fs.startValue.getTerm
  				case 10 => fs.formString
  				case _ => null
  			}
  		}
  	}  	 

  
  override def setValueAt(value:Object,row:Int,column:Int):Unit= {  	
  	if(row==fieldDefList.size&&ownfieldTable) {
  		// adding field  		
  		val newField =new FieldDefinition("",DataType.undefined )
  		fieldDefList=fieldDefList:+newField
  		fieldSettingList=fieldSettingList:+new FieldSetting(fieldDefList.size-1+offset)
  	}
  	if ( column!=0 &&
  	!(column<4 && (  !ownfieldTable)) &&
  	!(column==2 && !_isCreating)) {
			if (column < 4) {
				val field = fieldDefList(row)
				//println("Mod set value:"+value+" col:"+column)
				fieldDefList = fieldDefList.updated( row,
					column match {
						case 1 => field.setName(value.toString)
						case 2 => field.setType(value.asInstanceOf[DTWrap].typ)
						case _ => field.setEnumID(value.asInstanceOf[EnumDefinition].id)
					})

			}
			else {
				val fs = {
					val w = fieldSettingList(row)
					if (w.fieldNr == -1) { // empty setting
						val n = new FieldSetting(row + offset)
						fieldSettingList = fieldSettingList.updated( row, n)
						n
					}
					else w
				}
				column match {
					case 4 => fs.readOnly = value.asInstanceOf[Boolean].booleanValue
					case 5 => fs.showFormula = value.asInstanceOf[Boolean].booleanValue
					case 6 => fs.formatField = value.asInstanceOf[Boolean].booleanValue
					case 7 => fs.visible = value.asInstanceOf[Boolean].booleanValue
					case 8 => fs.editor = value.toString
					case 9 => fs.startValue = StringParser.parse(value.toString, fieldDefList(row).typ).withException
					case 10 => fs.formString = value.toString
					case _ =>
				}
			}
			isDirty = true
			//System.out.println("set Value ready "+fieldDefList.mkString("\n "))
			fireTableDataChanged()
		}
  }

  def setValues(newDefs:Seq[AbstractFieldDefinition],newSettings:Seq[FieldSetting],noffset:Int):Unit= {
    //println("set values "+newSettings.mkString("\n"))
    offset=noffset
  	fieldDefList=newDefs
  	fieldSettingList=newSettings
  	isDirty=false
  	super.fireTableDataChanged()
  }
  
  def update(theClass:ServerObjectClass): Unit = if(isDirty&& ownfieldTable) theClass.ownFields=fieldDefList

  
  def getFieldSettings: Seq[FieldSetting] =fieldSettingList.filter(_.fieldNr> -1)
}