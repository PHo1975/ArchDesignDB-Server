/**
 * Author: Peter Started:26.07.2010
 */
package management.databrowser

import javax.swing.table.AbstractTableModel
import definition.typ._
import definition.data._
import definition.expression.StringParser
import server.storage._
import transaction.handling._
import definition.comm._
import definition.expression._

import scala.util.control.NonFatal

/** Table model for the Instance data field table
 *  will show Fields of a certain Instance data
 */
object InstFieldTableModel extends AbstractTableModel {
  var theClass:AbstractObjectClass=null
  var instance:InstanceData=null  
	
	def setClass(newClass:AbstractObjectClass) = 	{
		theClass=newClass
		instance=null
		fireTableStructureChanged()
	}
  
  def setInstance(newInst:InstanceData) =   {  	
  	instance=newInst  	
  	fireTableStructureChanged()
  }
		
  def getRowCount():Int =  {
     if(theClass==null) 0
     else theClass.fields.size+ 2     
  }
     
  override def isCellEditable(rowIndex:Int,columnIndex:Int) = (rowIndex>0) && (columnIndex==1)
  
  def getColumnCount():Int = 4
  
  def getValueAt(row:Int,column:Int):java.lang.Object =   {
  	if(theClass==null) " "
  	else 
    column match {
  		case 0 =>    	row match{    		
    		case 0 => "Owner"
    		case 1 => "SU-Owner"
    		case _ => (row-2).toString+"|"+theClass.fields(row-2).name
      }
  		case 1 => if(instance==null) " " else  row match {    		
    		case 0 => instance.owners.mkString(", ")
    		case 1 => instance.secondUseOwners .mkString(", ")
    		case _ => if(row<=instance.fieldData.size) instance.fieldData(row-2).getTerm else " " 
    	}
  		case 2 => if(instance==null || row<2) " " else {
				if(row-2 < instance.fieldValue.size) instance.fieldValue(row-2).toString
				else "array 2 small"
  		}
  		case 3 => if(instance==null || row<2) " " else {
				if(row-2 < instance.fieldValue.size)
  			getTermTree(instance.fieldData(row-2))
				else "array 2 small"
  		}  		
  		case _ => "bla"
  	}    
  }
  
  def getTermTree(ex:Expression):String= try{
      ex match {
        case c:Constant=> c.getTerm
        case b:BinaryOperation=> "("+getTermTree(b.left)+" "+b.operator.opChar+" " +getTermTree(b.right)+")="+b.getValue
        case co:CollectingFuncCall=>co.getTerm+"="+co.getValue
        case r:FieldReference=>r.getTerm+"="+r.getValue
        case p:ParentFieldRef=>p.getTerm+"="+p.getValue
        case f:FunctionCall=>f.name+"("+f.params .map(getTermTree).mkString(", ")+")="+f.getValue
      }
    } catch {case NonFatal(e)=>util.Log.e("ex:"+ex.getTerm,e);"Error("+ex.getTerm+")"
	case other:Throwable =>println(other);System.exit(0);null}

  
  override def setValueAt(obj:Object,row:Int,column:Int) =  {
  	if((row>1)&&(column==1)&& instance!=null)  	{
  		val f:Byte=(row-2).toByte  	  		
  		TransactionManager.doTransaction(0,ClientCommands.writeField.id.toShort,instance.ref,false,0, {	
  			TransactionManager.tryWriteInstanceField(instance.ref,f,StringParser.parse(obj.toString).withException)
  		})
  		
  	}
  }
  
  override def getColumnName(column:Int) =  {
  	column match {
  		case 0 => "Field"
  		case 1 => "Term"
  		case 2 => "Value"
  		case 3 => "TermTree"
  		case _ => "***"
  	}
  }
}