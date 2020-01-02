/**
 * Author: Peter Started:22.12.2010
 */
package client.print

import client.comm.ClientQueryManager
import client.dialog.Toast
import client.ui.ClientApp
import definition.data.{FormDescription, ParameterDescription, ResultElement}
import definition.expression._
import definition.typ.DataType
import javax.swing.table.AbstractTableModel


/**
 * 
 */
class ParamTabMod extends AbstractTableModel  {	
	val paramValues=collection.mutable.ArrayBuffer[(String,String,Constant)]()
	var paramDefs:Seq[ParameterDescription]=_	
	var currentForm:FormDescription= _	
	var currentParentType:Int = _
	
	def getEmptyValue(name:String): Constant =paramValues.find(_._1 == name) match {
	  case Some(paramV)=> paramV._3
	  case None => EMPTY_EX;
	}
	
	def setParamValue(name:String,desc:String,value:Constant): Unit = paramValues.indexWhere(_._1== name) match {
	  case -1=> paramValues.append((name,desc,value))
	  case ix=> paramValues(ix)=(name,desc,value)
	}
	
	def loadForm(newForm:FormDescription,getValueFunc:(String)=>Constant= getEmptyValue): Unit = {
	  //System.out.println("load Form newParams:"+newForm.params.mkString(", "))
		val newParams=newForm.params	
		if(paramDefs!=null) // remove unused params in list			
			for(op<-paramDefs;if !newParams.contains(op)){
			  val ix=paramValues.indexWhere(_._1==op.name)
			  //System.out.println("remove "+op)
			  if(ix> -1) paramValues.remove(ix)
			}													
	  // add new params
		for(np<-newParams) {
			val theValue=getValueFunc(np.name)
			val valChecked=if(theValue.isNullConstant) np.defaultValue else theValue
			val ix=if(paramDefs==null) -1 else paramValues.indexWhere(_._1 == np.name)		  
			if(ix<0) paramValues append( (np.name,np.desc,valChecked)) //append value
			else paramValues(ix)=(np.name,np.desc,valChecked) // OPTION : change value	to default	
		}		
		val dateValue=(PrintQuestionHandler.dateParamKey,"Druckdatum",getValueFunc(PrintQuestionHandler.dateParamKey)) 
		paramValues.indexWhere(_._1 == PrintQuestionHandler.dateParamKey ) match {
		  case -1 => paramValues.append(dateValue)
		  case ix => paramValues(ix)=dateValue
		}
	  paramDefs=newParams
		fireTableDataChanged()
	}
	
	def getRowCount: Int =  paramValues.size

  def getColumnCount: Int =  2

  def getValueAt(rowIndex: Int, columnIndex: Int): Object =
  	columnIndex match {
  		case 0 => paramValues(rowIndex)._1   		
  		case 1 => paramValues(rowIndex)._2  		
  		case 2 => paramValues(rowIndex)._3
  	}  

  
  def convertToType(ex:Constant,dtype:Int): Constant = 	ex.convertTo(dtype match {
			case 1 => DataType.IntTyp
			case 2 => DataType.DoubleTyp
			case 3 => DataType.CurrencyTyp
			case 4 => DataType.StringTyp
			case 5 => DataType.BoolTyp
			case 6 => DataType.DateTyp
			case 7 => DataType.VectorTyp
			case _ => DataType.undefined
		})

  
  
  override def setValueAt(value:Object,row: Int, col: Int): Unit = if(col==2){
  	val oldV=paramValues(row)
  	val dataType=paramDefs.find(_.name ==oldV._1) match {
  		case Some(pd)=> pd.dataType 
  		case _ => if(paramValues(row)._1 == PrintQuestionHandler.dateParamKey)6 else throw new IllegalArgumentException("cant find Datatype for "+oldV._1)
  	}
  	val dat:Constant= if(dataType==4) StringConstant(value.toString) else
  	  value match {
  	  case st:String =>
				convertToType((StringParser.parse(value.toString) match {
          case ex: Expression => ex
          case err: ParserError =>
						ClientQueryManager.printErrorMessage(err.message)
						new Toast(err.message, null, ClientApp.top)
						throw new IllegalArgumentException(err.message)
				}).getValue,dataType)
			case b:java.lang.Boolean=> BoolConstant(b)
  	}
  	  
  	//println("set Value "+dat+" "+dat.getType)
  	paramValues(row)=(oldV._1,oldV._2,dat)
  	fireTableDataChanged()
  }
  
  override def getColumnClass(col:Int): Class[_ >: String <: Object] = {
    super.getColumnClass(col)
  	col match {
  		case 0 => classOf[String]
  		case 1 => classOf[String]
  		case 2 => classOf[Object]
  	}
  }
  
  override def isCellEditable(row:Int,col:Int): Boolean =	col==2

  
  def getParams:Iterable[ResultElement]= paramValues.map(a=>ResultElement(a._1,a._3))

	
}