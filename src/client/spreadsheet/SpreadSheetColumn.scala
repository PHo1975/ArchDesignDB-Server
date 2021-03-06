package client.spreadsheet

import client.comm.ClientQueryManager
import definition.comm.KeyAble
import definition.data.{InstanceData, Reference}
import definition.expression.{Expression, IntConstant, StringConstant}
import definition.typ.DataType

import scala.collection.immutable.HashMap
import scala.util.control.NonFatal


class SpreadSheetColCellList(val col:Int,controller:SpreadSheetController){ 
  //val lock=new Object
	var cellMap: HashMap[Int, SpreadSheetCell] =collection.immutable.HashMap[Int,SpreadSheetCell]()

	def maxRowNum: Int = if(cellMap.isEmpty)0 else cellMap.keys.max
	
	override def toString: String ="CellList "+col+" list:"+cellMap.mkString(" | ")

	def getCell(row:Int): SpreadSheetCell = {if(cellMap.contains(row))cellMap(row) else null}

	def addCell(cell:SpreadSheetCell):Unit={
    cellMap.find(_._2.ref==cell.ref) match{
      case Some((_,oldCell))=>cellMap= cellMap-oldCell.row
      case None =>
    }
    cellMap=cellMap.updated(cell.row,cell)
  }

	def removeCell(row:Int):Unit= cellMap= cellMap- row

	def removeCell(rref:Reference):Boolean =
    cellMap.find(a=> a._2.ref==rref) match {
      case Some ((row,_))=>
        cellMap=cellMap- row
        true
      case _=>false
    }

	
	def getValue(row:Int): Expression = getCell(row) match {
		case null => null
		case cell => cell.exp
	}
	
	def findCellByRef(ref:Reference): Option[SpreadSheetCell] = cellMap.values.find(_.ref==ref)
		
	protected def getDBDataType(typ:DataType.Value):Int= typ match {
	  case DataType.DoubleTyp=>SpreadSheet.spreadSheetDoubleCellType	
	  case DataType.StringTyp=>SpreadSheet.spreadSheetStringCellType				 
		case o => throw new IllegalArgumentException("Unkown datatype "+o)
	}

	protected def getDBDataType(ex:Expression):Int= ex.getType match {
				case DataType.DoubleTyp|DataType.IntTyp|DataType.LongTyp|DataType.BoolTyp|DataType.FieldRefTyp|DataType.undefined=>SpreadSheet.spreadSheetDoubleCellType
				case DataType.VariableTyp=> getDBDataType(ex.getValue)
				case DataType.BinOp=> getDBDataType(ex.getValue)
				case DataType.FunctionCall=> getDBDataType(ex.getValue)
				case DataType.StringTyp=>SpreadSheet.spreadSheetStringCellType				 
				case o => throw new IllegalArgumentException("Unkown datatype "+o)
	}


	def parseCell(text:String):Expression= try {
	  controller.myParser.parse(text)
	} catch {
		case NonFatal(e)=> StringConstant(text)
		case other:Throwable =>println(other);System.exit(0);null
	}

	def clear(): Unit = cellMap=collection.immutable.HashMap.empty

}


class SpreadSheetColumnData(val col:Int,val ref:Reference,val name:String,val width:Int,controller:SpreadSheetController) extends KeyAble[Int]  {
  def key: Int =col
	def this(data:InstanceData,ncontroller:SpreadSheetController)= this(data.fieldValue.head.toInt,data.ref, data.fieldValue(1).toString,data.fieldValue(2).toInt,ncontroller)
	def writeWidth(newWidth:Int): Unit = {
	  //println("write colWidth "+col+" Width:"+width)
	  ClientQueryManager.writeInstanceField(ref,2,IntConstant(newWidth))
	  } 
	override def toString: String ="ColumnData "+col + " ref:"+ref+" width:"+width
}