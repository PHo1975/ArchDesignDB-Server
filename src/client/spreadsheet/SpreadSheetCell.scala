package client.spreadsheet
import definition.data.InstanceData
import definition.data.Reference
import definition.expression.Expression
import definition.typ.DataType
import java.io.DataInput
import definition.data.Referencable
import definition.data.EMPTY_REFERENCE
import definition.expression.EMPTY_EX

/*trait SpreadSheetCell {
  def row:Int
  def col:SpreadSheetColumn
  def ref:Reference
  def isSameType(other:SpreadSheetCell):Boolean
}

class DoubleSpreadSheetCell(val row:Int,val col:SpreadSheetColumn,val ref:Reference,val value:Double) extends SpreadSheetCell {
   def this(ncol:SpreadSheetColumn,data:InstanceData) =this(data.fieldValue(0).toInt,ncol,data.ref,data.fieldValue(1).toDouble)
   override def toString="["+row+"] "+value
   def isSameType(other:SpreadSheetCell):Boolean= other.isInstanceOf[DoubleSpreadSheetCell]
}

class StringSpreadSheetCell(val row:Int,val col:SpreadSheetColumn,val ref:Reference,val value:String) extends SpreadSheetCell {
   def this(ncol:SpreadSheetColumn,data:InstanceData) =this(data.fieldValue(0).toInt,ncol,data.ref,data.fieldValue(1).toString)
   override def toString="["+row+"] "+value
   def isSameType(other:SpreadSheetCell):Boolean= other.isInstanceOf[StringSpreadSheetCell]
}*/

class SpreadSheetCell(val row:Int,val col:Int,val ref:Reference,val exp:Expression,val dataType:DataType.Value) extends Referencable {
   def this(data:InstanceData,ntype:DataType.Value) =this(data.fieldValue.head.toInt,data.fieldValue(1).toInt,data.ref,data.fieldData(2),ntype)
   override def toString=ref.sToString+"["+row+"] "+exp.getValue.toString
   def isSameType(other:SpreadSheetCell):Boolean= other.dataType==dataType
}


 

object SpreadSheetCell {
   
   def apply(data:InstanceData)= new SpreadSheetCell(data,data.ref.typ match {
     case SpreadSheet.spreadSheetDoubleCellType => DataType.DoubleTyp
     case SpreadSheet.spreadSheetStringCellType => DataType.StringTyp
     case _=> throw new IllegalArgumentException("Unknown Cell Type "+data.ref.typ)
   })
   def apply(ref:Reference,in:DataInput):SpreadSheetCell= {
     val nfields=in.readByte	
     if(nfields==0) {util.Log.e("undefined object "+ref);null}
     else {
       if (nfields != 3) util.Log.e("SpreadSheetCell wrong number of fields " + nfields + " " + ref)
       val col = Expression.read(in).getValue.toInt
       val row = Expression.read(in).getValue.toInt
       val exp = Expression.read(in)
       InstanceData.readOwners(in)
       InstanceData.readSecondUseOwners(in)
       in.readBoolean
       new SpreadSheetCell(row, col, ref, exp, ref.typ match {
         case SpreadSheet.spreadSheetDoubleCellType => DataType.DoubleTyp
         case SpreadSheet.spreadSheetStringCellType => DataType.StringTyp
         case _ => throw new IllegalArgumentException("Unknown Cell Type " + ref.typ)
       })
     }
   }
}