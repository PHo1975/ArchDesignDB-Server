package client.spreadsheet
import definition.data.InstanceData
import definition.expression._
import definition.typ.DataType

import java.io.DataOutput

class SpreadSheetParser extends StringParser {
  
  def spreadSheetVariableName:Parser[String]="""[a-zA-Z]{1,2}\d{1,4}""".r

  def spreadSheetVariable: Parser[Expression] = spreadSheetVariableName ^^ (name => SSVariable(name, EMPTY_EX))
  
  def spreadSheetCollFunc:Parser[Expression]=( (ident <~ "(") ~ (spreadSheetVariableName<~":")~(spreadSheetVariableName <~")")) ^^ { case name ~ start ~ end => SSCollProxy(name, new RangeSelection(SpreadSheetUtil.lettersToCoords(start), SpreadSheetUtil.lettersToCoords(end))) }
  
  override  def elem : Parser[Expression] =( currValue||| unitNumber)|
       groupedNumber ^^ {y => val doubleVal=y.replace(".","").replace(',','.').toDouble
         if(StringParser.isIntValue(doubleVal)) IntConstant(doubleVal.toInt) else DoubleConstant(doubleVal) } |
  	   doubleNumber ^^ {y => DoubleConstant(y.replace(',','.').toDouble) } |  	   
       intNumber ^^ {x => DoubleConstant(x.toInt)}      |trueVal|falseVal| 
       stringVariable^^ {s=> StringConstant(s.substring(1,s.length-1))}|
       fieldRef | function |variable |spreadSheetVariable|spreadSheetCollFunc| collFunction | parentFieldRef | vector |
        ("(" ~> comp <~ ")") | failure("Zahl oder Wert fehlt") 
        
  def parse(text : String,expectedType:DataType.Value=DataType.undefined):Expression = {
		if(text.length==0) Expression.generateNullConstant(expectedType)
    else {
      val result: ParseResult[Expression] = parseAll(comp, text)
      result match {
        case Success(x, _) => x
        case NoSuccess(err, next) =>
          throw new IllegalArgumentException("Failure when parsing " +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" + next.pos.longString)
        case other=> throw   new IllegalArgumentException ("Other Error when parsing:"+other)
      }
    }
	}      

}


case class SSCollProxy(name:String,range:SpreadSheetRange) extends Expression {

  def getType =  DataType.DoubleTyp

  def getValue = EMPTY_EX

  //def createCopy(): Expression = new SSCollProxy(name,range)

  def getChildCount: Int =  0 

  def getChildNr(ix: Int): Expression =  null 

  def getTerm: String = name+"("+(range match {case rs:RangeSelection =>if(rs.cols.start== -1) "undefiniert" else 
    SpreadSheetUtil.columnIdToLetter(rs.cols.start)+(rs.rows.start+1)+":"+
    SpreadSheetUtil.columnIdToLetter(rs.cols.end)+(rs.rows.end+1)
  case _=> range.toString})+")"

  def isConstant: Boolean =  false

  def write(file: DataOutput): Unit = {
  	file.writeByte(DataType.VariableTyp.id) 	
  	file.writeUTF(name)  	  	
  }
  
  def encode: String = "$Y"+getTerm+";"

  def delta(deltaX: Int, deltaY: Int) = SSCollProxy(name, SpreadSheetRange.delta(range, deltaX, deltaY))
  
  def equalsData(data:InstanceData):Boolean= {
    if(data.fieldData.size!=6) false
    else {
      data.fieldData(1) match {
        case c:CollectingFuncCall=>  if(name!=c.name) false
          else range match {
              case RangeSelection(cols,rows)=>
                cols.start==data.fieldValue(2).toInt && cols.end==data.fieldValue(4).toInt&&
                rows.start==data.fieldValue(3).toInt && rows.end==data.fieldValue(5).toInt
              case _=> false
          }
        
        case _=> false
      }
    }
  }
}

case class SSVariable(name:String,getValue:Constant) extends Expression {

  def getType =  DataType.VariableTyp


  //def createCopy(): Expression = new SSVariable(name,getValue)

  def getChildCount: Int =  0 

  def getChildNr(ix: Int): Expression =  null 

  def getTerm: String =  name 

  def isConstant: Boolean =  false

  def write(file: DataOutput): Unit = {
  	file.writeByte(DataType.VariableTyp.id)  	
  	file.writeUTF(SpreadSheet.spreadSheetModulName)
  	file.writeUTF(name)  	  	
  }

  def encode: String = "$Y"+getTerm+";"  
}