/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import java.io.DataOutput

import definition.expression.{Constant, EMPTY_EX, Expression}
import definition.typ.DataType

/**
 * 
 */
class PrintVariable(val name:String) extends Expression {
	//println("New PrintVariable:"+name)
	var value:Constant=EMPTY_EX
	
	def getType:DataType.Value=value.getType
	
	def getValue:Constant=value

  //def createCopy():Expression=new PrintVariable(name)
	
	def getChildCount:Int=0
	
	def getChildNr(ix:Int):Expression= null
	
	def getTerm:String="PR_"+name
	
	def isConstant:Boolean	=false
	
	def write(file:DataOutput):Unit = {
     file.writeByte(DataType.StringTyp.id)
     file.writeUTF("PR_"+name)
	}

  override def toString: String = getTerm

  def updateVariable(cx: Context): Unit = value = cx.getVarValue(name)

  def encode: String = getTerm

}

class PlaceHolderPrintVariable(nname:String) extends PrintVariable(nname) {}