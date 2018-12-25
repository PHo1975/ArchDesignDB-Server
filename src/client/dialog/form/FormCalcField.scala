/**
 * Author: Peter Started:21.04.2011
 */
package client.dialog.form

import definition.data.InstanceData
import definition.expression.{Expression, FieldReference}
import definition.typ.form.FormDataField
import definition.typ.{AbstractObjectClass, DataType, HorAlign}

import scala.swing.Label

/**
 * 
 */
class FormCalcField(val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val align:HorAlign.Value,val expression:Expression) extends Label with FormElement with FormDataField {

	val fieldNr:Byte=0.toByte
	var refList:List[FieldReference]=expression.getElementList[FieldReference](DataType.FieldRefTyp,Nil)
	
	setupComponent(this)
	
	//println("Calc init "+expression.getTerm+" "+expression.getValue)
	text=expression.getValue.toString

	horizontalAlignment=FormElement.horAlignToScala(align)
	
  def wantShutDown(): Unit = {  }

  def shutDown(): Unit = {  }

  def setDataValue(dvalue: InstanceData,nclass:AbstractObjectClass): Unit = {
  	for(r<-refList) {
  		r.setCachedValue(dvalue.fieldValue (r.remField ))
  	}
  	text=expression.getValue.toString
  }
  
  def makeCopy = new FormCalcField(minWidth,maxWidth,minHeight,maxHeight,align,expression.createCopy())
  
  def toXML = <CalcField iw={minWidth.toString} aw={maxWidth.toString} ih={minHeight.toString} ah={maxHeight.toString} align={align.id.toString} ex={expression.encode}/>

}