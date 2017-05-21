/**
 * Author: Peter Started:13.04.2011
 */
package client.dialog.form

import definition.typ.HorAlign

import scala.swing.Label
import scala.xml.Elem

/**
 * 
 */
class FormLabel(val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val atext:String,val align:HorAlign.Value) extends Label with FormElement {
	
	text=atext
	horizontalAlignment=FormElement.horAlignToScala(align)
	setupComponent(this)

  def toXML: Elem =
  	 <FormLabel  iw={minWidth.toString} aw={maxWidth.toString} ih={minHeight.toString} ah={maxHeight.toString} align={align.id.toString}
  	 text={atext}/>


  override def toString(): String = "Label " + atext
	
	def makeCopy = new FormLabel(minWidth,maxWidth,minHeight,maxHeight,atext,align)
	
	//override def getProperties= ("Text",atext)::("Align",align.toString)::super.getProperties  
}




