/**
 * Author: Peter Started:24.11.2010
 */
package client.dataviewer

import client.dataviewer.InstanceRenderer.partUnits
import client.ui.ViewConstants
import definition.expression._
import definition.typ._
import util.{JavaUtils, Log}

import java.awt.Color
import javax.swing.border.Border
import javax.swing.{JLabel, UIManager}
import scala.swing._
import scala.util.control.NonFatal


class RendererLabel extends Label {  
  override lazy val peer: JLabel = 
    new JLabel("", null, Alignment.Left.id) with SuperMixin {
			override def validate(): Unit = {}

			override def invalidate(): Unit = {}
	}

	val nofocusBorder: Border = UIManager.getBorder("Table.cellNoFocusBorder")
	val focusBorder: Border = UIManager.getBorder("Table.focusCellHighlightBorder")
	val focusForeground: Color = UIManager.getColor("Table.focusCellForeground")
	val focusBackground: Color = UIManager.getColor("Table.focusCellBackground")

}

/**
 * 
 */
class InstanceRenderer(theClass:AbstractObjectClass) extends RendererLabel {
  
	def config(t:Table, isSelected: Boolean, focused: Boolean, expression: Expression,row:Int, col: Int) :Unit= try {
		font=t.font
		if (focused)  border= focusBorder
		else border=nofocusBorder
		
  	background=if(isSelected)  t.selectionBackground else
  	  if(expression==null) InstanceRenderer.emptyColor else
		  /*if (row % 2 == 0)InstanceRenderer.alternateColor
  	  			else*/ Color.white
		
		//print("ex:"+expression+" row:"+row+" col:"+col)
  	if(expression== null || expression.isNullConstant) {
  		if(col>0&&theClass.fields (col-1).typ ==DataType.BoolTyp)text= "\u25cc"
  		else text=""  		
  	}
  	else if(col>0){
  		val fieldFormat=theClass.fieldSetting(col-1)

  		if (fieldFormat.showFormula) {	
  			text=expression.getTerm
  			this.horizontalAlignment=Alignment.Left
  			foreground=Color.black
  		}
  		else { // show value
  			val fieldDef=theClass.fields(col-1)
  			val value=expression.getValue
  			//println(" "+value+" "+)
  			import definition.typ.DataType._
  			text = if(value==null) "" else (fieldDef.typ match {
  				case IntTyp | LongTyp =>
						horizontalAlignment=Alignment.Right
						value.toLong.toString
					case DoubleTyp|UnitNumberTyp =>
						horizontalAlignment=Alignment.Right
						try {
              val unitAdd=if (value.getType == DataType.UnitNumberTyp) " " + value.toUnitNumber.unitFraction.toString else ""
							if(partUnits.contains(unitAdd)&& (value.toDouble==Math.round(value.toDouble)) ) value.toInt.toString+unitAdd
							else if (fieldFormat.formString.length > 0) fieldFormat.formString.format(value.toDouble) + unitAdd
              else value.toDouble.toString+unitAdd
            } catch {
							case NonFatal(e)=>Log.e("field:"+(col-1)+" formString:"+fieldFormat.formString,e); value.toDouble.toString
						  case other:Throwable =>println(other);System.exit(0);null
						}
					case BoolTyp =>
						horizontalAlignment=Alignment.Right
						if(value.toBoolean) "\u221A" else "\u25a1"
					case StringTyp =>
						horizontalAlignment=Alignment.Left
						value.toString
					case VectorTyp =>
						horizontalAlignment=Alignment.Left
						value.toVector.shortToString
					case CurrencyTyp =>
						horizontalAlignment=Alignment.Right
						f"${value.toDouble}%,.2f "+CurrencyConstant.currencySign
					case DateTyp =>
						//println("date "+value.toString+" "+value.toDate.toDateString)
						horizontalAlignment=Alignment.Center
						if(fieldFormat.formString .length>0) fieldFormat.formString.format(JavaUtils.toJavaDate(value.toDate))
            else util.JavaUtils.shortDateFormat.format(JavaUtils.toJavaDate(value.toDate))
					case _ =>
						horizontalAlignment=Alignment.Center
						value.toString
				}).replaceAll("\n"," | ")
  			foreground= if(isSelected) t.selectionForeground
  			else expression.getType match {  			
  				case BinOp |CollFunctionCall |FunctionCall => Color.blue
  				case FieldRefTyp|ParentRefTyp => InstanceRenderer.linkColor
  				case _ => Color.black  			
  			}
  		}
  		if (focused && !isSelected) {
				foreground=focusForeground
				background=focusBackground
			}
  	}  	  
	}	catch {
		case NonFatal(e)=>Log.e("col:"+(col-1)+" class:"+theClass.name+" ex:"+expression,e)
		case other:Throwable =>println(other);System.exit(0)
	}
}

object InstanceRenderer {	
	val alternateColor=new Color(250,251,254)
	val emptyColor: Color = ViewConstants.leftPanelColor
	//Color.lightGray.brighter()
	val linkColor=new Color(0,150,0)
	val partUnits: Array[String] =Array(" Stück"," Stck"," Pausch","Sack","Paar")
}