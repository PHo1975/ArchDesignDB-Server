/**
 * Author: Peter Started:27.05.2011
 */
package client.graphicsView

import java.awt.Color

import client.dialog.{FieldEditor, PanelPart, SidePanelComponent}
import definition.expression.{Constant, IntConstant}
import javax.swing.JColorChooser

import scala.swing.event.ButtonClicked
import scala.swing.{Button, Color, Panel}

/**
 *
 */
class ColorFieldEditor extends FieldEditor {

	val colorBut=new Button with SidePanelComponent[Color] {
		val allowedFields:Map[String,Byte]=Map(("Plane",3),("RoomGroup",2),("Room",2),("PolyElem",0),("LineElem",0),("ArcElem",0),
      ("EllipseElem", 0), ("TextElem", 0), ("DimLineElem", 0), ("AreaPolygon", 1), ("BitmapElem", 0), ("PolyLineElem", 0), ("MeasurePolyLine", 1), ("Wohnfläche", 1))
    val backColor: Color = background
    val defaultValue: Color = Color.black

		focusable=false
		addSearchLookup({
		  case c:GraphElem => new Color(c.color)
		})

    override def setValue(newColor: Option[Color]): Unit = {
			println("setValue "+newColor)
		  visible= ColorFieldEditor.showEditor
		  super.setValue(newColor)
			newColor match {
				case Some(color)=> background=color
				case _ => background=backColor
			}
		}

    def getConstant(value: Color): Constant = IntConstant(value.getRGB)
		def valueFromConstant(c:Constant)=new Color(c.toInt)
	}

  val allowedClassNames: Iterable[String] = colorBut.allowedFields.keys
	val fieldComponents=Seq(colorBut)

	val panel=new PanelPart("Farbe:",colorBut) {
	  xLayoutAlignment=0d
		listenTo(colorBut)
		reactions+={
			case e:ButtonClicked=>
				if(dataList!=null){
					val color=JColorChooser.showDialog(this.peer,"Farbe auswählen",colorBut.currentValue.getOrElse(colorBut.defaultValue))
					if(color!=null) {
					  storeValue(color,colorBut)
					  colorBut.background=color
					}
				}
		}
	}

  def getPanel:Panel=panel
}

object ColorFieldEditor {
  var showEditor:Boolean=true
}

