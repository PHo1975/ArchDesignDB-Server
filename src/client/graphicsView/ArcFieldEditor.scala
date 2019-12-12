package client.graphicsView

import client.dialog._
import util.StrToDouble

import scala.swing.{BoxPanel, Orientation, Panel}

/**
 * Created by Kathi on 17.05.2015.
 */
class ArcFieldEditor extends FieldEditor {

  val diameterField=new SidePanelDoubleTextField(Map(("ArcElem",4)),this,3)
  val startAngleField=new SidePanelDoubleTextField(Map(("ArcElem",5)),this,3)
  val endAngleField=new SidePanelDoubleTextField(Map(("ArcElem",6)),this,3)
  val lengthLabel=new SidePanelLabel(Map(("ArcElem",4)),this){
    override def setValue(newValue: Option[String]): Unit = {
      super.setValue(newValue)
      newValue match {
        case Some(StrToDouble(diameter)) if(endAngleField.currentValue.isDefined&&startAngleField.currentValue.isDefined)=>
          var delta =Math.abs(endAngleField.currentValue.get  -startAngleField.currentValue.get)
          if(delta>360d) delta=delta % 360
          text="%3.5f".format(diameter*2*Math.PI*delta/360d)
        case _ => text=defaultValue
      }
    }
  }

  diameterField.addSearchLookup{case arc:ArcElement=>arc.diameter}
  startAngleField.addSearchLookup{case arc:ArcElement=> arc.startAngle}
  endAngleField.addSearchLookup{case arc:ArcElement=> arc.endAngle}
  lengthLabel.addSearchLookup{case arc:ArcElement=>arc.diameter.toString}

  val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents+= new PanelPart("Radius:",diameterField)+=
      new PanelPart("Start W:",startAngleField)+=
      new PanelPart("End W:",endAngleField)+=
      new PanelPart("Länge:",lengthLabel)
  }

  override val fieldComponents=List(diameterField,startAngleField,endAngleField,lengthLabel)

  override def getPanel: Panel = panel

  override def allowedClassNames: Iterable[String] = List("ArcElem")
}


class EllipseFieldEditor extends FieldEditor {

  val diameter1Field = new SidePanelDoubleTextField(Map(("EllipseElem", 4)), this, 3)
  val diameter2Field = new SidePanelDoubleTextField(Map(("EllipseElem", 5)), this, 3)
  val mainAngleField = new SidePanelDoubleTextField(Map(("EllipseElem", 6)), this, 3)
  val startAngleField = new SidePanelDoubleTextField(Map(("EllipseElem", 7)), this, 3)
  val endAngleField = new SidePanelDoubleTextField(Map(("EllipseElem", 8)), this, 3)

  diameter1Field.addSearchLookup{case ell:EllipseElement=>ell.r1}
  diameter2Field.addSearchLookup{case ell:EllipseElement=>ell.r2}
  mainAngleField.addSearchLookup{case ell:EllipseElement=>ell.mainAngle}
  startAngleField.addSearchLookup{case ell:EllipseElement=> ell.startAngle}
  endAngleField.addSearchLookup{case ell:EllipseElement=> ell.endAngle}

  val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents+= new PanelPart("Radius 1:",diameter1Field)+=
      new PanelPart("Radius 2:",diameter2Field)+=
      new PanelPart("HauptW:",mainAngleField)+=
      new PanelPart("startW:",startAngleField)+=
      new PanelPart("endW:",endAngleField)
  }

  override val fieldComponents=List(diameter1Field,diameter2Field,mainAngleField,
    startAngleField,endAngleField)

  override def getPanel: Panel = panel

  override def allowedClassNames: Iterable[String] = List("EllipseElem")
}
