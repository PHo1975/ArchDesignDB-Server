package client.graphicsView

import client.dialog._

import scala.swing.{Orientation, BoxPanel, Panel}

/**
 * Created by Kathi on 17.05.2015.
 */
class ArcFieldEditor extends FieldEditor {

  val diameterField=new SidePanelDoubleTextField(Map(("ArcElem",4)),this,3)
  val startAngleField=new SidePanelDoubleTextField(Map(("ArcElem",5)),this,3)
  val endAngleField=new SidePanelDoubleTextField(Map(("ArcElem",6)),this,3)

  diameterField.addSearchLookup{case arc:ArcElement=>arc.diameter}
  startAngleField.addSearchLookup{case arc:ArcElement=> arc.startAngle}
  endAngleField.addSearchLookup{case arc:ArcElement=> arc.endAngle}

  val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents+= new PanelPart("Radius:",diameterField)+=
      new PanelPart("startW:",startAngleField)+=
      new PanelPart("endW:",endAngleField)
  }

  override val fieldComponents=List(diameterField,startAngleField,endAngleField)

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
