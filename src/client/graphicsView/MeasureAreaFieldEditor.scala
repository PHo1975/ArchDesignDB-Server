package client.graphicsView

import client.dialog._

import scala.swing.{BoxPanel, Orientation, Panel}


class MeasureAreaFieldEditor extends FieldEditor {

  val factorFieldEditor=new SidePanelExpressionTextField(Map(("AreaPolygon",11),("Wohnfläche",11)),this){
    addSearchLookup({
      case a:AreaPolyElement=> a.factor
    })
  }
  val resultLabel=new SidePanelLabel(Map(("AreaPolygon",0),("Wohnfläche",0)),this){
    addSearchLookup({
      case a:AreaPolyElement=> "%1.4f ".format(a.result.value)+a.result.unitFraction.toString()
    })
  }

  override def fieldComponents: Seq[SidePanelComponent[_]] =Seq(factorFieldEditor,resultLabel)

  override def allowedClassNames: Iterable[String] =List("AreaPolygon","Wohnfläche")

  val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents+= 	new PanelPart("Faktor:",factorFieldEditor)
    contents+= new PanelPart("Ergebnis:",resultLabel)
    //textField.preferredSize=new Dimension(40,55)
    xLayoutAlignment=0d
  }

  override def getPanel: Panel = panel
}


class MeasureLineFieldEditor extends FieldEditor {

  val factorFieldEditor=new SidePanelExpressionTextField(Map(("MeasurePolyLine",12)),this){
    addSearchLookup({
      case a:MeasureLineElement=> a.factor
    })
  }
  val resultLabel=new SidePanelLabel(Map(("MeasurePolyLine",0)),this){
    addSearchLookup({
      case a:MeasureLineElement=> "%1.4f ".format(a.result.value)+a.result.unitFraction.toString()
    })
  }

  override def fieldComponents: Seq[SidePanelComponent[_]] =Seq(factorFieldEditor,resultLabel)

  override def allowedClassNames: Iterable[String] =List("MeasurePolyLine")

  val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents+= 	new PanelPart("Faktor:",factorFieldEditor)
    contents+= new PanelPart("Ergebnis:",resultLabel)
    //textField.preferredSize=new Dimension(40,55)
    xLayoutAlignment=0d
  }

  override def getPanel: Panel = panel
}
