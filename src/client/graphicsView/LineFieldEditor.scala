package client.graphicsView

import java.awt.Dimension

import client.dialog.{FieldEditor, SidePanelComponent}
import definition.expression.{Expression, UNDEFINED_VECTOR, VectorConstant}

import scala.swing.{BoxPanel, Label, Orientation, Panel}


class GetVectorComponent (val allowedFields:Map[String,Byte],val editor:FieldEditor) extends SidePanelComponent[VectorConstant]{
  override def defaultValue: VectorConstant = UNDEFINED_VECTOR
  override def getConstant(value: VectorConstant): Expression = value
  override def valueFromConstant(c: Expression): VectorConstant = c.getValue.toVector
}

class LineFieldEditor extends FieldEditor {
  var startValue:VectorConstant=UNDEFINED_VECTOR
  var endValue:VectorConstant=UNDEFINED_VECTOR
  val dxLabel = new Label()
  val dyLabel = new Label()
  val lengthLabel = new Label()
  val angleLabel= new Label()
  val labels=Seq(dxLabel,dyLabel,lengthLabel,angleLabel)


  val startComp: GetVectorComponent =new GetVectorComponent(Map("LineElem"->3),this){
    override def setValue(newValue: Option[VectorConstant]): Unit = {
      startValue=newValue match {
        case Some(v)=>v
        case _ => UNDEFINED_VECTOR
      }
    }
    addSearchLookup{case line:LineElement=>line.startPoint}
  }

  val endComp: GetVectorComponent =new GetVectorComponent(Map("LineElem"->3),this){
    override def setValue(newValue: Option[VectorConstant]): Unit = {
      endValue=newValue match {
        case Some(v)=>v
        case _ => UNDEFINED_VECTOR
      }
      setLabels()
    }
    addSearchLookup{case line:LineElement=>line.endPoint}
  }

  def setLabels(): Unit = {
    if(startValue==UNDEFINED_VECTOR || endValue==UNDEFINED_VECTOR) {
      for(l<-labels) l.text=""
    } else {
      val delta=(endValue-startValue)
      dxLabel.text=delta.x.toString
      dyLabel.text=delta.y.toString
      lengthLabel.text=delta.toDouble.toString
      angleLabel.text=(Math.atan2(delta.y,delta.x)*180d/Math.PI).toString
    }
  }

  for(l<-labels) l.horizontalAlignment = scala.swing.Alignment.Left


  override def fieldComponents: Seq[SidePanelComponent[_]] = Seq(startComp,endComp)

  val allowedClassNames: Iterable[String] = Seq("LineElem")

  lazy val getPanel: Panel = new BoxPanel(Orientation.Vertical){
    contents+=getPanelPart("dx:",dxLabel)+=getPanelPart("dy:",dyLabel)+=
      getPanelPart("Länge:",lengthLabel)+=getPanelPart("Winkel:",angleLabel)
    maximumSize=new Dimension(Short.MaxValue,100)
    //preferredSize=new Dimension(ViewConstants.sidePanelWidth-20,100)
    xLayoutAlignment=0d
    opaque=false
    //background=Color.green
  }
}
