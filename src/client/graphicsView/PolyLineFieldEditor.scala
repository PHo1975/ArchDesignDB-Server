package client.graphicsView

import java.awt.Dimension

import client.dialog.{FieldEditor, PanelPart, SidePanelDoubleTextField}

import scala.swing.{BoxPanel, Panel}

/**
 * Created by Peter Holzer on 11.03.2017 .
 */
class PolyLineFieldEditor extends FieldEditor {
  val className1 = "PolyLineElem"
  val className2 = "MeasurePolyLine"
  val widthEdit = new SidePanelDoubleTextField(Map((className1, 4), (className2, 5)), this)
  val alignEdit = new SidePanelDoubleTextField(Map((className1, 5), (className2, 6)), this)
  val opaqueEdit = new SidePanelDoubleTextField(Map((className1, 6), (className2, 7)), this)

  widthEdit.addSearchLookup({ case p: PolyLineElement => p.width })
  alignEdit.addSearchLookup({ case p: PolyLineElement => p.align })
  opaqueEdit.addSearchLookup({ case p: PolyLineElement => p.opaquity })

  override val fieldComponents = Seq(widthEdit, alignEdit, opaqueEdit)

  override def allowedClassNames: Iterable[String] = Seq(className1, className2)

  lazy val panel = new BoxPanel(scala.swing.Orientation.Vertical) {
    opaque = false
    val labels = List("Dicke", "Ausricht", "Opak")
    val pans: Seq[PanelPart] = (fieldComponents, labels).zipped.map((c, l) => {
      val pp = new PanelPart(l, c)
      pp.maximumSize = new Dimension(FieldEditor.panelSize.width, FieldEditor.panelSize.height)
      pp
    })
    contents ++= pans
    preferredSize = new Dimension(70, 100)
    maximumSize = new Dimension(Short.MaxValue, 100)
  }

  override def getPanel: Panel = panel
}
