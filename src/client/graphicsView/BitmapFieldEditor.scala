package client.graphicsView

import java.awt.Dimension

import client.dialog.{FieldEditor, SidePanelDoubleTextField, SidePanelTextArea}

import scala.swing.{BoxPanel, Orientation, ScrollPane}

/**
 * Created by Kathi on 23.06.2015.
 */
class BitmapFieldEditor extends FieldEditor {
  lazy val allowedClassNames=Seq("BitmapElem")
  lazy val fileNameField=new SidePanelTextArea(Map("BitmapElem"->1),this)
  lazy val dpiField=new SidePanelDoubleTextField(Map("BitmapElem"->2),this)
  lazy val angleField=new SidePanelDoubleTextField(Map("BitmapElem"->4),this)
  lazy val scaleYField=new SidePanelDoubleTextField(Map("BitmapElem"->3),this)
  lazy val scaleField=new SidePanelDoubleTextField(Map("BitmapElem"->5),this)

  angleField.addSearchLookup{case symb:BitmapElem=>symb.angle }
  scaleYField.addSearchLookup{case symb:BitmapElem=>symb.aspectY }
  fileNameField.addSearchLookup{case symb:BitmapElem=>symb.fileName}
  dpiField.addSearchLookup{case bitm:BitmapElem=>bitm.dpi}
  scaleField.addSearchLookup{case symb:BitmapElem=>symb.scale }

  lazy val fileNameScroller=new ScrollPane{
    viewportView=fileNameField
  }

  lazy val panel=new BoxPanel(Orientation.Vertical){
    contents+=getPanelPart("Pfad:",fileNameScroller)+=getPanelPart("DPI:",dpiField)+=
      getPanelPart("Masstab:",scaleField)+= getPanelPart("Winkel:",angleField)+=getPanelPart("AspectY:",scaleYField)
    maximumSize=new Dimension(Short.MaxValue,200)
    xLayoutAlignment=0d
    opaque=false
  }

  lazy val fieldComponents=Seq(angleField,scaleYField,dpiField,fileNameField,scaleField)
  def getPanel=panel
}
