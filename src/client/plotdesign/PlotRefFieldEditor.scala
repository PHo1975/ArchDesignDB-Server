package client.plotdesign


import java.awt.Color

import client.dataviewer.ViewConstants
import client.dialog._
import client.graphicsView.ScaleModel
import definition.expression.{Constant, IntConstant}
import javax.swing.BorderFactory

import scala.swing.{BoxPanel, Label, Orientation, Swing}

class PlotRefFieldEditor extends FieldEditor {
   val className="LayerPlotRef"
  def allowedClassNames=Seq(className)
  
  val angleEditor=new SidePanelDoubleTextField(Map((className,2)),this)
  angleEditor.addSearchLookup({case t:LayerRef=> t.angle})
  
  val textScaleEditor=new SidePanelDoubleTextField(Map((className,10)),this)
  textScaleEditor.addSearchLookup({case t:LayerRef=> t._textScale})
 
  
  val layerNameLabel=new SidePanelLabel(Map((className,0)),this){
     addSearchLookup({case l:LayerRef=>println("lay:"+l);l.layerName})
     border=BorderFactory.createLineBorder(Color.gray)
   }
  
  val scaleRenderer= new Label with RenderComponent[Int] {
    font = ViewConstants.labelFont

    def setStyle(scale: Int): Unit = text = "1:" + s"${math.round(1d / ScaleModel.scales(scale)).toInt}"

    def setEmpty(): Unit = text = ""
	}

  lazy val scaleList: Seq[Int] = ScaleModel.scales.keys.toSeq
   
  lazy val scaleCombo=new SidePanelComboBox(scaleList,scaleRenderer,this,Map(className -> 1)){
    val defaultValue=7

    def getConstant(value: Int): Constant = IntConstant(value)

    def valueFromConstant(c: Constant): Int = c.toInt
    
    override def setValue(newScale:Option[Int]):Unit= {	    
	    super.setValue(newScale)
	    selfSelected=true
	    selection.index= newScale match {
	      case Some(nScale)=> scaleList.indexOf(nScale)
	      case _ => -1
	    }		    
	  }
	  addSearchLookup({
      case t:LayerRef => if(t.scale==0)t.layerScale else t.scale
    })
  }
  
  lazy val fieldComponents=Seq(scaleCombo,angleEditor,textScaleEditor,layerNameLabel)
  
  lazy val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents += getPanelPart("Mass:",scaleCombo) += getPanelPart("Winkel:",angleEditor)	+=
      getPanelPart("Textzoom",textScaleEditor)+=Swing.VStrut(10)+=layerNameLabel
  }

  def getPanel: BoxPanel = panel
}