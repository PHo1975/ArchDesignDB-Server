package client.plotdesign


import java.awt.Color

import client.dataviewer.ViewConstants
import client.dialog._
import client.graphicsView.ScaleModel
import definition.data.PlotFilter
import definition.expression.{Constant, IntConstant}
import javax.swing.BorderFactory

import scala.collection.immutable
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, CheckBox, Label, Orientation, Swing}

class PlotRefFieldEditor extends FieldEditor {
  val className="LayerPlotRef"
  def allowedClassNames: Seq[String] =Seq(className)
  
  val angleEditor=new SidePanelDoubleTextField(Map((className,2)),this)
  angleEditor.addSearchLookup({case t:LayerRef=> t.angle})
  
  val textScaleEditor=new SidePanelDoubleTextField(Map((className,10)),this)
  textScaleEditor.addSearchLookup({case t:LayerRef=> t._textScale})
 
  
  val layerNameLabel=new SidePanelLabel(Map((className,0)),this){
     addSearchLookup({case l:LayerRef=>l.layerName})
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


  class FilterEditor extends BoxPanel(Orientation.Vertical) with SidePanelComponent[PlotFilter]{
    val checkboxNames=Array("Linien","Kreise","Elipsen","Füllflächen","PolygonLinien","Bitmaps","Texte","Maßlinien","Symbole","Symbolfüller","Messfläche","Wohnfläche","MessPolyLinie")
    val checkboxValues=Array(1,2,4,8,16,32,64,128,256,512,1024,2048,4096)
    val sum: Int =1+2+4+8+16+32+64+128+256+512+1024+2048+4096

    class FilterCheckBox(name:String,id:Int) extends CheckBox(name){
      focusable=false
      opaque=false
      def clicked():Unit={
        val oldValue=currentValue match{
          case Some(value)=>value.filter
          case None => 0
        }
        val newValue =if(this.selected) oldValue | id
        else oldValue & (sum-id)
        storeValue(new PlotFilter(newValue),FilterEditor.this)
      }
    }
    val checkboxes: immutable.IndexedSeq[FilterCheckBox] =for(i<-0 until 13) yield new FilterCheckBox (checkboxNames(i),checkboxValues(i))
    contents++=checkboxes

    listenTo(checkboxes:_* )

    reactions+={
      case ButtonClicked(but:FilterCheckBox)=>but.clicked()
    }

    def resetAllBoxes(): Unit =checkboxes.foreach(_.selected=false)

    override def allowedFields: Map[String, Byte] = Map((className,9))

    override def defaultValue: PlotFilter = PlotFilter.emptyFilter

    override def getConstant(value: PlotFilter)= IntConstant(value.filter)

    override def valueFromConstant(c: Constant): PlotFilter = new PlotFilter(c.toInt)

    override def setValue(newValue: Option[PlotFilter]): Unit = {
      super.setValue(newValue)
      resetAllBoxes()
      newValue match {
        case Some(filter)=>
          for(i<-0 until 13;if (filter.filter&checkboxValues(i))>0)checkboxes(i).selected=true
        case None =>System.out.println("Set value empty")
      }
    }
    addSearchLookup({case l:LayerRef=>l.filter})
  }

  val filterEditor=new FilterEditor()
  
  lazy val fieldComponents: Seq[SidePanelComponent[_]] =Seq(scaleCombo,angleEditor,textScaleEditor,layerNameLabel,filterEditor)
  
  lazy val panel:BoxPanel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents += getPanelPart("Mass:",scaleCombo) += getPanelPart("Winkel:",angleEditor)	+=
      getPanelPart("Textzoom",textScaleEditor)+=Swing.VStrut(10)+=layerNameLabel+=Swing.VStrut(10)+=
    new Label("Elemente ausblenden:")+=filterEditor
  }

  def getPanel: BoxPanel = panel
}