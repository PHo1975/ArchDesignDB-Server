package client.dialog.symbolbrowser

import java.awt.Dimension

import client.dialog._
import client.graphicsView.symbol.{SymbolElem, SymbolFiller, SymbolOrient}
import client.ui.ViewConstants

import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, ButtonGroup, CheckBox, Label, Orientation, RadioButton, Swing}



class SymbolFieldEditor extends FieldEditor {
   lazy val allowedClassNames=Seq("SymbolElem","SymbolFiller")
   
   lazy val angleField=new SidePanelDoubleTextField(Map("SymbolElem"->2,"SymbolFiller"->2),this)
   lazy val scaleField=new SidePanelDoubleTextField(Map("SymbolElem"->3,"SymbolFiller"->3),this)
   
   angleField.addSearchLookup{case symb:SymbolElem=>symb.angle;case fill:SymbolFiller=>fill.angle }
   scaleField.addSearchLookup{case symb:SymbolElem=>symb.scale;case fill:SymbolFiller=>fill.scale }
   
   lazy val panel=new BoxPanel(Orientation.Vertical){
     contents+=getPanelPart("Winkel:",angleField)+=getPanelPart("Zoom:",scaleField)
     maximumSize = new Dimension(Short.MaxValue, 64 * ViewConstants.fontScale / 100)
     xLayoutAlignment=0d
     opaque=false
   }
   
   lazy val fieldComponents=Seq(angleField,scaleField)

  def getPanel: BoxPanel = panel
}





class SymbolFillerFieldEditor extends FieldEditor {
   lazy val allowedClassNames=Seq("SymbolFiller")
   
   lazy val firstField=new SidePanelDoubleTextField(Map("SymbolFiller"->8),this,4) {
     override def filter(st: Double): Boolean = if (fixedCheckBox.selected) st > 1 else true
     addSearchLookup{ case fill:SymbolFiller=>
       numElemsLab.text="Anzahl: "+fill.fillData.elemRange.size.toString
       fill.fillData match {
         case a:SymbolFiller#FixedFill=> a.numElems.toDouble
         case b:SymbolFiller#DivideFill=>b.minDist
         case c:SymbolFiller#TileFill=>c.distance
         case _=>0d
       }
     }
   }  
   
   lazy val secondField=new SidePanelDoubleTextField(Map("SymbolFiller"->9),this,4) {     
     addSearchLookup{ case fill:SymbolFiller=> fill.fillData match {
       case b:SymbolFiller#DivideFill=>b.offset
       case c:SymbolFiller#TileFill=>c.startOffset
       case _=>0d
     }}
   }
   
   lazy val codeField=new IntSidePanelComponent {
     val allowedFields=Map("SymbolFiller"->7.toByte)
     addSearchLookup{ case fill:SymbolFiller=>
       numElemsLab.text="Anzahl: "+fill.fillData.elemRange.size.toString
       fill.code
     }

     override def setValue(value: Option[Int]): Unit = {
       //println("Set value code "+value)
       super.setValue(value)
       selfSelected=true
       value match {
         case Some(code)=>SymbolOrient.fillModeFromCode(code) match {
           case 0=> setFixed()
           case 1=> setTile((code&128)>0)
           case 2=> setDefDist((code&128)>0)
           case _=>
         }
         case None => setNone()
       } 
       selfSelected=false
     }
   }   
      
   val fixedCheckBox=new RadioButton("Feste Einteilung")
   val defDistCheckBox=new RadioButton("Definierter Abstand")
   val tileCheckBox=new RadioButton("Kacheln")
   val circleCheckBox=new RadioButton("Kreisform")  
   val thirdCheckBox=new CheckBox("..")
   val bGroup=new ButtonGroup(fixedCheckBox,defDistCheckBox,circleCheckBox,tileCheckBox)
   var selfSelected=false

  val part1: PanelPart = getPanelPart("a1:", firstField)
  val part2: PanelPart = getPanelPart("Max-Dist:", secondField)
  val numElemsLab: Label = ViewConstants.label("..")
   numElemsLab.xLayoutAlignment=0d
   
   lazy val panel=new BoxPanel(Orientation.Vertical){
     contents+=fixedCheckBox+=tileCheckBox+=defDistCheckBox+=circleCheckBox+=Swing.VStrut(10)+=part1+=part2+=
       thirdCheckBox+=Swing.VStrut(15)+=numElemsLab
     maximumSize = new Dimension(Short.MaxValue, 214 * ViewConstants.fontScale / 100)
     xLayoutAlignment=0d
     opaque=false
   }
   
   lazy val fieldComponents=Seq(firstField,secondField,codeField)
   
   panel.listenTo(fixedCheckBox,defDistCheckBox,circleCheckBox,tileCheckBox,thirdCheckBox)   
   panel.reactions+= {
     case ButtonClicked(`fixedCheckBox`)if !selfSelected =>
       setFixed()
       setFillMode(0)
     case ButtonClicked(`tileCheckBox`)if !selfSelected =>
       setTile(thirdCheckBox.selected)
       setFillMode(1)
     case ButtonClicked(`defDistCheckBox`)if !selfSelected =>
       setDefDist(thirdCheckBox.selected)
       setFillMode(2)
     case ButtonClicked(`thirdCheckBox`)if !selfSelected =>
       val currValue=codeField.currentValue match{
         case Some(v)=>v
         case _=> 0
       }
       this.storeValue((currValue & -129)+(if(thirdCheckBox.selected)128 else 0), codeField)
   }

  def getPanel: BoxPanel = panel
   
   private def setFillMode(mode:Int)= {
     val currValue=codeField.currentValue match{
       case Some(v)=>v
       case _=> 0
     }
     this.storeValue((currValue & -97)+(mode<<5), codeField)
   }
   
   private def setFixed()={
     selfSelected=true
     fixedCheckBox.selected=true
     part1.visible=true
     part1.label.text="Teilungen:"
     part2.visible=false
     thirdCheckBox.visible=false
     selfSelected=false
   }
   
   private def setTile(thirdChecked:Boolean)={
     selfSelected=true
     tileCheckBox.selected=true
     part1.visible=true
     part1.label.text="Abstand:"
     part2.label.text="Versatz:"
     thirdCheckBox.visible=true
     thirdCheckBox.text="Zuschneiden"
     thirdCheckBox.selected=thirdChecked
     part2.visible=true
     selfSelected=false
   }
   
   
   private def setDefDist(thirdChecked:Boolean)={
     selfSelected=true
     defDistCheckBox.selected=true
     part1.visible=true
     part1.label.text="MinDist:"
     part2.label.text="Versatz:"     
     part2.visible=true
     thirdCheckBox.visible=true
     thirdCheckBox.text="Randfugen"
     thirdCheckBox.selected=thirdChecked
     selfSelected=false
   }
   
   private def setNone()= {
     selfSelected=true
     bGroup.peer.clearSelection()
     part1.visible=false   
     part2.visible=false
     thirdCheckBox.visible=false
     selfSelected=false
   }
}