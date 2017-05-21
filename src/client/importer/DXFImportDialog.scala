package client.importer

import java.awt.{Dimension, Point, Rectangle}
import java.io.File

import client.dataviewer.ViewConstants
import client.graphicsView._
import definition.typ.SystemSettings
import util.StrToDouble

import scala.swing.event.{ButtonClicked, EditDone, SelectionChanged}
import scala.swing.{BorderPanel, BoxPanel, Button, CheckBox, ComboBox, Dialog, Label, ListView, Orientation, ScrollPane, Swing, TabbedPane, TextField, Window}

class DXFImportDialog(w:Window,settings:DXFSettings,files:Seq[File]) extends Dialog(w) {
  modal=true
  
  def showDialog(pos:Point):Boolean = {
    val cpos=FileImportManager.correctScreenPos(pos,850,650)    
    bounds=new Rectangle(cpos.x,cpos.y,870,670)
    layerView.listData=settings.layers
    layerView.peer.setSelectedIndices(settings.layers.indices.toArray)
    visible=true    
    settings.selectedLayers=layerView.selection.indices.toSeq
    okResult
  }
  
  val importBut=new Button("Importieren")
  val cancelBut=new Button("Abbruch")
  
  val buttonPanel=new BoxPanel(Orientation.Horizontal){
    contents+=importBut+=Swing.HGlue+=cancelBut
  }
  
  var okResult:Boolean=false
  
  val filesListView=new ListView(files)
  val filesLab: Label = ViewConstants.label("Dateien:")
  val scaleLab: Label = ViewConstants.label("Masstab:")
  val scaleValues: collection.Map[String, Int] = SystemSettings().enums("DrawingScales").enumValues
  val scales: Seq[String] = scaleValues.keys.toSeq
  val scaleCombo=new ComboBox(scales)
  scaleCombo.maximumSize=new Dimension(100,30)
  val dxLab: Label = ViewConstants.label("dx:")
  val dyLab: Label = ViewConstants.label("dy:")
  val dxEdit= new TextField("0")
  val dyEdit=new TextField("0")
  
  val lineCheckBox=new CheckBox("Linien")
  val arcCheckBox=new CheckBox("Kreise")
  val ellCheckBox=new CheckBox("Ellipsen")
  val textCheckBox=new CheckBox("Text")
  val colorPenCouplingBox=new CheckBox("Farb-Stift-Kopplung")
  
  lineCheckBox.selected=true
  arcCheckBox.selected=true
  ellCheckBox.selected=true
  textCheckBox.selected=true
  
  
  
  val headerPanel=new BoxPanel(Orientation.Horizontal) {
    contents+=new BorderPanel(){
      add(filesLab,BorderPanel.Position.North)
      add(new ScrollPane {
      	viewportView=filesListView
      	maximumSize=new Dimension(400,Short.MaxValue)
      },BorderPanel.Position.Center)
    }
    contents+=Swing.HStrut(20)
    contents+= new BoxPanel(Orientation.Vertical) {
      contents+=new BoxPanel(Orientation.Horizontal) {
        contents+=scaleLab
        contents+=scaleCombo
        contents+=Swing.HGlue
      }
      contents+=new BoxPanel(Orientation.Horizontal) {
        contents+=dxLab
        contents+=dxEdit
      }
      contents+=new BoxPanel(Orientation.Horizontal) {
        contents+=dyLab
        contents+=dyEdit
      }
      contents+=Swing.VGlue
    }
  }
  
  val layerView= new ListView(settings.layers)
  layerView.selection.intervalMode=ListView.IntervalMode.MultiInterval
  
  val layerPane=new BoxPanel(Orientation.Vertical){
    contents+=new ScrollPane{
      viewportView=layerView
    }
  }
  
  val elementsPane=new BoxPanel(Orientation.Vertical){
    contents += ViewConstants.label("Elemente")
    contents+=colorPenCouplingBox+=lineCheckBox+=arcCheckBox+=ellCheckBox+=textCheckBox
  }
  
  val contentsPane=new BoxPanel(Orientation.Horizontal){
    contents+=layerPane+=Swing.HStrut(20)+=elementsPane+=Swing.HStrut(20)
  }
  
  val assignLineStyleBut=new Button(" > ")
  val unknownLineStylesView=new ListView[UnknownLineStyle]
  val lineStylesView=new ListView(LineStyleHandler.stylesList)
  unknownLineStylesView.selection.intervalMode=ListView.IntervalMode.Single
  lineStylesView.selection.intervalMode=ListView.IntervalMode.Single
  //val lineInfoLab=ViewConstants.label("")
  
  
  val lineStylePane=new BoxPanel(Orientation.Horizontal) {
    contents+= new BorderPanel() {
      add(ViewConstants.label("Unbekannte Linienstile in Datei"), BorderPanel.Position.North)
      add(new ScrollPane{
        viewportView=unknownLineStylesView
      },BorderPanel.Position.Center)
      add(ViewConstants.label("<html><br> nicht zugeordnete Linienstile <Br>werden in der Datenbank neu angelegt</html>"), BorderPanel.Position.South)
    } 
    contents+=assignLineStyleBut
    contents+=new BorderPanel() {
      add(ViewConstants.label("Linienstile in Datenbank"), BorderPanel.Position.North)
      add(new ScrollPane{
      	viewportView=lineStylesView
      },BorderPanel.Position.Center)
    }
  }
  
  val assignHatchStyleBut=new Button(" > ")
  val unknownHatchStylesView=new ListView[HatchStyle]
  val hatchStylesView=new ListView(HatchHandler.hatchList)
  val unknownFontsView=new ListView[String]
  val fontsView=new ListView(FontHandler.fontList)
  val assignFontStyleBut=new Button(" > ")
  val allTextBlackBut=new CheckBox("Alle Texte schwarz")
  allTextBlackBut.selected=true
  val fontScaleEdit=new TextField("1")
  fontScaleEdit.maximumSize=new Dimension(100,50)
  val fontAdjustYEdit=new TextField("0")
  fontAdjustYEdit.maximumSize=new Dimension(100,50)

  //val infoLab=ViewConstants.label("")
  
  
  val hatchStylePane=new BoxPanel(Orientation.Horizontal) {
    contents+= new BorderPanel() {
      add(ViewConstants.label("Unbekannte Schraffuren in Datei"), BorderPanel.Position.North)
      add(new ScrollPane{
        viewportView=unknownHatchStylesView
      },BorderPanel.Position.Center)
      add(ViewConstants.label("<html><br> nicht zugeordnete Schraffuren <Br>werden in der Datenbank neu angelegt</html>"), BorderPanel.Position.South)
    } 
    contents+=assignHatchStyleBut
    contents+=new BorderPanel() {
      add(ViewConstants.label("Schraffuren in Datenbank"), BorderPanel.Position.North)
      add(new ScrollPane{
      	viewportView=hatchStylesView
      },BorderPanel.Position.Center)
    }
  } 
  
  val fontStylePane=new BoxPanel(Orientation.Horizontal) {
    contents+= new BorderPanel() {
      add(ViewConstants.label("Unbekannte Schriften in Datei"), BorderPanel.Position.North)
      add(new ScrollPane{
        viewportView=unknownFontsView
      },BorderPanel.Position.Center)      
    }
    contents+=assignFontStyleBut
    contents+= new BorderPanel() {
      add(ViewConstants.label("Schriften in Datenbank"), BorderPanel.Position.North)
      add(new ScrollPane{
        viewportView=fontsView
      },BorderPanel.Position.Center)
    }
    contents+=Swing.VStrut(20)
    contents+=new BoxPanel(Orientation.Vertical) {
     contents+= allTextBlackBut
     contents+=Swing.VStrut(50)
     contents+= new BoxPanel(Orientation.Horizontal) {
       contents += ViewConstants.label("Font-Skalierung:")
       contents+=fontScaleEdit       
     }
     contents+= new BoxPanel(Orientation.Horizontal) {
       contents += ViewConstants.label("Höhenanpassung:")
       contents+=fontAdjustYEdit       
     }
    }
    
  }
  
  val centerPanel=new TabbedPane{
    pages+=new TabbedPane.Page("Elemente",contentsPane)
    pages+=new TabbedPane.Page("LinienStil",lineStylePane)
    pages+=new TabbedPane.Page("Schraffuren",hatchStylePane)
    pages+=new TabbedPane.Page("Schriften",fontStylePane)
  }
  
  val mainPanel=new BorderPanel {
      add(buttonPanel,BorderPanel.Position.South)
      add(headerPanel,BorderPanel.Position.North)
      add(centerPanel,BorderPanel.Position.Center)
      
  }
  
  listenTo(cancelBut,importBut,assignLineStyleBut,scaleCombo.selection,assignFontStyleBut,
      assignHatchStyleBut,allTextBlackBut,fontScaleEdit,fontAdjustYEdit,dxEdit,dyEdit)
  
  reactions+= {
    case ButtonClicked(`cancelBut`)=> visible=false
    case ButtonClicked(`importBut`)=>
      okResult=true
      visible=false
    case SelectionChanged(`scaleCombo`)=>
      val scale=ScalePanel.splitScaleText(scaleCombo.selection.item)
      settings.drawingScale=scale._2/scale._1
      settings.drawingScaleID=scaleValues(scaleCombo.selection.item)
      settings.analyzeFile(files.head)
      unknownLineStylesView.listData = settings.unknownLineStyles
      unknownHatchStylesView.listData=settings.unknownHatchStyles
      unknownFontsView.listData=settings.unknownFonts
    case ButtonClicked(`assignFontStyleBut`)=>
      if(unknownFontsView.selection.items.nonEmpty && fontsView.selection.items.nonEmpty ) {
        FontHandler.addAlias(fontsView.selection.items.head.name,unknownFontsView.selection.items.head)
      }
    case ButtonClicked(`assignLineStyleBut`)=> 
      for(importIx<-unknownLineStylesView.selection.indices.headOption;
      		lix <-lineStylesView.selection.indices.headOption ) {
        settings.lineStyleMapping(settings.unknownLineStyles(importIx).name)= LineStyleHandler.stylesList(lix).ix
      }
     
    case ButtonClicked(`assignHatchStyleBut`)=> 
      for(importIx<-unknownHatchStylesView.selection.indices.headOption;
      		lix <-hatchStylesView.selection.indices.headOption ) {
        settings.hatchStyleMapping(settings.unknownHatchStyles(importIx).name)= HatchHandler.hatchList(lix).ix
      }
     
    case ButtonClicked(`allTextBlackBut`)=> settings.allTextsBlack=allTextBlackBut.selected
    case EditDone(`fontScaleEdit`) => fontScaleEdit.text match {
        case StrToDouble(value)=>settings.fontScale=value
        case _=>
      }
    
    case EditDone(`fontAdjustYEdit`) =>
      fontAdjustYEdit.text match {
        case StrToDouble(value)=>settings.textYAdjust=value
        case _=>
      }

    case EditDone(`dxEdit`)=> dxEdit.text match {
      case StrToDouble(value)=> settings.dx=value
      case _=>
    }

    case EditDone(`dyEdit`)=> dyEdit.text match {
      case StrToDouble(value)=> settings.dy=value
      case _=>
    }
  }
  
  contents=mainPanel 
  scaleCombo.selection.index=scales.indexWhere(_ =="1:50")
}