package client.plotdesign

import java.awt.{Color, Insets}

import client.graphicsView.ScalePanel
import client.print.{MediaSizeWrapper, MediaTrayWrapper, PrintOutDialog}
import client.ui.{ClientApp, ViewConstants}
import javax.print.attribute.standard.{MediaSizeName, MediaTray}
import javax.swing.BorderFactory
import util.MyComboBox

import scala.swing.event.{ButtonClicked, SelectionChanged}
import scala.swing.{BoxPanel, Button, ButtonGroup, Label, Orientation, RadioButton, Swing, TextField, ToggleButton}


class PDHeaderPanel(val controller:PlotDesignController) extends BoxPanel(scala.swing.Orientation.Vertical) {  
  val firstLine=new BoxPanel(Orientation.Horizontal)
  val secondLine=new BoxPanel(Orientation.Horizontal)
  val portraitBut=new RadioButton("")
  val landscapeBut=new RadioButton("")
  val orientGroup=new ButtonGroup(portraitBut,landscapeBut)
  val zoomAllBut=new Button("Alles")
	val zoomInBut=new ToggleButton("+")
	val zoomOutBut=new Button("-")  
	val scaleEdit=new TextField("")  
  val miniInsets=new Insets(0,0,0,0)
  val designNameLabel: Label = ViewConstants.label()
  var combosAdjusting=false
  val printBut=new Button("")
  val archiveBut=new Button("")
  val versionBut=new Button("Ver.")
  val theGroupName="PlotDesignHeader"
  
  printBut.icon=PrintOutDialog.printerIcon
  printBut.tooltip="Drucken"
  //val insets=new Insets(0,0,0,0)
  printBut.margin=miniInsets
  portraitBut.icon=PrintOutDialog.portraitIcon
  portraitBut.margin=miniInsets
  portraitBut.tooltip="Hochformat"
  landscapeBut.icon=PrintOutDialog.landscapeIcon
  landscapeBut.margin=miniInsets
  landscapeBut.tooltip="Querformat"
  archiveBut.icon=PrintOutDialog.archiveIcon
  archiveBut.tooltip="Archiv öffnen ..."
  archiveBut.margin=miniInsets
  versionBut.tooltip="Versionen verwalten"
  versionBut.margin=miniInsets

  val printerBox=new MyComboBox[String](controller.pageModel.printServiceNames.toSeq)
  val maxComboSize=new java.awt.Dimension(90,40)
  printerBox.maximumSize=maxComboSize
  val sizeBox: MyComboBox[MediaSizeWrapper] = new MyComboBox(List(MediaSizeWrapper(MediaSizeName.ISO_A4))) {
		peer.setModel(controller.pageModel.mediaModel)
  }
  sizeBox.maximumSize=maxComboSize
  val trayCombo:MyComboBox[MediaTrayWrapper]=new MyComboBox(List(new MediaTrayWrapper(MediaTray.MAIN))){
		peer.setModel(controller.pageModel.trayModel)
  }
  trayCombo.maximumSize=maxComboSize
  val formCombo=new MyComboBox(PrintFormInfo.plotDesignForms)  
  formCombo.maximumSize=maxComboSize
	zoomAllBut.margin=miniInsets
	zoomAllBut.focusable=false
	zoomInBut.margin=miniInsets
	zoomAllBut.focusable=false	
	zoomOutBut.margin=miniInsets
	zoomOutBut.focusable=false
	portraitBut.focusable=false
	landscapeBut.focusable=false
	scaleEdit.maximumSize=new java.awt.Dimension(80,25)
  scaleEdit.minimumSize=maximumSize
  background=Color.gray
  designNameLabel.background=Color.lightGray
  designNameLabel.opaque=true
  designNameLabel.border=BorderFactory.createEmptyBorder(10,5,10,5)
  
  controller.scaleModel.registerScaleListener(()=>{
		val sc=controller.scaleModel.getScaleRatio		
		scaleEdit.text=ScalePanel.scaleToText(sc._1)+" : "+ScalePanel.scaleToText(sc._2)
	})
  contents+=firstLine+=secondLine+=ClientApp.createHLine
  firstLine.xLayoutAlignment=0f
  secondLine.xLayoutAlignment=0f
	firstLine.contents+=designNameLabel+=Swing.HStrut(10)+=printBut+=archiveBut+=Swing.HStrut(10)+=versionBut+=Swing.Glue+=formCombo+=printerBox
	secondLine.contents+=zoomAllBut+=zoomInBut+=zoomOutBut+=Swing.HStrut(10)+=scaleEdit+=Swing.HStrut(10)+=Swing.HGlue+=
	  sizeBox+=trayCombo+=Swing.HStrut(10)+=portraitBut+=landscapeBut
  firstLine.contents foreach(_.font=ViewConstants.smallFont)
  secondLine.contents foreach(_.font=ViewConstants.smallFont)
  designNameLabel.font=ViewConstants.tableFont
  
  listenTo(portraitBut,landscapeBut,zoomAllBut,zoomInBut,zoomOutBut,printerBox.selection,trayCombo.selection,sizeBox.selection,printBut,archiveBut,versionBut,formCombo.selection)
  
  reactions+= {
    case SelectionChanged(`printerBox`)=> if(!combosAdjusting){
      combosAdjusting=true
      controller.pageModel.setPrinter(printerBox.selection.index)
      combosAdjusting=false
    } 
    case SelectionChanged(`sizeBox`) if !combosAdjusting =>
      combosAdjusting=true
      controller.pageModel.setMediaWrapper(sizeBox.selection.item)
      combosAdjusting=false
    case SelectionChanged(`trayCombo`) if !combosAdjusting =>
      combosAdjusting=true
      controller.pageModel.setMediaTray(trayCombo.selection.item)
      combosAdjusting=false
    case SelectionChanged(`formCombo`)=> if(!combosAdjusting){
      combosAdjusting=true      
      controller.pageModel.setForm(formCombo.selection.item.id)
      combosAdjusting=false
    }
    
    
    case ButtonClicked(`portraitBut`) => if(!combosAdjusting){
      combosAdjusting=true
      controller.pageModel.setOrientation(true)
      combosAdjusting=false
    }
    case ButtonClicked(`landscapeBut`) => if(!combosAdjusting){
      combosAdjusting=true
      controller.pageModel.setOrientation(false)
      combosAdjusting=false
    }
    case ButtonClicked(`zoomAllBut`) => controller.zoomAll()
    case ButtonClicked(`zoomInBut`) => controller.scaleModel.zoomPlus(0.5,0.5)
    case ButtonClicked(`zoomOutBut`) => controller.zoomOut()
    case ButtonClicked(`printBut`) => controller.print()
    case ButtonClicked(`archiveBut`)=> controller.showArchive()
    case ButtonClicked(`versionBut` )=> controller.showVersions()
    
  }
  
  //def model=controller.scaleModel

  def setName(id: Int, name: String): Unit = {
    designNameLabel.text="    "+id+". "+name+" "
  }

  def selectForm(inst: Int): Unit = {
    val ix=PrintFormInfo.plotDesignForms.indexWhere(_.id==inst)
    combosAdjusting=true
    formCombo.selection.index=ix
    combosAdjusting=false
  }

  def selectPrinter(ix: Int): Unit = {
    combosAdjusting=true
    printerBox.selection.index=ix
    combosAdjusting=false
  }

  def selectSize(ix: Int): Unit = {
    //println("selectSize "+ix)
    combosAdjusting=true
    sizeBox.selection.index=ix
    combosAdjusting=false
  }

  def selectTray(ix: Int): Unit = {
    combosAdjusting=true
    trayCombo.selection.index=ix
    combosAdjusting=false
  }

  def selectOrientation(portrait: Boolean): Unit = if (!combosAdjusting) {
    combosAdjusting=true
    if(portrait) portraitBut.selected=true else landscapeBut.selected=true
    combosAdjusting=false
  }

  def turnoutAdjusting(): Unit = combosAdjusting = false

  def showFormDialog(): Unit = {
    
  }
}