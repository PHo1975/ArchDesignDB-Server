package client.plotdesign

import scala.swing.BoxPanel
import scala.swing.Component
import scala.swing.Button
import scala.swing.ToggleButton
import scala.swing.TextField
import util.MyComboBox
import scala.swing.ButtonGroup
import scala.swing.RadioButton
import java.awt.Font
import scala.swing.Swing
import java.awt.Insets
import java.awt.Color
import client.graphicsView.ScalePanel
import scala.swing.Label
import javax.print.PrintServiceLookup
import javax.print.PrintService
import scala.swing.event.SelectionChanged
import client.print.MediaSizeWrapper
import javax.print.attribute.standard.MediaSizeName
import client.print.{MediaTrayWrapper,AutoTray}
import javax.print.attribute.standard.MediaTray
import scala.swing.event.ButtonClicked
import javax.swing.BorderFactory
import client.print.PrintOutDialog
import client.dataviewer.ViewConstants
import scala.swing.Orientation
import client.ui.ClientApp


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
  val designNameLabel=new Label()
  var combosAdjusting=false
  val printBut=new Button("")
  val archiveBut=new Button("")
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
  val printerBox=new MyComboBox[String](controller.pageModel.printServiceNames)
  val maxComboSize=new java.awt.Dimension(90,40)
  printerBox.maximumSize=maxComboSize  
  val sizeBox:MyComboBox[MediaSizeWrapper]=new MyComboBox(List(new MediaSizeWrapper(MediaSizeName.ISO_A4))){
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
	firstLine.contents+=designNameLabel+=Swing.HStrut(10)+=printBut+=archiveBut+=Swing.HStrut(10)+=Swing.Glue+=formCombo+=printerBox
	secondLine.contents+=zoomAllBut+=zoomInBut+=zoomOutBut+=Swing.HStrut(10)+=scaleEdit+=Swing.HStrut(10)+=Swing.HGlue+=
	  sizeBox+=trayCombo+=Swing.HStrut(10)+=portraitBut+=landscapeBut
  firstLine.contents foreach(_.font=ViewConstants.smallFont)
  secondLine.contents foreach(_.font=ViewConstants.smallFont)
  designNameLabel.font=ViewConstants.tableFont
  
  listenTo(portraitBut,landscapeBut,zoomAllBut,zoomInBut,zoomOutBut,printerBox.selection,trayCombo.selection,sizeBox.selection,printBut,archiveBut,formCombo.selection)
  
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
    case ButtonClicked(`zoomInBut`) => controller.zoomInClicked()
    case ButtonClicked(`zoomOutBut`) => controller.zoomOut()
    case ButtonClicked(`printBut`) => controller.print()
    case ButtonClicked(`archiveBut`)=> controller.showArchive()
    
  }
  
  //def model=controller.scaleModel
  
  def setName(id:Int,name:String)= {
    designNameLabel.text="    "+id+". "+name+" "
  } 
  
  def selectForm(inst:Int)= {
    val ix=PrintFormInfo.plotDesignForms.indexWhere(_.id==inst)
    combosAdjusting=true
    formCombo.selection.index=ix
    combosAdjusting=false
  }
  
  def selectPrinter(ix:Int)= {
    combosAdjusting=true
    printerBox.selection.index=ix
    combosAdjusting=false
  }
  
  def selectSize(ix:Int) = {
    //println("selectSize "+ix)
    combosAdjusting=true
    sizeBox.selection.index=ix
    combosAdjusting=false
  } 
  
  def selectTray(ix:Int) = {
    combosAdjusting=true
    trayCombo.selection.index=ix
    combosAdjusting=false
  }
  
  def selectOrientation(portrait:Boolean) = if(!combosAdjusting){
    combosAdjusting=true
    if(portrait) portraitBut.selected=true else landscapeBut.selected=true
    combosAdjusting=false
  } 
  
  def turnoutAdjusting()= combosAdjusting=false  
  
  def showFormDialog() = {
    
  }
}