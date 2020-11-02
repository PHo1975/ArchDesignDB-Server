package client.graphicsView

import java.awt.{Color, Dimension, Font, Graphics2D}

import client.dialog._
import client.ui.ViewConstants
import definition.expression.{Constant, Expression, IntConstant, StringConstant}

import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, ButtonGroup, GridPanel, Orientation, RadioButton, ToggleButton}



class TextStyleEditor extends FieldEditor {
  final val HORBITS: Int = 8 + 16
  final val VERBITS: Int = 1 + 2
  final val FRAMEBITS: Int = 1024 + 2048
  val tcn="TextElem"
  val allowedClassNames=Seq(tcn)    
  
  lazy val fontCombo=new SidePanelFontBox(this,Map((tcn,3)))
  fontCombo.addSearchLookup({case t:TextElement=>  FontHandler.getFontInfo(t.fontName)})
  
  val sizeEditor=new SidePanelDoubleTextField(Map((tcn,4)),this)
  sizeEditor.addSearchLookup({case t:TextElement=> t.height})
  val angleEditor=new SidePanelDoubleTextField(Map((tcn,7)),this)
  angleEditor.addSearchLookup({case t:TextElement=> t.textAngle})
  
  val textStylePanel=new BoxPanel(Orientation.Vertical) with SidePanelComponent[Int] {
    xLayoutAlignment=0d
    val allowedFields=Map((tcn,6.toByte))
    val boldBut=new ToggleButton("F")
    val italicBut = new ToggleButton("K")
    val capitalBut = new ToggleButton("G")
    val alLeftBut=new RadioButton("<     ")
    val alHCentBut=new RadioButton("|     ")
    val alRightBut=new RadioButton(">")
    val alTopBut=new RadioButton("^     ")
    val alVCentBut=new RadioButton("-     ")
    val alBotBut=new RadioButton("v")
    val alHGroup=new ButtonGroup
    val noFrameBut = new RadioButton("X  ")
    val roundFrameBut = new RadioButton("( )  ")
    val squareFrameBut = new RadioButton("[ ]")
    alHGroup.buttons+=alLeftBut+=alHCentBut+=alRightBut
    val alHBox=new BoxPanel(Orientation.Horizontal)
    alHBox.opaque=false
    alHBox.contents++=alHGroup.buttons
    val alVGroup=new ButtonGroup    
    alVGroup.buttons+=alTopBut+=alVCentBut+=alBotBut
    val alVBox=new BoxPanel(Orientation.Horizontal)
    alVBox.opaque=false
    alVBox.contents++=alVGroup.buttons
    val frameGroup = new ButtonGroup
    frameGroup.buttons += noFrameBut += roundFrameBut += squareFrameBut
    val frameBox = new BoxPanel(Orientation.Horizontal)
    frameBox.contents ++= frameGroup.buttons

    contents += getPanelPart("Stil:", new GridPanel(1, 3) {contents += boldBut += italicBut += capitalBut; opaque = false}) +=
      getPanelPart("HBezug:", alHBox) += getPanelPart("VBezug:", alVBox) += getPanelPart("Rahmen", frameBox)

    listenTo(boldBut, italicBut, capitalBut)
    listenTo(alHGroup.buttons.toSeq:_*)
    listenTo(alVGroup.buttons.toSeq: _*)
    listenTo(frameGroup.buttons.toSeq: _*)
		reactions+={			
			case ButtonClicked(`boldBut`)=> writeStyleBit(GraphElemConst.boldStyle,boldBut.selected)
			case ButtonClicked(`italicBut`)=> writeStyleBit(GraphElemConst.italicStyle,italicBut.selected)
      case ButtonClicked(`capitalBut`) => writeStyleBit(GraphElemConst.capitalStyle, capitalBut.selected)
			case ButtonClicked(`alLeftBut`)=> if(alLeftBut.selected) writeStyleMapped(HORBITS,0)
			case ButtonClicked(`alHCentBut`)=> if(alHCentBut.selected) writeStyleMapped(HORBITS,8)
			case ButtonClicked(`alRightBut`)=> if(alRightBut.selected) writeStyleMapped(HORBITS,16)
			case ButtonClicked(`alTopBut`)=> if(alTopBut.selected) writeStyleMapped(VERBITS,2)
			case ButtonClicked(`alVCentBut`)=> if(alVCentBut.selected) writeStyleMapped(VERBITS,1)
			case ButtonClicked(`alBotBut`)=> if(alBotBut.selected) writeStyleMapped(VERBITS,0)
      case ButtonClicked(`noFrameBut`) => if (noFrameBut.selected) writeStyleMapped(FRAMEBITS, 0)
      case ButtonClicked(`roundFrameBut`) => if (roundFrameBut.selected) writeStyleMapped(FRAMEBITS, 1024)
      case ButtonClicked(`squareFrameBut`) => if (squareFrameBut.selected) writeStyleMapped(FRAMEBITS, 2048)
    }   
    
    addSearchLookup({
      case t:TextElement => t.style 
    })
    
    val defaultValue=0
    var isBoldSet: Option[Boolean] = _
    var isItalicSet: Option[Boolean] = _
    var isCapitalSet: Option[Boolean] = _
    var horAlign: Int = -2
    var verAlign: Int = -2
    var frames: Int = -2

    def getConstant(value: Int): Constant = IntConstant(value)

    def valueFromConstant(c: Expression): Int = c.getValue.toInt

    def writeStyle(func: (Int) => Int): Unit = storeValueMapped(this, func)

    def writeStyleBit(styleBit: Int, newValue: Boolean): Unit =
    	writeStyle( (oldStyle)=>if(newValue) oldStyle | styleBit else oldStyle & (~styleBit) )

    def writeStyleMapped(bitMap: Int, newValue: Int): Unit =
      writeStyle(o => (o & (~bitMap)) + newValue)

    
    override def resetSearchValue():Unit= {
      isBoldSet=null
      isItalicSet=null
      isCapitalSet = null
      horAlign= -2
      verAlign= -2
      frames = -2
    }

    override def internSetSearchValue(style: Int): Unit = {
     // println("internSetSearchValue " + GraphElemConst.styleIsBold(style) + " isSet: " + isBoldSet)
    	if(isBoldSet!=None){
    		if(isBoldSet==null) isBoldSet=Some(GraphElemConst.styleIsBold(style))
    		else if (isBoldSet.get!=GraphElemConst.styleIsBold(style)) isBoldSet=None
    	}
    	if(isItalicSet!=None){
    		if(isItalicSet==null) isItalicSet=Some(GraphElemConst.styleIsItalic(style))
    		else if (isItalicSet.get!=GraphElemConst.styleIsItalic(style)) isItalicSet=None
    	}
      if (isCapitalSet != None) {
        if (isCapitalSet == null) isCapitalSet = Some(GraphElemConst.styleIsCapital(style))
        else if (isCapitalSet.get != GraphElemConst.styleIsCapital(style)) isCapitalSet = None
      }
    	if(horAlign != -1) {
        val da = style & HORBITS
    		if(horAlign == -2)horAlign=da else if(horAlign!=da) horAlign= -1      		  
    	}
    	if(verAlign != -1) {
        val da = style & VERBITS
    		if(verAlign == -2)verAlign=da else if(verAlign!=da) verAlign= -1
      }
      if (frames != -1) {
        val da = style & FRAMEBITS
        if (frames == -2) frames = da else if (frames != da) frames = -1
      }
    }
  
    override def updateSearchValue():Unit = {
      //println("updateSearchvale isBoldSet " + isBoldSet)
      if (isBoldSet == null || isBoldSet.isEmpty) {boldBut.selected = false; boldBut.foreground = Color.GRAY}
    		else {boldBut.foreground=Color.BLACK;boldBut.selected=isBoldSet.get   }
      if (isItalicSet == null || isItalicSet.isEmpty) {italicBut.selected = false; italicBut.foreground = Color.GRAY}
    		else {italicBut.foreground=Color.BLACK;italicBut.selected=isItalicSet.get   }
      if (isCapitalSet == null || isCapitalSet.isEmpty) {capitalBut.selected = false; capitalBut.foreground = Color.GRAY}
      else {capitalBut.foreground = Color.BLACK; capitalBut.selected = isCapitalSet.get}
    		if(horAlign<0)alHGroup.peer.clearSelection()
    		else horAlign match {
    			case 0 => alLeftBut.selected=true
          case GraphElemConst.hCenterStyle => alHCentBut.selected = true
          case GraphElemConst.rightStyle => alRightBut.selected = true
    			case _=>alHGroup.peer.clearSelection()
    		}
    		if(verAlign<0) alVGroup.peer.clearSelection()
    		else verAlign match {
          case GraphElemConst.bottomStyle => alTopBut.selected = true
    			case 1 =>alVCentBut.selected=true
    			case 0=>alBotBut.selected=true
    			case _=>alVGroup.peer.clearSelection()
        }
      if (frames < 0) frameGroup.peer.clearSelection()
      else frames match {
        case 0 => noFrameBut.selected = true
        case GraphElemConst.roundBorderSyle => roundFrameBut.selected = true
        case GraphElemConst.squareBorderSyle => squareFrameBut.selected = true
      }
    		resetSearchValue()
    }
  }
  
  lazy val fieldComponents=Seq(fontCombo,sizeEditor,angleEditor,textStylePanel)
  	
	lazy val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents += getPanelPart("Schrift:",fontCombo) += getPanelPart("Höhe:",sizeEditor)+=textStylePanel+=
	  getPanelPart("Winkel:",angleEditor)
    //preferredSize=new Dimension(70,190)
    maximumSize = new Dimension(Short.MaxValue, 190 * ViewConstants.fontScale / 100)
  }

  def getPanel: BoxPanel = panel
}

class FontPreviewPan extends RenderComponent[FontInfo] {  
  opaque=true
  preferredSize = new Dimension(40 * ViewConstants.fontScale / 100, 28 * ViewConstants.fontScale / 100)
  protected var fontName: String = "Arial"

  def setStyle(f: FontInfo): Unit = {
    fontName=f.name
    peer.setToolTipText(fontName)
    if (f.name.length > 0) font = new Font(fontName, 0, (ViewConstants.tableFont.getSize * 1.1).toInt)
  }

  def setEmpty(): Unit = {
    fontName=""
    peer.setToolTipText("")      
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)    
    g.setColor(background)
    g.fill(g.getClipBounds)       
    g.setColor(foreground)
    if (font != null) g.setFont(font)
    g.drawString(fontName, 2, 18)
  }
}


class SidePanelFontBox(neditor:FieldEditor,val nallowedFields:Map[String,Byte]) 
		extends SidePanelComboBox(FontHandler.fontList,new FontPreviewPan,neditor,nallowedFields) {
  val defaultValue: FontInfo = FontHandler.defaultFont

  def getConstant(value: FontInfo): Constant = StringConstant(value.name)

  def valueFromConstant(c: Expression): FontInfo = FontHandler.getFontInfo(c.getValue.toString)
    override def setValue(newFont:Option[FontInfo]):Unit= {
      super.setValue(newFont)
      if (newFont.isEmpty || newFont == null) selection.index = -1
      else {      	
      	selfSelected=true      	
      	selection.index=FontHandler.fontList.indexWhere(_.name==newFont.get.name)
      }
    }     
  } 