package client.graphicsView

import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.Graphics2D
import scala.swing.BoxPanel
import scala.swing.ButtonGroup
import scala.swing.GridPanel
import scala.swing.Orientation
import scala.swing.RadioButton
import scala.swing.ToggleButton
import scala.swing.event.ButtonClicked
import definition.expression.Constant
import definition.expression.IntConstant
import definition.expression.StringConstant
import client.dialog.FieldEditor
import client.dialog.RenderComponent
import client.dialog.SidePanelComboBox
import client.dialog.SidePanelComponent
import client.dialog.SidePanelDoubleTextField



class TextStyleEditor extends FieldEditor {
  final val HORBITS=8+16
  final val VERBITS=1+2  
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
    val italicBut=new ToggleButton("K")    
    val alLeftBut=new RadioButton("<     ")
    val alHCentBut=new RadioButton("|     ")
    val alRightBut=new RadioButton(">")
    val alTopBut=new RadioButton("^     ")
    val alVCentBut=new RadioButton("-     ")
    val alBotBut=new RadioButton("v")
    val alHGroup=new ButtonGroup
    alHGroup.buttons+=alLeftBut+=alHCentBut+=alRightBut
    val alHBox=new BoxPanel(Orientation.Horizontal)
    alHBox.opaque=false
    alHBox.contents++=alHGroup.buttons
    val alVGroup=new ButtonGroup    
    alVGroup.buttons+=alTopBut+=alVCentBut+=alBotBut
    val alVBox=new BoxPanel(Orientation.Horizontal)
    alVBox.opaque=false
    alVBox.contents++=alVGroup.buttons
    
    contents+=getPanelPart("Stil:",new GridPanel(1,2){contents+=boldBut+=italicBut;opaque=false})+=  
	  getPanelPart("HBezug:",alHBox)+=getPanelPart("VBezug:",alVBox)
	  
	  listenTo(boldBut,italicBut)
    listenTo(alHGroup.buttons.toSeq:_*)
		listenTo(alVGroup.buttons.toSeq:_*)		
		reactions+={			
			case ButtonClicked(`boldBut`)=> writeStyleBit(GraphElemConst.boldStyle,boldBut.selected)
			case ButtonClicked(`italicBut`)=> writeStyleBit(GraphElemConst.italicStyle,italicBut.selected)
			case ButtonClicked(`alLeftBut`)=> if(alLeftBut.selected) writeStyleMapped(HORBITS,0)
			case ButtonClicked(`alHCentBut`)=> if(alHCentBut.selected) writeStyleMapped(HORBITS,8)
			case ButtonClicked(`alRightBut`)=> if(alRightBut.selected) writeStyleMapped(HORBITS,16)
			case ButtonClicked(`alTopBut`)=> if(alTopBut.selected) writeStyleMapped(VERBITS,2)
			case ButtonClicked(`alVCentBut`)=> if(alVCentBut.selected) writeStyleMapped(VERBITS,1)
			case ButtonClicked(`alBotBut`)=> if(alBotBut.selected) writeStyleMapped(VERBITS,0)
    }   
    
    addSearchLookup({
      case t:TextElement => t.style 
    })
    
    val defaultValue=0
    var isBoldSet:Option[Boolean]=null
    var isItalicSet:Option[Boolean]=null
    var horAlign= -2
    var verAlign= -2
    
    def getConstant(value:Int):Constant=new IntConstant(value)  
    def valueFromConstant(c:Constant)=c.toInt
    
    def writeStyle(func:(Int)=>Int):Unit= storeValueMapped(this,func)
    def writeStyleBit(styleBit:Int,newValue:Boolean) = 
    	writeStyle( (oldStyle)=>if(newValue) oldStyle | styleBit else oldStyle & (~styleBit) )
    
    def writeStyleMapped(bitMap:Int,newValue:Int)= {    
    		val filter= ~bitMap
    		writeStyle( o=> (o & filter)+newValue  )
    }
    
    override def resetSearchValue():Unit= {
      isBoldSet=null
      isItalicSet=null
      horAlign= -2
      verAlign= -2
    }
  
    override def internSetSearchValue(style:Int)=  {
    	if(isBoldSet!=None){
    		if(isBoldSet==null) isBoldSet=Some(GraphElemConst.styleIsBold(style))
    		else if (isBoldSet.get!=GraphElemConst.styleIsBold(style)) isBoldSet=None
    	}
    	if(isItalicSet!=None){
    		if(isItalicSet==null) isItalicSet=Some(GraphElemConst.styleIsItalic(style))
    		else if (isItalicSet.get!=GraphElemConst.styleIsItalic(style)) isItalicSet=None
    	}
    	if(horAlign != -1) {
    		val da=style& (8+16)
    		if(horAlign == -2)horAlign=da else if(horAlign!=da) horAlign= -1      		  
    	}
    	if(verAlign != -1) {
    		val da=style& (1+2)
    		if(verAlign == -2)verAlign=da else if(verAlign!=da) verAlign= -1      		  
    	} 
    }
  
    override def updateSearchValue():Unit = {
    		if(isBoldSet==None||isBoldSet==null) {boldBut.selected=false;boldBut.foreground=Color.GRAY}
    		else {boldBut.foreground=Color.BLACK;boldBut.selected=isBoldSet.get   }
    		if(isItalicSet==None||isItalicSet==null) {italicBut.selected=false;italicBut.foreground=Color.GRAY}
    		else {italicBut.foreground=Color.BLACK;italicBut.selected=isItalicSet.get   }
    		if(horAlign<0)alHGroup.peer.clearSelection()
    		else horAlign match {
    			case 0 => alLeftBut.selected=true
    			case 8 => alHCentBut.selected=true
    			case 16 => alRightBut.selected=true
    			case _=>alHGroup.peer.clearSelection()
    		}
    		if(verAlign<0) alVGroup.peer.clearSelection()
    		else verAlign match {
    			case 2 =>alTopBut.selected=true
    			case 1 =>alVCentBut.selected=true
    			case 0=>alBotBut.selected=true
    			case _=>alVGroup.peer.clearSelection()
    		}    
    		resetSearchValue()
    }
  }
  
  lazy val fieldComponents=Seq(fontCombo,sizeEditor,angleEditor,textStylePanel)
  	
	lazy val panel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents += getPanelPart("Schrift:",fontCombo) += getPanelPart("Höhe:",sizeEditor)+=textStylePanel+=
	  getPanelPart("Winkel:",angleEditor)
		preferredSize=new Dimension(70,190)
		maximumSize=new Dimension(Short.MaxValue,190)	
  }   
  
  def getPanel=panel  
}

class FontPreviewPan extends RenderComponent[FontInfo] {  
  opaque=true  
  preferredSize=new Dimension(40,28)
  var fontName:String="Arial"
  
  def setStyle(f:FontInfo)= {
    fontName=f.name
    peer.setToolTipText(fontName)
    //repaint
  }
  
  def setEmpty()= {
    fontName=""
    peer.setToolTipText("")      
  }
  
  override def paintComponent(g:Graphics2D)= {
    super.paintComponent(g)    
    g.setColor(background)
    g.fill(g.getClipBounds)       
    g.setColor(foreground)
    if(fontName.length>0) {
    	g.setFont(new Font(fontName,0,17))
    	g.drawString(fontName,2,18)
    }
  }
}


class SidePanelFontBox(neditor:FieldEditor,val nallowedFields:Map[String,Byte]) 
		extends SidePanelComboBox(FontHandler.fontList,new FontPreviewPan,neditor,nallowedFields) {
    val defaultValue=FontHandler.defaultFont
    def getConstant(value:FontInfo):Constant=new StringConstant(value.name)  
    def valueFromConstant(c:Constant)=FontHandler.getFontInfo(c.toString)
    override def setValue(newFont:Option[FontInfo]):Unit= {
      super.setValue(newFont)
      if(newFont==None||newFont==null)selection.index= -1
      else {      	
      	selfSelected=true      	
      	selection.index=FontHandler.fontList.indexWhere(_.name==newFont.get.name)
      }
    }     
  } 