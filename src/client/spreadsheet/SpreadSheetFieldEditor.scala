package client.spreadsheet

import java.awt.{Color, Dimension}

import client.dialog.{ActionPanel, CustomStrokeButton, _}
import client.graphicsView.Handlers._
import client.graphicsView._
import client.ui.ViewConstants
import definition.data.Referencable
import definition.typ.{AllClasses, SelectGroup}

import scala.collection.Iterable
import scala.swing.event.ButtonClicked
import scala.swing.{AbstractButton, Alignment, BorderPanel, BoxPanel, ButtonGroup, CheckBox, GridPanel, Label, Orientation, Panel, RadioButton, Swing, ToggleButton}
class SpreadSheetFieldEditor extends FieldEditor {
  
  var spreadSheetSG:Option[SpreadSheetSelectGroup]=None  
  val widthList=List(5f,10f,15f,20f,25f,30f,40f)
  val spreadSheetGroupName="Tabellenkalkulation"
    
  def createCheckBox(cname:String):CheckBox={
    val ret=new CheckBox(cname)
    ret.maximumSize=new Dimension(Short.MaxValue,24)
    ret.horizontalAlignment=Alignment.Left
    ret.xLayoutAlignment=0
    ret
  }  
  
  lazy val addRowsBut=new CustomStrokeButton(spreadSheetGroupName,"Zeilen einfügen",()=>{spreadSheetSG.foreach(_.controller.addRows())},11)
  lazy val removeRowsBut=new CustomStrokeButton(spreadSheetGroupName,"Zeilen löschen",()=>{spreadSheetSG.foreach(_.controller.removeRows())},12)
  lazy val addColsBut=new CustomStrokeButton(spreadSheetGroupName,"Spalten einfügen",()=>{spreadSheetSG.foreach(_.controller.addCols())},11)
  lazy val removeColsBut=new CustomStrokeButton(spreadSheetGroupName,"Spalten löschen",()=>{spreadSheetSG.foreach(_.controller.removeCols())},12)
  lazy val copyClipBut=new CustomStrokeButton(spreadSheetGroupName,"Kopieren",()=>{spreadSheetSG.foreach(_.controller.copyToClipBoard())},13)


  lazy val stringCellName: String = AllClasses.get.getClassByID(SpreadSheet.spreadSheetStringCellType).name
  lazy val doubleCellName: String = AllClasses.get.getClassByID(SpreadSheet.spreadSheetDoubleCellType).name
  lazy val formatName: String = AllClasses.get.getClassByID(SpreadSheet.spreadSheetFormatSetType).name
  lazy val colDataName: String = AllClasses.get.getClassByID(SpreadSheet.spreadSheetColumnType).name
  
  lazy val allowedClassNames: Iterable[String] = Seq(stringCellName,doubleCellName,colDataName)
  
  lazy val numberFormatPanel=new SSNumberFormatPanel(this)
  
  
  lazy val fontCombo=new ActiveComboBox(FontHandler.fontList,new FontPreviewPan){
    def elemClicked(item: FontInfo): Unit = {
      //println("Change Font :"+item)
      writeFieldValue(4,new SpreadSheetFormat(font=Some(item.name)))      
    }
  }
  lazy val fontSizeEditor=new ActiveNumberSpinner(1,40,1) {
    def fieldChanged(n: Number): Unit = {
      val newSize=n.floatValue
      if(newSize>0) writeFieldValue(5,new SpreadSheetFormat(fontSize=Some(newSize)))      
    }
  }
  
  val cellFormatRenderer = new Label with RenderComponent[CellFormat.Value]{
    horizontalAlignment=Alignment.Left
    def setStyle(a:CellFormat.Value):Unit= text=a match {
      case CellFormat.Currency=>" Währung"
      case CellFormat.Date=>" Datum"
      case CellFormat.Fraction=>" Bruch"
      case CellFormat.Number=>" Zahl"
      case CellFormat.Text=>" Text"
      case CellFormat.Percent=>" Prozent"
      case _=>" -"
    }
    def setEmpty():Unit=text=""
  }
  
  lazy val cellFormCombo=new ActiveComboBox(CellFormat.values.toSeq,cellFormatRenderer) {
    def elemClicked(item: CellFormat.Value): Unit = {
      //println("cellFormCobo clicked "+item)
      writeFieldValue(2,new SpreadSheetFormat(cellFormat=item))
      applyFormatPanel(item)
    }
  }
  
  val boldBut=new ToggleButton("F")
  val italicBut=new ToggleButton("K")  
  val underlineBut=new ToggleButton("U")
  val alLeftBut=new RadioButton("<     ")
  val alHCentBut=new RadioButton("|     ")
  val alRightBut=new RadioButton(">")
  val alTopBut=new RadioButton("^     ")
  val alVCentBut=new RadioButton("-     ")
  val alBotBut=new RadioButton("v")
  val alHGroup=new ButtonGroup
  alHGroup.buttons+=alLeftBut+=alHCentBut+=alRightBut
  val alHBox=new BoxPanel(Orientation.Horizontal)
  alHBox.contents++=alHGroup.buttons
  alHBox.opaque=false
  val alVGroup=new ButtonGroup  
  alVGroup.buttons+=alTopBut+=alVCentBut+=alBotBut
  val alVBox=new BoxPanel(Orientation.Horizontal)
  alVBox.opaque=false
  alVBox.contents++=alVGroup.buttons
  val showFormulaCheck: CheckBox = createCheckBox("Zeige Formeln")
  val visibleCheck: CheckBox = createCheckBox("Inhalt sichtbar")
  val lineBreakCheck: CheckBox = createCheckBox("Zeilenumbruch")

  val fieldComponents: Seq[SidePanelComponent[_]] = Seq.empty
  
  val formatContainer=new BoxPanel(Orientation.Vertical) {
    opaque=false
    xLayoutAlignment=0
    maximumSize=new Dimension(Short.MaxValue,140)
  }
  
  val topBorderRadio=new RadioButton
  topBorderRadio.horizontalAlignment=Alignment.Center
  val leftBorderRadio=new RadioButton
  val rightBorderRadio=new RadioButton
  val bottomBorderRadio=new RadioButton
  bottomBorderRadio.horizontalAlignment=Alignment.Center  
  val borderRadios=new ButtonGroup(topBorderRadio,leftBorderRadio,rightBorderRadio,bottomBorderRadio)
  val noBorderRadio=new RadioButton("Leer")
  val someBorderRadio=new RadioButton("Rahmen:")
  val hasBorderGroup=new ButtonGroup(noBorderRadio,someBorderRadio)
  
  
  val borderSelectPanel=new BorderPanel{
    opaque=false
    add(topBorderRadio,BorderPanel.Position.North)
    add(new BorderPanel{
      opaque=false
      add(leftBorderRadio,BorderPanel.Position.West)
      val box=Swing.RigidBox(new Dimension(50,50))
      box.border=Swing.LineBorder(Color.black)
      add(box,BorderPanel.Position.Center)
      add(rightBorderRadio,BorderPanel.Position.East)
    },BorderPanel.Position.Center)
    add(bottomBorderRadio,BorderPanel.Position.South)    
  }  
  
  val borderWidthRenderer = new Label with RenderComponent[Float]{
    horizontalAlignment=Alignment.Left
    def setStyle(a:Float):Unit= text=f"$a%2.1f"
    def setEmpty():Unit=text=""

    font = ViewConstants.labelFont
  }
  
  lazy val borderWidthCombo=new ActiveComboBox(widthList,borderWidthRenderer) {
    def elemClicked(item: Float): Unit = {
      //println("BorderWidthCobo clicked "+item) 
      someBorderRadio.selected=true
      borderRadios.selected match {
        case Some(selected)=> writeBorder(getBorderIx(selected),Some(BorderInfo(item,0,borderStyleCombo.selection.item match {
          case a if a> -1 => a
          case _=> 0
        	})))        
        case None =>
      }
    }
  }

  lazy val lineStyles: Seq[Int] = LineStyleHandler.styles.map(_.ix)
  
  lazy val borderStyleCombo:ActiveComboBox[Int,StylePreviewPan]=new ActiveComboBox(lineStyles,new StylePreviewPan) {
    def elemClicked(item: Int): Unit = {
      //println("BorderStyleCobo clicked "+item) 
      someBorderRadio.selected=true
      borderRadios.selected match {
        case Some(selected)=> writeBorder(getBorderIx(selected),Some(BorderInfo(borderWidthCombo.selection.item match {
          case a if a> -1 =>a
          case _ => 1f
        },0,item)))
        case None =>
      }
    }
  } 
  
  val borderSettingsPanel=new BoxPanel(Orientation.Vertical) {
    opaque=false
    contents+=noBorderRadio+=someBorderRadio+=borderWidthCombo+=borderStyleCombo    
  }
  
  val borderDefPanel= new BoxPanel(Orientation.Vertical) {
    opaque=false
    xLayoutAlignment=0
    contents+= new BoxPanel(Orientation.Horizontal) {
      opaque=false
      contents+=borderSelectPanel+=borderSettingsPanel
    }
    listenTo(borderRadios.buttons.toSeq:_*)
    reactions+={
      case ButtonClicked(r:RadioButton)=>showBorder(getBorderIx(r))      
    }    
  }
  
  def getBorderIx(r:AbstractButton):Int = r match {
    case `leftBorderRadio`=> 0
    case `topBorderRadio`=> 1
    case `rightBorderRadio`=> 2
    case `bottomBorderRadio`=> 3
  }
  
  def showBorder(borderNr:Int):Unit={
    //println("Show Border "+borderNr)
    spreadSheetSG match {      
      case Some(selectGroup)=> selectGroup.commonFormat.getBorder(borderNr) match {
        case Some(borderInfo)=>if(borderInfo.width==0f) noBorderRadio.selected=true else {
          someBorderRadio.selected=true
          borderWidthCombo.selfSelected=true
          borderWidthCombo.selection.index=widthList.indexWhere(_==borderInfo.width)
          borderStyleCombo.selfSelected=true
          borderStyleCombo.selection.index=lineStyles.indexWhere(_==borderInfo.style)
        }
        case None=>
          hasBorderGroup.peer.clearSelection()
          borderWidthCombo.selfSelected=true
          borderWidthCombo.selection.index= -1
          borderStyleCombo.selfSelected=true
          borderStyleCombo.selection.index= -1
      }
      case None =>
    }
  }

  def writeBorder(ix: Int, value: Option[BorderInfo]): Unit = {
    writeFieldValue(14+ix,ix match {
      case 0=> SpreadSheetFormat(leftBorder=value)
      case 1=> SpreadSheetFormat(topBorder=value)
      case 2=> SpreadSheetFormat(rightBorder=value)
      case 3=> SpreadSheetFormat(bottomBorder=value)
    })
  } 
  
  val borderLabel=new Label("Umrandung:")
  borderLabel.font = ViewConstants.labelFont
  borderLabel.xLayoutAlignment=0
  
  lazy val panel=new BoxPanel(Orientation.Vertical) {
    xLayoutAlignment=0.5d
    opaque=false
    val haLab = new Label("Horizontale Ausrichtung")
    haLab.font = ViewConstants.labelFont
    val vaLab = new Label("Vertikale Ausrichtung")
    vaLab.font = ViewConstants.labelFont
    contents += getPanelPart("Schrift:",fontCombo) += getPanelPart("Größe:",fontSizeEditor)+=
      getPanelPart("Stil:",new GridPanel(1,3){contents+=boldBut+=italicBut+=underlineBut;opaque=false})+=Swing.VStrut(5)+=
      haLab += getPanelPart(" ", alHBox) +=
      vaLab += getPanelPart(" ", alVBox) += Swing.VStrut(5) +=
          getPanelPart("",showFormulaCheck)+=getPanelPart("",visibleCheck)+=getPanelPart("",lineBreakCheck)+=Swing.VStrut(5)+=            
          getPanelPart("Inhalt:",cellFormCombo)+=formatContainer+=Swing.VStrut(5)+=borderLabel+=borderDefPanel+=Swing.VStrut(15)
		//preferredSize=new Dimension(70,470)
		maximumSize=new Dimension(Short.MaxValue,530)	
    listenTo(boldBut,italicBut,underlineBut,showFormulaCheck,visibleCheck,lineBreakCheck)
    listenTo(alHGroup.buttons.toSeq:_*)
		listenTo(alVGroup.buttons.toSeq:_*)	
		listenTo(hasBorderGroup.buttons.toSeq:_*)
		
    reactions+={
      case ButtonClicked(`boldBut`)=>writeFieldValue(6,SpreadSheetFormat(bold=Some(boldBut.selected)))
      case ButtonClicked(`italicBut`)=>writeFieldValue(7,SpreadSheetFormat(italic=Some(italicBut.selected)))
      case ButtonClicked(`underlineBut`)=>writeFieldValue(8,SpreadSheetFormat(underline=Some(underlineBut.selected)))
      case ButtonClicked(`alLeftBut`)=> if(alLeftBut.selected) writeFieldValue(0,SpreadSheetFormat(horAlign=HorAlign.LEFT))
			case ButtonClicked(`alHCentBut`)=> if(alHCentBut.selected) writeFieldValue(0,SpreadSheetFormat(horAlign=HorAlign.CENTER))
			case ButtonClicked(`alRightBut`)=> if(alRightBut.selected) writeFieldValue(0,SpreadSheetFormat(horAlign=HorAlign.RIGHT))
			case ButtonClicked(`alTopBut`)=> if(alTopBut.selected) writeFieldValue(1,SpreadSheetFormat(vertAlign=VertAlign.TOP))
			case ButtonClicked(`alVCentBut`)=> if(alVCentBut.selected) writeFieldValue(1,SpreadSheetFormat(vertAlign=VertAlign.CENTER))
			case ButtonClicked(`alBotBut`)=> if(alBotBut.selected) writeFieldValue(1,SpreadSheetFormat(vertAlign=VertAlign.BOTTOM))
			case ButtonClicked(`showFormulaCheck`)=>writeFieldValue(13,SpreadSheetFormat(showFormula=Some(showFormulaCheck.selected)))
			case ButtonClicked(`visibleCheck`)=>writeFieldValue(12,SpreadSheetFormat(visible=Some(visibleCheck.selected)))
			case ButtonClicked(`lineBreakCheck`)=>writeFieldValue(11,SpreadSheetFormat(lineBreak=Some(lineBreakCheck.selected)))			
			case ButtonClicked(`noBorderRadio`)=>borderRadios.selected match {
        case Some(selected) => writeBorder(getBorderIx(selected), Some(BorderInfo(0f, 0, 0)))
			  case None =>
			}
			case ButtonClicked(`someBorderRadio`)=> borderRadios.selected match {
			  case Some(selected)=>borderWidthCombo.selection.item match {
          case (item) => writeBorder(getBorderIx(selected), Some(BorderInfo(item, 0, 0)))
			  }
			  case None =>
			}
    }
  }   

  def getPanel: Panel = panel 
  
  def boolCheck(formatValue:Option[Boolean],button:AbstractButton):Unit=
     button.selected=formatValue match {
          case Some(bold)=>button.foreground=Color.black; bold
          case None =>button.foreground=Color.gray; false
        }
  
  override def setData(data:Iterable[SelectGroup[_<:Referencable]]): Unit = {
    spreadSheetSG=None   
    //rintln("Set Data :"+data.mkString(","))
    if (data.nonEmpty) data.head match {
      case fsg:SpreadSheetSelectGroup=>
        spreadSheetSG=Some(fsg)
        val format=fsg.commonFormat
        fsg.range match {
          case r:RowsSelection=>
            ActionPanel.addCustomButtons(Seq(copyClipBut,addRowsBut,removeRowsBut))
          case c:ColsSelection=>
            ActionPanel.addCustomButtons(Seq(copyClipBut,addColsBut,removeColsBut))
          case _ =>ActionPanel.addCustomButtons(Seq(copyClipBut))
        }
        fontCombo.selection.index= format.font match {
          case Some(font)=> fontCombo.selfSelected=true; FontHandler.fontList.indexWhere(_.name==font)
          case None => -1
        }
        //println("FontSize:"+format.fontSize)
        format.fontSize match {
          case Some(size)=> fontSizeEditor.setValue(size)
          case None => fontSizeEditor.setToUndefined()
        }
        boolCheck(format.bold,boldBut)
        boolCheck(format.italic,italicBut)
        boolCheck(format.underline,underlineBut)
        boolCheck(format.showFormula,showFormulaCheck)
        boolCheck(format.visible,visibleCheck)
        boolCheck(format.lineBreak,lineBreakCheck)

        format.horAlign match {
          case HorAlign.LEFT=> alLeftBut.selected=true
          case HorAlign.CENTER=> alHCentBut.selected=true
          case HorAlign.RIGHT=> alRightBut.selected=true
          case _=> alHGroup.peer.clearSelection()
        }
        format.vertAlign match {
          case VertAlign.TOP => alTopBut.selected=true
          case VertAlign.CENTER => alVCentBut.selected=true
          case VertAlign.BOTTOM => alBotBut.selected=true
          case _=> alVGroup.peer.clearSelection()
        }
        cellFormCombo.selfSelected=true
        cellFormCombo.selection.index=format.cellFormat.id
        applyFormatPanel(format.cellFormat)
        borderRadios.selected match {
          case Some(but)=> showBorder(getBorderIx(but))
          case _ =>
        }
      case _ =>
    }    
  }
  
  
  
  def applyFormatPanel(value:CellFormat.Value):Unit= {
  		value match {
  			case CellFormat.Number=>
          numberFormatPanel.setTextFormat(spreadSheetSG  flatMap(_.commonFormat.numberFormat))
          formatContainer.contents.clear()
          formatContainer.contents+=numberFormatPanel
        case _=> formatContainer.contents.clear()
  		}
  		panel.revalidate()
  		panel.repaint()
  }


  def writeFieldValue(field: Int, newValue: SpreadSheetFormat): Unit =
    spreadSheetSG.foreach(value=>value.controller.formatList.setFormatValue(value.range,field,newValue))
}