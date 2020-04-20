package client.spreadsheet

import java.awt.font.{LineMetrics, TextAttribute, TextLayout}
import java.awt.{Color, Dimension, Font, Graphics, Graphics2D}

import client.graphicsView.LineStyleHandler
import client.ui.ViewConstants
import definition.expression.{CurrencyConstant, Expression}
import javax.swing.border.{Border, MatteBorder}
import javax.swing.{JComponent, UIManager}
import util.StringUtils

import scala.swing.{Component, Table}
import scala.util.control.NonFatal

class SpreadSheetRenderer(controller:SpreadSheetController) extends  Component   {
  var myPrefSize:Dimension=new Dimension(10,10)
  var lineMetrics: LineMetrics = _
  override lazy val peer: JComponent = 
    new JComponent( ) with SuperMixin {
      override def validate(): Unit = {}

      override def invalidate(): Unit = {}

      override def paintComponent(g: Graphics): Unit = {
		  super.paintComponent(g)
		  val g2=g.asInstanceOf[Graphics2D]
		  g2.setColor(background)
		  g2.fill(g2.getClip())		  
		  if(text.length>0) {
			  val layout=new TextLayout(text,font,SpreadSheet.frc)
			  val txBounds=layout.getBounds
			  val x=(horOffset-txBounds.getX).toFloat
			  val y=(verOffset+(if(lineMetrics==null)-txBounds.getY else lineMetrics.getAscent()+lineMetrics.getLeading())).toFloat
			  g.setColor(foreground)
			  if(text.contains('{')){ // subtext
				  val (normalTexts,subTexts)= StringUtils.splitBracketed(text, '{','}')
				  val smallFont=font.deriveFont(font.getSize2D()*0.7f)
				  var brxpos=0f
				  for(ix<-normalTexts.indices;ntext=normalTexts(ix)){	    
				    val nlayout=new TextLayout(ntext,font,SpreadSheet.frc)
				    nlayout.draw(g2,x+brxpos,y)				    
				    brxpos+=nlayout.getAdvance+1f
				    if(ix<subTexts.size ) {	      
				      val slayout=new TextLayout(subTexts(ix),smallFont,SpreadSheet.frc)
				      slayout.draw(g2,x+brxpos,y+1.5f)				      
				      brxpos+=slayout.getAdvance+1f
				    }
				  }
				} else layout.draw(g2,x,y )
			  //System.out.print(" |  ho:"+horOffset+" vo:"+verOffset+" txB:"+txBounds)			  
		  }	  
		}

      override def getPreferredSize: Dimension = myPrefSize
	}

  val nofocusBorder: Border = UIManager.getBorder("Table.cellNoFocusBorder")
  val focusBorder: Border = UIManager.getBorder("Table.focusCellHighlightBorder")
  val focusForeground: Color = UIManager.getColor("Table.focusCellForeground")
  val focusBackground: Color = UIManager.getColor("Table.focusCellBackground")

  var horOffset:Int=0
	var verOffset:Int=0
  var fontColor: Color = Color.black
  var text:String=""
	
  /** gets the current text for the given expression
   *   
   */  
	def textFromExpression(expression:Expression,formats:List[SpreadSheetFormatRange]):String= {
    if (expression == null || expression.isNullConstant || !SpreadSheetFormat.boolFromFormats(formats, _.visible)) ""
    else {
      if (SpreadSheetFormat.boolFromFormats(formats, _.showFormula))
				SpreadSheetFormat.valueFromFormatList(formats,_.numberFormat) match {
					case Some(formatString)=> expression.getReadableTerm(formatString)
						case None => expression.getReadableTerm
				}
  		else { // show value  		  
  			val value=expression.getValue
  			import definition.typ.DataType._
  			if(value==null) "" else (value.getType match {
  				case IntTyp | LongTyp =>val formString=SpreadSheetFormat.valueFromFormatList(formats,_.numberFormat).getOrElse("")
  					if(formString .length>0) try { formString.format(value.toDouble)} catch {case NonFatal(e)=>util.Log.e(" Wrong format:["+formString+"]",e); value.toDouble.toString;
						case other:Throwable =>println(other);System.exit(0);null} else value.toLong.toString
  				
  				case DoubleTyp =>
						val formString=SpreadSheetFormat.valueFromFormatList(formats,_.numberFormat).getOrElse("")
						if(formString .length>0) try { formString.format(value.toDouble)} catch {case NonFatal(e)=>util.Log.e(" Wrong format:["+formString+"]",e); value.toDouble.toString;
						case other:Throwable =>println(other);System.exit(0);null}
            else value.toDouble.toString
					case BoolTyp => if(value.toBoolean) "\u221A" else "\u25a1"
  				case StringTyp =>
						value.toString
					case VectorTyp =>	value.toVector.shortToString
  				case CurrencyTyp =>	f"${value.toDouble}%.2f "+CurrencyConstant.currencySign
  				case _ =>  value.toString
  				    				
  			}).replaceAll("\n"," | ") 			
  		}  		
  	}  
	}  
	
	/** if a cell is empty, this function checks, if there are cells in the neighboarhood that
	 *  have content overrunning into this cell
	 * 
	 */
	def checkit(col:Int,row:Int,table:Table):(String,List[SpreadSheetFormatRange])= {
	  var deltaX=0	  
	  for(prevCol<- col-1 to 0 by -1){  
	    val prevExpression=controller.tableModel.getValueAt(row,prevCol).asInstanceOf[Expression]
	    deltaX -= table.peer.getColumnModel().getColumn(prevCol).getWidth-1
	    
	    if(prevExpression!=null && !prevExpression.isNullConstant) {	      
	      val prevFormats=controller.formatList.filterFittingFormats(prevCol,row)
	      val prevText=textFromExpression(prevExpression,prevFormats)	      
	      if(prevText.length>0) {
		      val fontName=prevFormats.find(_.format.font.isDefined).flatMap(_.format.font).getOrElse(SpreadSheet.tableFont.getName)
          val fontSize = prevFormats.find(_.format.fontSize.isDefined).flatMap(_.format.fontSize).getOrElse(SpreadSheet.tableFont.getSize().toFloat) * ViewConstants.fontScale / 100
					val bold=SpreadSheetFormat.boolFromFormats(prevFormats,_.bold)
					val italic=SpreadSheetFormat.boolFromFormats(prevFormats,_.italic)
					val underline=SpreadSheetFormat.boolFromFormats(prevFormats,_.underline)
					var prevFont=new Font(fontName,(if(bold)Font.BOLD else 0)+(if(italic)Font.ITALIC else 0),fontSize.toInt).deriveFont(fontSize)		
					if(underline) {
					  val attrMap = new java.util.Hashtable[TextAttribute,Object]()
			      attrMap.put(TextAttribute.UNDERLINE,TextAttribute.UNDERLINE_LOW_ONE_PIXEL)
            prevFont=font.deriveFont(attrMap)
          }
		      
		      val bounds=prevFont.getStringBounds(prevText,SpreadSheet.frc)
		      val prevPrefWidth=bounds.getWidth().toInt
		  		val currWidth=table.peer.getColumnModel().getColumn(prevCol).getWidth -1
		      val prevHorAlign= prevFormats.find(_.format.horAlign != HorAlign.UNDEFINED).fold(HorAlign.LEFT)(_.format.horAlign)match {
		  	    case HorAlign.LEFT=> 1
		  	    case HorAlign.CENTER =>(currWidth-prevPrefWidth)/2
		  	    case HorAlign.RIGHT => currWidth-prevPrefWidth -1
		  	  }
		      //System.out.print ( " prevHorAlign:"+prevHorAlign+" prevWidth:"+prevPrefWidth)
		      if(deltaX+prevHorAlign+prevPrefWidth>0) {
		        font=prevFont
		        lineMetrics=font.getLineMetrics("ÄqprW",SpreadSheet.frc)
		        horOffset=deltaX
		        fontColor=new Color(prevFormats.find(_.format.fontColor.isDefined).flatMap(_.format.fontColor).getOrElse(0))   
		        return (prevText,prevFormats)
		      }
		      return ("",null)
	      }
	    }
	  }
	  ("",null)
	}

	
	/** set up the renderer */
  def config(table:Table, isSelected: Boolean, focused: Boolean, expression: Expression,row:Int, col: Int) :Unit= {    
    var formats=controller.formatList.filterFittingFormats(col,row) 
    horOffset=0
    text=textFromExpression(expression,formats)
    if(text.length==0) {
      // check for previous cells
      val (ntext,nformats)=checkit(col,row,table)
      if(ntext.length>0){
        formats=nformats
        text=ntext
      }
    } else {
      val fontName = formats.find(_.format.font.isDefined).flatMap(_.format.font).getOrElse(ViewConstants.tableFont.getName)
      val fontSize = formats.find(_.format.fontSize.isDefined).flatMap(_.format.fontSize).getOrElse(ViewConstants.tableFont.getSize().toFloat) * ViewConstants.fontScale / 100
			val bold=SpreadSheetFormat.boolFromFormats(formats,_.bold)
			val italic=SpreadSheetFormat.boolFromFormats(formats,_.italic)
			val underline=SpreadSheetFormat.boolFromFormats(formats,_.underline)
			font=new Font(fontName,(if(bold)Font.BOLD else 0)+(if(italic)Font.ITALIC else 0),fontSize.toInt).deriveFont(fontSize)		
			if(underline) {
			  val attrMap = new java.util.Hashtable[TextAttribute,Object]()
	      attrMap.put(TextAttribute.UNDERLINE,TextAttribute.UNDERLINE_LOW_ONE_PIXEL)
        font=font.deriveFont(attrMap)
      }
	    lineMetrics=font.getLineMetrics("ÄqprW",SpreadSheet.frc)
	    fontColor=new Color(formats.find(_.format.fontColor.isDefined).flatMap(_.format.fontColor).getOrElse(0))    
    }
		/*if (focused)  border= focusBorder
		else*/ border={
		  val borderInfos=Seq(formats.find(_.format.leftBorder.isDefined).flatMap(_.format.leftBorder),
		    formats.find(_.format.topBorder.isDefined).flatMap(_.format.topBorder),
		    formats.find(_.format.rightBorder.isDefined).flatMap(_.format.rightBorder),
		    formats.find(_.format.bottomBorder.isDefined).flatMap(_.format.bottomBorder))
		  val borderWidths=borderInfos.map(_.fold(0f)(_.width).toInt)
		  val borderStyles=borderInfos.map(_.fold(0)(_.style))
		  if(borderWidths.exists(_ != 0f)) new SpreadSheetBorder(borderWidths,borderStyles,focused)
		  else if(focused)focusBorder else nofocusBorder
		}
  	background=if(isSelected)  table.selectionBackground 
  	else Color.white
  	if (focused && !isSelected ) {                        
				foreground=focusForeground
				background=focusBackground                        
		}
     
    foreground= if(isSelected) table.selectionForeground else fontColor 		
		if(text.length>0) {
  		val bounds=font.getStringBounds(text,SpreadSheet.frc)
  		val currWidth=table.peer.getColumnModel().getColumn(col).getWidth -1
  		val currHeight=table.peer.getRowHeight(row)
  		myPrefSize=new Dimension(bounds.getWidth().toInt,bounds.getHeight().toInt)
  		formats.find(_.format.horAlign != HorAlign.UNDEFINED).fold(HorAlign.LEFT)(_.format.horAlign)match {
  	    case HorAlign.LEFT=> horOffset+=1
  	    case HorAlign.CENTER =>horOffset +=(currWidth-myPrefSize.width)/2
  	    case HorAlign.RIGHT => horOffset +=currWidth-myPrefSize.width -1
  	  }
  		formats.find(_.format.vertAlign != VertAlign.UNDEFINED).fold(VertAlign.TOP)(_.format.vertAlign)
  	   match {
  	    case VertAlign.TOP=> verOffset=1
  	    case VertAlign.CENTER=> verOffset=(currHeight-myPrefSize.height)/2
  	    case VertAlign.BOTTOM=> verOffset=currHeight-myPrefSize.height-1
  	  } 		
  		
  	}  	  
	}		
}


class SpreadSheetBorder(borderWidths:Seq[Int],borderStyles:Seq[Int],focus:Boolean) extends 
		  MatteBorder(borderWidths(1),borderWidths.head,borderWidths(3)+15,borderWidths(2),Color.black){

  override def paintBorder(c: java.awt.Component, g: java.awt.Graphics, x: Int, y: Int, width: Int, height: Int): Unit = {
    val insets = getBorderInsets(c)
    val oldColor = g.getColor()
    val g2=g.asInstanceOf[Graphics2D]
    g.translate(x, y)
    if (color != null) {
      g.setColor(color)
      val ileft=insets.left/20
      val iright=insets.right/20
      val itop=insets.top/20
      val ibottom=insets.bottom/20
      if(borderWidths.head>0) {
      	g2.setStroke( LineStyleHandler.createStroke(2,borderWidths.head*5f,borderStyles.head))
      	g.drawLine(ileft,itop,ileft,height-itop-ibottom)
      }  
      if(borderWidths(1)>0) {
	      g2.setStroke( LineStyleHandler.createStroke(2,borderWidths(1)*5f,borderStyles(1)))
	      g.drawLine(ileft,itop,width-ileft-iright,itop)
      }
      if(borderWidths(2)>0) {
	      g2.setStroke( LineStyleHandler.createStroke(2,borderWidths(2)*5f,borderStyles(2)))
	      g.drawLine(width - iright, itop, width - iright, height - itop-ibottom)
      }
      if(borderWidths(3)>0) {
	      g2.setStroke( LineStyleHandler.createStroke(2, borderWidths(3) *5f,borderStyles(3)))
	      g.drawLine(ileft, height-ibottom-1, width - iright, height - ibottom-1)
      } 
      if(focus) {
        g2.setStroke(LineStyleHandler.createStandardStrokes().head)
        g2.setColor(SpreadSheet.focusColor)
        g.drawRect(0, 0, width-1, height-2)
      }
    }
    g.translate(-x, -y)
    g.setColor(oldColor)
  }
}