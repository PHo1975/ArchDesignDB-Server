/**
 * Author: Peter Started:10.10.2010
 */
package client.graphicsView

import java.awt.BasicStroke
import java.awt.Color
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Component
import scala.swing.Dimension
import scala.swing.Graphics2D
import scala.swing.GridPanel
import scala.swing.Label
import util.MyListView
import scala.swing.Panel
import scala.swing.Table
import client.dataviewer.InstanceRenderer
import client.dialog.FieldEditor
import client.dialog.InplaceFieldEditor
import client.dialog.RenderComponent
import definition.data.LineStyle
import definition.expression.Constant
import definition.expression.Expression
import definition.expression.IntConstant
import javax.swing.DefaultCellEditor
import javax.swing.JTable
import javax.swing.table.TableCellEditor
import util.MyComboBox
import client.dialog.SidePanelComboBox


class BoolEditor extends InplaceFieldEditor {
  val myCheck=new CheckBox
  myCheck.opaque=true
  myCheck.background=Color.white
  val renderer=new CheckBoxRenderer
  def getEditor:TableCellEditor=new DefaultCellEditor(myCheck.peer){
    
    override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ) = {  		
  		myCheck.selected=value match {
  		  case c:Expression => c.getValue.toBoolean
  		  case _ => false
  		}
  		myCheck.peer
  	}
  	override def getCellEditorValue():java.lang.Object =  	{
  		if (myCheck.selected)java.lang.Boolean.TRUE.asInstanceOf[AnyRef]
  		else  java.lang.Boolean.FALSE.asInstanceOf[AnyRef]  		
  	}
  }
  def createRenderer:Table.Renderer[Expression]=new Table.AbstractRenderer[Expression,CheckBoxRenderer](new CheckBoxRenderer){
    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: Expression, row: Int, column: Int): Unit = {
      component.setStyle(a)
      component.background=if(isSelected)  table.selectionBackground 
  	  else  if (row % 2 == 0)InstanceRenderer.alternateColor 
  	  			else Color.white
    }
  }  
}

class CheckBoxRenderer extends CheckBox("  ") with RenderComponent[Expression] {
   opaque=true
   background=Color.white
   def setStyle(a:Expression):Unit= {
     this.selected=a.getValue.toBoolean
   }
   def setEmpty():Unit=this.selected=false
  
}

object LineStyleEditor{
  val widthList=List(0,10,15,20,25,35,50,70,100,200)
}

class LineWidthRenderer extends Label with RenderComponent[Int] {
	  def setStyle(width:Int)= text=f"${width.toDouble / 100d}%3.2f"
	  def setEmpty()= text=""
	}	

/**
 * 
 */
class LineStyleEditor extends FieldEditor {
	import client.graphicsView.LineStyleEditor._
	val allowedClassNames=Seq("LineElem","ArcElem","PolyElem","EllipseElem","AreaPolygon")
	val widthFieldNr=1.toByte
	val styleFieldNr=2.toByte	
	
	lazy val widthCombo=new SidePanelComboBox(widthList,new LineWidthRenderer(),this,(allowedClassNames.map((_,widthFieldNr)):+(("AreaPolygon",2.toByte))).toMap) {
	  val defaultValue=0
    def getConstant(value:Int):Constant=new IntConstant(value)  
    def valueFromConstant(c:Constant)=c.toInt
    override def setValue(newWidth:Option[Int]):Unit= {	    
	    super.setValue(newWidth)
	    selfSelected=true
	    selection.index= newWidth match {
	      case Some(nWidth)=> widthList.indexOf(nWidth)
	      case _ => -1
	    }	    
	  }
	  addSearchLookup({
      case t:LinearElement => t.lineWidth
    })
	}
	
	lazy val styleCombo=new SidePanelComboBox(LineStyleHandler.styles.map(_.ix),new StylePreviewPan,this,
	    (allowedClassNames.map((_,styleFieldNr)):+(("AreaPolygon",3.toByte))).toMap) {
	  val defaultValue=LineStyleHandler.undefinedStyle.ix
	  def getConstant(value:Int)= new IntConstant(value)
	  def valueFromConstant(c:Constant)=c.toInt
	  override def setValue(newStyle:Option[Int]):Unit= {	
	    //println("set Value "+newStyle+" "+LineStyleHandler.styles.mkString(","))
	    super.setValue(newStyle)
	    selfSelected=true
	    selection.index= newStyle match {
	      case Some(nStyle)=> nStyle
	      case _=> -1
	    }	    	    
	  }
	  addSearchLookup({
      case t:LinearElement => t.lineStyle
    })
	}		
	
	lazy val panel=new GridPanel(2,1) {
	  opaque=false
	  contents+= getPanelPart("Dicke",widthCombo)+= getPanelPart("Strichart",styleCombo)	  		
		preferredSize=new Dimension(70,64)
		maximumSize=new Dimension(Short.MaxValue,64)	
	  xLayoutAlignment=0d
	}
	
	lazy val fieldComponents=Seq(widthCombo,styleCombo)	
  def getPanel:Panel=panel  
}

class InplaceStyleEditor() extends InplaceFieldEditor{  
  val styleCombo=new MyComboBox(LineStyleHandler.styles)  
  val editor = new DefaultCellEditor(styleCombo.peer) {  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ) = {  		
  		styleCombo.selection.index=value match {
  		  case c:Constant => c.toInt
  		  case _ => -1
  		}
  		styleCombo.peer
  	}
  	override def getCellEditorValue():java.lang.Object =
  	{
  		styleCombo.peer.getSelectedItem() match {
  		  case ls:LineStyle=>ls.ix.asInstanceOf[AnyRef]
  		  case _=> 0.asInstanceOf[AnyRef]
  		}
  	}
  }
  
  val previewPrototype=new StylePreviewPan    
  def getEditor=editor  
  def createRenderer=new Table.AbstractRenderer[Expression,StylePreviewPan](new StylePreviewPan){
    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: Expression, row: Int, column: Int): Unit = {
      component.setStyle(null,a.asInstanceOf[Constant].toInt)
    }
  }  
  	
	styleCombo.renderer=new MyListView.AbstractRenderer[LineStyle,StylePreviewPan](previewPrototype){
		def configure(list: MyListView[LineStyle], isSelected: Boolean, focused: Boolean, a: LineStyle, index: Int): Unit = {
			component.setStyle(a,index)
		}		
	}   
}

  
class StylePreviewPan extends Component with RenderComponent[Int] { 
  val numWidth=20
  var stroke=new BasicStroke(2f)
  var theIndex=0
  opaque=true  
  lazy val strokes=LineStyleHandler.createStandardStrokes()
  val b=new Button
  preferredSize=new Dimension(40,25)
  val standBack=b.background
  
  
  def setStyle(a:LineStyle,ix:Int)= {
    stroke=if(ix== -1){
     if(a==null)null else strokes(a.ix)     
    } else strokes(ix)
    peer.setToolTipText(a.name)
    theIndex=ix
  }
  
  def setStyle(a:Int)= {
    stroke=if(a< 0 || a >= strokes.size) null
    else strokes(a)
    peer.setToolTipText(LineStyleHandler.styles.find(_.ix==a)match {
      case Some(style)=>style.name case _=> a.toString
    })
    if(a<strokes.size) theIndex=a
  }
  
  def setEmpty()= stroke=null
  
  override def paintComponent(g:Graphics2D)= {
    super.paintComponent(g)
    val back=if(stroke==null)standBack else background
    g.setColor(back) 
    val cb=g.getClipBounds    
    val siz=size
    if(stroke!=null)  {
      g.fillRect(0, 0, numWidth, siz.height)
      g.setFont(font)
      g.setColor(foreground)
      g.drawString(theIndex.toString, 0, 20)
      g.setColor(back)
      g.fillRect(numWidth,0,siz.width,siz.height)
    	g.setStroke(stroke)    
    	g.setColor(foreground)    	
    	g.drawLine(numWidth+5,siz.height/2,siz.width-2,siz.height/2)
    }
  }
}