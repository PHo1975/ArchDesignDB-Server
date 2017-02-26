package client.graphicsView

import client.dialog.InplaceFieldEditor
import scala.swing.ComboBox
import javax.swing.DefaultCellEditor
import javax.swing.JTable
import definition.expression.{Expression,Constant}
import scala.swing.Table
import util.MyListView
import client.dataviewer.InstanceRenderer
import java.awt.Color
import util.MyComboBox
import javax.swing.JComboBox


class HatchInplaceEditor() extends InplaceFieldEditor{
  
  val styleCombo=new MyComboBox(HatchHandler.hatchList)
  val previewPrototype=new HatchLabelPreview
  
  val editor = new DefaultCellEditor(styleCombo.peer) {  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ) = {  		
  		styleCombo.selection.index=value match {
  		  case c:Constant => if(c.toInt>=HatchHandler.hatchList.size)-1 else c.toInt
  		  case _ => -1
  		}
  		styleCombo.peer
  	}
  	override def getCellEditorValue():java.lang.Object =
  	{
  		styleCombo.peer.getSelectedItem() match {
  		  case ls:HatchStyle=>ls.ix.asInstanceOf[AnyRef]
  		  case _=> 0.asInstanceOf[AnyRef]
  		}
  	}
  }  
  
  def getEditor=editor  
  
  def createRenderer=new Table.AbstractRenderer[Expression,HatchLabelPreview](new HatchLabelPreview){
    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: Expression, row: Int, column: Int): Unit = {
      val ix=if(a==null) -1 else a.asInstanceOf[Constant].toInt
      val hatch=HatchHandler.quickGetHatch(ix)
      component.hatchPreview.whiteBackground=true
      component.background=if(isSelected)  table.selectionBackground 
  	  else  if (row % 2 == 0)InstanceRenderer.alternateColor 
  	  			else Color.white
      component.setValue(hatch.ix,hatch.name)      
    }
  }  
  	
	styleCombo.renderer=new MyListView.AbstractRenderer[HatchStyle,HatchLabelPreview](previewPrototype){
		def configure(list: MyListView[HatchStyle], isSelected: Boolean, focused: Boolean, a: HatchStyle, index: Int): Unit = {
		  val hatch=if(a==null) HatchHandler.undefinedHatch else a
			component.setValue(hatch.ix,hatch.name)
		}		
	} 
  
}