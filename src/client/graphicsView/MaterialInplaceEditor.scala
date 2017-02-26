package client.graphicsView


import scala.swing.ComboBox
import javax.swing.DefaultCellEditor
import javax.swing.JTable
import definition.expression.{Expression,Constant}
import scala.swing.Table
import util.MyListView
import scala.swing.BorderPanel
import scala.swing.Label
import java.awt.Dimension
import client.dataviewer.InstanceRenderer
import java.awt.Color
import client.dialog.InplaceFieldEditor
import util.MyComboBox


class MaterialInplaceEditor() extends InplaceFieldEditor{
  
  
  val styleCombo=new MyComboBox(MaterialHandler.materialList)
  
  val editor = new DefaultCellEditor(styleCombo.peer) {  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ) = {  		
  		styleCombo.selection.index=value match {
  		  case c:Constant => MaterialHandler.materialList.indexWhere(_.ix==c.toInt) 		    
  		  case _ => -1
  		}
  		styleCombo.peer
  	}
  	override def getCellEditorValue():java.lang.Object =
  	{
  		styleCombo.peer.getSelectedItem() match {
  		  case ls:MaterialDef=>ls.ix.asInstanceOf[AnyRef]
  		  case _=> 0.asInstanceOf[AnyRef]
  		}
  	}
  }
  
  val previewPrototype=new HatchLabelPreview
  
  
  def getEditor=editor  
  
  def createRenderer=new Table.AbstractRenderer[Expression,HatchLabelPreview](new HatchLabelPreview){
    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: Expression, row: Int, column: Int): Unit = {
      val ix=if(a==null)-1 else a.asInstanceOf[Constant].toInt
      component.hatchPreview.whiteBackground=true
      component.background=if(isSelected)  table.selectionBackground 
  	  else  if (row % 2 == 0)InstanceRenderer.alternateColor 
  	  			else Color.white
      val mat=if(MaterialHandler.materialMap.contains(ix)) MaterialHandler.materialMap(ix) else MaterialHandler.undefinedMaterial
      component.setValue(mat.hatch,mat.name)      
    }
  }
  
  	
	styleCombo.renderer=new MyListView.AbstractRenderer[MaterialDef,HatchLabelPreview](previewPrototype){
		def configure(list: MyListView[MaterialDef], isSelected: Boolean, focused: Boolean, a: MaterialDef, index: Int): Unit = {
		  val mat=if(a==null)MaterialHandler.undefinedMaterial else a
			component.setValue(mat.hatch,mat.name)
		}		
	} 
  
}

class HatchLabelPreview extends BorderPanel{
   val hatchPreview=new HatchPreview
   val label=new Label
   var currentMaterial:MaterialDef=MaterialHandler.undefinedMaterial
   preferredSize=new Dimension(100,33)
   add(hatchPreview,BorderPanel.Position.West)
   add(label,BorderPanel.Position.Center)   
   
   def setValue(nhatch:Int,ltext:String) = {
     hatchPreview.setHatch(HatchHandler.quickGetHatch(nhatch),true,true)
     label.text=" "+ltext     
     label.background=background
     label.foreground=foreground
     peer.setToolTipText(ltext)
   }
}