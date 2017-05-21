package client.graphicsView


import java.awt.{Color, Dimension}
import javax.swing.{DefaultCellEditor, JComboBox, JTable}

import client.dataviewer.{InstanceRenderer, ViewConstants}
import client.dialog.InplaceFieldEditor
import definition.expression.{Constant, Expression}
import util.{MyComboBox, MyListView}

import scala.swing.{BorderPanel, Label, Table}


class MaterialInplaceEditor() extends InplaceFieldEditor{
  
  
  val styleCombo=new MyComboBox(MaterialHandler.materialList)

  val editor: DefaultCellEditor = new DefaultCellEditor(styleCombo.peer) {
    override def getTableCellEditorComponent(table: JTable, value: Object, isSelected: Boolean, row: Int, column: Int): JComboBox[MaterialDef] = {
  		styleCombo.selection.index=value match {
  		  case c:Constant => MaterialHandler.materialList.indexWhere(_.ix==c.toInt) 		    
  		  case _ => -1
  		}
  		styleCombo.peer
  	}

    override def getCellEditorValue(): java.lang.Object = {
  		styleCombo.peer.getSelectedItem() match {
  		  case ls:MaterialDef=>ls.ix.asInstanceOf[AnyRef]
  		  case _=> 0.asInstanceOf[AnyRef]
  		}
  	}
  }
  
  val previewPrototype=new HatchLabelPreview


  def getEditor: DefaultCellEditor = editor
  
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
  val label: Label = ViewConstants.label()
   var currentMaterial:MaterialDef=MaterialHandler.undefinedMaterial
   preferredSize=new Dimension(100,33)
   add(hatchPreview,BorderPanel.Position.West)
   add(label,BorderPanel.Position.Center)

  def setValue(nhatch: Int, ltext: String): Unit = {
     hatchPreview.setHatch(HatchHandler.quickGetHatch(nhatch),true,true)
     label.text=" "+ltext     
     label.background=background
     label.foreground=foreground
     peer.setToolTipText(ltext)
   }
}