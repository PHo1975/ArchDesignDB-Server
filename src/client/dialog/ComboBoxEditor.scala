/**
 * Author: Peter Started:18.12.2010
 */
package client.dialog
import javax.swing._

/**
 * 
 */
class ComboBoxEditor(box:JComboBox[_]) extends DefaultCellEditor(box) {  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ): JComboBox[_] = {
  		val editor:JComboBox[_] =super.getTableCellEditorComponent( table, value, isSelected, row, column ).asInstanceOf[JComboBox[_]]
      editor.setSelectedItem( value )
  		editor
  	}
  	override def getCellEditorValue:java.lang.Object =
  	{
  		getComponent.asInstanceOf[JComboBox[_]].getSelectedItem
    }
  }