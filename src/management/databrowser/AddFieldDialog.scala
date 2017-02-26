package management.databrowser

import scala.swing.Dialog
import scala.swing.Window
import server.storage.ServerObjectClass
import definition.typ.DTWrap
import scala.swing.ComboBox
import scala.swing.BoxPanel
import scala.swing.Label
import java.awt.Dimension
import scala.swing.Orientation
import scala.swing.Button
import java.awt.Color
import scala.swing.event.ButtonClicked
import definition.typ.DataType

class AddFieldDialog (w:Window) extends Dialog(w) {
  val fieldName= new FormatLine(100,"Field Name:",()=>"",(text)=> {})
  fieldName.xLayoutAlignment=0
  var typCombo=new ComboBox[DTWrap](Seq.empty)
  var selectedType:Option[DataType.Value]=None
  var currentClass:Option[ServerObjectClass]=None
  val typeLabel=new Label("DataType:")  
  typeLabel.preferredSize=new Dimension(100,0)
 
  val typeBox=new BoxPanel(Orientation.Horizontal) {
    contents+=typeLabel+=typCombo      
    xLayoutAlignment=0
  }
  val okButton=new Button("Ok")
  val cancelButton=new Button("Cancel")
  val buttonBox=new BoxPanel(Orientation.Horizontal) {
    contents+=okButton+=cancelButton
    xLayoutAlignment=0
  }
 
  val mainPanel=new BoxPanel(Orientation.Vertical) {
    contents+=fieldName+=typeBox+=buttonBox    
    preferredSize=new Dimension(300,90) 
    listenTo(okButton,cancelButton)
    reactions+= {      
      case ButtonClicked(`okButton`)=> if(fieldName.edit.text.trim.length>0){
        TypeDefPanel.addField(fieldName.edit.text.trim, typCombo.selection.item.typ)
        close
      } else println("fieldName not defined "+fieldName.edit.text)
      case ButtonClicked(`cancelButton`)=> close
    }
  }
  
  contents=mainPanel
  
 
  def showDialog(aClass:ServerObjectClass,types:Seq[DTWrap])= {	
		currentClass match {
		  case Some(_)=>
		  case None => //typCombo.peer.setModel(ComboBox.newConstantModel(types))		       
		}		
		currentClass=Some(aClass)
		visible=true	
 }
}