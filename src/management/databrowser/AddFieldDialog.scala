package management.databrowser

import java.awt.Dimension

import client.dataviewer.ViewConstants
import definition.typ.{DTWrap, DataType}
import server.storage.ServerObjectClass

import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, ComboBox, Dialog, Label, Orientation, Window}

class AddFieldDialog (w:Window) extends Dialog(w) {
  val fieldName= new FormatLine(100,"Field Name:",()=>"",(text)=> {})
  fieldName.xLayoutAlignment=0
  var typCombo=new ComboBox[DTWrap](Seq.empty)
  var selectedType:Option[DataType.Value]=None
  var currentClass:Option[ServerObjectClass]=None
  val typeLabel: Label = ViewConstants.label("DataType:")
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


  def showDialog(aClass: ServerObjectClass, types: Seq[DTWrap]): Unit = {
		currentClass match {
		  case Some(_)=>
		  case None => //typCombo.peer.setModel(ComboBox.newConstantModel(types))		       
		}		
		currentClass=Some(aClass)
		visible=true	
 }
}