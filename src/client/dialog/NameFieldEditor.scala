package client.dialog

import java.awt.Dimension
import scala.swing.BoxPanel
import scala.swing.Dialog
import scala.swing.Orientation
import definition.data.Named
import scala.swing.ScrollPane

class NameFieldEditor extends FieldEditor { 
  
  val textField:SidePanelTextArea=new SidePanelTextArea(Map(("Teilvolumen",0),("Plane",0),("RoomBox",1),("AreaPolygon",9)),this) {
    //println("name field ")
  	override	def fieldChanged(newVal:String)= {
  		if(dataList.size==1||( currentValue.isDefined || Dialog.showConfirmation(getPanel,"Unterschiedliche Objekte gleichzeitig umbenennen ?",
  				"Mehrere Objekte markiert",Dialog.Options.OkCancel,Dialog.Message.Question,null)==Dialog.Result.Ok ))        
  			storeValue(newVal,this)
  	}    
    addSearchLookup({  case n:Named=>n.name })
  }  
  
  val fieldComponents=Seq(textField)
  val allowedClassNames=textField.allowedFields.map(_._1)
  
  val panel=new BoxPanel(Orientation.Vertical) {
	  opaque=false
    val textScroller = new ScrollPane{
      viewportView=textField 
      maximumSize=new Dimension(Short.MaxValue,80)
      minimumSize=new Dimension(50,30)
    }
	  val pp=new PanelPart("Name:",textScroller)
	  contents+= 	pp	  
	  pp.maximumSize=new Dimension(Short.MaxValue,80)
	  //textField.preferredSize=new Dimension(40,55)
	  xLayoutAlignment=0d
	}
  
  def getPanel=panel
  
   
    
  
  
}