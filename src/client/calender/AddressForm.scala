package client.calender

import client.calender.CalendarHelper._
import client.comm.ClientQueryManager
import definition.data.{OwnerReference, Reference}
import definition.expression.StringConstant
import javafx.scene.control.{Label, TextArea, TextField, TextInputControl}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{HBox, Priority, VBox}


trait ActiveInputControl {
  self:TextInputControl=>
  def doneCallBack:(String)=>Unit
  var oldText:String=""      
    onChanged[java.lang.Boolean](focusedProperty(),(o,n)=>if(n==false)editDone())    
    
    def _setText(text:String)={
      self.setText(text)
      oldText=text
    }
    
    def editDone()= if(getText!=oldText) {
      oldText=getText
      doneCallBack(getText)    
    }
}

class ActiveField(val doneCallBack:(String)=>Unit) extends TextField with ActiveInputControl {  	
    setOnAction(handleEvent(e=>editDone()))        
  } 

trait Indexed{
  def ix:Int
}

class IndexedActiveField(val ix:Int,dcb:(String)=>Unit) extends ActiveField(dcb) with Indexed

class ActiveTextArea(val doneCallBack:(String)=>Unit) extends TextArea with ActiveInputControl {
   setWrapText(true)
   setOnKeyTyped(handleEvent(e=>{
     e.getCode() match {
       case KeyCode.ENTER if e.isControlDown() => editDone();e.consume()
       case KeyCode.F2 =>editDone();e.consume()
       case _=>
     }
   }))   
}

class IndexedActiveArea(val ix:Int,dcb:(String)=>Unit) extends ActiveTextArea(dcb) with Indexed



class LabeledActiveField(name:String,lWidth:Double,doneCallBack:(String)=>Unit) extends HBox {    
    val label=new Label(name)    
    label.getStyleClass().add("adress-form-label")
    val textField=new ActiveField(doneCallBack)
    textField.getStyleClass().add("adress-form-textField")
    if(lWidth> -1) label.setPrefWidth(lWidth)   
    HBox.setHgrow(textField, Priority.SOMETIMES)    
    getChildren.addAll(label,textField)    
    def setText(text:String)= textField._setText(text)       
  }  


class AddressForm(mod:CalendarModel) extends VBox {
  import client.calender.AddressForm._
  
  var currentAddress:Option[AdTreeNode]=None
  val fields=Seq(new FormActiveField("Vorname:",0),new FormActiveField("Name:",1),new FormActiveField("Straße:",2),new FormActiveField("PLZ:",3),new FormActiveField("Ort:",4),
      new FormActiveField("Telefon:",5),new FormActiveField("Fax:",6),new FormActiveField("Email:",7),new FormActiveField("z.Hd.:",8))
  
  getChildren.addAll(fields:_*)  
  
  def handleKey(e:KeyEvent,func:(Int)=>Unit):Unit= e.getTarget() match{
        case f:ActiveField=>f.getParent match {
          case fo:FormActiveField=> func(fo.dbFieldNr)          
          case o => 
        } 
        case o => 
      } 
    
  
  addEventFilter(KeyEvent.KEY_PRESSED,
  handleEvent[KeyEvent](e => e.getCode() match {
    case KeyCode.ENTER|KeyCode.DOWN=> handleKey(e,(fieldNr)=>fields(if(fieldNr>7)0 else fieldNr+1).textField.requestFocus() )
    case KeyCode.UP=> handleKey(e,(fieldNr)=>fields(if(fieldNr<1)8 else fieldNr-1).textField.requestFocus() )      
    case o => 
  }))
  
  
  def loadAdress(addressNode:AdTreeNode): Unit = {
    currentAddress=Option(addressNode)
    addressNode match {      
      case ad:Address=>
        fields.head.setText(ad.prename)
        fields(1).setText(ad.name)
        fields(2).setText(ad.street)
        fields(3).setText(ad.zip)
        fields(4).setText(ad.city)
        fields(5).setText(ad.phone)
        fields(6).setText(ad.fax)
        fields(7).setText(ad.email)
        fields(8).setText(ad.pers)
      case o=>fields.foreach(_.setText(""))
    }   
  }
  class FormActiveField(nname:String,val dbFieldNr:Int) extends LabeledActiveField(nname,labelWidth,(text)=>{
    currentAddress match {      
      case Some(ad:Address)=> ClientQueryManager.writeInstanceField(ad.ref, dbFieldNr.toByte, new StringConstant(text))    
     
      case Some(fo:Folder)=>
        // create new Address
        val inst=ClientQueryManager.createInstance(mod.addressType, Array(new OwnerReference(1,fo.ref)))
        ClientQueryManager.writeInstanceField(Reference(mod.addressType,inst),dbFieldNr.toByte,new StringConstant(text))
      case _=> // do nothing
    }
  })
      
}

object AddressForm {
  val labelWidth=90d
}



