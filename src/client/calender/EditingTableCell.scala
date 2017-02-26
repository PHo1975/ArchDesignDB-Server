package client.calender

import javafx.scene.Parent
import javafx.scene.control.TableView.TableViewSelectionModel
import javafx.scene.control._
import javafx.scene.input.{KeyCode, KeyEvent}

class EditingTableView[T] extends TableView[T]{
  var isEditing= false
  import client.calender.CalendarHelper._
  addEventFilter[KeyEvent](KeyEvent.KEY_PRESSED,handleEvent(e=>{     
      e.getCode() match {
        case KeyCode.TAB=>          
          if(!isEditing) {
            e.consume()
            if(e.isShiftDown()) getSelectionModel().selectLeftCell()
            else getSelectionModel().selectRightCell()
          }
         	if(!isFocused()) requestFocus()
        case _=>
      }
    }))     
} 

trait CellInfoReceiver[T] {
  var mouseOverColumn:TableColumn[_,T]=_
  var mouseOverRow:Int=0
  var currentItem:T=_
  var startEditChar:Option[String]=None
}


abstract class EditingTableCell[R, D](receiver:CellInfoReceiver[D]) extends TableCell[R,D] {
  import client.calender.CalendarHelper._
	var textField:TextInputControl = _
	var mouseOver=false 
	var oldView:Parent=_
	
	EditingTableCell.this.setOnDragEntered(handleEvent(e=> {receiver.currentItem=getItem()
	  receiver.mouseOverColumn=getTableColumn()
	  receiver.mouseOverRow=getIndex()
	}))

	def convertToEditString(data:D):String	
	def createItem(st:String):D
	def createView(data:D,oldView:Parent):Parent
	def isEditable(data:D):Boolean
	
	def commit():Unit= if(textField!=null) {
	 setTableEditing(false) 
   if(textField.getText()!=null) {
     if(getItem!=null && convertToEditString(getItem)== textField.getText()) {        
       cancelEdit()
       return
     }     
     commitEdit( createItem(textField.getText()))      
   }
	 else cancelEdit()
	}
  
  
  def setTableEditing(value:Boolean): Unit =getTableView match {
	   case editTable:EditingTableView[R] =>editTable.isEditing=value;
	   case _=>
	}	  
 
	
	def createTextField: TextArea ={
	  val ret=new TextArea("")
	  ret.setPrefRowCount(1)	  
	  ret.setPrefHeight(12)	  
	  //println("tx:"+getString)  
	  
	  //ret.setFocusTraversable(false)
	  //ret.setAlignment(Pos.BOTTOM_CENTER)	  
  
    ret.setOnMouseEntered(handleEvent(e=>mouseOver=true))
    ret.setOnMouseExited(handleEvent(e=>mouseOver=false))
    onChanged[java.lang.Boolean](ret.focusedProperty(),(o,n)=> {      
      if(!n &&isEditing && !mouseOver)commit()
      })
    ret.addEventFilter[KeyEvent](KeyEvent.KEY_PRESSED,handleEvent(e=>{      
      e.getCode match {
        case KeyCode.TAB=> commit();selectModel.selectRightCell()
        case KeyCode.ENTER|KeyCode.F2=>e.consume();commit();
        case KeyCode.ESCAPE=>
          e.consume()
          cancelEdit()
        case KeyCode.UP=>e.consume();commit();selectModel.selectAboveCell()
        case KeyCode.DOWN=>e.consume();commit();selectModel.selectBelowCell()
        case _ =>
      }
    }))    
    
    ret
  }
	
	@inline def selectModel: TableViewSelectionModel[R] = getTableView().getSelectionModel()
	
		
	def getString: String = if(getItem() == null) ""  else convertToEditString(getItem())
	
	override def startEdit():Unit= {	
	  if(getItem()!=null&& !isEditable(getItem())) return
	  super.startEdit()	  
	  if (textField == null) {
        textField = createTextField
    } else println(Thread.currentThread.getStackTrace.drop(2).mkString("\n  "))
    textField.setText(getString)    
    setText(null)
    setGraphic(textField)    
    //textField.selectAll()
    setTableEditing(true)
    runInFx{
      textField.requestFocus()      
      runInFx{
        val text=textField.getText()
        //println("Text:"+text+" "+textField.getLength()+" "+textField.isFocused())
        textField.end()
      }
    }    
	}
	
	
	override def cancelEdit(): Unit = {
	  setTableEditing(false)
    super.cancelEdit()
	  setText(null)
    intSetView(getItem)
    //setGraphic(createView(getItem));    
    textField=null	     
	}
	
	def intSetView(item:D): Unit ={
	  val ret=createView(item,oldView)
	  oldView=ret
	  setGraphic(ret)
	  if(ret!=null) {
	    ret.setFocusTraversable(false)
	    //ret.requestLayout()
	  }
	}
	
	
	override def updateItem(item:D,empty:Boolean): Unit = {
	  super.updateItem(item,false)
	  if(empty){
	    setText(null)
      setGraphic(null)
    }	//else println("UpdateItem "+item+" empty:"+empty+" edit:"+isEditing)
	  if (isEditing()) {
	      if (textField != null) {
	          textField.setText(getString)
	      }
	      setText(null)
      setGraphic(textField)
	  } else {
	      setText(null)
	      intSetView(item)
	      
	  }   
	}	
}