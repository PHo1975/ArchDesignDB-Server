/**
 * Author: Peter Started:13.04.2011
 */
package client.dialog.form

import java.awt.Color

import definition.data.InstanceData
import definition.expression.{EMPTY_EX, Expression, ParserError}
import definition.typ.form.FormDataField
import definition.typ.{AbstractObjectClass, DataType, FieldSetting, HorAlign}
import javax.swing.event._
import javax.swing.undo.{CannotUndoException, UndoManager}
import util.Log

import scala.swing.event.{ButtonClicked, EditDone, FocusGained, FocusLost}
import scala.swing.{Alignment, BorderPanel, BoxPanel, Component, Orientation, ScrollPane, Swing, TextArea, TextComponent, TextField}
import scala.util.control.NonFatal
import scala.xml.Node




trait AbstractFormTextField extends FormDataField with FormElement{
  self : Component =>
  def editComponent:TextComponent  
  var oldText:String=_
  var shuttedDown=false	
	var expr:Expression=_
	var fieldFormat:Option[FieldSetting]=None
	var dirty:Boolean=false
	
	def context:FormCreateContext
	
	setupComponent(this)
	listenTo(editComponent)
	
	editComponent.peer.getDocument.addDocumentListener(new DocumentListener {
		def insertUpdate(e:DocumentEvent ): Unit = if(!dirty) setDirty(true)
    def removeUpdate(e:DocumentEvent ): Unit = if(!dirty) setDirty(true)
    def changedUpdate(e:DocumentEvent ): Unit ={} //System.out.println("change "+e)
	})
  
	def setDirty(newValue:Boolean): Unit = {
    //System.out.println("\nfield "+fieldNr+"set dirty "+newValue+" "+Thread.currentThread().getStackTrace().drop(1).take(3).mkString("\n"))
		dirty=newValue
		editComponent.background= if(dirty)FormElement.editColor else Color.white
	}
  
  def saveOnExit(): Unit = if(!shuttedDown&&dirty)
		for (l<-listener) 
			if(editComponent.text!=oldText){
			  l.parseValue(fieldNr, editComponent.text) match {
				  case ex:Expression =>
            l.fieldChanged(fieldNr, ex )
            editComponent.text=getRenderedText
          case err:ParserError =>
            context.showError(err.message,editComponent.peer)
            //val textField=  e.source.peer
            val toff=if(err.offset==0) 1 else err.offset
            editComponent.peer.setSelectionStart(toff-1)
            editComponent.peer.setSelectionEnd(toff)
        }
			} 
    
  
	
	reactions += {		
		case f:FocusGained => if(!shuttedDown&&expr!=null) {
			//editComponent.text= if(expr.getType==DataType.StringTyp)expr.toString else expr.getTerm
			setDirty(false)
		}		
	}
			
  def wantShutDown(): Unit = { 
  	if (dirty)for(l<-listener){
  		l.fieldChanged( fieldNr, l.parseValue(fieldNr,editComponent.text).withException)
  	}
  }

  def shutDown(): Unit = {  
  	editComponent.text=""
  	setDirty(false)
  	shuttedDown=true
  }

  
  
  def alignRight()
  
  def getRenderedText: String ={
    val value=expr.getValue
    if(value==null || value==EMPTY_EX) ""
    else value.getType match {
        case DataType.IntTyp|DataType.LongTyp|DataType.DoubleTyp|DataType.CurrencyTyp|DataType.UnitNumberTyp=>
          fieldFormat match {
            case Some(f)=> if(f.formString.length>0)
              try {
                val unitAdd=if (value.getType == DataType.UnitNumberTyp) " " + value.toUnitNumber.unitFraction.toString else ""
                f.formString.format(value.toDouble)+unitAdd
              } catch {case  NonFatal(e)=> Log.e("Formstring:"+f.formString+" "+e);value.toString }
              else value.toString
            case None=> value.toString
          }
        case DataType.VectorTyp=> value.toVector.shortToString
        case other=> value.toString
      }

  }


  
  def setDataValue(dvalue:InstanceData,nclass:AbstractObjectClass): Unit = {
    editComponent.editable= !nclass.fieldSetting(fieldNr).readOnly
  	expr=dvalue.fieldData(fieldNr)
  	fieldFormat=nclass.fieldSettingMap.get(fieldNr)  	
  	oldText=if(expr.getType==DataType.StringTyp)expr.toString
  	  else expr.getTerm
  	editComponent.text =getRenderedText
  	setDirty(false)
  }  
}


class FormTextField (val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val align:HorAlign.Value,val fieldNr:Byte,ctext:FormCreateContext) extends TextField with AbstractFormTextField {
  def context: FormCreateContext =ctext
  horizontalAlignment=FormElement.horAlignToScala(align)
  reactions += {
		case e:EditDone=> saveOnExit() 
  }
  def editComponent =this
  
  def makeCopy=new FormTextField(minWidth,maxWidth,minHeight,maxHeight,align,fieldNr,context)
  def alignRight(): Unit =horizontalAlignment=Alignment.Right
  def toXML: Node = {  
  	<TextField iw={minWidth.toString} aw={maxWidth.toString} ih={minHeight.toString} ah={maxHeight.toString} align={align.id.toString} field={fieldNr.toString}/>
  }
  override def toString(): String = "TextField F"+fieldNr
}

class FormTextArea( val minWidth:Int,val maxWidth:Int,val minHeight:Int,val maxHeight:Int, 
	val fieldNr:Byte,val showPrintBut:Boolean,ctext:FormCreateContext) extends BorderPanel with AbstractFormTextField {
  lazy val editComponent=new TextArea
  def context=ctext
  val scroller=new ScrollPane{
    viewportView=editComponent
    horizontalScrollBarPolicy=ScrollPane.BarPolicy.Never
    //preferredSize=new Dimension(Short.MaxValue,Short.MaxValue)
  }
  val undoManager = new UndoManager()
  /*val im = editComponent.peer.getInputMap(JComponent.WHEN_FOCUSED);
	val am = editComponent.peer.getActionMap();*/	
  val printBut=context.getIconableButton("Print",FormElement.textEditorGroup,"Drucken")
  val saveBut=context.getIconableButton("Save",FormElement.textEditorGroup,"änderungen speichern")
  printBut.focusable=true
  val undoBut=context.getIconableButton("Undo",FormElement.textEditorGroup,"änderungen rückgängig machen")
  val redoBut=context.getIconableButton("Redo",FormElement.textEditorGroup,"änderungen wiederherstellen")
  val copyBut=context.getIconableButton("Copy",FormElement.textEditorGroup,"Markierten Text in die Zwischenablage kopieren")
  val pasteBut=context.getIconableButton("Paste",FormElement.textEditorGroup,"Text aus der Zwischenablage an Cursorposition einfügen")
  val cutBut=context.getIconableButton("Cut",FormElement.textEditorGroup,"Markierten Text ausschneiden und in die Zwischenablage einfügen")
  val maxBut=context.getIconableToggleButton("[ ]",FormElement.textEditorGroup,"TextFeld vergrößern/verkleinern")
  val buttonBar=new BoxPanel(Orientation.Horizontal)    
  buttonBar.contents+=saveBut+=printBut+=Swing.HStrut(10)+=undoBut+=redoBut+=Swing.HStrut(10)+=copyBut+=cutBut+=pasteBut+=maxBut+=Swing.HGlue+=maxBut
  add(scroller,BorderPanel.Position.Center)
  add(buttonBar,BorderPanel.Position.North)
  editComponent.lineWrap=true
  editComponent.wordWrap=true  
  printBut.visible=showPrintBut
  def makeCopy=new FormTextArea(minWidth,maxWidth,minHeight,maxHeight,fieldNr,showPrintBut,context)
  def alignRight()={}
  def toXML: Node = {  
  	<TextArea iw={minWidth.toString} aw={maxWidth.toString} ih={minHeight.toString} ah={maxHeight.toString}
  	field={fieldNr.toString} printBut={if(showPrintBut)"1" else "0"}/>  	
  }
  def undo(): Unit =try {
    if (undoManager.canUndo) undoManager.undo()
    updateUndoButtons()
  } catch {case e:CannotUndoException=>  Log.e("undo",e)  }
  def redo(): Unit =try {
    if (undoManager.canRedo) undoManager.redo()
    updateUndoButtons()
  } catch {case e:CannotUndoException=>  Log.e("redo",e)  }
  override def toString(): String = "TextArea F"+fieldNr
  
  editComponent.peer.addCaretListener((e: CaretEvent) => {
    val textSelected = e.getDot != e.getMark
    cutBut.enabled = textSelected
    copyBut.enabled = textSelected
  })
  
  listenTo(saveBut,undoBut,redoBut,copyBut,pasteBut,maxBut,cutBut,printBut)
  reactions += {
    case f:FocusLost=> saveOnExit()
    case ButtonClicked(`saveBut`)=>saveOnExit()
    case ButtonClicked(`maxBut`)=>for(l<-listener) l.flipMaximizeWindow(maxBut.selected)
    case ButtonClicked(`printBut`)=>if(showPrintBut)for(l<-listener) l.print()
    case ButtonClicked(`undoBut`)=>undo()
    case ButtonClicked(`redoBut`)=>redo()
    case ButtonClicked(`copyBut`)=>editComponent.copy()
    case ButtonClicked(`cutBut`)=>editComponent.cut()
    case ButtonClicked(`pasteBut`)=>editComponent.paste()
  } 
	editComponent.peer.getDocument.addUndoableEditListener(new UndoableEditListener() {
	    override def undoableEditHappened(e:UndoableEditEvent): Unit ={
	      undoManager.addEdit(e.getEdit)
	      updateUndoButtons()
	    }     
	})
	
	override def setDataValue(dvalue:InstanceData,nclass:AbstractObjectClass): Unit = {
	  super.setDataValue(dvalue,nclass)
	  undoManager.discardAllEdits()
	  updateUndoButtons()
	  for(l<-listener) l.flipMaximizeWindow(maxBut.selected)
	}
	
	def updateUndoButtons(): Unit ={
	  undoBut.enabled=undoManager.canUndo
	  redoBut.enabled=undoManager.canRedo
	}
	
	override def setDirty(newValue:Boolean): Unit =  super.setDirty(newValue)
}
