/**
 * Author: Peter Started:10.10.2010
 */
package client.dialog

import client.comm.ClientQueryManager
import client.ui.{ClientApp, ViewConstants}
import definition.data._
import definition.expression._
import definition.typ.{AllClasses, DataType, SelectGroup}

import java.awt.Dimension
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.table.TableCellEditor
import javax.swing.{JSpinner, SpinnerNumberModel}
import scala.swing.event.{EditDone, SelectionChanged}
import scala.swing.{Alignment, BorderPanel, ComboBox, Component, Label, ListView, Panel, Swing, Table, TextArea, TextComponent, TextField}


trait AbstractFieldEditor
/** abstract definition of a field editor
 * 
 */
trait FieldEditor extends AbstractFieldEditor {  
  var dataList:Iterable[SelectGroup[_ <:Referencable]]=Seq.empty
  var inited=false
  var allowedClassIds:Map[String, Int]=Map.empty

  def fieldComponents: Seq[SidePanelComponent[_]]
  
  def allowedClassNames:Iterable[String]  
      
	def getPanel:Panel
	
	def setData(data:Iterable[SelectGroup[_<:Referencable]]): Unit = {
    dataList=data
    if(!inited) init()
    if(data!=null) {      
      for(group <-data;el <-group.children;fc<-fieldComponents)         
          fc.checkSearchValue(el)
      for(fc<-fieldComponents)
        fc.updateSearchValue()
    }
  }

  def init(): Unit = if (!inited) {
    inited=true
    ClientQueryManager.registerSetupListener(()=>{
    val ac=AllClasses.get
	  allowedClassIds=allowedClassNames.iterator.filter(ac.exists).map(a=> a->ac.getClassIDByName(a) ).toMap
	  for(f<-fieldComponents)
	    f.createFieldMap(allowedClassIds)
  })
  }

  def getPanelPart(lname: String, partComp: Component): PanelPart = new PanelPart(lname, partComp)

  def storeValue[A](newValue: A, component: SidePanelComponent[A]): Unit = {
    //println("store Value" +newValue+" comp:"+component.getClass+" datasize:"+dataList.size+" typMap:"+component.fieldMap)
    val instList=dataList.flatMap(_.children.filter(inst=>component.fieldMap.contains(inst.ref.typ )))
	  if(instList.nonEmpty){
	    val newConstant=component.getConstant(newValue)		    
			val typedMap=instList.groupBy(_.ref.typ)		
			val keySet=typedMap.keySet
			if(keySet.map(aType=>component.fieldMap(aType)).size==1) // all types have the same field Number to change
			  ClientQueryManager.writeInstancesField(instList,component.fieldMap(instList.head.ref.typ),newConstant) //write together  
			else for(typ <-keySet) // write for distinct types separately
				ClientQueryManager.writeInstancesField(typedMap(typ),component.fieldMap(typ),newConstant)
		}    
  }

  def storeValueMapped[A](component: SidePanelComponent[A], func: (A) => A): Unit = {
    //println("Store Value mapped "+component.getClass)
    val instList=dataList.flatMap(_.children.filter(inst=>component.fieldMap.contains(inst.ref.typ )))
	  if(instList.nonEmpty)
	    for(elem<-instList){	
	    val oldValue=component.elemToValueLookup(elem)
	    val newValue=func(oldValue)
	    val newConstant=component.getConstant(newValue)			
		  ClientQueryManager.writeInstanceField(elem.ref,component.fieldMap(elem.ref.typ),newConstant)
		}   
  }
}

object FieldEditor {
  val labelSize = new Dimension(Short.MaxValue, 20 * ViewConstants.fontScale / 100)
  val comboSize = new Dimension(Short.MaxValue, 28 * ViewConstants.fontScale / 100)
  val panelSize = new Dimension(Short.MaxValue, 32 * ViewConstants.fontScale / 100)
  val panelLabelPrefSize = new Dimension(60 * ViewConstants.fontScale / 100, 20 * ViewConstants.fontScale / 100)
}

class PanelPart(lname:String,partComp:Component) extends BorderPanel {
	opaque=false
  val label: Label = getLabel(lname)
	add(label,BorderPanel.Position.West)
	add(partComp,BorderPanel.Position.Center)	  
	//maximumSize=FieldEditor.panelSize
	xLayoutAlignment=0

  def getLabel(lname: String): Label = {
    val label = new Label(lname)
    label.font = ViewConstants.labelFont
		label.preferredSize=FieldEditor.panelLabelPrefSize    
		label.horizontalAlignment=Alignment.Left
		label
	}
}



trait InplaceFieldEditor extends AbstractFieldEditor {
  def getEditor:TableCellEditor
  def createRenderer:Table.Renderer[Expression]
}


trait RenderComponent[A] extends Component{
    def setStyle(a:A):Unit
    def setEmpty():Unit
  }


class RenderedComboBox[A,C<:RenderComponent[A]](items: Seq[A],theRender:C) extends ComboBox(items) {
  xLayoutAlignment=0
  preferredSize=FieldEditor.comboSize
  	renderer=new ListView.AbstractRenderer[A,C](theRender){
  	  def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: A, index: Int): Unit = {
  		  if(a!=null) {component.setStyle(a);} else component.setEmpty()  	}
	}
}


trait SidePanelComponent[A] {
  def allowedFields:Map[String,Byte] // "ClassName" -> DBFieldNr
  var fieldMap:Map[Int,Byte]=Map.empty // ClassID -> DBFieldNr
  var currentValue:Option[A]=None
  
  var elemToValueLookup:PartialFunction[Referencable,A]={
    	case inst:InstanceData if fieldMap.contains(inst.ref.typ) =>
    		valueFromConstant(inst.fieldValue(fieldMap(inst.ref.typ)))   	
    }

  protected var searchValue: Option[A] = _

  def createFieldMap(nameMap: Map[String, Int]): Unit = fieldMap = allowedFields.keysIterator.filter(AllClasses.get.exists).
  map(elem => nameMap(elem) -> allowedFields.apply(elem).toByte).toMap
  
  def defaultValue:A
  
  def setValue(newValue:Option[A]):Unit= currentValue=newValue    
   
  def getConstant(value:A):Expression
  
  def valueFromConstant(c:Expression):A
  
  def resetSearchValue():Unit= searchValue=null

  def internSetSearchValue(newValue: A): Unit = if (searchValue == null) searchValue = Some(newValue)
    	else if(searchValue.isDefined && searchValue.get!=newValue) searchValue=None
  
  def checkSearchValue(value:Referencable):Unit= {
    if(elemToValueLookup.isDefinedAt(value)) internSetSearchValue(elemToValueLookup(value))
  }

  def addSearchLookup(newCase: PartialFunction[Referencable, A]): Unit = {
    elemToValueLookup=newCase orElse elemToValueLookup
  }
  
  def updateSearchValue():Unit = {
    if(searchValue==null) searchValue=None
    setValue(searchValue)
    resetSearchValue()
  }
  
}

trait IntSidePanelComponent extends SidePanelComponent[Int] {
   def defaultValue=0

  def getConstant(value: Int): Expression = IntConstant(value)
   def valueFromConstant(c:Expression):Int=c.getValue.toInt
}



abstract class ActiveComboBox[A,C<:RenderComponent[A]](items: Seq[A],theRender:C) extends RenderedComboBox(items,theRender)  {
  var selfSelected=false  
  listenTo(this.selection)  
  reactions+= {
    case s:SelectionChanged=>
      if(selfSelected) selfSelected=false
			else if(selection.index> -1){
			  elemClicked(selection.item)
			}
  }
  def elemClicked(item:A): Unit
}


abstract class SidePanelComboBox[A,C<:RenderComponent[A]](items: Seq[A],theRender:C,editor:FieldEditor,val allowedFields:Map[String,Byte])  
extends ActiveComboBox[A,C](items,theRender) with SidePanelComponent[A] {
  focusable=false

  def elemClicked(item: A): Unit = editor.storeValue(item, this)
}


trait ActiveTextComponent extends TextComponent{
  var dirty=false  
  
  listenTo(this)
  peer.getDocument.addDocumentListener(new DocumentListener{
    def insertUpdate(e: DocumentEvent): Unit = dirty = true

    def removeUpdate(e: DocumentEvent): Unit = dirty = true

    def changedUpdate(e: DocumentEvent): Unit = dirty = true
  })

  override def text_=(t: String): Unit = {
    super.text_=(t)
    dirty=false
  }
  
  def fieldChanged(newVal:String):Unit
  
  reactions+= {
    case e:EditDone=> if(dirty){
      dirty=false      
      fieldChanged(text.trim)
      CreateActionList.focusLastContainer()
    }  
  }  
} 

trait ActiveTextField extends TextField with ActiveTextComponent



abstract class ActiveNumberSpinner(minV:Number,maxV:Number,step:Number) extends Component{
  var selfChanged=false
  lazy val n: Comparable[Number] = minV.asInstanceOf[Comparable[Number]]
  lazy val n1: Comparable[Number] = maxV.asInstanceOf[Comparable[Number]]
  lazy val model: SpinnerNumberModel = {
    val m = new javax.swing.SpinnerNumberModel(minV, n, n1, step)
    m.addChangeListener { _ =>
      if (!selfChanged) {
        m.getValue match {
          case n: Number => fieldChanged(n)
          case _ =>
        }
      }
    }
    m
  }
  
  override lazy val peer: JSpinner = {
    val p= new JSpinner(model) with SuperMixin
    val ed=p.getEditor().asInstanceOf[JSpinner.NumberEditor].getTextField
    ed.addFocusListener(new java.awt.event.FocusAdapter {
      override def focusGained(e: java.awt.event.FocusEvent): Unit = {
        //println("focus"+model.undefined+" "+model.getValue+" "+p.getEditor().asInstanceOf[JSpinner.NumberEditor].getTextField.getText.size)
        if(ed.getText.length==0) p.setValue(Integer.valueOf(0))
      }
    })
    p
  }
    
  
  def fieldChanged(newVal:Number):Unit

  def setValue(value: Number): Unit = {
    selfChanged=true    
    Swing.onEDT({
      peer.setValue(maxV)
      peer.setValue(value)
      selfChanged=false
     })        
  }

  def setToUndefined(): Unit = {
    selfChanged=true
    //model.setToUndefined()
    peer.setValue(minV)
    peer.getEditor.asInstanceOf[JSpinner.NumberEditor].getTextField.setText("")
    //println("Set to undefined ")          
    selfChanged=false
  }
} 


trait SidePanelTextComponent extends  ActiveTextComponent with SidePanelComponent[String] {
  val defaultValue=""
  def editor:FieldEditor

  def getConstant(value: String): Expression = StringParser.parse(value,DataType.StringTyp) match {
    case e:Expression=>e
    case _=>StringConstant(value)
  }

  def valueFromConstant(c: Expression): String = c.getValue.toString
  def filter(text:String)=true

  override def setValue(newValue: Option[String]): Unit = {
  	super.setValue(newValue)
  	newValue match {
  		case Some(sVal)=> text=sVal
  		case _ => text=defaultValue
  	}
  }

  def fieldChanged(newVal: String): Unit = if (filter(newVal)) {
    editor.storeValue(newVal,this)
    CreateActionList.focusLastContainer()
  }
}




class SidePanelTextField(val allowedFields:Map[String,Byte],val editor:FieldEditor) extends TextField with SidePanelTextComponent
class SidePanelTextArea(val allowedFields:Map[String,Byte],val editor:FieldEditor) extends TextArea with SidePanelTextComponent{
  wordWrap=true
  lineWrap=true

  def pasteLineBreak(): Unit = peer.insert("\n", peer.getCaretPosition)
  
  protected override def onFirstSubscribe(): Unit = {
    super.onFirstSubscribe()
	  peer.addKeyListener(new java.awt.event.KeyAdapter{
      override def keyPressed(e: java.awt.event.KeyEvent): Unit = {
	      e.getKeyCode match {
	        case java.awt.event.KeyEvent.VK_ENTER=> if(e.isControlDown){
            pasteLineBreak()
          }
          else {
	          e.consume()
	          publish(EditDone(null))
	        }
	        case _=>
	      }
	    }
	  })
	  peer.addFocusListener(new java.awt.event.FocusAdapter{
      override def focusLost(e: java.awt.event.FocusEvent): Unit = {
	      publish(EditDone(null))
	    }
	  })
  }
}

trait SidePanelDoubleComponent extends  ActiveTextComponent with SidePanelComponent[Double] {
  val defaultValue=0d
  def editor:FieldEditor
  def getConstant(value:Double):Expression=new DoubleConstant(value)

  def valueFromConstant(c: Expression): Double = c.getValue.toDouble
  def filter(value:Double)=true
  def precision:Int

  val formatPattern: String = "%3." + precision + "f"

  override def setValue(newValue: Option[Double]): Unit = {
    super.setValue(newValue)
    newValue match {
      case Some(dVal)=> text=formatPattern.format(dVal)
      case _ => text=""
    }
  }

  def fieldChanged(newVal: String): Unit = {
    //println("Field changed "+text+" newVal:"+newVal)
    //println(Thread.currentThread.getStackTrace.drop(1).take(15).mkString("\n "))
    StringParser.parse(text, DataType.DoubleTyp) match {
      case ParserError(message,pos)=>
        caret.position=pos
        new Toast(message,this.peer,ClientApp.top).visible=true
      case exp:Expression=>
        editor.storeValue(exp.getValue.toDouble,this)
        CreateActionList.focusLastContainer()
    }  
  }   
}

trait SidePanelExpressionComponent extends ActiveTextComponent with SidePanelComponent[Expression] {
  val defaultValue=EMPTY_EX
  val editor:FieldEditor
  def getConstant(value:Expression)=value
  def valueFromConstant(c:Expression)=c

  override def setValue(newValue: Option[Expression]): Unit = {
    super.setValue(newValue)
    newValue match {
      case Some(dVal)=> text=dVal.getTerm
      case _ => text=""
    }
  }

  def fieldChanged(newVal: String): Unit =
    StringParser.parse(text) match {
      case ParserError(message, pos) =>
        caret.position = pos
        new Toast(message, this.peer, ClientApp.top).visible = true
      case exp: Expression =>
        editor.storeValue(exp, this)
        CreateActionList.focusLastContainer()
    }
}

class SidePanelExpressionTextField(val allowedFields:Map[String,Byte],val editor:FieldEditor,val precision:Int=1) extends
  TextField with SidePanelExpressionComponent




class SidePanelDoubleTextField(val allowedFields:Map[String,Byte],val editor:FieldEditor,val precision:Int=1) extends 
  TextField with SidePanelDoubleComponent


class MultiLineLabel extends Label() {
  opaque=true
  background = ViewConstants.leftPanelColor

  override def text_=(st: String): Unit = {
    super.text_=("<HTML><Center>"+st+"</html>")
    maximumSize = new Dimension(ViewConstants.sidePanelWidth - 30, preferredSize.height)
    tooltip = text
  }
  font=ViewConstants.questionFont
  //preferredSize=new Dimension(DialogManager.sidePanelWidth-30,40)
  maximumSize = new Dimension(ViewConstants.sidePanelWidth - 30, Short.MaxValue)

  def puretext: String =text.substring(14,text.length-7).split("<br>").head
}


class SidePanelLabel(val allowedFields:Map[String,Byte],val editor:FieldEditor) extends MultiLineLabel with SidePanelComponent[String] {
  val defaultValue=""

  def getConstant(value: String): Expression = StringConstant(value)

  def valueFromConstant(c: Expression): String = c.getValue.toString

  override def setValue(newValue: Option[String]): Unit = {
  	super.setValue(newValue)
  	newValue match {
  		case Some(sVal: String)=> text=sVal
  		case _ => text=defaultValue
  	}
  }   
       
}


 