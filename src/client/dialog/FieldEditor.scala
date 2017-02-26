/**
 * Author: Peter Started:10.10.2010
 */
package client.dialog

import java.awt.Dimension
import javax.swing.JSpinner
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.table.TableCellEditor

import client.comm.ClientQueryManager
import client.dataviewer.ViewConstants
import client.ui.ClientApp
import definition.data._
import definition.expression.{Constant, DoubleConstant, Expression, IntConstant, ParserError, StringConstant, StringParser}
import definition.typ.{AllClasses, DataType, SelectGroup}

import scala.swing.event.{EditDone, SelectionChanged}
import scala.swing.{Alignment, BorderPanel, ComboBox, Component, Label, ListView, Panel, Swing, Table, TextArea, TextComponent, TextField}


trait AbstractFieldEditor
/** abstract definition of a field editor
 * 
 */
trait FieldEditor extends AbstractFieldEditor {  
  var dataList:Iterable[SelectGroup[_ <:Referencable]]=Seq.empty
  var inited=false
  var allowedClassIds:Map[String,Int]=Map.empty
  val fieldComponents:Seq[SidePanelComponent[_]]  
  
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
  
  def init()= if(!inited){
    inited=true
    ClientQueryManager.registerSetupListener(()=>{    
	  allowedClassIds=allowedClassNames.map(a => a -> AllClasses.get.getClassIDByName(a) ).toMap
	  for(f<-fieldComponents)
	    f.createFieldMap(allowedClassIds)
  })
  }
	
  def getPanelPart(lname:String,partComp:Component)=new PanelPart(lname,partComp)
  
  def storeValue[A](newValue:A,component:SidePanelComponent[A]) = {
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
  
  def storeValueMapped[A](component:SidePanelComponent[A],func:(A)=>A)= {
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
  val labelSize=new Dimension(Short.MaxValue,20)
  val comboSize=new Dimension(Short.MaxValue,28)
  val panelSize=new Dimension(Short.MaxValue,32)
  val panelLabelPrefSize=new Dimension(60,20)
}

class PanelPart(lname:String,partComp:Component) extends BorderPanel {
	opaque=false
  val label=getLabel(lname)
	add(label,BorderPanel.Position.West)
	add(partComp,BorderPanel.Position.Center)	  
	maximumSize=FieldEditor.panelSize
	xLayoutAlignment=0

	def getLabel(lname:String)= {
		val label=new Label(lname)    
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
  
  protected var searchValue:Option[A]=null
  
  def createFieldMap(nameMap:Map[String,Int])= fieldMap= allowedFields map (elem => nameMap(elem._1) -> elem._2.toByte)
  
  def defaultValue:A
  
  def setValue(newValue:Option[A]):Unit= currentValue=newValue    
   
  def getConstant(value:A):Constant
  
  def valueFromConstant(c:Constant):A
  
  def resetSearchValue():Unit= searchValue=null
  
  def internSetSearchValue(newValue:A)=  if(searchValue==null) searchValue= Some(newValue) 
    	else if(searchValue.isDefined && searchValue.get!=newValue) searchValue=None
  
  def checkSearchValue(value:Referencable):Unit= {
    if(elemToValueLookup.isDefinedAt(value)) internSetSearchValue(elemToValueLookup(value))
  }	
  
  def addSearchLookup(newCase:PartialFunction[Referencable,A])= {
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
   def getConstant(value:Int):Constant=new IntConstant(value)  
   def valueFromConstant(c:Constant):Int=c.toInt
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
  def elemClicked(item:A)= editor.storeValue(item,this)  
}


trait ActiveTextComponent extends TextComponent{
  var dirty=false  
  
  listenTo(this)
  peer.getDocument.addDocumentListener(new DocumentListener{
    def insertUpdate(e:DocumentEvent)=dirty=true    
    def removeUpdate(e:DocumentEvent)=dirty=true
    def changedUpdate(e:DocumentEvent)=dirty=true
  })
  
  override def text_=(t: String) ={
    super.text_=(t)
    dirty=false
  }
  
  def fieldChanged(newVal:String):Unit
  
  reactions+= {
    case e:EditDone=> if(dirty){
      dirty=false      
      fieldChanged(text.trim)
      NewButtonsList.focusLastContainer()
    }  
  }  
} 

trait ActiveTextField extends TextField with ActiveTextComponent



abstract class ActiveNumberSpinner(minV:Number,maxV:Number,step:Number) extends Component{
  var selfChanged=false
  lazy val n= minV.asInstanceOf[Comparable[Number]]
  lazy val n1= maxV.asInstanceOf[Comparable[Number]]
  lazy val model={ val m=new javax.swing.SpinnerNumberModel(minV,n,n1,step)
    	m.addChangeListener(new javax.swing.event.ChangeListener() {
    		def stateChanged(e:javax.swing.event.ChangeEvent)= if (!selfChanged){
    			m.getValue match {
    			  case n:Number => fieldChanged(n )    			  
    			  case _=>
    			}    			    			 
    		}    
   	}	)  
    m
  }
  
  override lazy val peer: JSpinner = {
    val p= new JSpinner(model) with SuperMixin
    val ed=p.getEditor().asInstanceOf[JSpinner.NumberEditor].getTextField
    ed.addFocusListener(new java.awt.event.FocusAdapter {
      override def focusGained(e:java.awt.event.FocusEvent)= {
        //println("focus"+model.undefined+" "+model.getValue+" "+p.getEditor().asInstanceOf[JSpinner.NumberEditor].getTextField.getText.size)
        if(ed.getText.length==0) p.setValue(new java.lang.Integer(0))
      }
    })
    p
  }
    
  
  def fieldChanged(newVal:Number):Unit  
  
  def setValue(value:Number)= {
    selfChanged=true    
    Swing.onEDT({
      peer.setValue(maxV)
      peer.setValue(value)
      selfChanged=false
     })        
  }
  
  def setToUndefined()={
    selfChanged=true
    //model.setToUndefined()
    peer.setValue(minV)
    peer.getEditor().asInstanceOf[JSpinner.NumberEditor].getTextField.setText("")   
    //println("Set to undefined ")          
    selfChanged=false
  }
} 


trait SidePanelTextComponent extends  ActiveTextComponent with SidePanelComponent[String] {
  val defaultValue=""
  def editor:FieldEditor
  def getConstant(value:String):Constant=new StringConstant(value)  
	def valueFromConstant(c:Constant)=c.toString
  def filter(text:String)=true
	
	override def setValue(newValue:Option[String])= {	
  	super.setValue(newValue)
  	newValue match {
  		case Some(sVal)=> text=sVal
  		case _ => text=defaultValue
  	}
  } 
  
  def fieldChanged(newVal:String)=  if(filter(newVal)){    
    editor.storeValue(newVal,this)
    NewButtonsList.focusLastContainer()
  }
}




class SidePanelTextField(val allowedFields:Map[String,Byte],val editor:FieldEditor) extends TextField with SidePanelTextComponent
class SidePanelTextArea(val allowedFields:Map[String,Byte],val editor:FieldEditor) extends TextArea with SidePanelTextComponent{
  wordWrap=true
  lineWrap=true
  
  def pasteLineBreak()=peer.insert("\n", peer.getCaretPosition)
  
  protected override def onFirstSubscribe(): Unit = {
    super.onFirstSubscribe()
	  peer.addKeyListener(new java.awt.event.KeyAdapter{
	    override def keyPressed(e:java.awt.event.KeyEvent)= {
	      e.getKeyCode() match {
	        case java.awt.event.KeyEvent.VK_ENTER=> if(e.isControlDown()){
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
	    override def focusLost(e:java.awt.event.FocusEvent)= {
	      publish(EditDone(null))
	    }
	  })
  }
}

trait SidePanelDoubleComponent extends  ActiveTextComponent with SidePanelComponent[Double] {
  val defaultValue=0d
  def editor:FieldEditor
  def getConstant(value:Double):Constant=new DoubleConstant(value)  
  def valueFromConstant(c:Constant)=c.toDouble
  def filter(value:Double)=true
  def precision:Int
  val formatPattern="%3."+precision+"f"
  
  override def setValue(newValue:Option[Double])= { 
    super.setValue(newValue)
    newValue match {
      case Some(dVal)=> text=formatPattern.format(dVal)
      case _ => text=""
    }
  } 
  
  def fieldChanged(newVal:String)={
    //println("Field changed "+text+" newVal:"+newVal)
    //println(Thread.currentThread.getStackTrace.drop(1).take(15).mkString("\n "))
    StringParser.parse(text, DataType.DoubleTyp) match {
      case ParserError(message,pos)=>
        caret.position=pos
        new Toast(message,this.peer,ClientApp.top).visible=true
      case exp:Expression=>
        editor.storeValue(exp.getValue.toDouble,this)
        NewButtonsList.focusLastContainer()
    }  
  }   
}

class SidePanelDoubleTextField(val allowedFields:Map[String,Byte],val editor:FieldEditor,val precision:Int=1) extends 
  TextField with SidePanelDoubleComponent


class MultiLineLabel extends Label() {
  opaque=true
  background=DialogManager.leftPanelColor
  override def text_=(st:String)= {
    super.text_=("<HTML><Center>"+st+"</html>")
    maximumSize=new Dimension(DialogManager.sidePanelWidth-30,preferredSize.height)
  }
  font=ViewConstants.questionFont
  //preferredSize=new Dimension(DialogManager.sidePanelWidth-30,40)
  maximumSize=new Dimension(DialogManager.sidePanelWidth-30,Short.MaxValue)
}


class SidePanelLabel(val allowedFields:Map[String,Byte],val editor:FieldEditor) extends MultiLineLabel with SidePanelComponent[String] {
  val defaultValue=""
  def getConstant(value:String):Constant=new StringConstant(value)  
	def valueFromConstant(c:Constant)=c.toString
	
	override def setValue(newValue:Option[String])= {	
  	super.setValue(newValue)
  	newValue match {
  		case Some(sVal)=> text=sVal
  		case _ => text=defaultValue
  	}
  }   
       
}


 