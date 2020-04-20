package client.dialog.form

import java.awt.{Color, Dimension}

import client.ui.ViewConstants
import definition.expression.{Expression, StringConstant}
import definition.typ.HorAlign
import definition.typ.form.{AbstractFormElement, DataChangeListener}
import javax.swing.JComponent
import util.XMLUtils._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.swing.{AbstractButton, Alignment, Component, ToggleButton}
import scala.util.control.NonFatal
import scala.xml._

/**
 * Created by Peter on 30.03.2015.
 */
trait FormCreateContext{
  def getIconableButton(commandName:String,groupName:String,ntooltipText:String):AbstractButton
  def getIconableToggleButton(commandName:String,groupName:String,ntooltipText:String):ToggleButton
  def showError(text:String,component:JComponent):Unit
}

/** Super class of all screen form elements
 *
 */
trait FormElement extends AbstractFormElement {
  def minWidth:Int
  def maxWidth:Int
  def minHeight:Int
  def maxHeight:Int
  def toXML:scala.xml.Node

  //type InitFunc=(FormElement)=>Unit

  def setupComponent(comp: Component): Unit = {
    comp.font = ViewConstants.labelFont
    if (minWidth>0&&minHeight>0) {
      comp.minimumSize= new Dimension(minWidth*ViewConstants.fontScale/100,minHeight*ViewConstants.fontScale/100)
      comp.preferredSize=comp.minimumSize
    }
    if(maxWidth!=0||maxHeight!=0)
      comp.maximumSize= new Dimension(if(maxWidth<1)Short.MaxValue else maxWidth *ViewConstants.fontScale/100,
        if(maxHeight<1)Short.MaxValue else maxHeight*ViewConstants.fontScale/100)
    if(maxHeight<0)
      comp.preferredSize=comp.maximumSize
  }


  def updateProperty(propName: String, newValue: String, inf: Option[InitFunc] = None, context: FormCreateContext): FormElement = {
    val node=this.toXML.asInstanceOf[Elem]
    val newAttribs= changeAttribute(node.attributes,propName,newValue)
    val newXML=node.copy(attributes=newAttribs)
    val ne=FormElement.readElement(newXML,context)
    for (i<-inf) {
      ne match {
        case fb:FormBox=> fb.foreach(i)
        case other => i(other)
      }
    }
    ne
  }


  def select(selCol:Color):Unit= {
    if(! FormElement.backgroundMap.contains(this.getClass) )
      FormElement.backgroundMap(this.getClass)=this.asInstanceOf[Component].background
    this.asInstanceOf[Component].background=selCol
  }

  def deselect(): Unit = {
    this.asInstanceOf[Component].background=
      if( FormElement.backgroundMap.contains(this.getClass) )
        FormElement.backgroundMap(this.getClass)
      else Color.yellow
  }

  def makeCopy: FormElement


}


object FormElement {
  val formColor=new Color(210,210,215)
  val editColor=new Color(255,255,230)
  val textEditorGroup="TextEditor"

  val backgroundMap: mutable.Map[Class[_], Color] = collection.mutable.Map[Class[_], Color]()
  val validFormElements=List("FormLabel","TextField","TextArea","FormBox","CalcField","FormButton")

  def horAlignToScala(a: HorAlign.Value): Alignment.Value = {

    a match {
      case HorAlign.Left=> Alignment.Left
      case HorAlign.Right=> Alignment.Right
      case HorAlign.Center=> Alignment.Center
      case _ => Alignment.Left
    }
  }

  object NullListener extends DataChangeListener {
    def fieldChanged(field: Byte, newValue: Expression): Unit =
      util.Log.w("Reactor not initialized  field:" +field+" "+newValue)
    def parseValue(fieldNr:Byte,text:String):Expression = {
      StringConstant(text)
    }

    def flipMaximizeWindow(max: Boolean): Unit = {}

    def print(): Unit = {}
  }
  object EmptyNode extends Elem (null,"",Null,xml.TopScope,false,null)


  def readElement(node: scala.xml.Node,context:FormCreateContext):FormElement = {
    node.label match {
      case "FormLabel" => getFormLabel(node)
      case "TextField" => getTextField(node,context)
      case "TextArea" => getTextArea(node,context)
      case "FormBox" => FormBox(node,context)
      case "CalcField" => getCalcField(node)
      case "FormButton" => getFormButton(node,context)
      case other => throw new IllegalArgumentException("Unknown Form element :"+other)
    }
  }

  def readElements(node:xml.Node,context:FormCreateContext): Seq[FormElement] = {
    node.child.filter(n=>validFormElements.contains(n.label)). map(anode=> FormElement.readElement(anode,context)).toSeq
  }

  def getElementByName(name: String, context: FormCreateContext): Component with FormElement = {
    name match {
      case "FormLabel" => new FormLabel(0,0,20,0,"Label",HorAlign.Left )
      case "TextField" => new FormTextField(80,0,30,30,HorAlign.Left,0,context)
      case "TextArea" => new FormTextArea(80,0,30,0,0,true,context)
      case "FormBox" => FormBox(EmptyNode,context)
      case "CalcField" => new FormCalcField(0,0,20,0,HorAlign.Left,StringConstant("Calc"))
      case "FormButton" => new FormButton("Button",0,0,25,0,"")
      case other => throw new IllegalArgumentException("Unknown Form element :"+other)
    }
  }

  def parseInt(value:String):Int = try {
    value.toInt
  } catch {case NonFatal(e) => 0
    case other:Throwable =>println(other);System.exit(0);0}

  def getFormLabel(node:scala.xml.Node) =  new FormLabel(readOptInt(node,"@iw"),readOptInt(node,"@aw"),
    readOptInt(node,"@ih"),readOptInt(node,"@ah"),(node \"@text").text,
    HorAlign(FormElement.parseInt((node \"@align").text)))
  def getFormButton(node:scala.xml.Node,context:FormCreateContext) =  new FormButton((node \"@text").text,readOptInt(node,"@iw"),readOptInt(node,"@aw"),
    readOptInt(node,"@ih"),readOptInt(node,"@ah"),(node \"@listener").text)

  def getTextField(node:xml.Node,context:FormCreateContext)= new FormTextField(readOptInt(node,"@iw"),readOptInt(node,"@aw"),
    readOptInt(node,"@ih"),readOptInt(node,"@ah"),
    HorAlign(FormElement.parseInt((node \"@align").text)),(node \"@field").text.toByte,context)

  def getTextArea(node:xml.Node,context:FormCreateContext)= new FormTextArea(readOptInt(node,"@iw"),readOptInt(node,"@aw"),
    readOptInt(node,"@ih"),readOptInt(node,"@ah"),(node \"@field").text.toByte,(node \"@printBut").text=="1",context)

  def getCalcField(node:xml.Node)= new FormCalcField(readOptInt(node,"@iw"),readOptInt(node,"@aw"),
    readOptInt(node,"@ih"),readOptInt(node,"@ah"),
    HorAlign(FormElement.parseInt((node \"@align").text)),Expression.decode((node \"@ex").text)._1)
}