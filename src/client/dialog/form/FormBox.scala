/**
 * Author: Peter Started:13.04.2011
 */
package client.dialog.form

import definition.data.InstanceData
import definition.typ.AbstractObjectClass
import definition.typ.form._
import util.XMLUtils._

import scala.swing.{BoxPanel, Component, Orientation}
import scala.xml.Node

/** trait for accessing the addBox of the Designer
 * 
 */
trait SpecialComponent extends Component with AbstractSpecialComponent {
	var _formBox:FormBox
	override def formBox:AbstractFormBox=_formBox
}
/**
 * 
 */
case class FormBox(minWidth:Int, maxWidth:Int, minHeight:Int, maxHeight:Int,
									 orient:Orientation.Value, elements:Seq[FormElement],var specialComp:Option[SpecialComponent]=None) extends BoxPanel(orient) with AbstractFormBox with FormElement {
	
	background=FormElement.formColor
	setupComponent(this)

  def horizontalOrient: Boolean = orient == Orientation.Horizontal

	elements.foreach {
		case e: Component => contents += e
		case other => util.Log.e("unknown type:" + other)
	}
	for(c<-specialComp) {
		contents+=c
		c._formBox=this
	}

  def toXML: Node = 	<FormBox iw={minWidth.toString} aw={maxWidth.toString} ih={minHeight.toString} ah={maxHeight.toString} orient={orient.id.toString}>
    {elements.map(_.toXML)}
  	</FormBox>
  
  
  def addElement(newElement:AbstractFormElement,pos:Int= -1):FormBox = newElement match{
		case ne:FormElement =>
			val nb=new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,
				if(pos < 0)	elements:+ne
				else (elements.take(pos):+ne)++elements.drop(pos)
				,specialComp)

			for( c<-specialComp) c._formBox=nb
			nb
		case _=> util.Log.e("unknown type "+newElement); this
  }

  def setSpecialComp(s: AbstractSpecialComponent): Unit = s match {
		case sp:SpecialComponent =>
			specialComp=Some(sp)
			sp._formBox=this
			contents+=sp
		case _=> util.Log.w("wrong special Comp "+s)
  }

  def makeCopy: FormBox = {
  	new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,elements.map(_.makeCopy))
  }
  
  def updateElement(pos:Int,newElement:AbstractFormElement):FormBox= newElement match {
		case ne:FormElement =>
			val nb=new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,elements.updated(pos,ne),specialComp)
			for( c<-specialComp) c._formBox=nb
			nb
		case _=> util.Log.w("wrong update Element "+newElement) ;this
  }
  
  def updateElement(oldValue:AbstractFormElement,newValue:AbstractFormElement):(FormBox,Boolean) = {
  	//println("updateElement oldValue:"+oldValue+" newValue "+newValue)
  	if(oldValue eq this)  (newValue.asInstanceOf[FormBox],true)
    else {
      for (ix <- elements.indices; el = elements(ix)) {
        if (el eq oldValue) return (updateElement(ix, newValue), true)
        el match {
          case fb: FormBox =>
						val nfb = fb.updateElement(oldValue, newValue)
						if (nfb._2) return (updateElement(ix, nfb._1), true)
					case _ =>
        }
      }
    }
  	(this,false)
  }
  
  def deleteElement(elem:AbstractFormElement):(FormBox,Boolean) = {
  	//println("Delete Element "+elem+" from box:"+toString)
  	for(ix <-elements.indices;el=elements(ix) ) {
  		if(el eq elem) 
  			return (new FormBox(minWidth,maxWidth,minHeight,maxHeight,orient,elements.filterNot(_ eq elem),specialComp),true)
  		el match {  			
  			case fb:FormBox =>
					val nfb=fb.deleteElement(elem)
					if(nfb._2) return (updateElement(ix,nfb._1),true)
				case _ =>
  		}
  	}
  	(this,false)
  }

  override def toString(): String = "(FormBox " + orient + " Elems:" + elements.mkString(" , ") + " )"
  
  
  def foreach(s:(AbstractFormElement)=>Unit):Unit= {
  	s(this)
  	elements.foreach {
			case fb: FormBox => fb.foreach(s)
			case other => s(other)
		}
  }
  
  def setDataValue(dvalue:InstanceData,nclass:AbstractObjectClass):Unit= elements.foreach {
		case df: FormDataField => df.setDataValue(dvalue, nclass)
		case fb: FormBox => fb.setDataValue(dvalue, nclass)
		case _ =>
	}
  
  def setListener(nlist:Option[DataChangeListener]):Unit = elements.foreach {
		case df: FormDataField => df.setListener(nlist)
		case fb: FormBox => fb.setListener(nlist)
		case _ =>
	}
  
  /** finds the formbox that contains the given element
   * 
   * @param el the element 
   * @return (the containing Formbox, index of element in Formbox)
   */
  def findFormBoxFor(el:AbstractFormElement):(FormBox,Int)= {
  	val ix=elements.indexWhere(_ eq el)
  	if(ix> -1) (this,ix)
  	else {
      for (e <- elements) e match {
        case fb: FormBox =>
					val ret = fb.findFormBoxFor(el)
					if (ret._2 > -1) return ret
				case _ =>
      }
      (null, -1)
    }
  }
}

object FormBox {
	def apply(node:Node,context:FormCreateContext):FormBox = {
		new FormBox(readOptInt(node,"@iw"),readOptInt(node,"@aw"),
		readOptInt(node,"@ih"),readOptInt(node,"@ah")
			,Orientation(FormElement.parseInt((node \"@orient").text)),FormElement.readElements(node,context))
	}
	
	
}