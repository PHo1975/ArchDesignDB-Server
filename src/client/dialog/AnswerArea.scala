/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import client.dataviewer.ViewConstants
import definition.expression.Constant
import definition.typ._

import scala.collection.mutable.ArrayBuffer
import scala.reflect._
import scala.swing._



/**
 * 
 */
class AnswerArea extends BoxPanel(scala.swing.Orientation.Vertical ) {
  val boolPool=new PanelPool[BoolAnswerPanel]
  val intPool=new PanelPool[IntAnswerPanel]
  val doublePool=new PanelPool[DoubleAnswerPanel]
  val stringPool=new PanelPool[StringAnswerPanel]
  val optionPool=new PanelPool[OptionAnswerPanel]
  val poolArray=Array(boolPool,intPool,stringPool,doublePool,optionPool)
  val eitherLab: Label = createOrLab("  Entweder")
  
  opaque=true
  background = ViewConstants.leftPanelColor
  
  val customPools=new collection.mutable.HashMap[DataType.Value,PanelPool[_ <: AnswerPanel]]
  
  var func: (AnswerDefinition,Constant)=>Unit = _


  def createOrLab(text: String): Label = {
    val ol = ViewConstants.label(text)
    ol.opaque=true
    ol.background = ViewConstants.eitherColor
    ol.xLayoutAlignment=0.5d
    ol.maximumSize=new Dimension(Short.MaxValue,25)
    ol.preferredSize = new Dimension(ViewConstants.sidePanelWidth, 25)
    ol
  }

  def reset(): Unit = {
  	//System.out.println("answerArea reset")
  	poolArray foreach( _.reset())
  	customPools.values.foreach( _.reset())
  	contents.clear()
  	revalidate()
  	repaint()
  }

  def loadAnswerDefinitions(pquestion: ParamQuestion): Unit = Swing.onEDT {
    //println("load answer def "+pquestion)
  	reset()
    var hasFocusSet = false
  	pquestion match{
      case question:DialogQuestion=>
        var counter=0
        contents+=Swing.VStrut(15)
        if(question.possibleAnswers.size>1) contents+=eitherLab
        for(ans <-question.possibleAnswers ) {
          val panel= ans.dataType match {
            case DataType.BoolTyp =>  boolPool.setupPanel()
            case DataType.IntTyp =>  intPool.setupPanel()
            case DataType.LongTyp => intPool.setupPanel()
            case DataType.DoubleTyp => doublePool.setupPanel()
            case DataType.StringTyp => stringPool.setupPanel()
            case DataType.EnumTyp => optionPool.setupPanel()
            case a =>
              if(customPools.contains(a)) {
                val custPool=customPools(a)
                custPool.setupPanel()
              }
              else throw new IllegalArgumentException ("Answertype "+a+" not supported ")
          }
          panel.loadParamAnswer(ans)
          //println("Add Panel:"+panel.getClass()+" size:"+panel.size+" prefSize:"+panel.preferredSize)
          contents+=panel
          panel.peer.invalidate()
          counter +=1
          if(counter<question.possibleAnswers.size)
            contents+=Swing.VStrut(15)+=createOrLab("  oder ")
          else contents+=Swing.VStrut(20)
          if (!hasFocusSet && panel.setFocus()) hasFocusSet = true
        }
      case panelQuestion:PanelQuestion =>
        panelQuestion.panel match {
          case p:Component => contents+=p
          case o=> util.Log.e("unknown panel type "+o)
        }
        panelQuestion.panel.open()
        panelQuestion.panel.setFocus()
    }
  	revalidate()
  	repaint()
  }

  def registerAnswerCallBack(nfunc: (AnswerDefinition, Constant) => Unit): Unit = {
  	func=nfunc
  }

  def registerCustomPanel[T <: AnswerPanel](typ: DataType.Value)(implicit m: ClassTag[T]): Unit = {
  	customPools(typ)=new PanelPool[T]
  }
  
  class PanelPool [T <: AnswerPanel](implicit m: ClassTag[T]) {
    val pool: ArrayBuffer[T] = ArrayBuffer[T]()
  	var usedPanels=0

    def setupPanel(): T = pool.synchronized {
  		usedPanels+= 1
  		if(pool.size>=usedPanels) pool(usedPanels-1)
  		else {
  			val newPan = m.runtimeClass.newInstance.asInstanceOf[T]
  			newPan.registerAnswerCallBack(func)
  			pool append newPan
  			newPan
  		}
  	}

    def reset(): Unit = pool.synchronized {
  		for(i <- 0 until usedPanels) pool(i).reset()
  		usedPanels=0  		
  	}
  }
}

