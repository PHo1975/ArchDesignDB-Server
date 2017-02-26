/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import definition.typ._
import definition.expression.Constant
import client.comm.ClientQueryManager
import java.awt.Color
import scala.reflect._



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
  val eitherLab=createOrLab("  Entweder")  
  
  opaque=true
  background=DialogManager.leftPanelColor
  
  val customPools=new collection.mutable.HashMap[DataType.Value,PanelPool[_ <: AnswerPanel]]
  
  var func: (AnswerDefinition,Constant)=>Unit = _
  
  
  def createOrLab(text:String)= {
    val ol=new Label(text)
    ol.opaque=true
    ol.background=DialogManager.eitherColor
    ol.xLayoutAlignment=0.5d
    ol.maximumSize=new Dimension(Short.MaxValue,25)
    ol.preferredSize=new Dimension(DialogManager.sidePanelWidth,25)
    ol
  }
  
  def reset()= {
  	//System.out.println("answerArea reset")
  	poolArray foreach( _.reset())
  	customPools.values.foreach( _.reset())
  	contents.clear()
  	revalidate()
  	repaint()
  }
  
  def loadAnswerDefinitions(pquestion:ParamQuestion) = Swing.onEDT{
  	reset()
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
          counter +=1
          if(counter<question.possibleAnswers.size)
            contents+=Swing.VStrut(15)+=createOrLab("  oder ")
          else contents+=Swing.VStrut(20)
          panel.setFocus()
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
  
  def registerAnswerCallBack(nfunc: (AnswerDefinition,Constant)=>Unit) =  {
  	func=nfunc
  }
  
  def registerCustomPanel[T <: AnswerPanel](typ:DataType.Value)(implicit m: ClassTag[T]) = {
  	customPools(typ)=new PanelPool[T]
  }
  
  class PanelPool [T <: AnswerPanel](implicit m: ClassTag[T]) {
  	val pool=ArrayBuffer[T]()
  	var usedPanels=0
  	def setupPanel() = pool.synchronized{
  		usedPanels+= 1
  		if(pool.size>=usedPanels) pool(usedPanels-1)
  		else {
  			val newPan = m.runtimeClass.newInstance.asInstanceOf[T]
  			newPan.registerAnswerCallBack(func)
  			pool append newPan
  			newPan
  		}
  	}
  	def reset() = pool.synchronized{
  		for(i <- 0 until usedPanels) pool(i).reset()
  		usedPanels=0  		
  	}
  }
}

