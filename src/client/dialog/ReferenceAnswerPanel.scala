/**
 * Author: Peter Started:28.05.2011
 */
package client.dialog
import client.graphicsView.GraphElem
import definition.expression.{BoolConstant, IntConstant, ObjectReference, VectorConstant}
import definition.typ.{AnswerDefinition, DataType, DialogQuestion}

import scala.swing._
/**
 * 
 */

trait ObjectSelectListener{
	def objectsSelected(obj:ObjectReference,editable:Boolean): Unit
	def objectsSelectedWithPoint(obj:ObjectReference,point:VectorConstant,editable:Boolean): Unit
	def segmentPartSelected(obj:ObjectReference,point1:VectorConstant,point2:VectorConstant): Unit
	def tempObjectSelected(el:Int): Unit
}

trait DefaultObjectSelectListener extends ObjectSelectListener{
  def objectsSelected(obj: ObjectReference, editable: Boolean): Unit = {}

  def objectsSelectedWithPoint(obj: ObjectReference, point: VectorConstant, editable: Boolean): Unit = {}

  def segmentPartSelected(obj: ObjectReference, point1: VectorConstant, point2: VectorConstant): Unit = {}

  def tempObjectSelected(el: Int): Unit = {}
}

class TempChooseAnswerDef(nname:String,val elements:Seq[GraphElem])
	 extends AnswerDefinition(nname,DataType.ObjectRefTyp,None) 

class ReferenceAnswerPanel extends AnswerPanel with ObjectSelectListener {
  val filterBut=new Button("Elemente filtern")  
  val cuttedBut=new ToggleButton("Geschnittene Elemente")
  val onlyFullBut=new ToggleButton("Nur ganze Elemente")
  var active=false
  
  filterBut.focusable=false
  cuttedBut.focusable=false
  onlyFullBut.focusable=false
  
  filterBut.xLayoutAlignment=0.5d
  cuttedBut.xLayoutAlignment=0.5d
  onlyFullBut.xLayoutAlignment=0.5d
  
  contents+=infoLabel += Swing.RigidBox(new Dimension(0,10))+=filterBut+=cuttedBut+=onlyFullBut
  
  listenTo(filterBut,cuttedBut,onlyFullBut)

  override def loadParamAnswer(answerDesc: AnswerDefinition): Unit = {
  	super.loadParamAnswer(answerDesc)
  	active=true
  	//System.out.println("set Active "+answerDesc.name)
  	if(AnswerPanelsData.currentViewController!=null) {
  	  answerDesc match {
  	    case tm:TempChooseAnswerDef =>
          FollowMouseToast.showToast(DialogManager.questionField.puretext+" : "+answerDesc.name,AnswerPanelsData.currentViewController.canvas.peer)
  	      AnswerPanelsData.currentViewController.chooseTempObject(this,tm.elements)
  	    case _ =>
          //System.out.println("AnswerPanel constraint:"+answerDesc.constraint)
          if (AnswerPanelsData.currentViewController.askForObjectSelection(this,answerDesc.constraint))
            FollowMouseToast.appendToastText("oder "+answerDesc.name)
          else FollowMouseToast.showToast(DialogManager.questionField.puretext+" : "+answerDesc.name,AnswerPanelsData.currentViewController.canvas.peer)
      }
  	}  		
  }


  override def reset(): Unit = {
  	//System.out.println("Refpanel reset "+active)

  	super.reset()
  	if(active) {  		
  		active=false
  		if(AnswerPanelsData.currentViewController!=null) 
  			AnswerPanelsData.currentViewController.cancelModus()
  	}
  }


  def objectsSelected(objs: ObjectReference, editable: Boolean): Unit = {
    //System.out.println("Objects selected "+objs)
    DialogManager.answerGiven(ansParm,objs)
    AnswerPanelsData.currentViewController match {      
      case ctrl:AbstractViewController[_,_] if ctrl.selectObject_addModifiableInfo =>
        for(q<-ansParm.followQuestion)
    		DialogManager.answerGiven(q.asInstanceOf[DialogQuestion].possibleAnswers.head,BoolConstant(editable))
      case _=>      
    }   
  }


  def objectsSelectedWithPoint(obj: ObjectReference, point: VectorConstant, editable: Boolean): Unit = {
    println("objectSelectedWithPoint "+obj+" p: "+point+" follow:"+ansParm.followQuestion)
    DialogManager.answerGiven(ansParm,obj)
    DialogManager.answerGiven(ansParm,point)
    AnswerPanelsData.currentViewController match {
      case ctrl:AbstractViewController[_,_] if ctrl.selectObject_addModifiableInfo =>
        for(q<-ansParm.followQuestion)
          DialogManager.answerGiven(q.asInstanceOf[DialogQuestion].possibleAnswers.head,BoolConstant(editable))
      case _=>
    }
    //reset()
  }


  def segmentPartSelected(obj: ObjectReference, p1: VectorConstant, p2: VectorConstant): Unit = {
  	DialogManager.answerGiven(ansParm,obj)
  	for(q<-ansParm.followQuestion){
  		val ansParm1=q.asInstanceOf[DialogQuestion].possibleAnswers.head
  		DialogManager.answerGiven(ansParm1,p1)
  		for(q1<-ansParm1.followQuestion){
  			val ansParm2=q1.asInstanceOf[DialogQuestion].possibleAnswers.head
  			DialogManager.answerGiven(ansParm2,p2)      
  		}
  	}
  }

  def tempObjectSelected(el: Int): Unit = {
    DialogManager.answerGiven(ansParm, IntConstant(el))
  }
}

