/**
 * Author: Peter Started:28.05.2011
 */
package client.dialog
import definition.data.Reference
import scala.swing._
import definition.typ.AnswerDefinition
import definition.expression.ObjectReference
import definition.expression.VectorConstant
import definition.typ.{DialogQuestion,DataType}
import client.graphicsView.GraphElem
import definition.expression.{IntConstant,BlobConstant}
import definition.expression.BoolConstant
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
  def objectsSelected(obj:ObjectReference,editable:Boolean)={}
  def objectsSelectedWithPoint(obj:ObjectReference,point:VectorConstant,editable:Boolean)= {}
	def segmentPartSelected(obj:ObjectReference,point1:VectorConstant,point2:VectorConstant)= {}
	def tempObjectSelected(el:Int)	= {}
}

class TempChooseAnswerDef(nname:String,val elements:Seq[GraphElem])
	 extends AnswerDefinition(nname,DataType.ObjectRefTyp,None) 

class ReferenceAnswerPanel extends AnswerPanel with ObjectSelectListener {
  val filterBut=new Button("Elemente filtern")  
  val cuttedBut=new ToggleButton("Geschnittene Elemente")
  val onlyFullBut=new ToggleButton("Nur ganze Elemente")
  //val textLabel=new Label()
  var active=false
  
  filterBut.focusable=false
  cuttedBut.focusable=false
  onlyFullBut.focusable=false
  
  filterBut.xLayoutAlignment=0.5d
  cuttedBut.xLayoutAlignment=0.5d
  onlyFullBut.xLayoutAlignment=0.5d
  
  contents+=infoLabel += Swing.RigidBox(new Dimension(0,10))+=filterBut+=cuttedBut+=onlyFullBut
  
  listenTo(filterBut,cuttedBut,onlyFullBut)
  
  override def loadParamAnswer(answerDesc:AnswerDefinition) = {
  	super.loadParamAnswer(answerDesc)
  	active=true
  	//System.out.println("set Active "+answerDesc.name)
  	if(AnswerPanelsData.currentViewController!=null) {
  	  answerDesc match {
  	    case tm:TempChooseAnswerDef =>
  	      AnswerPanelsData.currentViewController.chooseTempObject(this,tm.elements)
  	    case _ =>
          //System.out.println("AnswerPanel constraint:"+answerDesc.constraint)
          AnswerPanelsData.currentViewController.askForObjectSelection(this,answerDesc.constraint)
      }
  		  		
  	}  		
  }
  
 
  
  override def reset()= {
  	//System.out.println("Refpanel reset "+active)
  	super.reset()
  	if(active) {  		
  		active=false
  		if(AnswerPanelsData.currentViewController!=null) 
  			AnswerPanelsData.currentViewController.cancelModus()
  	}
  }
  
  
  def objectsSelected(objs:ObjectReference,editable:Boolean) = {
    //System.out.println("Objects selected "+objs)
    func(ansParm,objs)
    AnswerPanelsData.currentViewController match {      
      case ctrl:AbstractViewController[_,_] if ctrl.selectObject_addModifiableInfo =>
        for(q<-ansParm.followQuestion)
    		func(q.asInstanceOf[DialogQuestion].possibleAnswers.head,BoolConstant(editable))
      case _=>      
    }   
  }
  
  
  def objectsSelectedWithPoint(obj:ObjectReference,point:VectorConstant,editable:Boolean) = {
    println("objectSelectedWithPoint "+obj+" p: "+point+" follow:"+ansParm.followQuestion)
    func(ansParm,obj)
    func(ansParm,point)
    AnswerPanelsData.currentViewController match {
      case ctrl:AbstractViewController[_,_] if ctrl.selectObject_addModifiableInfo =>
        for(q<-ansParm.followQuestion)
          func(q.asInstanceOf[DialogQuestion].possibleAnswers.head,BoolConstant(editable))
      case _=>
    }
    //reset()
  }
  
  
  def segmentPartSelected(obj:ObjectReference,p1:VectorConstant,p2:VectorConstant)= {
  	func(ansParm,obj)
  	for(q<-ansParm.followQuestion){
  		val ansParm1=q.asInstanceOf[DialogQuestion].possibleAnswers.head
  		func(ansParm1,p1)
  		for(q1<-ansParm1.followQuestion){
  			val ansParm2=q1.asInstanceOf[DialogQuestion].possibleAnswers.head
  			func(ansParm2,p2)      
  		}
  	}
  }
  
  def tempObjectSelected(el:Int) = {
    func(ansParm,new IntConstant(el))
  }
  
  
  
  def setFocus()={}
  
  
  

}

