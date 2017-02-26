package client.dialog

import definition.expression.VectorConstant
import scala.swing._
import definition.expression.BlobConstant
import definition.typ.AnswerDefinition
import scala.swing.event.ButtonClicked

trait SelectPointsListener extends BracketListener {
  def pointsSelected(points:Seq[VectorConstant]):Unit
}

class BlobAnswerPanel extends AnswerPanel with SelectPointsListener  {
  
  val bracketBut=new ToggleButton("Summe")
  var active=false
  var selectModel:PointSelectModel=null
  
  
  def pointsSelected(points:Seq[VectorConstant]):Unit= {      
    func(ansParm,BlobConstant.fillData(out=> {
      out.writeInt(points.size)
      for(p<-points) p.write(out)
    }))
  }  
  
  def bracketModeStarted()={
    bracketBut.selected=true
  }
  
  
  bracketBut.focusable=false  
  bracketBut.xLayoutAlignment=0.5d  
  contents+=infoLabel += Swing.RigidBox(new Dimension(0,10))+=bracketBut
  listenTo(bracketBut)
  
  reactions+= {
    case ButtonClicked(`bracketBut`)=>if(AnswerPanelsData.currentViewController!=null )
      AnswerPanelsData.currentViewController.flipPointSelectionBracketMode()
  }
  
  def setFocus(): Unit = {  }  
  
  
  override def loadParamAnswer(answerDesc:AnswerDefinition) = {
  	super.loadParamAnswer(answerDesc)
  	active=true  
  	if(answerDesc.constraint=="SelectPoints"&& AnswerPanelsData.currentViewController!=null) {	  
	  	AnswerPanelsData.currentViewController.askForPointSelection(this)  
	  	//selectModel=AnswerPanelsData.currentViewController.pointSelectModel
	  } 	  		
  }
  
  
  override def reset()= {  	
  	super.reset()
  	bracketBut.selected=false
  	if(active) {  		
  		active=false
  		if(AnswerPanelsData.currentViewController!=null) 
  			AnswerPanelsData.currentViewController.cancelModus()
  			//selectModel.deselect()
  	}
  }

}