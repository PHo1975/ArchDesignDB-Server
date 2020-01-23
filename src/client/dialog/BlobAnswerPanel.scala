package client.dialog

import definition.expression.{BlobConstant, VectorConstant}
import definition.typ.AnswerDefinition

import scala.swing._
import scala.swing.event.ButtonClicked

trait SelectPointsListener extends BracketListener {
  def pointsSelected(points:Seq[VectorConstant]):Unit
}

class BlobAnswerPanel extends AnswerPanel with SelectPointsListener  {
  
  val bracketBut=new ToggleButton("Summe")
  var active=false
  //var selectModel: PointSelectModel = _
  
  def pointsSelected(points:Seq[VectorConstant]):Unit= {
    DialogManager.answerGiven(ansParm,BlobConstant.fillData(out=> {
      out.writeInt(points.size)
      for(p<-points) p.write(out)
    }))
  }

  def bracketModeStarted(): Unit = {
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

  override def loadParamAnswer(answerDesc: AnswerDefinition): Unit = {
  	super.loadParamAnswer(answerDesc)
  	active=true
    if (answerDesc.constraint == "SelectPoints" && AnswerPanelsData.currentViewController != null) {
      AnswerPanelsData.currentViewController.askForPointSelection(this)
      FollowMouseToast.showToast("Punkte auswählen",AnswerPanelsData.currentViewController.canvas.peer)
    }
  }

  override def reset(): Unit = {
  	super.reset()
  	bracketBut.selected=false
  	if(active) {  		
  		active=false
  		if(AnswerPanelsData.currentViewController!=null) 
  			AnswerPanelsData.currentViewController.cancelModus()
  	}
  }

}