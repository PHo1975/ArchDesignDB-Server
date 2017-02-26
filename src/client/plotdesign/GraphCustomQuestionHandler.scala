package client.plotdesign

import client.dialog.CustomQuestionHandler
import client.dialog.FocusContainer
import client.dialog.DialogManager
import definition.expression.VectorConstant
import java.awt.Graphics2D
import definition.typ.{ParamQuestion, CommandQuestion, DialogQuestion}
import definition.expression.DoubleConstant
import client.graphicsView.GraphElemConst

object GraphCustomQuestionHandler extends CustomQuestionHandler {
  
  private var graphController:Option[PlotDesignController]=None
  lazy val funcMap =collection.immutable.HashMap[String,(PlotDesignController) => Unit]("Zuschneiden"->setCutRect,
      "Move"->move)
  
  def load(question:ParamQuestion,container:FocusContainer):Unit ={
    container match {
      case gc:PlotDesignController =>
        graphController=Some(gc)
        question match {
          case c:CommandQuestion=> if(funcMap.contains(c.commandName))funcMap(c.commandName)(gc)
          else util.Log.e("GCQH load funcName "+c.commandName+" not found !")
          case o=> util.Log.e("wrong question type "+o)
        }
      case _=> graphController=None;util.Log.e("Unknown controller:"+container)
    }          
  } 
  
  lazy val rectStartQuestion=singlePointQuestion("Rechteck Zuschnitt","von Punkt")
  lazy val rectEndQuestion=singlePointQuestion("Rechteck Zuschnitt","bis Punkt")
  
  def setCutRect(gc:PlotDesignController):Unit={     
     DialogManager.startInterQuestion(rectStartQuestion,(answerList)=> {
      val startPoint=answerList.last.result.toVector      
      def diagRectDragger(pos:VectorConstant,g:Graphics2D)= {
				val sm=gc.scaleModel
				val scy=sm.yToScreen(startPoint.y)
				val py=sm.yToScreen(pos.y)
				val sx=sm.xToScreen(scala.math.min(startPoint.x,pos.x))
				val sy=scala.math.min(scy,py)
				//print("pos:"+pos+" x:"+sx+" y:"+sy)
				GraphElemConst.drawRectFloat(g,sx,sy,sm.xToScreen(math.max(pos.x,startPoint.x))-sx,math.max(scy,py)-sy)
			}
      gc.setCustomDragger(diagRectDragger)
      DialogManager.startInterQuestion(rectEndQuestion,(answerList1)=> {
        val endPoint=answerList1.last.result.toVector
        
        DialogManager.processResults()
      })
      
     })
  }
  
  def move(gc:PlotDesignController):Unit={
    DialogManager.startInterQuestion(new DialogQuestion("Verschieben<br>Distanz angeben",
		moveStartAnswers) ,(answerList)=>{
      answerList.head.result match{
        case d:DoubleConstant=> DialogManager.startInterQuestion(singleNumberQuestion("Verschieben","Delta Y eingeben:"),
            (answerList)=>{DialogManager.processResults()})
        case startP:VectorConstant=>  
           val selMod=gc.selectModel
        	 gc.setCustomDragger((pos,g)=>{
             val sm=gc.scaleModel
             val delta=pos-startP
             for(lay<-selMod.selectionList){
               lay.drawFrame(g,delta)
             }
        	 })
          DialogManager.startInterQuestion(singlePointQuestion("Verschieben","'Nach Punkt' angeben"),
            (answerList)=>DialogManager.processResults() )
      }
		})  
  }

}