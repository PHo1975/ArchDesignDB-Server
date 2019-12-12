package client.plotdesign

import java.awt.Graphics2D

import client.dialog.{CustomQuestionHandler, DialogManager, FocusContainer}
import client.graphicsView.GraphElemConst
import definition.expression.{DoubleConstant, VectorConstant}
import definition.typ.{CommandQuestion, DialogQuestion, ParamQuestion}

import scala.collection.immutable.HashMap

object PlotCustomQuestionHandler extends CustomQuestionHandler {
  
  private var graphController:Option[PlotDesignController]=None
  lazy val funcMap: HashMap[String, (PlotDesignController) => Unit] = collection.immutable.HashMap[String, (PlotDesignController) => Unit]("Zuschneiden" -> setCutRect,
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

  lazy val rectStartQuestion: DialogQuestion = singlePointQuestion("Rechteck Zuschnitt", "von Punkt", Some(false))
  lazy val rectEndQuestion: DialogQuestion = singlePointQuestion("Rechteck Zuschnitt", "bis Punkt", Some(false))
  
  def setCutRect(gc:PlotDesignController):Unit={     
     DialogManager.startIntermediateQuestion(rectStartQuestion, answerList=> {
      val startPoint=answerList.last.result.toVector      
      def diagRectDragger(pos:VectorConstant,g:Graphics2D): Unit = {
				val sm=gc.scaleModel
				val scy=sm.yToScreen(startPoint.y)
				val py=sm.yToScreen(pos.y)
				val sx=sm.xToScreen(scala.math.min(startPoint.x,pos.x))
				val sy=scala.math.min(scy,py)
				//print("pos:"+pos+" x:"+sx+" y:"+sy)
				GraphElemConst.drawRectFloat(g,sx,sy,sm.xToScreen(math.max(pos.x,startPoint.x))-sx,math.max(scy,py)-sy)
			}
      gc.setCustomDragger(diagRectDragger)
      DialogManager.startIntermediateQuestion(rectEndQuestion, _ => DialogManager.processResults() )
     })
  }
  
  def move(gc:PlotDesignController):Unit={
    DialogManager.startIntermediateQuestion(DialogQuestion("Verschieben<br>Distanz angeben",
      moveStartAnswers), answerList => {
      answerList.head.result match{
        case d:DoubleConstant=> DialogManager.startIntermediateQuestion(singleNumberQuestion("Verschieben","Delta Y eingeben:"),
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
          DialogManager.startIntermediateQuestion(singlePointQuestion("Verschieben", "'Nach Punkt' angeben", Some(false)),
            _=>DialogManager.processResults() )
      }
		})  
  }

}