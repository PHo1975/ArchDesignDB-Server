package client.dialog.symbolbrowser

import client.graphicsView.GraphViewController
import client.dialog.CustomPanelQuestion
import client.dialog.DialogManager
import client.graphicsView.GraphSettingsHandler
import client.model.PathControllable
import java.awt.Dimension
import client.dialog.CustomQuestionHandler
import client.graphicsView.GraphCustomQuestionHandler
import definition.typ.AnswerDefinition
import definition.typ.DataType
import java.awt.BasicStroke
import client.graphicsView.ColorMap
import definition.expression.VectorConstant
import util.GraphUtils
import definition.expression.NULLVECTOR

object SymbolBrowserController {
  val folderType=110
  val symbolType=411
  val previewSize=new Dimension(160,175)
  val thumbSize=new Dimension(159,159)
  val answer=new AnswerDefinition("symbol",DataType.ObjectRefTyp,None)
  val pointAnswer=new AnswerDefinition("Absetzpunkt angeben",DataType.VariableTyp,None)
  val maximumSize=new Dimension(DialogManager.sidePanelWidth,Short.MaxValue)
  val refCrossStroke=new BasicStroke
  private var graphController:Option[GraphViewController]=None
  
  lazy val panel=new SymbolBrowserPanel  
  lazy val chooseSymbolQuestion=new CustomPanelQuestion(panel)  
  lazy val symbolRootFolder=GraphSettingsHandler.getMainFolder("Symbole")
  lazy val placeSymbolPanel=new SymbolPlacementPanel
  lazy val placeSymbolQuestion=new CustomPanelQuestion(placeSymbolPanel)
  
  def createSymbol(gc:GraphViewController):Unit= {
    panel.createStampMode=false
    DialogManager.startInterQuestion(chooseSymbolQuestion,(answerList)=>  {
      //println("next Step "+answerList.mkString)      
      for (stamp<-panel.selectedStamp) {        
        val graphElems=stamp.generateElements(Map.empty,0d)        
        gc.setCustomDragger((pos,g)=>{
          val sm=gc.scaleModel
          val angle=placeSymbolPanel.angle
          val scale=placeSymbolPanel.scale
          val radAngle=angle*Math.PI/180d
          val sina=Math.sin(radAngle)
          val cosa=Math.cos(radAngle)
          val rotator=(v:VectorConstant)=> new VectorConstant(v.x*cosa-v.y*sina+pos.x,v.x*sina+v.y*cosa+pos.y,0)             
          for(el<-graphElems)
            el.drawRotated(g, sm, ColorMap.selectColor,angle, rotator)
        })
        DialogManager.startInterQuestion(placeSymbolQuestion,(answerList)=>{
          DialogManager.processResults() 
        })         
      }
      
    })
  }
  
  def createSymbolStamp(gc:GraphViewController):Unit= {
    panel.createStampMode=true
    DialogManager.startInterQuestion(GraphCustomQuestionHandler.singlePointQuestion("Symbol erstellen","Bezugspunkt wählen"),(answerList)=>{
      DialogManager.startInterQuestion(GraphCustomQuestionHandler.singleTextQuestion("Symbol erstellen","Name des neuen Symbols:"),(answerList)=>{
        DialogManager.startInterQuestion(chooseSymbolQuestion,(answerList)=>  {
          util.Log.e("should not happen "+answerList.mkString)
        })  
      })
    })
  }
  
  def changeSymbol(gc:GraphViewController):Unit= {
    panel.createStampMode=false
    DialogManager.startInterQuestion(chooseSymbolQuestion,(answerList)=>  {
      DialogManager.processResults()
    })
  }
  
  def createSymbolFiller(gc:GraphViewController):Unit= {
    panel.createStampMode=false
    DialogManager.startInterQuestion(chooseSymbolQuestion,(answerList)=>  {
      //println("next Step "+answerList.mkString)      
      for (stamp<-panel.selectedStamp) {        
        val graphElems=stamp.generateElements(Map.empty,0d)        
        gc.setCustomDragger((pos,g)=>{
          val sm=gc.scaleModel
          val angle=placeSymbolPanel.angle          
          val radAngle=angle/180*Math.PI
          val cosa=math.cos(radAngle)
          val sina=math.sin(radAngle)
          val scale=placeSymbolPanel.scale          
          def rotator(p:VectorConstant):VectorConstant=           
           new VectorConstant(p.x*cosa-p.y*sina+pos.x,p.x*sina+p.y*cosa+pos.y,0)          
          for(el<-graphElems)
            el.drawRotated(g, sm, ColorMap.selectColor,angle, rotator)
        })
        DialogManager.startInterQuestion(placeSymbolQuestion,(answerList)=>{
          DialogManager.startInterQuestion(GraphCustomQuestionHandler.singlePointQuestion("SymbolFüller erstellen","Bereich bis Punkt"),(answerList)=>{
            DialogManager.startInterQuestion(GraphCustomQuestionHandler.singleIntQuestion("SymbolFüller erstellen", "Anzahl Teilungen"),(answerList)=>{            
              DialogManager.processResults()  
            })  
          })
        })         
      }
      
    })
  }
  
}