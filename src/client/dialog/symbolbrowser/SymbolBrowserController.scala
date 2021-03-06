package client.dialog.symbolbrowser

import java.awt.{BasicStroke, Dimension}

import client.dialog.{AnswerPanelsData, CustomPanelQuestion, DialogManager}
import client.graphicsView.{ColorMap, GraphCustomQuestionHandler, GraphSettingsHandler, GraphViewController}
import client.ui.ViewConstants
import definition.data.InstanceData
import definition.expression.VectorConstant
import definition.typ.{AnswerDefinition, DataType}

object SymbolBrowserController {
  val folderType=110
  val symbolType=411
  val previewSize=new Dimension(160,175)
  val thumbSize=new Dimension(159,159)
  val answer=new AnswerDefinition("symbol",DataType.ObjectRefTyp,None)
  val pointAnswer = new AnswerDefinition("Absetzpunkt angeben", DataType.VariableTyp, None, AnswerPanelsData.NOSTRICT_HIT)
  val maximumSize = new Dimension(ViewConstants.sidePanelWidth, Short.MaxValue)
  val refCrossStroke=new BasicStroke
  //private var graphController:Option[GraphViewController]=None
  
  lazy val panel=new SymbolBrowserPanel
  lazy val chooseSymbolQuestion: CustomPanelQuestion = CustomPanelQuestion(panel)
  lazy val symbolRootFolder: Option[InstanceData] = GraphSettingsHandler.getMainFolder("Symbole")
  lazy val placeSymbolPanel=new SymbolPlacementPanel
  lazy val placeSymbolQuestion: CustomPanelQuestion = CustomPanelQuestion(placeSymbolPanel)
  
  def createSymbol(gc:GraphViewController):Unit= {
    panel.createStampMode=false
    DialogManager.startIntermediateQuestion(chooseSymbolQuestion, _ =>  {
      //println("next Step "+answerList.mkString)      
      for (stamp<-panel.selectedStamp) {        
        val graphElems=stamp.generateElements(Map.empty,0d)        
        gc.setCustomDragger((pos,g)=>{
          val sm=gc.scaleModel
          val angle=placeSymbolPanel.angle
          //val scale=placeSymbolPanel.scale
          val radAngle=angle*Math.PI/180d
          val sina=Math.sin(radAngle)
          val cosa=Math.cos(radAngle)
          val rotator=(v:VectorConstant)=> new VectorConstant(v.x*cosa-v.y*sina+pos.x,v.x*sina+v.y*cosa+pos.y,0)             
          for(el<-graphElems)
            el.drawRotated(g, sm, ColorMap.selectColor,angle, rotator)
        })
        DialogManager.startIntermediateQuestion(placeSymbolQuestion, _ =>{
          DialogManager.processResults() 
        })         
      }
    })
  }
  
  def createSymbolStamp(gc:GraphViewController):Unit= {
    panel.createStampMode=true
    DialogManager.startIntermediateQuestion(GraphCustomQuestionHandler.singlePointQuestion("Symbol erstellen", "Bezugspunkt wählen", None), _ => {
      DialogManager.startIntermediateQuestion(GraphCustomQuestionHandler.singleTextQuestion("Symbol erstellen","Name des neuen Symbols:"), _ =>{
        DialogManager.startIntermediateQuestion(chooseSymbolQuestion, _ =>  {
          util.Log.e(" CreateSymbolStamp should not happen ")
        })  
      })
    })
  }
  
  def changeSymbol(gc:GraphViewController):Unit= {
    panel.createStampMode=false
    DialogManager.startIntermediateQuestion(chooseSymbolQuestion, _ =>  {
      DialogManager.processResults()
    })
  }
  
  def createSymbolFiller(gc:GraphViewController):Unit= {
    panel.createStampMode=false
    DialogManager.startIntermediateQuestion(chooseSymbolQuestion, _ =>  {
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
        DialogManager.startIntermediateQuestion(placeSymbolQuestion, _ =>{
          DialogManager.startIntermediateQuestion(GraphCustomQuestionHandler.singlePointQuestion("SymbolFüller erstellen", "Bereich bis Punkt", None), _ => {
            DialogManager.startIntermediateQuestion(GraphCustomQuestionHandler.singleIntQuestion("SymbolFüller erstellen", "Anzahl Teilungen"), _ =>{
              DialogManager.processResults()  
            })  
          })
        })         
      }
      
    })
  }
}