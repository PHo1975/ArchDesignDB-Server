/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import java.awt.{Insets, Color,Dimension}
import javax.swing.{BorderFactory, KeyStroke, UIDefaults}

import client.comm.{ClientQueryManager, ErrorListener, KeyStrokeManager, KeyStrokeReceiver}
import client.dataviewer._
import client.graphicsView.AbstractSelectModel
import client.print.PrintQuestionHandler
import client.ui.ClientApp
import definition.data._
import definition.expression.Constant
import definition.typ._
import util.Log
import scala.swing._
import scala.swing.event._
import scala.util.control.NonFatal

/** manages the user dialog and provides the DialogArea panel
 * 
 */

case class ResultElement(question:ParamQuestion, answer:AnswerDefinition, result:Constant)


case class CustomPanelQuestion(override val panel:CustomPanel) extends PanelQuestion{
  def toXML:scala.xml.Node=null
  def name=panel.name
	def classID=5
}



object DialogManager extends SelectListener /*with ActionPanListener*/{
  var propField:Byte=0
	var createdNewElements=0	
	var dialogIsActive=false
	var isServerEnquiry=false	
	var hasRebound=false	
	var selectedInstances:Iterable[SelectGroup[_<:Referencable]] = _
	var actionGroups:Iterable[SelectGroup[_<:Referencable]] = _	
	var currentQuestion:Option[ParamQuestion]= None
	var repeatQuestion:Option[DialogQuestion]= None
	var customAnswerListener=collection.mutable.Stack[((Seq[ResultElement]) => Unit,Option[ParamQuestion])]()
	 // in field paramquestion is the current question stored, when its only a temporary answerlistener that should not be stored
	 // so that the current question can be restored after the temporary question is answered
	//
	var isQuestionRepeating=false
	var currentAction:Option[ActionTrait]= None
	var createType:Option[Int]=None
	var customQuestionHandlerList=collection.mutable.HashMap[String,CustomQuestionHandler]()
	var hasDraggerToast:Option[AbstractViewController[_,_]]=None  
  
  val hoverColor=Color.cyan.darker
  
  val leftPanelColor=new Color(242,242,247)
  val eitherColor=new Color(225, 225, 225)
	val sidePanelWidth=195
	val buttonDefaults=new UIDefaults()
	buttonDefaults.put("Button.contentMargins", new Insets(6,6,6,6))
  val toggleButtonDefaults=new UIDefaults()
	toggleButtonDefaults.put("ToggleButton.contentMargins", new Insets(7,7,7,7))
  val lock=new Object
	val selectField=new MultiLineLabel()
	
	selectField.background=DialogManager.leftPanelColor
	val questionField=new MultiLineLabel()	
	questionField.border=BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.lightGray),BorderFactory.createEmptyBorder(1,1,1,1))

	
	val buttonSize=new Dimension(sidePanelWidth-10,30)
  val minButtonSize=new Dimension(sidePanelWidth-10,40)
	
	val selectLabScroller=new ScrollPane{
	  opaque=false
	  viewportView=selectField
	  background=leftPanelColor	  	
	  preferredSize=new Dimension(sidePanelWidth+20,70)
	  maximumSize=new Dimension(sidePanelWidth+20,70)
	  peer.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
	  border=BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "  Ausgewählt:")
	}
	
	/*val questionScroller=new ScrollPane{
	  opaque=false
	  viewportView=questionField	  
	  preferredSize=new Dimension(sidePanelWidth,60)
	  maximumSize=new Dimension(sidePanelWidth,60)		  
	  peer.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)	  
	}*/
	val errorField=new MultiLineLabel()
	
	val errorScroller=new ScrollPane {
			viewportView=errorField		
			errorField.background=leftPanelColor
			errorField.foreground=new Color(200,0,0)
			errorField.font=ViewConstants.errorFont 
			opaque=false
			preferredSize=new Dimension(sidePanelWidth+20,80)
			maximumSize=new Dimension(sidePanelWidth+20,80)	
			border=null
		}
	val errorListener=new ErrorListener  {
		def printError(errorText:String)= errorField.text= errorText
	}
  
	val answerArea=new AnswerArea ()
	val cancelBut=new Button("Aktion abbrechen")	
	val answerList=new scala.collection.mutable.ArrayBuffer[ResultElement]()	
	
  lazy val dialogPanel = new BoxPanel(Orientation.Vertical ) {
	  xLayoutAlignment=0.5f
	  opaque=false
		border=BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(8,0,6,0),
		    BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(8,0,6,0),"Aktion ausführen:"))
		contents+= questionField+=	 answerArea += Swing.VStrut(10)+=ClientApp.createHLine+=Swing.VStrut(10)+= cancelBut+=Swing.VGlue
		for(i<-contents)i.xLayoutAlignment=0.5f
    visible=false
    listenTo(cancelBut)
    reactions+= {
     case ButtonClicked(`cancelBut`) => reset()
    }
    maximumSize=new Dimension(sidePanelWidth,Short.MaxValue)

    ClientApp.sock.startupFinishListener+=(()=> {
      KeyStrokeManager.registerReceiver(new KeyStrokeReceiver {
        override def commandName: String = "Cancel"
        override def setStroke(stroke: KeyStroke): Unit = {}
        override def groupName: String = "Dialog"
        override def strokeHit(): Unit = reset()
      })
	  })
  }  
	
  ClientQueryManager.registerErrorListener(errorListener)
  LogOutputStream.registerListener(errorListener)
  cancelBut.visible=false
  cancelBut.xLayoutAlignment=0.5
  ClientQueryManager.registerSetupListener(()=> initCustomQuestionHandlers())
  answerArea.registerAnswerCallBack(answerGiven)
	
	def sidePanelDialogStart():Unit= {
	  dialogPanel.visible=true
	  ActionPanel.visible=false
		NewButtonsList.unregisterActionButtons()
		cancelBut.visible=true
		ClientApp.fieldEditPan.visible=false
		dialogPanel.revalidate()
	}
	
	def sidePanelDialogEnd():Unit= {	  
		dialogPanel.visible=false
		ActionPanel.visible=true
		NewButtonsList.registerActionButtons()				
		cancelBut.visible=false
		ClientApp.fieldEditPan.visible=true
		dialogPanel.revalidate()
	}
	
	def resetDraggerToast()= for(controller<-hasDraggerToast ){
    controller.resetDraggerToast()
    hasDraggerToast=None    
  } 
	
		
	def reset():Unit= lock.synchronized{
	 //println("\nReset isactive:"+dialogIsActive+" "+answerList.mkString+" is repeating:"+isQuestionRepeating+" hasRep:"+hasRebound)
   //println(Thread.currentThread().getStackTrace.drop(1).take(15).mkString("\n "))
   //println("..... ")
	  resetDraggerToast()
		if(dialogIsActive) {
      dialogIsActive=false
			if(isQuestionRepeating&& answerList.nonEmpty) { // when stopped during repeating, keep the results that are already there
				isQuestionRepeating=false
        processResults()
			}
			else if(isServerEnquiry){
				isServerEnquiry=false
				ClientQueryManager.answerEnquiry(Seq.empty)			
			}			
			answerArea.reset()      
			questionField.text=""
			errorField.text=""	  	
		  sidePanelDialogEnd()
      hasRebound=false
				
      for(c<-currentQuestion) c match {
        case pq:CustomPanelQuestion=> pq.panel.shutDown()
        case _=>
      }
			currentQuestion=None
			repeatQuestion=None	
      propField=0
      actionGroups=Seq.empty
      customAnswerListener.clear()
      isQuestionRepeating=false
      currentAction=None      
			answerList.clear()
      ActionPanel.restoreButtonBindings()
		}
		for(lc<-NewButtonsList.lastContainer) 				
				lc.actionStopped()		
	}
	
	// from DataViewController selection listener 
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Iterable[SelectGroup[T]],alsoSelected:Iterable[T]=Nil) = {
		//println("selectionChanged "+groups.mkString+" dialogIsActive "+dialogIsActive+" "+Thread.currentThread().getName)
    selectedInstances=groups    
		if(dialogIsActive){
		  if(hasRebound && sender.isInstanceOf[AbstractSelectModel[_]]) {
		    actionGroups=groups		
        //println("REBOUND "+currentAction+" ct:"+createType)
		  } else reset()	
		} 
		val numInstances=selectedInstances.foldLeft(0)(_+_.children.size)
		if(numInstances==0) selectField.text=" Kein Objekt ausgewählt"
		else {
			val typ=AllClasses.get.getCommonClassForGroups(selectedInstances)			
			val alsoText=if(alsoSelected.size<2) "" else "<br>von "+alsoSelected.size+" Elementen"
			selectField.text=" "+numInstances.toString+" "+
			  (if(typ< 0) "" else AllClasses.get.getClassByID(typ).getDescriptionOrName+" - ")+"Objekt"+(if(numInstances>1)"e " else " ")+ alsoText
		}
		ClientApp.updateSidePanel()
	}
	
	
	/** start an interactive question. When the answer is given, the listener function is called. It must call the processResult
	 * function in DialogManager !
	 * storeAnswer: should the answer be stored on the answer stack or is is just a temporary question from the point panel
	 */
	def startInterQuestion(question:ParamQuestion,listener:(Seq[ResultElement]) => Unit,storeAnswer:Boolean=true):Unit= {
	  customAnswerListener.push((listener,if(!storeAnswer)currentQuestion else None))
	  sidePanelDialogStart()
    dialogIsActive=true
	  loadQuestion(question)
	}
	
	
	// from ActionPanel listener	
	def startActionDialog(action:ActionTrait,groupList:Iterable[SelectGroup[_<:Referencable]]):Unit = lock.synchronized{
	 //("start actionDialog "+dialogIsActive+" ch: "+groupList.mkString(",")+" hasrebound:"+hasRebound)
		if(dialogIsActive&& !hasRebound)reset()
    if(hasRebound)hasRebound=false
		if(groupList.exists(_.children.isEmpty)) throw new IllegalArgumentException("Start Action "+action.name+" groupList.elements empty")
		action.question match {
		    case Some(question) =>
					initAction()
					actionGroups=groupList
					createType=None
					currentAction=Some(action)
					loadQuestion(question)
				case None =>  for(group<-groupList)ClientQueryManager.executeAction(group.parent,group.children,action.name,Seq())
		  }	
	}
	
	
	def startCreateActionDialog(action:ActionTrait,elems:SelectGroup[_<:Referencable],ncreateType:Int,npropField:Byte)= lock.synchronized {
	  if(dialogIsActive&& !hasRebound)reset()
    if(hasRebound)hasRebound=false
	  action.question match {
		    case Some(question) =>
					initAction()
					actionGroups=List(elems)
					createType=Some(ncreateType)
					currentAction=Some(action)
					//println("start Create "+question.getClass())
					propField=npropField
					loadQuestion(question)
				case None => for(lc<-NewButtonsList.lastContainer) {
		      		lc.createActionStarted(1)
		      		val formatValues= lc .getCreationFormatValues(ncreateType)			  
		      		ClientQueryManager.executeCreateAction(elems.children,ncreateType,npropField,action.name,Seq(),formatValues)
		      	}
	  }
	}
  
	def startEnquiryDialog(question:ParamQuestion):Unit = lock.synchronized{
	  //println("Start EnquiryDialog:"+question+" active:"+dialogIsActive)
		if(dialogIsActive)reset()
		customAnswerListener.clear()
		isServerEnquiry=true
		repeatQuestion=None
		isQuestionRepeating=false
		answerList.clear()
		//System.out.println("StartEnquiryDialog "+question)
		sidePanelDialogStart()
    dialogIsActive=true
		loadQuestion(question)
	}
  
  
	def startDraggerToast(controller:AbstractViewController[_,_])={
	  resetDraggerToast()
	  hasDraggerToast=Some(controller)	  
	}
	
	
	private def initAction()={
	  createdNewElements=0
	  isServerEnquiry=false	
	  customAnswerListener.clear()
	  answerList.clear()	  
	  isQuestionRepeating=false 
	  sidePanelDialogStart()	  		
	  dialogIsActive=true	
	}
	
	def increaseNumCreatedElements(amount:Int=1)= createdNewElements+=amount  
		    
	
	
	private def loadQuestion(question:ParamQuestion) = {    
	  // println("Load Question "+question)
	  errorField.text=""	  
		currentQuestion=Some(question)
		question match {
			case q:DialogQuestion=>
				repeatQuestion=if(q.repeat) Some(q) else None
				questionField.text=q.name
				answerArea.loadAnswerDefinitions(q)
			case p:PanelQuestion=>
				repeatQuestion=None
				if(! p.panel.load(actionGroups)) reset()
        else {
          answerArea.loadAnswerDefinitions(p)
          questionField.text = p.name
        }
			case c:CommandQuestion =>
				repeatQuestion=None
				for (cont<-NewButtonsList.lastContainer)
					if(customQuestionHandlerList.contains(c.moduleName)) customQuestionHandlerList(c.moduleName).load(c,cont)
					else Log.e("unknown Module "+c.moduleName)

			case x:XMLQuestion =>
				repeatQuestion=None
				for(cont<-NewButtonsList.lastContainer)
					if(customQuestionHandlerList.contains(x.moduleName))customQuestionHandlerList(x.moduleName).load(x,cont)
					else Log.e("unknown Module "+x.moduleName)
		}		
	}	
	
	
	// adds answer Data without Dialog processing. Used by IPE-Mode creating
	def addAnswer(parm:AnswerDefinition,result:Constant):Unit= if(dialogIsActive&& ! hasRebound){
	  answerList += ResultElement(null,parm,result)
	} else util.Log.e("add Answer on dactive"+dialogIsActive+" reb:"+hasRebound)
	
	def answerGiven(parm:AnswerDefinition,result:Constant):Unit = lock.synchronized{
		if(dialogIsActive && !hasRebound){
			//System.out.println("Answer given "+parm+" "+result)
		  val answer=ResultElement(currentQuestion.get,parm,result)
			answerList += answer      
			parm.followQuestion match {
				case Some(question) => loadQuestion(question)
				case None => //println("CustomAnswerListener "+customAnswerListener.mkString)
          if(customAnswerListener.isEmpty) {
            //println("repeatquestion:"+repeatQuestion)
            repeatQuestion match {
              case Some(rQuestion) =>
								isQuestionRepeating=true
								loadQuestion(repeatQuestion.get) // if repeatquestion was changed by a custom listener
							case None=>  // action is done
								processResults()
						}
          }
          else {
            val (listener,lastQuestion)=customAnswerListener.head
            //println("Listener "+listener+" lastQuestion:"+lastQuestion)
            repeatQuestion match {
              case Some(rQuestion) => isQuestionRepeating=true ; listener(answerList)
              case None=>
								customAnswerListener.pop()
								lastQuestion match {
                  case Some(lquestion)=>
										answerList.remove(answerList.size-1)
										listener(Seq(answer))
										loadQuestion(lquestion)
									case None => listener(answerList)//;processResults()
                }
						}
          }
			}			
		} else util.Log.e("answer given on dactive"+dialogIsActive+" reb:"+hasRebound)
	}
	
	def processCustomEnquiry(resultList:Seq[(String,Constant)])= {
		isServerEnquiry=false
		ClientQueryManager.answerEnquiry(resultList)
	}
	
	def processResults():Unit = {
		val resultList=answerList.map(x =>(x.answer.name,x.result))
    var repeatWithoutCAS=false
		//System.out.println("process Results :"+resultList.mkString("\n")+"\n currentAction:"+currentAction+"\n actionGroups:"+actionGroups+"\n")
		if(isServerEnquiry){
			isServerEnquiry=false
			ClientQueryManager.answerEnquiry(resultList)
		} else if(actionGroups!=null) {
      currentAction match {
        case Some(ca) if (ca.question.isDefined && ca.question.get.repeat) || ca.rebound =>
					hasRebound=true
					NewButtonsList.lastContainer match {
            case Some(lc) if lc.hasCreateActionStarted =>
							//println("reapeat with CAS "+ca.name+" "+ca.question.get)
							lc.onCASReceived{()=>repeatAction(ca,createType,propField)}
						case _ =>
							//("repeat without cAS "+ca.name+" "+ca.question.get)
							//println(Thread.currentThread.getStackTrace.drop(1).take(10).mkString("\n "))
							repeatWithoutCAS=true
					}
				case _ => hasRebound=false
			}
      //println("createType "+createType)
		  createType match {
		  	case Some(ct)=> for(group <-actionGroups) {			  
		  		val formatValues= NewButtonsList.lastContainer match {
		  			case Some(lc) =>
							lc.createActionStarted(if(createdNewElements==0) 1 else createdNewElements)
							lc.getCreationFormatValues(ct)
						case None=>Nil
		  		}
		  		for(ca<-currentAction)
		  		  ClientQueryManager.executeCreateAction(group.children,ct,propField,ca.name,resultList,formatValues)
		  	}
		  	case None=> for(group <-actionGroups; ca<-currentAction)
		  	  ClientQueryManager.executeAction(group.parent,group.children,ca.name,resultList)
		  }	
      if(!hasRebound)reset()
      if(repeatWithoutCAS) for(ca<-currentAction) repeatAction(ca,createType,propField)
		}
							
	}
  
  protected def repeatAction(ca:ActionTrait,crType:Option[Int],prField:Byte)={
    //println("Repeat "+createType+" "+ca.name+" "+ actionGroups+" dialogIsActive:"+dialogIsActive+" "+Thread.currentThread().getName)
    crType match {
      case Some(ct)=>if(actionGroups.nonEmpty) startCreateActionDialog(ca,actionGroups.head,ct,prField)
      case None =>  startActionDialog(ca,actionGroups)
    }
  }
	
	def initCustomQuestionHandlers() = if(SystemSettings()!=null){	
		customQuestionHandlerList("client.print.PrintQuestionHandler")=PrintQuestionHandler		
		customQuestionHandlerList("client.graphicsView.GraphCustomQuestionHandler")=client.graphicsView.GraphCustomQuestionHandler
		customQuestionHandlerList("client.plotdesign.GraphCustomQuestionHandler")=client.plotdesign.GraphCustomQuestionHandler
		val settingsString=SystemSettings().getClientSetting("CustomQuestionHandlers")
		if(settingsString.trim.length>0) {
			val moduleNames=settingsString.split(',')
			//println("ModuleNames "+moduleNames.mkString)
			for (m <-moduleNames) try {
				customQuestionHandlerList(m)= Class.forName(m).newInstance.asInstanceOf[CustomQuestionHandler]
			} catch {
				case NonFatal(e) => util.Log.e("trying to instantiate CustomQuestionHandler module:'"+m+"' \n",e)
			}
		}
	}
}