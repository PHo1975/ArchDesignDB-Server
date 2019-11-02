/**
 * Author: Peter Started:26.09.2010
 */
package client.dialog

import java.awt.{Color, Dimension}

import client.comm.{ClientQueryManager, KeyStrokeManager, KeyStrokeReceiver}
import client.dataviewer._
import client.graphicsView.AbstractSelectModel
import client.print.PrintQuestionHandler
import client.ui.ClientApp
import definition.data._
import definition.expression.Constant
import definition.typ._
import javax.swing.{BorderFactory, KeyStroke}
import util.Log

import scala.collection.mutable
import scala.swing._
import scala.swing.event._
import scala.util.control.NonFatal

/** manages the user dialog and provides the DialogArea panel
 * 
 */

case class CustomPanelQuestion(override val panel:CustomPanel) extends PanelQuestion{
  def toXML:scala.xml.Node=null
	def name: String = panel.name
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
	protected var repeatQuestion: Option[DialogQuestion] = None
	var customAnswerListener: List[(Seq[ResultElement] => Unit, Option[ParamQuestion])] = Nil
	 // in field paramquestion is the current question stored, when its only a temporary answerlistener that should not be stored
	 // so that the current question can be restored after the temporary question is answered
	//
	var isQuestionRepeating=false
	var currentAction:Option[ActionTrait]= None
	var createType:Option[Int]=None
	var customQuestionHandlerList: mutable.HashMap[String, CustomQuestionHandler] = collection.mutable.HashMap[String, CustomQuestionHandler]()
	var hasDraggerToast:Option[AbstractViewController[_,_]]=None
  val lock=new Object
	val selectField=new MultiLineLabel()
	selectField.background = ViewConstants.leftPanelColor
	val questionField=new MultiLineLabel()	
	questionField.border=BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.lightGray),BorderFactory.createEmptyBorder(1,1,1,1))


	lazy val selectLabScroller = new ScrollPane {
		opaque = true
		background = Color.yellow
	  viewportView=selectField
		background = ViewConstants.leftPanelColor
		preferredSize = new Dimension(ViewConstants.sidePanelWidth + 20 * ViewConstants.fontScale / 100, 70 * ViewConstants.fontScale / 100)
		maximumSize = new Dimension(ViewConstants.sidePanelWidth + 20 * ViewConstants.fontScale / 100, 70 * ViewConstants.fontScale / 100)
	  peer.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
	  border=BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "  Ausgewählt:")
	}
	

	val errorField=new MultiLineLabel()
	
	val errorScroller=new ScrollPane {
			viewportView=errorField
		errorField.background = ViewConstants.leftPanelColor
			errorField.foreground=new Color(200,0,0)
			errorField.font=ViewConstants.errorFont 
			opaque=false
		//background=Color.cyan
		preferredSize = new Dimension(ViewConstants.sidePanelWidth + 20 * ViewConstants.fontScale / 100, 60)
		maximumSize = new Dimension(ViewConstants.sidePanelWidth + 20 * ViewConstants.fontScale / 100, 80)
			border=null
		}

	def printError(errorText: String): Unit =
		errorField.text = errorText
  
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
		maximumSize = new Dimension(ViewConstants.sidePanelWidth, Short.MaxValue)

    ClientApp.sock.startupFinishListener+=(()=> {
      KeyStrokeManager.registerReceiver(new KeyStrokeReceiver {
        override def commandName: String = "Cancel"
        override def setStroke(stroke: KeyStroke): Unit = {}
        override def groupName: String = "Dialog"
        override def strokeHit(): Unit = reset()
      })
	  })
  }  

  cancelBut.visible=false
	cancelBut.focusable = false
  cancelBut.xLayoutAlignment=0.5
  ClientQueryManager.registerSetupListener(()=> initCustomQuestionHandlers())
  answerArea.registerAnswerCallBack(answerGiven)
	
	def sidePanelDialogStart():Unit= {
	  dialogPanel.visible=true
	  ActionPanel.visible=false
		CreateActionList.unregisterActionButtons()
		cancelBut.visible=true
		ClientApp.fieldEditPan.visible=false
		dialogPanel.revalidate()
	}
	
	def sidePanelDialogEnd():Unit= {	  
		dialogPanel.visible=false
		ActionPanel.visible=true
		CreateActionList.registerActionButtons()
		cancelBut.visible=false
		ClientApp.fieldEditPan.visible=true
		dialogPanel.revalidate()
	}

	def resetDraggerToast(): Unit = for (controller <- hasDraggerToast) {
    controller.resetDraggerToast()
    hasDraggerToast=None    
  } 
	
		
	def reset():Unit= lock.synchronized{
	 //println("\nReset isactive:"+dialogIsActive+" "+answerList.mkString+" is repeating:"+isQuestionRepeating+" hasRep:"+hasRebound)
		//println(Thread.currentThread().getStackTrace.slice(1, 16).mkString("\n "))
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
      customAnswerListener=Nil
      isQuestionRepeating=false
      currentAction=None      
			answerList.clear()
      ActionPanel.restoreButtonBindings()
		}
		for(lc<-CreateActionList.lastContainer)
				lc.actionStopped()		
	}
	
	// from DataViewController selection listener 
	def selectionChanged[T <: Referencable](sender: SelectSender, groups: Iterable[SelectGroup[T]], alsoSelected: Iterable[T] = Nil): Unit = {
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
	def startInterQuestion(question:ParamQuestion, listener: Seq[ResultElement] => Unit, storeAnswer:Boolean=true):Unit= {
	  customAnswerListener=(listener,if(!storeAnswer)currentQuestion else None) :: customAnswerListener
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


	def startCreateActionDialog(action: ActionTrait, ownerRef:Reference, ncreateType: Int, npropField: Byte): Unit = lock.synchronized {
	  if(dialogIsActive&& !hasRebound)reset()
    if(hasRebound)hasRebound=false
	  action.question match {
		    case Some(question) =>
					initAction()
					//actionGroups=List(elems)
					createType=Some(ncreateType)
					currentAction=Some(action)
					//println("start Create "+question.getClass())
					propField=npropField
					loadQuestion(question)
				case None => for(lastC<-CreateActionList.lastContainer) {
		      		lastC.createActionSubmitted(1)
		      		val formatValues= lastC .getCreationFormatValues(ncreateType)
		      		ClientQueryManager.executeCreateAction(ownerRef,ncreateType,npropField,action.name,Seq(),formatValues)
		      	}
	  }
	}
  
	def startEnquiryDialog(question:ParamQuestion):Unit = lock.synchronized{
	  //println("Start EnquiryDialog:"+question+" active:"+dialogIsActive)
		if(dialogIsActive)reset()
		customAnswerListener=Nil
		isServerEnquiry=true
		repeatQuestion=None
		isQuestionRepeating=false
		answerList.clear()
		//System.out.println("StartEnquiryDialog "+question)
		sidePanelDialogStart()
    dialogIsActive=true
		loadQuestion(question)
	}


	def startDraggerToast(controller: AbstractViewController[_, _]): Unit = {
	  resetDraggerToast()
	  hasDraggerToast=Some(controller)	  
	}
	
	
	private def initAction(): Unit ={
	  createdNewElements=0
	  isServerEnquiry=false	
	  customAnswerListener=Nil
	  answerList.clear()	  
	  isQuestionRepeating=false 
	  sidePanelDialogStart()	  		
	  dialogIsActive=true	
	}

	def increaseNumCreatedElements(amount: Int = 1): Unit = createdNewElements += amount

	def setRepeatQuestion(q: DialogQuestion): Unit = {
		repeatQuestion = if (q.repeat) Some(q) else None
		questionField.text = q.name
		answerArea.loadAnswerDefinitions(q)
	}
	
	private def loadQuestion(question:ParamQuestion): Unit = {
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
				for (cont<-CreateActionList.lastContainer)
					if(customQuestionHandlerList.contains(c.moduleName)) customQuestionHandlerList(c.moduleName).load(c,cont)
					else Log.e("unknown Module "+c.moduleName)

			case x:XMLQuestion =>
				repeatQuestion=None
				for(cont<-CreateActionList.lastContainer)
					if(customQuestionHandlerList.contains(x.moduleName))customQuestionHandlerList(x.moduleName).load(x,cont)
					else Log.e("unknown Module "+x.moduleName)
		}		
	}	
	
	
	// adds answer Data without Dialog processing. Used by IPE-Mode creating
	def addAnswer(parm:AnswerDefinition,result:Constant):Unit= if(dialogIsActive&& ! hasRebound){
	  answerList += ResultElement(parm.name,result)
	} else util.Log.e("add Answer on dactive"+dialogIsActive+" reb:"+hasRebound)
	
	def answerGiven(parm:AnswerDefinition,result:Constant):Unit = lock.synchronized{
		if(dialogIsActive && !hasRebound){
			//System.out.println("Answer given "+parm+" "+result+"\n customAnswerListener:"+customAnswerListener+" repeatQuestion:"+repeatQuestion)
		  val answer=ResultElement(parm.name,result)
			answerList += answer      
			parm.followQuestion match {
				case Some(question) => loadQuestion(question)
				case None => //println("CustomAnswerListener "+customAnswerListener.mkString)
          if(customAnswerListener.isEmpty) {
            //println("repeatquestion:"+repeatQuestion)
            repeatQuestion match {
              case Some(rQuestion) =>
								isQuestionRepeating=true
								loadQuestion(rQuestion) // if repeatquestion was changed by a custom listener
							case None=>  // action is done
								processResults()
						}
          }
          else {
            val (listener,lastQuestion)=customAnswerListener.head
            //println("Listener "+listener+" lastQuestion:"+lastQuestion)
            repeatQuestion match {
							case Some(rQuestion) =>
								isQuestionRepeating = true
								rQuestion match {
									case dq: DialogQuestion => answerArea.loadAnswerDefinitions(dq) // update pointpanel precision
									case _ =>
								}
								listener(answerList)
              case None=>
								customAnswerListener=customAnswerListener.tail
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
		} else util.Log.e("answer given on dactive"+dialogIsActive+" reb:"+hasRebound+" parm:"+parm.toString+" result:"+result+" currentQuestion:"+currentQuestion)
	}

	def processCustomEnquiry(resultList: Seq[ResultElement]): Unit = {
		isServerEnquiry=false
		ClientQueryManager.answerEnquiry(resultList)
	}
	
	def processResults():Unit = {
    var repeatWithoutCAS=false
		//System.out.println("process Results :"+resultList.mkString("\n")+"\n currentAction:"+currentAction+"\n actionGroups:"+actionGroups+"\n")
		if(isServerEnquiry){
			isServerEnquiry=false
			ClientQueryManager.answerEnquiry(answerList)
		} else if(actionGroups!=null) {
      currentAction match {
        case Some(ca) if (ca.question.isDefined && ca.question.get.repeat) || ca.rebound =>
					hasRebound=true
					CreateActionList.lastContainer match {
            case Some(lc) if lc.hasCreateActionStarted =>
							//println("reapeat with CAS "+ca.name+" "+ca.question.get)
							lc.onCreatedDataReceived{ ()=>repeatAction(ca,createType,propField)}
						case _ =>
							//("repeat without cAS "+ca.name+" "+ca.question.get)
							repeatWithoutCAS=true
					}
				case _ => hasRebound=false
			}
      //println("createType "+createType)
		  createType match {
		  	case Some(ct)=>
		  		CreateActionList.lastContainer match {
		  			case Some(lc) =>
							lc.createActionSubmitted(if(createdNewElements==0) 1 else createdNewElements)
							val formatValues= lc.getCreationFormatValues(ct)
							for(ca<-currentAction;owner<-lc.ownerRef)
								ClientQueryManager.executeCreateAction(owner.ref,ct,propField,ca.name,answerList,formatValues)
						case None=>
		  		}
		  	case None=> for(ca<-currentAction;group <-actionGroups )
		  	  ClientQueryManager.executeAction(group.parent,group.children,ca.name,answerList)
		  }	
      if(!hasRebound)reset()
      if(repeatWithoutCAS) for(ca<-currentAction) repeatAction(ca,createType,propField)
		}
							
	}

	protected def repeatAction(ca: ActionTrait, crType: Option[Int], prField: Byte): Unit = {
    //println("Repeat "+createType+" "+ca.name+" "+ actionGroups+" dialogIsActive:"+dialogIsActive+" "+Thread.currentThread().getName)
    crType match {
      case Some(ct)=>for(lc<-CreateActionList.lastContainer;owner<-lc.ownerRef) startCreateActionDialog(ca,owner.ref,ct,prField)
      case None =>  startActionDialog(ca,actionGroups)
    }
  }

	def initCustomQuestionHandlers(): Unit = if (SystemSettings() != null) {
		customQuestionHandlerList("client.print.PrintQuestionHandler")=PrintQuestionHandler		
		customQuestionHandlerList("client.graphicsView.GraphCustomQuestionHandler")=client.graphicsView.GraphCustomQuestionHandler
		customQuestionHandlerList("client.plotdesign.GraphCustomQuestionHandler")=client.plotdesign.GraphCustomQuestionHandler
		val settingsString=SystemSettings().getClientSetting("CustomQuestionHandlers")
		if(settingsString.trim.length>0) {
			val moduleNames=settingsString.split(',')
			//println("ModuleNames "+moduleNames.mkString)
			for (m <-moduleNames) try {
				customQuestionHandlerList(m)= Class.forName(m).getConstructor().newInstance().asInstanceOf[CustomQuestionHandler]
			} catch {
				case NonFatal(e) => util.Log.e("trying to instantiate CustomQuestionHandler module:'"+m+"' \n",e)
			}
		}
		println("Dialog settings done")
	}
}