package client.dataviewer.todolist

import client.comm.{ClientQueryManager, ListDataModel}
import client.dialog.{FieldEditor, SidePanelComponent}
import client.ui.{ClientApp, ViewConstants}
import definition.data.{OwnerReference, Referencable}
import definition.expression.BoolConstant
import definition.typ.SelectGroup
import util.Log

import java.awt.Dimension
import java.awt.event._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.{AbstractListModel, JList, ListSelectionModel}
import scala.collection.mutable.ArrayBuffer
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, Label, ListView, Orientation, Panel, ScrollPane, Swing}

class ProjectFieldEditor extends FieldEditor {

  val projectList: ArrayBuffer[ProjectGoalsModel] = ArrayBuffer[ProjectGoalsModel]()
  lazy val goalDialog=new GoalDialog(ClientApp.top)
  lazy val workDialog=new WorkingStageDialog(ClientApp.top)
  val workStageModel=new ListDataModel(List(170),data=> new WorkingStage(data))

  val goalListModel =new AbstractListModel[ProjectGoal]() {
    override def getSize: Int = projectList.foldLeft(0)((s,model)=>s+model.dataList.getSize)
    override def getElementAt(i: Int): ProjectGoal = {
      val projects=projectList.iterator
      var found=false
      var currentRange=0
      var result:ProjectGoal=null
      while(projects.hasNext&& ! found){
        val currentProject=projects.next()
        if(currentRange+currentProject.dataList.getSize>i) {
          result=currentProject.dataList.theList(i-currentRange)
          found=true
        }
        else currentRange+=currentProject.dataList.getSize
      }
      if(!found) {Log.e("ProjectGoal not found ix:"+i);null}
      else result
    }
    def update(): Unit =this.fireContentsChanged(this,0,projectList.length-1)
  }

  val goalList=new JList(goalListModel)
  goalList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  val workStageList=new JList(workStageModel)
  val createGoalBut=new Button("+")
  createGoalBut.tooltip = "Ziel erstellen"
  val deleteGoalBut=new Button("-")
  deleteGoalBut.tooltip = "Ziel Löschen"
  val editGoalBut=new Button("B")
  editGoalBut.tooltip = "Ziel bearbeiten"
  val finishGoalBut=new Button("V")
  finishGoalBut.tooltip = "Ziel als erledigt markieren"

  val createWorkBut=new Button("+")
  createWorkBut.tooltip = "Arbeitsschritt erstellen"
  val deleteWorkBut=new Button("-")
  deleteWorkBut.tooltip = "Arbeitsschritt löschen"
  val editWorkBut=new Button("B")
  editWorkBut.tooltip = "Arbeitsschritt erstellen"
  val finishWorkBut=new Button("V")
  finishWorkBut.tooltip = "Arbeitsschritt als erledigt markieren"


  goalList.addListSelectionListener(new ListSelectionListener {
    override def valueChanged(listSelectionEvent: ListSelectionEvent): Unit = {
      if(listSelectionEvent.getValueIsAdjusting) createWorkBut.enabled = true
    }
  })

  goalList.addMouseListener(new MouseAdapter {
    override def mouseClicked(mouseEvent: MouseEvent): Unit =
    mouseEvent.getClickCount match {
      case 1=> openWorkStages()
      case 2=> editGoal()
      case _ =>
    }
  })

  workStageList.addMouseListener(new MouseAdapter {
    override def mouseClicked(mouseEvent: MouseEvent): Unit =
      mouseEvent.getClickCount match {
        case 2=> editWork()
        case _ =>
      }
  })

  def shutDown():Unit= {
    for(p<-projectList) {
      p.dataList.shutDown()
      workStageModel.shutDown()
    }
  }

  override def setData(data: Iterable[SelectGroup[_ <: Referencable]]): Unit = {
    shutDown()
    projectList.clear()
    goalList.clearSelection()
    for(group<-data;d<-group.children;if d.ref.typ==202)
      projectList += ProjectGoalsModel.loadProject(d.ref,this)
    //prListModel.update()
    createWorkBut.enabled = false
  }

  def openWorkStages(): Unit ={
    workStageModel.shutDown()
    goalList.getSelectedIndex match {
      case -1 =>
      case ix =>
        workStageList.clearSelection()
        workStageModel.load(goalListModel.getElementAt(ix).ref,1,()=>println("work loaded"))
    }
  }



  lazy val panel:BoxPanel =new BoxPanel(Orientation.Vertical) {
    //contents += new ScrollPane( ListView.wrap(new JList(prListModel)))
    contents+= new BoxPanel(Orientation.Horizontal ){
      contents+=new Label("Ziele:")+=Swing.HGlue+=createGoalBut+=editGoalBut+=deleteGoalBut+=finishGoalBut
    }
    listenTo(createGoalBut,deleteGoalBut ,editGoalBut,finishGoalBut,createWorkBut,deleteWorkBut,editWorkBut,finishWorkBut)
    reactions+={
      case  ButtonClicked(`createGoalBut`)=> createGoal()
      case  ButtonClicked(`editGoalBut`)=> editGoal()
      case  ButtonClicked(`deleteGoalBut`)=> deleteGoal()
      case  ButtonClicked(`finishGoalBut`)=> finishGoal()
      case  ButtonClicked(`createWorkBut`)=> createWork()
      case  ButtonClicked(`editWorkBut`)=> editWork()
      case  ButtonClicked(`deleteWorkBut`)=> deleteWork()
      case  ButtonClicked(`finishWorkBut`)=> finishWork()
    }
    goalList.addMouseMotionListener(new MouseMotionAdapter {
      override def mouseMoved(e: MouseEvent): Unit ={
        goalList.locationToIndex(e.getPoint) match{
          case -1 => goalList.setToolTipText("")
          case ix => val el=goalListModel.getElementAt(ix)
            val headText=if(projectList.size>1)"Projekt:"+el.project.prName+"<br>" else ""
            val dateText=el.erledigt.toString() match {
              case "0.0.0" => ""
              case o=> "<br>Fertigstellung bis: "+o
            }
            goalList.setToolTipText("<html>"+headText+el.name+"<br>"+el.bemerkung+dateText+"</html>")
        }
      }
    })
    contents += new ScrollPane( ListView.wrap(goalList))
    contents+=new BoxPanel(Orientation.Horizontal){
      contents+=new Label("Arbeitsschritte:")+=Swing.HGlue+=createWorkBut+=editWorkBut+=deleteWorkBut+=finishWorkBut
    }
    contents+=new ScrollPane(ListView.wrap(workStageList))
    maximumSize = new Dimension(Short.MaxValue, 750* ViewConstants.fontScale / 100)
    for(c<-contents)c.xLayoutAlignment = 0d
    xLayoutAlignment=0d
    opaque=false
  }

  override def getPanel: Panel = panel

  override def fieldComponents: Seq[SidePanelComponent[_]] = Seq.empty

  override def allowedClassNames: Iterable[String] = List("Projekt")

  def createGoal(): Unit = {
    val parentProject=  goalList.getSelectedIndex match {
      case -1 => if(projectList.size==1) Some(projectList.head) else None
      case ix => if(ix>=goalListModel.getSize)
        if(projectList.size==1) Some(projectList.head) else None
      else Some(goalListModel.getElementAt(ix).project)
    }
    for(pp<-parentProject) {
      goalDialog.create(pp.dataObjRef)
    }
  }

  def getSelectedGoal: Option[ProjectGoal] =goalList.getSelectedIndex match {
    case -1 => None
    case ix=> Some(goalListModel.getElementAt(ix))
  }


  def editGoal(): Unit =
    for (g<-getSelectedGoal) {
      goalDialog.setLocationRelativeTo(panel)
      goalDialog.open(Some(g))
    }


  def finishGoal(): Unit = {
    for (g<-getSelectedGoal) if(!g.done){
      ClientQueryManager.writeInstanceField(g.ref, 3, BoolConstant(!g.done))
      ClientQueryManager.moveInstances(List(g.ref),OwnerReference(1,g.project.dataObjRef),OwnerReference(2,g.project.dataObjRef),-1)
    }
  }

  def deleteGoal(): Unit =
    for(g<-getSelectedGoal)
      ClientQueryManager.deleteInstance(g.ref)



  def createWork():Unit=
    for(g<-getSelectedGoal)
      workDialog.create(g.ref)


  def getSelectedWork: Option[WorkingStage] =workStageList.getSelectedIndex match {
    case -1 => None
    case ix=> Some(workStageModel.getElementAt(ix))
  }

  def editWork():Unit= {
    for(g<-getSelectedWork) workDialog.open(Some(g))
  }

  def deleteWork():Unit= {
    val refs=workStageList.getSelectionModel.getSelectedIndices.map(workStageModel.getElementAt)
    for(r<-refs)
      ClientQueryManager.deleteInstance(r.ref)
  }

  def finishWork():Unit= {

  }

  panel.peer.addComponentListener(new ComponentAdapter {
    override def componentHidden(e: ComponentEvent): Unit = {
      shutDown()
    }
  })

}
