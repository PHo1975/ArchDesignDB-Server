package client.dataviewer.todolist

import client.comm.{ClientQueryManager, ListDataModel}
import definition.data.{InstanceData, Referencable, Reference}
import definition.expression.DateConstant

class ProjectGoalsModel(prRef:Reference,val prName:String,val dataObjRef:Reference,editor:ProjectFieldEditor)  {
  val dataList:ListDataModel[ProjectGoal]=new ListDataModel(List(171),data=>new ProjectGoal(data,this)){
    override def fireContentsChanged(source: Any, index0: Int, index1: Int): Unit = {
        super.fireContentsChanged(source, index0, index1)
        editor.goalListModel.update()
      }

    override def fireIntervalAdded(source: Any, index0: Int, index1: Int): Unit = {
      super.fireIntervalAdded(source, index0, index1)
      editor.goalListModel.update()
      editor.goalList.setSelectedIndex(editor.goalListModel.getSize-1)
      editor.openWorkStages()
      editor.createWorkBut.enabled = true
    }

    override def fireIntervalRemoved(source: Any, index0: Int, index1: Int): Unit = {
      super.fireIntervalRemoved(source, index0, index1)
      editor.goalListModel.update()
      editor.openWorkStages()
    }
  }
  dataList.load(dataObjRef,1,editor.goalListModel.update)
  override def toString: String = prName
}



case class ProjectGoal(ref:Reference,name:String,bemerkung:String,erledigt:DateConstant,done:Boolean,project:ProjectGoalsModel) extends Referencable{
  def this(data:InstanceData,prj:ProjectGoalsModel)=this(data.ref,data.fieldValue(0).toString,data.fieldValue(1).toString,data.fieldValue(2).toDate,data.fieldValue(3).toBoolean,prj)
  override def toString=(if(done)"(v) "else "   ")+ name
}


case class WorkingStage(ref:Reference,name:String,description:String,status:String,doneAt:DateConstant) extends Referencable {
  def this(data:InstanceData)=this(data.ref,data.fieldValue(0).toString,data.fieldValue(1).toString,data.fieldValue(2).toString,data.fieldValue(3).toDate)

  override def toString: String = name
}



object ProjectGoalsModel {
  def loadProject(prRef:Reference,editor:ProjectFieldEditor)= {
    val data=ClientQueryManager.queryInstance(prRef,-1).headOption match {
      case Some(d)=>d
      case None => throw new IllegalArgumentException("No projectdata found "+prRef)
    }
    val dataObjRef=ClientQueryManager.queryInstance(prRef,1).headOption match {
      case Some(dr)=>dr
      case None => throw new IllegalArgumentException("No project info data found in project "+prRef)
    }
    new ProjectGoalsModel(prRef,data.fieldValue(0).toString,dataObjRef.ref,editor)
  }
}

