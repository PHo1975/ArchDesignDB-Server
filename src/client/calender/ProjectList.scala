package client.calender

import client.calender.CalendarHelper._
import client.comm.{ClientQueryManager, UserSettings}
import definition.comm.NotificationType
import definition.data.{InstanceData, Reference}
import javafx.collections.{FXCollections, ObservableList}

import scala.jdk.CollectionConverters._



case class ProjectInfo(ref:Reference,name:String,index:Int,wtModel:WeekTableModel,loadedCallBack:()=>Unit) extends DataSource { 
  def this(data:InstanceData,nindex:Int,nwtModel:WeekTableModel,lcb:()=>Unit)= this(data.ref,data.fieldValue.head.toString,nindex,nwtModel,lcb)
  override def toString: String =name
  
  lazy val folderRef: Reference =  ClientQueryManager.queryInstance(ref,1).headOption match {
    case Some(head)=> head.ref
    case None => throw new IllegalArgumentException ("Cant find folderRef for project "+ref)
  }
  lazy val adressRef: Reference =  ClientQueryManager.queryInstance(ref,0).headOption match {
    case Some(head)=> head.ref
    case None => throw new IllegalArgumentException ("Cant find addressRef for project "+ref)
  }
  def propField=0  
}


class OwnProject(val folderRef:Reference,val wtModel:WeekTableModel,val loadedCallBack:()=>Unit) extends DataSource {  
  def propField=1
  override def toString="Eigene Termine"
  def index=0  
  //override def loadStartReports(startRef:Reference,startMonth:Int,callBack:()=>Unit)= callBack()
  //override def loadEndReports(endRef:Reference,endMonth:Int,callBack:()=>Unit)= callBack()
}


class ProjectList(mod:CalendarModel)  {
  //var projectRoot:Reference=null
  var ownProject:OwnProject=_
  var subsID: Int = -1
  
  val list: ObservableList[DataSource] =FXCollections.observableList[DataSource](new java.util.ArrayList[DataSource])
  
  def load():Unit= {
    val projectRoot= UserSettings.rootRef
    UserSettings.basicFolders.get("Calendar") match {
      case Some(calRef)=> ownProject=new OwnProject(calRef,mod.weekTableModel,()=>{
        subsID=ClientQueryManager.createSubscription(projectRoot, 0)((command,data)=>CalendarHelper.runInFx{
		      command match {
		        case NotificationType.sendData|NotificationType.updateUndo=>
              list.clear()
              list.add(ownProject)
              var index=0
              val projData=data.filter(_.ref.typ==mod.projectType).toList
              if(projData.nonEmpty){
                val lastProj=projData.last
                uniCallBackLoop[InstanceData](projData, (el,callback)=> {
                  list.add(new ProjectInfo(el,{index+=1;index},mod.weekTableModel,callback))
                  if(el.ref==lastProj.ref) util.Log.w("Projects loaded")
                })
              }
            case NotificationType.childAdded|NotificationType.fieldChanged =>
		          val el=new ProjectInfo(data.head,list.size,mod.weekTableModel,()=>{})          
		          list.asScala.indexWhere(_.folderRef==el.folderRef) match{
		            case -1 =>list.add(el) 
		            case ix =>list.get(ix).shutDown();list.set(ix,el)
		          }
		          
		        case NotificationType.instanceRemoved =>
              val el=data.head
              val i=list.asScala.indexWhere { case p: ProjectInfo => p.ref == el.ref; case _ => false}
              if(i>=0) {
                list.get(i).shutDown()
                list.remove(i)
              }
          }
		    })        
      })
      case None=> util.Log.e("Cant find User-Folder 'Calendar' in settings ")
    }   
  }
  
  def findProject(ix:Int): Option[DataSource] = list.asScala find(_.index==ix)

  def findProjektName(ix:Int): String = findProject(ix) match {
    case Some(pr)=> pr.toString
    case _ => "unbekannte id "+ix
  }
  
  def shutDownMonths(): Unit = for(project <-list.asScala)
         project.shutDownMonth()
  
  def shutDown(): Unit = {
    if(subsID> -1) {
       for(project <-list.asScala)
         project.shutDown()
       ClientQueryManager.removeSubscription(subsID)
      subsID=0
    }
  }

}