package client.calender

import client.calender.CalendarHelper._
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.{DateConstant, IntConstant}


trait DataSource {
  def folderRef:Reference
  def propField:Int
  def index:Int
  val projMonthData=collection.mutable.HashMap[Int,Reference]()
  def loadedCallBack:()=>Unit
  def wtModel:WeekTableModel
  var eventStartSubstID= -1
  var eventEndSubstID= -1
  var reportStartSubstID= -1
  var reportEndSubstID= -1
  var _loadDayReports=true

  
  
  val mdSubsID= ClientQueryManager.createSubscription(folderRef, propField.toByte)((command,data)=> runInFx{
     command match {
       case NotificationType.sendData|NotificationType.updateUndo =>
         projMonthData.clear()
         projMonthData ++= data.map (d=> (d.fieldValue.head.toInt,d.ref))
         //println("getting proj data folder "+folderRef+" data:"++projMonthData.mkString(", ") )
         if(command==NotificationType.sendData) loadedCallBack()
       case NotificationType.childAdded|NotificationType.fieldChanged=>
         val d=data.head
         val theDate=d.fieldValue.head.toInt
         projMonthData(theDate)=d.ref
         for(adate<-wtModel.startDate;if adate == theDate) wtModel.loadMonth()
         for(edate<-wtModel.endDate;if edate == theDate) wtModel.loadMonth()
       case NotificationType.instanceRemoved=>
         val theDate=data.head.fieldValue.head.toInt
         projMonthData.remove(theDate)
         for(adate<-wtModel.startDate;if adate == theDate) wtModel.loadMonth()
         for(adate<-wtModel.endDate;if adate == theDate) wtModel.loadMonth()
     }
  })  
    
    
  def shutDown() = if(mdSubsID>0 ){
    shutDownMonth()
    ClientQueryManager.removeSubscription(mdSubsID)  
    projMonthData.clear()
  }
  
  
  def createMonthData(day:DateConstant):Reference= {
    val inst=ClientQueryManager.createInstance(wtModel.calModel.monthDataType,Array(new OwnerReference(propField,folderRef)))
    val ref=new Reference(wtModel.calModel.monthDataType,inst)
    ClientQueryManager.writeInstanceField(ref, 0, new IntConstant(day.year*100+day.month))
    ref
  }
  
  
  def getMonthParentRef(day:DateConstant)= projMonthData.get(day.year*100+day.month) match {
      case Some(ref)=>ref
      case None=>createMonthData(day)      
    }
  
  
  private def handleEventMonthCommands(month:Int, loadReadyCallback:()=>Unit)
  (command:NotificationType.Value,data:IndexedSeq[InstanceData])= 	
    runInFx{
      command match {
        case NotificationType.sendData|NotificationType.updateUndo =>
          for(d<-data) wtModel.addEvent(d,month,index)
          if(command==NotificationType.sendData) ClientQueryManager.runInPool(loadReadyCallback())
        case NotificationType.childAdded=> wtModel.addEvent(data.head,month,index,true)
        case NotificationType.fieldChanged=>
          val d=data.head
          wtModel.removeEvent(d.ref)
          wtModel.addEvent(d,month,index,true)
        case NotificationType.instanceRemoved=> wtModel.removeEvent(data.head.ref)
      }          
    }
  
  def createDayReport(forDay:DateConstant):Reference= {    
    val parentRef=getMonthParentRef(forDay)
    //println("createDayReport "+forDay + " parentRef")
    val inst=ClientQueryManager.createInstance(wtModel.calModel.dayReportType,Array(new OwnerReference(1,parentRef)))
    val ref=new Reference(wtModel.calModel.dayReportType,inst)
    ClientQueryManager.writeInstanceField(ref,0,new IntConstant(forDay.day))
    ref
  }
   
  def loadMonth(loadDayReports:Boolean,callBack:()=>Unit)= {
    _loadDayReports=loadDayReports
    wtModel.startDate match {
      case Some(startMonth)=>      
       projMonthData.get(startMonth) match {
	      case Some(ref)=> eventStartSubstID=ClientQueryManager.createSubscription(ref,0)( handleEventMonthCommands(startMonth,()=>{
	          loadStartReports(ref,startMonth,callBack) }) )	      
	      case None=> loadEndDate(callBack)
	    }
      case None =>callBack()
    }   
  }
  
  def loadStartReports(startRef:Reference,startMonth:Int,callBack:()=>Unit)= {
    if(_loadDayReports) reportStartSubstID=ClientQueryManager.createSubscription(startRef,1) ( handleDayReportCommands(
        startMonth,wtModel.startDateDayReports,()=>{  loadEndDate(callBack) }) )
	  else loadEndDate(callBack)
  }
  
  def loadEndDate(callBack:()=>Unit)= {    
    wtModel.endDate match{
      case Some(endMonth)=> 
	    projMonthData.get(endMonth) match {
	      case Some (ref)=> 	        
	        eventEndSubstID=ClientQueryManager.createSubscription(ref,0)( handleEventMonthCommands(endMonth,()=>{
	          loadEndReports(ref,endMonth,callBack)}) )	      
	      case None=> callBack()
	    }
      case None=> callBack()
	  }
  }
  
  def loadEndReports(endRef:Reference,endMonth:Int,callBack:()=>Unit)= {
    if(_loadDayReports) {
    	reportEndSubstID=ClientQueryManager.createSubscription(endRef,1)( handleDayReportCommands(endMonth,wtModel.endDateDayReports,()=>{
	         callBack() }) )
    }
	  else callBack()
  }
  
  private def handleDayReportCommands(month:Int,reportList:DayReportList,loadReadyCallback:()=>Unit)
  (command:NotificationType.Value,data:IndexedSeq[InstanceData])= 	
    runInFx{
      command match {
        case NotificationType.sendData|NotificationType.updateUndo =>
          reportList.month=month
          reportList.list=data
          wtModel.getSelectedDate match {
            case Some(wtDate)=> if(wtDate.month==month%100&& wtDate.year==month/100 &&
                reportList.hasReportForDay(wtDate.day)){
                wtModel.reportHasChanged(wtDate,reportList.getReportForDay(wtDate.day))
              }
            case None =>
          }
          if(command==NotificationType.sendData) ClientQueryManager.runInPool(loadReadyCallback())
        case NotificationType.childAdded=>
          val h=data.head
          reportList.addReport(h)
          wtModel.reportHasChanged(reportList.getDate(h),new DayReport(h))
        case NotificationType.fieldChanged=>
          val d=data.head
          reportList.changeReport(d)
          wtModel.reportHasChanged(reportList.getDate(d),new DayReport(d))
        case NotificationType.instanceRemoved=>
          val d=data.head
          reportList.removeReport(data.head.ref)
          wtModel.reportHasChanged(reportList.getDate(d),DayReport.Empty)
      }
  }  
  
  def shutDownMonth()= {
    //projMonthData.clear()
    if(eventStartSubstID > -1) {
      ClientQueryManager.removeSubscription(eventStartSubstID)
      eventStartSubstID= -1
    }
    if(eventEndSubstID > -1){
      ClientQueryManager.removeSubscription(eventEndSubstID)
      eventEndSubstID= -1
    } 
     if(reportEndSubstID > -1){
      ClientQueryManager.removeSubscription(reportEndSubstID)
      reportEndSubstID= -1
    }  
      if(reportEndSubstID > -1){
      ClientQueryManager.removeSubscription(reportEndSubstID)
      reportEndSubstID= -1
    }  
  }
  
}