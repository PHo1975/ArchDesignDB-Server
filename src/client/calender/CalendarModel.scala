package client.calender

import definition.typ.SystemSettings
import javafx.scene.control.{Tab, TabPane}


class CalendarModel(val window:CalendarWindow) {
  var folderType: Int = -1
  var projectType: Int = -1
  var monthDataType: Int = -1
  var calEventType: Int = -1
  var addressType: Int = -1
  var dayReportType: Int = -1
 
  
  val projectList=new ProjectList(this)
  val weekTableModel=new WeekTableModel(this)
  val addressForm=new AddressForm(this)
  val reportTab=new Tab("Tagesbericht")  
  val tabPane= new TabPane
  lazy val eventForm=new EventForm(this)
  lazy val reportForm=new DayReportForm(this)
  
  def load(): Unit = {
    folderType=SystemSettings().systemTypes("Folder")
    projectType=SystemSettings().systemTypes("Project")
    monthDataType=SystemSettings().systemTypes("CalendarMonthData")
    calEventType= SystemSettings().systemTypes("CalendarEvent")
    addressType=SystemSettings().systemTypes("Address")
    dayReportType=SystemSettings().systemTypes("DayReport")
    projectList.load()   
  }
  
  def shutDown(): Unit = {
    projectList.shutDown()
    weekTableModel.shutDown()
  }

}