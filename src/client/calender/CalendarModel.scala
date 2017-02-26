package client.calender

import definition.data.Reference
import definition.typ.SystemSettings
import javafx.scene.Scene
import javafx.scene.control.Tab
import javafx.scene.control.TabPane


class CalendarModel(val window:CalendarWindow) {
  var folderType= -1
  var projectType= -1
  var monthDataType= -1
  var calEventType= -1
  var addressType = -1  
  var dayReportType = -1
 
  
  val projectList=new ProjectList(this)
  val weekTableModel=new WeekTableModel(this)
  val addressForm=new AddressForm(this)
  val reportTab=new Tab("Tagesbericht")  
  val tabPane= new TabPane
  lazy val eventForm=new EventForm(this)
  lazy val reportForm=new DayReportForm(this)
  
  def load()= {   
    folderType=SystemSettings().systemTypes("Folder")
    projectType=SystemSettings().systemTypes("Project")
    monthDataType=SystemSettings().systemTypes("CalendarMonthData")
    calEventType= SystemSettings().systemTypes("CalendarEvent")
    addressType=SystemSettings().systemTypes("Address")
    dayReportType=SystemSettings().systemTypes("DayReport")
    projectList.load()   
  }
  
  def shutDown() = {
    projectList.shutDown()
    weekTableModel.shutDown()
  }

}