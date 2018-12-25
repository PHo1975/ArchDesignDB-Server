package client.layout


import client.comm.UserSettings
import client.ui.ClientApp
import definition.comm.{PropertyGroup, PropertyValue}

import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked}



class WorkplaceDialog (w:Window) extends Dialog(w) {
  val openBut=new Button("öffnen")
  val cancelBut=new Button("Abbrechen")
  val newBut=new Button("Neu Anlegen...")
  val deleteBut=new Button("Löschen")
  val listView=new ListView[String]()  
  var openListener:(PropertyGroup)=>Unit = null
  
  
  val mainPanel=new BorderPanel(){
    listView.selection.intervalMode=ListView.IntervalMode.Single
		add(new ScrollPane () {
			viewportView = listView
		},BorderPanel.Position.Center)
		add(new BoxPanel(scala.swing.Orientation.Horizontal){
	    contents+=openBut+=newBut+=deleteBut+= cancelBut
		},BorderPanel.Position.South)
		listenTo(openBut,newBut,deleteBut,cancelBut,listView.mouse.clicks)
		reactions += {
			case ButtonClicked(`openBut`)=> openWorkplace()
			case ButtonClicked(`newBut`)=> newWorkplace()
			case ButtonClicked(`deleteBut`)=> deleteWorkplace()
			case ButtonClicked(`cancelBut`)=>cancel()
			case e:MouseClicked=> if(e.clicks==2) openWorkplace()
		}
		
	}  
  
  preferredSize=new Dimension(500,400)  
  modal=true
  title="Arbeitsflächen"
  contents=mainPanel
  
  
  def openWorkplace():Unit = {
    for(name<-selectedWorkplace;pgroup<-getPropertyGroup(name))      
      openListener(pgroup)      
    close()
  } 
  
  
  def newWorkplace():Unit = {
  		val name=Dialog.showInput[String](parent=mainPanel,title="Neue Arbeitsfläche erstellen",message="Name der Arbeitsfläche:",initial="")
  		for(n<-name) {
  			val list=UserSettings.getListProperty[PropertyGroup]("WindowSettings","Boxes") 

  			if(!list.exists(_.name==n)) {
  				val pg=new PropertyGroup(n,new collection.mutable.HashMap[String,PropertyValue]())
  				ClientApp.mainBox.storeSettings(pg)
  				val newList=pg +:list
  				UserSettings.setListProperty[PropertyGroup]("WindowSettings","Boxes", newList)
  				updateListView()
  			} else util.Log.e("Arbeitsfläche "+n+" schon vorhanden")
  		}
  		close()
  }
  
  def deleteWorkplace():Unit = {
    for(name<-selectedWorkplace) {
      val list=UserSettings.getListProperty[PropertyGroup]("WindowSettings","Boxes") 
      val newList=list.filterNot(_.name==name)
      UserSettings.setListProperty[PropertyGroup]("WindowSettings","Boxes",newList)
      updateListView()
    }
  } 
  
  def cancel():Unit = {
    openListener=null    
    close()
  }
  
  private def selectedWorkplace:Option[String] = {
    listView.selection.items.headOption    
  }
  
  private def getPropertyGroup(name:String)= {
    UserSettings.getListProperty[PropertyGroup]("WindowSettings","Boxes").find(_.name==name)
  }
  
  private def updateListView() = {
    val settingsList=UserSettings.getListProperty[PropertyGroup]("WindowSettings","Boxes").filter(_.name!="Default")
    listView.listData_=(settingsList.map(_.name))
  }
  
  def showWorkplaces(openBMListener:(PropertyGroup)=>Unit)= {    
    openListener=openBMListener
    updateListView()
    visible=true
  }
}