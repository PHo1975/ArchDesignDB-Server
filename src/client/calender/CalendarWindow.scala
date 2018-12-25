package client.calender


import java.net.{ConnectException, InetAddress}

import client.comm.{ClientQueryManager, ClientSocket, UserSettings}
import definition.comm._
import definition.expression.DateConstant
import javafx.application.{Platform, Application => FxApplication}
import javafx.geometry.Insets
import javafx.scene.Scene
import javafx.scene.control.TabPane.TabClosingPolicy
import javafx.scene.control._
import javafx.scene.layout.{HBox, Priority, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.stage.Stage

import scala.util.control.NonFatal


class ProjectCell extends ListCell[DataSource] {
  override def updateItem(item:DataSource,empty:Boolean)= {
    super.updateItem(item,empty)
    if(item!=null) {
	    val rect=new Rectangle(20,20)
	    rect.setFill(CalendarHelper.colors(item.index % 10))
	    rect.setStroke(Color.WHITE)
	    setGraphic(rect)
	    setText(item.toString)
	    getStyleClass().add("ProjectCell")
	    setId(item.index.toString)
    }
  } 
}


class WatchCheck[T](t:String,lv:ListView[T]) extends CheckBox(t) {
  
  var selfChanged=false
  import client.calender.CalendarHelper._
  onChanged[java.lang.Boolean](selectedProperty(),(o,n)=> if(!selfChanged){
      if(n) lv.getSelectionModel().selectAll()
      else lv.getSelectionModel.clearSelection()
    })
  onListChanged[Integer](lv.getSelectionModel().getSelectedIndices, c=> {
      selfChanged=true
      if(c.getList.size()==lv.getItems.size) setSelected(true)
      else setSelected(false)
      selfChanged=false
    })
}

class CalendarWindow extends FxApplication  {
  import client.calender.CalendarHelper._
  var primaryStage:Stage=_
  var model=new CalendarModel(this)  
  var sock:ClientSocket=_
  var calendar:CalendarGroup=_
  val treeView=new TreeView[AdTreeNode]
  var allUsersCheck:CheckBox=_//=new CheckBox("alle Benutzer")
  
  def setupStage(pStage:Stage): Unit ={
    import client.calender.CalendarHelper._
    primaryStage=pStage
    primaryStage.setTitle("Kalender")    
    primaryStage.getIcons.add(loadImage("calendar_64.png"))
    val treeViewWidth=260d    
    val c= new Button("Du da !")        
    val root=new VBox   
    val scene=new Scene(root,410,550)
    primaryStage.setScene(scene)
    val pvbox=new VBox    
    val eventFormTab=new Tab("Ereignis")  
    //var projListSelfChanged=false
    //var userListSelfChanged=false
    
    CalendarHelper.addressTreeView=treeView
    pvbox.setSpacing(10)
    val projectListView=new ListView[DataSource](model.projectList.list)
    projectListView.setCellFactory(callback(t=> new ProjectCell()  ))    
    projectListView.setPrefSize(310, 190)    
    projectListView.setFocusTraversable(false) 
    projectListView.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    
    model.weekTableModel.initProjectListView(projectListView,treeView)
    val allProjectCheck=new WatchCheck("alle Projekte",projectListView)
    allProjectCheck.setTooltip(new Tooltip("Alle Projekte auswählen"))
    allProjectCheck.setFocusTraversable(false)
    
    val prLab=new Label("Projekte:")
    prLab.setTooltip(new Tooltip("Sichtbare Projekte auswählen.\nMehrfachauswahl mit [Strg]+Click"))
    pvbox.getChildren.addAll(prLab,projectListView,allProjectCheck)    
    
    val pvBox2=new VBox
    pvBox2.setSpacing(10)
    val userListView=new ListView[UserInfo](UserList.list)
    userListView.setCellFactory(callback(t=> new UserCell()))
    userListView.setPrefSize(110, 190)
    userListView.setFocusTraversable(false)
    userListView.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    model.weekTableModel.initUserListView(userListView) 
    allUsersCheck=new WatchCheck("alle Benutzer",userListView)
    allUsersCheck.setTooltip(new Tooltip("Alle Benutzer auswählen"))
    allUsersCheck.setFocusTraversable(false)
    
    val userLab=new Label("Benutzer:")
    userLab.setTooltip(new Tooltip("Ereignisse von gewählten Benutzern filtern.\nMehrfachauswahl mit [Strg]+Klick)"))
    pvBox2.getChildren.addAll(userLab,userListView,allUsersCheck)    
    calendar=new CalendarGroup(model)  
      
    val topHBox=new HBox
    topHBox.setSpacing(20)    
    topHBox.getChildren.addAll(pvbox,pvBox2,calendar)    
    val tableView=new EditingTableView[RowData]    
    tableView.setId("WeekTable")
    tableView.setPrefHeight(520)
    tableView.setVisible(false)
    model.weekTableModel.initTable(tableView)
    onChanged[DateConstant](calendar.currentWeekProperty,(o,n)=>model.weekTableModel.setCurrentWeek(n))    
    val secondRowHBox=new HBox
    HBox.setHgrow(tableView, Priority.ALWAYS)
    treeView.setPrefWidth(treeViewWidth)   
    treeView.setShowRoot(true)
    treeView.setFocusTraversable(false)
    treeView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
    VBox.setVgrow(treeView,Priority.ALWAYS)    
    val treeViewVBox=new VBox
    treeViewVBox.setSpacing(10)
    val prAdLab=new Label("Projekt-Adressen:")
    prAdLab.setTooltip(new Tooltip("Projektadressen: \nGruppen durch Klick auf schwarzen Pfeil öffnen.\n"+
        "Adressen können mit Maus auf Kalendereinträge oder Tagesberichte gezogen werden"))
    val addAddressBut=new Button("Adresse anlegen")
    addAddressBut.setFocusTraversable(false)
    addAddressBut.setOnAction(handleEvent(e=>{model.weekTableModel.createAddress()}))
    val delAddressBut=new Button("löschen")
    delAddressBut.setOnAction(handleEvent(e=>{model.weekTableModel.deleteAddress()}))
    delAddressBut.setFocusTraversable(false)
    val addButtonBox=new HBox()
    addButtonBox.getChildren.addAll(addAddressBut,delAddressBut)
    treeViewVBox.getChildren.addAll(prAdLab,treeView,addButtonBox)
    model.weekTableModel.initAddressTreeView(treeView)
    secondRowHBox.setSpacing(5)
    val spacingBox1=new VBox
    spacingBox1.setMinWidth(12)
    spacingBox1.setStyle("-fx-background-color: linear-gradient(to right, lightgray, white);")
    secondRowHBox.getChildren.addAll(treeViewVBox,spacingBox1,tableView)
    val thirdRowHBox=new HBox
    thirdRowHBox.setSpacing(5)
    val spacingBox2=new VBox
    spacingBox2.setMinWidth(12)
    spacingBox2.setStyle("-fx-background-color: linear-gradient(to right, lightgray,white);")
    model.addressForm.setMinWidth(treeViewWidth)
    model.addressForm.setPrefWidth(treeViewWidth)
    HBox.setHgrow(model.tabPane, Priority.ALWAYS)
    eventFormTab.setContent(model.eventForm.form)
    model.reportTab.setContent(model.reportForm.form)
    model.reportTab.setDisable(true)
    model.tabPane.getTabs.addAll(eventFormTab,model.reportTab)
    model.reportTab.setTooltip(new Tooltip("Zur Anzeige des Tagesberichts muss EIN Projekt und ein Tag ausgewählt sein"))
    model.tabPane.setTabClosingPolicy(TabClosingPolicy.UNAVAILABLE)
    model.tabPane.getStyleClass.add(TabPane.STYLE_CLASS_FLOATING)
    thirdRowHBox.getChildren.addAll(model.addressForm,spacingBox2,model.tabPane)    
    onChanged[TreeItem[AdTreeNode]](treeView.getSelectionModel.selectedItemProperty(), (o, n)=> {
      n match {
        case m:MyTreeItem[_]=> m.getValue match {
          case ad:Address=>model.addressForm.loadAdress(ad)
          case p:ProjectTreeItem=> model.addressForm.loadAdress(null)           
          case ob=>model.addressForm.loadAdress(ob)
        }
        case _=>
      }
    })
    model.weekTableModel.setCurrentWeek(calendar.currentWeekProperty.get)
    val spacingBox3=new VBox
    spacingBox3.setMinHeight(17)  
    root.getChildren.addAll(topHBox,spacingBox3,secondRowHBox,thirdRowHBox)    
    root.setSpacing(0)    
    root.setPadding(new Insets(5,10,10,10))
    root.getStylesheets.add("client/calender/calendarComp.css")
    scene.setFill(Color.rgb(250,250,250,0.9))    	
  }
  
  
  override def start(pStage:Stage): Unit = {
    val args=getParameters.getRaw
    val serverName=args.get(0)
    try {
			sock=new ClientSocket(InetAddress.getByName(serverName),args.get(1).toInt,args.get(2),args.get(3),"Cal",false)
	  } catch { case c:ConnectException =>
      println("Kann Server an Adresse "+serverName+" nicht finden.")
      runInFx{FxMessageBox.showMessage("Kann Server an Adresse "+serverName+" nicht finden.", "Kalender", primaryStage)}
      Platform.exit()
    case NonFatal(e) =>
      println("Fehler beim Verbinden:"+e.toString)
      runInFx{FxMessageBox.showMessage("Fehler beim Verbinden:"+e.toString,"Kalender",primaryStage);}
      e.printStackTrace()
      Platform.exit()
    case other:Throwable =>println(other);System.exit(0);null
    }
	  if(sock!=null){ 
      sock.connectionBrokenListener=()=>{stop()}
	    sock.classesReadListener= ()=>{
        println("Calendar settings")
			  ClientQueryManager.setClientSocket(sock)  
			  ClientQueryManager.registerSetupListener (() => {
			    CalendarHelper.runInFx{
			      setupStage(pStage)
				    val windowWidth=UserSettings.getIntProperty("Calendar","WindowWidth")
						val windowHeight=UserSettings.getIntProperty("Calendar","WindowHeight")
						if(windowWidth>0&& windowHeight>0) {
						  val windowX=UserSettings.getIntProperty("Calendar","WindowXPos")
						  val windowY=UserSettings.getIntProperty("Calendar","WindowYPos")					  
						  primaryStage.setWidth(windowWidth)
						  primaryStage.setHeight(windowHeight)
						  primaryStage.setX(windowX)
						  primaryStage.setY(windowY)						  
						}
				    UserList.load()
				    allUsersCheck.setSelected(true)
				    model.load()
				    primaryStage.show()
					}
          println("Calendar settings done")
			  })
	    }
		  sock.start()
	  }
  }
  
  override def stop(): Unit = {
    if(sock!=null){
      saveWindowPositions()
      model.shutDown()
      ClientQueryManager.setClientSocket(sock)
      sock.quitApplication()
    }  
  }
  
  def saveWindowPositions(): Unit = {
	  //val defaultGroup=new PropertyGroup("Calendar",new collection.mutable.HashMap[String,PropertyValue]())  
	  UserSettings.setIntProperty("Calendar","WindowWidth",primaryStage.getWidth().toInt)
	  UserSettings.setIntProperty("Calendar","WindowHeight",primaryStage.getHeight().toInt)
	  UserSettings.setIntProperty("Calendar","WindowXPos",primaryStage.getX().toInt)
	  UserSettings.setIntProperty("Calendar","WindowYPos",primaryStage.getY().toInt)	 
	}
  
}