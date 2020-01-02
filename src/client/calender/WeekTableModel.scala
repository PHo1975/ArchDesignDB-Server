package client.calender

import client.calender.CalendarHelper.handleEvent
import client.comm.ClientQueryManager

import scala.collection.mutable
//import com.sun.javafx.scene.control.skin.LabeledText
import definition.comm.UserInfo
import definition.data.{InstanceData, OwnerReference, Referencable, Reference}
import definition.expression.{BoolConstant, DateConstant, IntConstant, StringConstant}
import javafx.beans.InvalidationListener
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.ListChangeListener.Change
import javafx.collections.{FXCollections, ObservableList}
import javafx.event.ActionEvent
import javafx.geometry.{Pos, Side}
import javafx.scene.Parent
import javafx.scene.control._
import javafx.scene.input.{ClipboardContent, TransferMode}
import javafx.scene.layout.{GridPane, HBox, Priority}
import javafx.scene.text.Text
import util.StrToInt

import scala.collection.immutable.IndexedSeq
import scala.jdk.CollectionConverters._



@SerialVersionUID(24288L) case class CalendarEvent(ref:Reference,day:DateConstant,time:Int,name:String,userID:Int,projID:Int,place:String,done:Boolean) extends Referencable{
  def this(data:InstanceData,date:DateConstant,nProjID:Int)= this(data.ref,date,
      data.fieldValue(2).toInt,data.fieldValue(1).toString,data.fieldValue(3).toInt,nProjID,data.fieldValue(4).toString,data.fieldValue(5).toBoolean)

  override def toString: String = if (name == "") "- / -" else name
}

class DumbObservable[T](val value:T) extends ObservableValue[T]{
  def addListener(listener: ChangeListener[_ >: T]): Unit = {}

  def removeListener(listener: ChangeListener[_ >: T]): Unit = {}

  def addListener(listener: InvalidationListener): Unit = {}

  def removeListener(listener: InvalidationListener): Unit = {}

  def getValue: T = value
}

case class RowData(var hour:Int,var daysInfo:Array[SimpleObjectProperty[Seq[CalendarEvent]]]=
     (for(i<-0 until 6) yield new SimpleObjectProperty[Seq[CalendarEvent]]).toArray ){
   val hourObserv=new DumbObservable(hour match {
     case 0=> "Aufgaben"
     case -1=> ""
     case _=>hour.toString+":00"     
   })

  def update(ndaysInfo: Array[SimpleObjectProperty[Seq[CalendarEvent]]]): RowData = RowData(hour, ndaysInfo)
}

class FirstRowCell extends TableCell[RowData,String] {
  override def updateItem(item: String, empty: Boolean): Unit = {
    super.updateItem(item,empty)
    if(item!=null) {	    
	    setText(item.toString)
	    getStyleClass.add("first-row-cell")
    }
  } 
}
  

class CalendarCell(r:CellInfoReceiver[Seq[CalendarEvent]],mod:CalendarModel) extends EditingTableCell[RowData,Seq[CalendarEvent]](r) {

  def setupLabel(chk: CheckBox, lb: Label, d: CalendarEvent): Unit = {
	  lb.setText(d.name)
	  lb.setTooltip(new Tooltip(d.name+" [ "+UserList.getShortName(d.userID)+" ] Proj: "+mod.projectList.findProjektName(d.projID)))
	  chk.setSelected(d.done)
	  if(d.projID>=0) {
	    val color=CalendarHelper.colors(d.projID % 10)
      lb.setStyle("-fx-background-color: rgb("+(color.getRed*255d).toInt+","+(color.getGreen*255d).toInt+","+(color.getBlue*255d).toInt+");" +
	    		"-fx-text-fill:black;-fx-padding:1;")
	  } 
  }

  def createGrid(data: Seq[CalendarEvent]): GridPane = {
    if(data.size==1 && data.head.projID== -1) null else {
	    val grid=new GridPane() 
	    grid.setHgap(2)
	    val ch=grid.getChildren
	    for(ix<-data.indices;d=data(ix)) {
	      val hb=new HBox
	      val chk=new CheckBox("")
	      chk.setFocusTraversable(false)
	      chk.setPrefWidth(18)
	      chk.setMinWidth(18)
	      val lb=new Label()
	      setupLabel(chk,lb,d)
	      lb.getStyleClass.add("event-label")
	      chk.setOnAction(handleEvent(e=>{
	        ClientQueryManager.writeInstanceField(d.ref, 5, BoolConstant(chk.isSelected))
	      }))
		    grid.getColumnConstraints.add(CalendarHelper.columnConstraint)
	      lb.setMaxWidth(Double.MaxValue)
		    lb.setMaxHeight(Double.MaxValue) 
		    lb.setTextOverrun(OverrunStyle.CLIP)
		    lb.setAlignment(Pos.CENTER_LEFT)
		    HBox.setHgrow(lb, Priority.SOMETIMES)
		    hb.setSpacing(3)
		    hb.getChildren.addAll(chk,lb)
	      GridPane.setConstraints(hb, ix,0)
	      ch.add(hb)    
	    }
	    grid
    }
  }

  override def getString: String = {
    //println("Get String "+r.startEditChar)
    r.startEditChar match {
	    case Some(s)=>
        r.startEditChar=None
        s
      case _=> super.getString
	  }
  }

  def convertToEditString(d: Seq[CalendarEvent]): String = if (d != null) d.head.name else ""

  def createItem(st: String) = List(CalendarEvent(null, DateConstant.NULL_DATE, 0, st, ClientQueryManager.getMyUserId, -1, "", done = false))
  
  def createView(data:Seq[CalendarEvent],oldView:Parent):Parent = if (data==null) null else {
    if(oldView!=null && data.size==1){
      val ch=oldView.asInstanceOf[GridPane].getChildren
      if(ch.size==1) {
        ch.get(0) match {
          case hb:HBox =>
            val hch=hb.getChildren
            if(hch.size==2) setupLabel(hch.get(0).asInstanceOf[CheckBox],hch.get(1).asInstanceOf[Label],data.head)
            oldView
          case _=>
        }
      }
    }
    createGrid(data)
  }

  def isEditable(data: Seq[CalendarEvent]): Boolean = data.size == 1
}



class MyTableColumn[T,S](nid:String,val ix:Int) extends TableColumn[T,S] {
  override def toString: String = "Col " + nid
}



class WeekTableModel(model:CalendarModel) extends CellInfoReceiver[Seq[CalendarEvent]] {
  import client.calender.CalendarHelper._
  
  val firstColWidth=66
  val calModel: CalendarModel = model
  val hourList: ObservableList[RowData] = FXCollections.observableList[RowData](new java.util.ArrayList[RowData])
  var view: TableView[RowData] = _
  var currentProjects:List[DataSource]=Nil
  var visibleUsers:mutable.Seq[Int]=mutable.Seq.empty
  var startDate:Option[Int]=None
  var endDate:Option[Int]=None
  var firstDayOfWeek: DateConstant = _
  var lastDayOfWeek: DateConstant = _
  val firstCol=new MyTableColumn[RowData,String]("Stunde",-1) 
  val jobsList=new Array[List[CalendarEvent]](6)
  var mouseOverColumn: TableColumn[_, Seq[CalendarEvent]] = _
  var mouseOverRow:Int= -1
  var currentItem: Seq[CalendarEvent] = _
  lazy val eventFromAddressMenuItems=Seq("Termin","Telefonat","")
  var startDateDayReports= new DayReportList(0,Seq.empty)
  var endDateDayReports= new DayReportList(0,Seq.empty)
  var openAndShowFormEvent=false  
  //var startEditChar:Option[String]=None
  
  firstCol.setMinWidth(firstColWidth)
  firstCol.setMaxWidth(firstColWidth)
  firstCol.setResizable(false)
  firstCol.setSortable(false)
  
  firstCol.setStyle("-fx-alignment: TOP_RIGHT;")  
  firstCol.setCellValueFactory(cellDataFactory[RowData,String](p=>p.getValue.hourObserv))  
  firstCol.setCellFactory(callback(t=> new FirstRowCell))
  val weekDays: Array[String] =Array("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag")

  def setMouseOverColumn(c: TableColumn[_, Seq[CalendarEvent]]): Unit = mouseOverColumn = c

  def setMouseOverRow(r: Int): Unit = mouseOverRow = r

  def setCurrentItem(i: Seq[CalendarEvent]): Unit = currentItem = i
  
  def createEvent(hour:Int,day:DateConstant,eventName:String):Option[Reference]= {
    if(currentProjects.size==1) {            
    	val parentRef=currentProjects.head.getMonthParentRef(day) 
    
      //println("parent ref:"+parentRef)
      val inst=ClientQueryManager.createInstance(model.calEventType,Array(new OwnerReference(0,parentRef)))
      val ref=new Reference(model.calEventType,inst)
      ClientQueryManager.writeInstanceField(ref, 0, IntConstant(day.day))
      ClientQueryManager.writeInstanceField(ref, 1, StringConstant(eventName))
      if(hour>=7 && hour<20)
        ClientQueryManager.writeInstanceField(ref, 2, IntConstant(hour))
      ClientQueryManager.writeInstanceField(ref, 3, IntConstant(ClientQueryManager.getMyUserId))
      Some(ref)
    }
    else None
  }

  val dayCols: IndexedSeq[MyTableColumn[RowData, Seq[CalendarEvent]]] = weekDays.indices.map(ix => {
    val col=new MyTableColumn[RowData,Seq[CalendarEvent]](weekDays(ix),ix)
    col.setId(ix.toString)
    col.setCellValueFactory(cellDataFactory[RowData,Seq[CalendarEvent]](p=> p.getValue.daysInfo(ix) )) 
    col.setResizable(false)
    col.setSortable(false) 
    col.setEditable(true)
    col.setCellFactory(callback(t=> {new CalendarCell(this,model)}))

    col.setOnEditCommit(handleEvent(e=> {
      val row=e.getRowValue
      //println("set on commit:"+e.getEventType()+" "+e.getNewValue()+ " "+view.isEditable())
      row.daysInfo(ix).getValue match {
        case null =>
          val day=firstDayOfWeek.addDays(ix)
          openAndShowFormEvent=true
          if(visibleUsers.contains(ClientQueryManager.getMyUserId))
            ClientQueryManager.runInPool{
              createEvent(row.hour,day,e.getNewValue.head.name)
          }
        case oldData => ClientQueryManager.writeInstanceField(oldData.head.ref, 1, StringConstant(e.getNewValue.head.name))
      }
    }))  
    col
  })     
  
  def getSelectedEvent:Option[CalendarEvent]= {    
    val sc=view.getSelectionModel.getSelectedCells
    if(sc.size==1) {
      val cell: TablePosition[_, _] =sc.get(0)
      val row: RowData =hourList.get(cell.getRow)
      val cellValues=row.daysInfo(cell.getColumn-1).getValue
      if(cellValues!=null && cellValues.size==1) 
        Some(cellValues.head)
      else None
    }
    else None    
  }


  def initTable(aview: TableView[RowData]): Unit = {
    view=aview
    view.getColumns.add(firstCol)
    view.getColumns.addAll(dayCols:_*)
    view.setEditable(false)   
    for(w<-dayCols) w.prefWidthProperty.bind(view.widthProperty.subtract(firstColWidth+20).divide(6))    
    val selMod=view.getSelectionModel
    selMod.setSelectionMode(SelectionMode.SINGLE)
    selMod.setCellSelectionEnabled(true)
    hourList.addAll((for (i <- 7 to 19) yield RowData(i)).asJavaCollection)
    hourList.add(RowData(0))
    view.setItems(hourList)    
    view.autosize()     
    onListChanged(view.getSelectionModel.getSelectedCells, (e:Change[_<: TablePosition[_,_]])=> if(!e.getList.isEmpty){
      getSelectedEvent match {
        case Some(event) => model.eventForm.loadEvent(event)
        case None => model.eventForm.loadNoEvent()
      }
      loadDayReport()
    }) 
    
    view.setOnKeyPressed(handleEvent(e=> {            
      if(e.getCode.isLetterKey||e.getCode.isDigitKey )
        view.getSelectionModel.getSelectedCells.asScala.headOption match {
	        case Some(a)=>
            //println("Start "+e.getCode().getName)
            startEditChar=Some(e.getCode.getName)
            e.consume()
            view.edit(a.getRow, a.getTableColumn.asInstanceOf[TableColumn[RowData,_]])
          case _=>
	      }
      
    }))
    
    view.setOnDragDetected(handleEvent (e=>{
      getSelectedEvent match {
        case Some(value)=>
          val db = view.startDragAndDrop(TransferMode.ANY:_*)
          val content = new ClipboardContent()
          content.putString(value.toString)
          content.put(CalendarHelper.CalendarDragFormat, value)
          db.setContent(content)
        case None =>
      }      
      e.consume()
   })) 
   
   view.setOnDragOver(handleEvent (event=> {
     if (event.getDragboard.hasContent(CalendarHelper.CalendarDragFormat) &&event.getX>firstColWidth){
       mouseOverColumn match {
         case c:MyTableColumn[_,_]=>
           //println("over y:"+mouseOverRow+" x:"+c.ix+" item:"+currentItem)
           if(currentItem==null|| currentItem.isEmpty ) event.acceptTransferModes(TransferMode.COPY_OR_MOVE:_*)
         case _=>
       }       
     } else if (event.getDragboard.hasContent(CalendarHelper.AddressDragFormat) &&event.getX>firstColWidth){
       event.acceptTransferModes(TransferMode.COPY_OR_MOVE:_*)
     }
     event.consume()
   }))   
   
   view.setOnDragDropped(event=> {
     var success=false
     mouseOverColumn match {       
       case co:MyTableColumn[_,_]=> if(co.ix> -1){
         val newDay=firstDayOfWeek.addDays(co.ix)
         //println("dropped "+co.ix+" "+newDay)
       	 if (event.getDragboard.hasContent(CalendarHelper.CalendarDragFormat) &&event.getX>firstColWidth&& currentItem==null){
           event.getDragboard.getContent(CalendarHelper.CalendarDragFormat) match {
             case cEvent:CalendarEvent =>
               model.projectList.findProject(cEvent.projID) match {
                 case Some(project)=>
                   val oldParentRef=project.getMonthParentRef(cEvent.day)
                   event.getTransferMode match {
		                 case TransferMode.MOVE=>
                       if(cEvent.day!=newDay){
                         ClientQueryManager.writeInstanceField(cEvent.ref, 0, IntConstant(newDay.day))
		                     if(cEvent.day.month!=newDay.month||cEvent.day.year!=newDay.year){ // move to other Monthdata
		                            ClientQueryManager.moveInstances(Seq(cEvent.ref),new OwnerReference(0.toByte,oldParentRef),
		                                  new OwnerReference(0.toByte,project.getMonthParentRef(newDay)), -1)
		                           success=true
		                     }
		                   }
                       if(mouseOverRow>13) {
		                     if(cEvent.time>=7 && cEvent.time<20) {
                           ClientQueryManager.writeInstanceField(cEvent.ref, 2, IntConstant(0)) // change to job
		                         success=true
		                       }
		                   } else {
		                     val newTime=mouseOverRow+7
                         if (newTime != cEvent.time) ClientQueryManager.writeInstanceField(cEvent.ref, 2, IntConstant(newTime))
		                     success=true
		                   }
                     case TransferMode.COPY=>ClientQueryManager.runInPool{
		                   val inst=ClientQueryManager.createInstance(model.calEventType, Array(new OwnerReference(0,project.getMonthParentRef(newDay))))
		                   val newRef=new Reference(model.calEventType,inst)
                       ClientQueryManager.writeInstanceField(newRef, 0, IntConstant(newDay.day))
                       if (cEvent.name.length > 0) ClientQueryManager.writeInstanceField(newRef, 1, StringConstant(cEvent.name))
                       ClientQueryManager.writeInstanceField(newRef, 3, IntConstant(cEvent.userID))
                       if (cEvent.place.length > 0) ClientQueryManager.writeInstanceField(newRef, 4, StringConstant(cEvent.place))
		                   if(mouseOverRow<14)
                         ClientQueryManager.writeInstanceField(newRef, 2, IntConstant(mouseOverRow + 7))
		                 }
		                 case _=>
		               }
                 case _=>
               }
           }                
	       } else if (event.getDragboard.hasContent(CalendarHelper.AddressDragFormat) &&event.getX>firstColWidth){
		       event.getDragboard.getContent(CalendarHelper.AddressDragFormat) match {
		         case add:Address=> if(currentItem==null||currentItem.isEmpty){ // address dragged to an empty cell		           
		            val buttons= eventFromAddressMenuItems.map(new MenuItem(_))
		            val callBack=handleEvent[ActionEvent](e=> {
		              e.getSource match {
		                case m:MenuItem=> ClientQueryManager.runInPool{
		                  createEvent(if(mouseOverRow<14) mouseOverRow+7 else 0,newDay,m.getText+" "+add.name) match {
		                    case Some(eventRef)=>
                          ClientQueryManager.secondUseInstances(List(add.ref), add.owner, new OwnerReference(1,eventRef), -1)
                        case None =>
		                  }
		                } 
		              }
		            })
		            for(b<-buttons) b.setOnAction(callBack)
		            new ContextMenu(buttons :_*).show(view,Side.LEFT,event.getX+100,event.getY)
		         } else { // address dragged on an existing event
		           ClientQueryManager.secondUseInstances(List(add.ref), add.owner,new OwnerReference(1,currentItem.head.ref), -1)
		         } 
		         case o=> util.Log.e("other content "+o)
		       }
		     }
       }
       case _=>
     }
     event.setDropCompleted(success)     
     event.consume()
   })
    
   onChanged(model.tabPane.getSelectionModel.selectedIndexProperty(), (b:Number, a:Number)=>{
     if(b.intValue()==0&&a.intValue()==1) {
       //println("DayReport opened")
       loadDayReport()
     }
   }) 
  }

  def createAddress(): Any = {
    (model.window.treeView.getSelectionModel.getSelectedItem match {
      case p:ProjectTreeItem =>None
      case f:FolderTreeItem =>Some(f.getValue.ref)      
      case a:MyTreeItem[_] => a.getParent match{
        case p:ProjectTreeItem => None
        case o=>Some(o.getValue.ref)
      }
      case _=>None
      }) match {
        case Some(folderRef)=>
          val inst=ClientQueryManager.createInstance(calModel.addressType, Array(new OwnerReference(1,folderRef)))
          ClientQueryManager.writeInstanceField(Reference(calModel.addressType, inst), 1, StringConstant("-leer-"))
        case None =>FxMessageBox.showMessage("Zum Anlegen einer Adresse erst Adress-Gruppe auswählen",
            "Adresse anlegen", model.window.primaryStage)
      }
    }

  def deleteAddress(): Unit = {
    (model.window.treeView.getSelectionModel.getSelectedItem match {
      case _:ProjectTreeItem =>None
      case _:FolderTreeItem =>None
      case a:MyTreeItem[_] => a.getParent match{
        case _:ProjectTreeItem => None
        case _=>Some(a.getValue)
      }
      case _=>None
      }) match {
        case Some(adress)=>
          FxMessageBox.showMessage("Adresse '"+adress.name+"' wirklich löschen ?","Adresse löschen",model.window.primaryStage,
              Seq("Ok","Abbruch")) match {
            case Some("Ok")=>ClientQueryManager.deleteInstance(adress.ref)
            case _=>
          }
        case None =>
      }
    }


  def initProjectListView(aview: ListView[DataSource], treeView: TreeView[AdTreeNode]): Unit = {
    onListChanged[DataSource](aview.getSelectionModel.getSelectedItems, c=>{
      c.getList match {
        case od:ObservableList[_]=>
          setProjects(od.toArray.toList.asInstanceOf[List[DataSource]])
          treeView.getRoot match {
            case null =>
            case root:MyTreeItem[_]=> root.shutDown();treeView.setRoot(null)
            case o=>util.Log.e("Unknown root type "+o)
          }
          view.setVisible(od.size>0)
          if(od.size()==1) {
            od.get(0) match {
              case pr:ProjectInfo =>
                val root=new ProjectTreeItem(new Folder(pr.adressRef,pr.name,""))
                treeView.setRoot(root)
                root.setExpanded(true)
              case other =>
            }
          }
        case _=>
      }      
    })
    aview.setOnDragOver(handleEvent(e=>{
      if( e.getDragboard.hasContent(CalendarDragFormat)) e.acceptTransferModes(TransferMode.COPY_OR_MOVE:_*)
      e.consume()
    }))
    
    aview.setOnDragDropped(handleEvent(e=>{
    if( e.getDragboard.hasContent(CalendarDragFormat)) {
      e.getDragboard.getContent(CalendarDragFormat) match {
        case ce:CalendarEvent =>    
          e.getTarget match {
            case t:Text =>t.getParent.getId match {
              case StrToInt(prjID)=> model.projectList.findProject(prjID) match {
                case Some(targetProject)=> 
                  e.getTransferMode match {
                    case TransferMode.MOVE=>
                      val thisProject=model.projectList.findProject(ce.projID).get
                      ClientQueryManager.moveInstances(List(ce.ref), new OwnerReference(0, thisProject.getMonthParentRef(ce.day)),
                          new OwnerReference(0,targetProject.getMonthParentRef(ce.day)), -1)
                    case _=>
                  }                
                case _=>
              }
              case _=>
            }           
            case _=>
          }
                  
        case _=>
      }  
    }
  }))
  }

  def initUserListView(aview: ListView[UserInfo]): Unit = {
    onListChanged[UserInfo](aview.getSelectionModel.getSelectedItems, c=>{
      c.getList match {
        case od:ObservableList[_]=>
          visibleUsers = od.asScala map (_.id)
          view.setEditable(visibleUsers.contains(ClientQueryManager.getMyUserId))
          loadMonth()
        case _=>
      }      
    })     
  }

  def initAddressTreeView(treeView: TreeView[AdTreeNode]): Unit = {
    treeView.setOnDragDetected(handleEvent(e=>{
      val selectedItem=treeView.getSelectionModel.getSelectedItem match {
        case tr:MyTreeItem[_]=> tr.getValue match {
          case ad:Address=>
            val db=treeView.startDragAndDrop(TransferMode.ANY:_*)
            val content = new ClipboardContent()
            content.putString(ad.toString)
            content.put(CalendarHelper.AddressDragFormat, ad)
            db.setContent(content)
          case _=>
        }
        case _=>
      }
      e.consume()
    }))
    
  }


  def shutDownMonth(): Unit = {
    openAndShowFormEvent=false
    model.projectList.shutDownMonths()
    clearSheet()
  }

  def shutDown(): Unit = {
    view.setEditable(false)
    shutDownMonth()
    clearSheet()
  }


  def setProjects(nproj: List[DataSource]): Unit = {
    shutDown()    
    currentProjects=nproj
    view.setEditable(currentProjects.size==1)
    if (currentProjects.size == 1) model.reportTab.setDisable(getSelectedDate.isEmpty)
    else {
      model.reportTab.setDisable(true)
      model.tabPane.getSelectionModel.select(0)
      model.reportForm.loadReport(None)
    }
    //println("load project:"+nproj+" ")
    loadMonth()
  }


  def clearSheet(): Unit = {
    for (hour <- hourList.iterator().asScala; days <- hour.daysInfo)
      days.setValue(null)
    for(i<-0 until 6) jobsList(i)=Nil
    hourList.remove(14, hourList.size)  
    startDateDayReports.list=Seq.empty
    endDateDayReports.list=Seq.empty
  }
  
  private def replaceIfSame[T <: Referencable](list:List[T],pattern:T):List[T]= {
    if(list==null  || list.isEmpty) pattern::Nil
    else if(list.head.ref==pattern.ref) pattern::list.tail
    else list.head::replaceIfSame(list.tail,pattern)
  }
  
  private def isVisibleDate(day:DateConstant)=day.julian>=firstDayOfWeek.julian&&day.julian<=lastDayOfWeek.julian 
  
  def dateFromDB(d:InstanceData,startMonth:Int): DateConstant = DateConstant(d.fieldValue.head.toInt,startMonth%100,startMonth/100)

  def addEvent(d: InstanceData, startMonth: Int, projID: Int, forceRedraw: Boolean = false): Unit = {
    val day=dateFromDB(d,startMonth)   
    if(isVisibleDate(day)){
      val event=new CalendarEvent(d,day,projID)
      if(visibleUsers.contains(event.userID)) {        
        val dayColumn=(day.julian-firstDayOfWeek.julian).toInt
	      if(event.time>6&&event.time<20) {
          val prop = hourList.asScala(event.time - 7).daysInfo(dayColumn)
	        val newValue=if(prop.getValue==null) List(event) else replaceIfSame(prop.getValue.toList,event)
	        prop.setValue(newValue)
	      } else {	 // job	        
	        val newList=if(jobsList(dayColumn).exists(_.ref==d.ref)) replaceIfSame(jobsList(dayColumn),event) else event::jobsList(dayColumn)	        
	        jobsList(dayColumn)=newList
          while (hourList.size < 14 + newList.size) hourList.add(RowData(-1))
          hourList.asScala(newList.size + 12).daysInfo(dayColumn).setValue(List(event))
	      }
        if(forceRedraw){          
          view.setItems(null)
          view.setItems(hourList)          
        }
        if(openAndShowFormEvent) {
          openAndShowFormEvent=false
          showEventForm(event)
        }
      }      
    }
  } 
  
  private def showEventForm(event:CalendarEvent): Unit = {
    if(model.tabPane.getSelectionModel.getSelectedIndex!=0) model.tabPane.getSelectionModel.select(0)
    model.eventForm.loadEvent(event)   
  }

  def removeEvent(ref: Reference): Unit = for (hour <- hourList.asScala; days <- hour.daysInfo; cell = days.getValue
                                               if cell != null; event <- cell; if event.ref == ref) {
      val newValue=cell.filterNot(_.ref==ref)
      days.setValue(if(newValue.isEmpty)null else newValue)
      //println("Remove event "+event+" "+event.ref+" time:"+event.time+" day:"+event.day+ " " )
      view.setItems(null)
      view.setItems(hourList)   
  }


  def loadMonth(): Unit = {
    shutDownMonth()
    if(currentProjects.nonEmpty)
	    for(startMonth<-startDate){       
	       if(currentProjects.size==1)
           currentProjects.head.loadMonth(loadDayReports = true, ()=>{
           CalendarHelper.runInFx{loadDayReport()}})
	       else uniCallBackLoop[DataSource](currentProjects,(el,callback)=> {
	         el.loadMonth(loadDayReports = false,callback)
         })
	    }   
  }


  def setCurrentWeek(weekDate: DateConstant): Unit = {
    shutDownMonth()       
    firstDayOfWeek=weekDate.addDays(/*if(weekDate.weekDay==1)-6 else*/ -weekDate.weekDay)
    lastDayOfWeek=firstDayOfWeek.addDays(6)
    startDate=Some(firstDayOfWeek.year*100+firstDayOfWeek.month)
    endDate=if(lastDayOfWeek.month!=firstDayOfWeek.month)Some(lastDayOfWeek.year*100+lastDayOfWeek.month) else None 
    val holidays=model.window.calendar.cals(1).holidays
    //println(holidays.keys.toSeq.sortBy(_.dayInYear).map(key=>key+" "+holidays(key)).mkString("\n"))
    for(cix<-dayCols.indices;col=dayCols(cix);day=firstDayOfWeek.addDays(cix)) {
      col.setText(if(holidays.contains(day)) holidays(day) else weekDays(cix) + " " + day.toDateString)
      //if(holidays.contains(day))col.setStyle("-fx-text-fill: #CCFF99")      
    }
    if(currentProjects.nonEmpty) loadMonth()
  }


  def reportHasChanged(day: DateConstant, report: DayReport): Unit = {
    getSelectedDate match {
      case Some(date)=> if(date==day) model.reportForm.loadReport(Some(report))
      case None =>
    }
  }
  
  
  def getSelectedDate:Option[DateConstant]= {
    val sc=view.getSelectionModel.getSelectedCells
    if(sc.size>0&&firstDayOfWeek!=null) Some(firstDayOfWeek.addDays(sc.get(0).getColumn-1))
    else None
  }


  def loadDayReport(): Unit = if (currentProjects.size == 1) {
    //println("load day report "+" "+startDateDayReports.month+" "+startDateDayReports.list.mkString(" | ")+" selDate:"+ getSelectedDate)
    getSelectedDate match {
      case Some(date)=>
        model.reportTab.setDisable(false)
        if( model.tabPane.getSelectionModel.getSelectedIndex==1){
          if(startDateDayReports.month%100==date.month&& startDateDayReports.month/100==date.year &&
              startDateDayReports.hasReportForDay(date.day)) model.reportForm.loadReport(Some(startDateDayReports.getReportForDay(date.day)))
          else if(endDateDayReports.month%100==date.month&& endDateDayReports.month/100==date.year &&
              endDateDayReports.hasReportForDay(date.day)) model.reportForm.loadReport(Some(endDateDayReports.getReportForDay(date.day)))
          else model.reportForm.loadReport(None)
        }
      case None=> model.reportForm.loadReport(None)	    
    }
  } else { 
    model.reportTab.setDisable(true)
    model.tabPane.getSelectionModel.select(0)
  }
  
  def createDayReport():Option[Reference] = {
    //println("Create Day Report "+getSelectedDate+" "+currentProjects.size)
    if(currentProjects.size==1) 
    	getSelectedDate map(date => currentProjects.head.createDayReport(date))
    else None  
  }
  
}