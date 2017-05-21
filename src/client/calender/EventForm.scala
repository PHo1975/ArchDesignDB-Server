package client.calender

import javafx.collections.{FXCollections, ObservableList}
import javafx.geometry.{HPos, Insets}
import javafx.scene.control._
import javafx.scene.control.cell.TextFieldListCell
import javafx.scene.input.TransferMode
import javafx.scene.layout.{ColumnConstraints, GridPane, Priority}

import client.calender.CalendarHelper._
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data.{EMPTY_REFERENCE, OwnerReference, Reference}
import definition.expression.{BoolConstant, StringConstant}

import scala.collection.JavaConverters._

class EventForm(mod:CalendarModel) {
   var eventSubsID: Int = -1
   var contactsSubsID: Int = -1
   var protocollSubsID: Int = -1
   var currentEvent:Option[CalendarEvent]= None
   val contactsList: ObservableList[Address] =FXCollections.observableList[Address](new java.util.ArrayList[Address])
   val protocollList: ObservableList[Folder] =FXCollections.observableList[Folder](new java.util.ArrayList[Folder])
   
   val form=new GridPane
   form.setHgap(10)
   form.setVgap(5)
   form.setPrefHeight(160d)  
   form.setPadding(new Insets(10))
   form.setVisible(false)
   //form.setGridLinesVisible(true)
   val eventTextField=new ActiveField((text)=>currentEvent match {
     case Some(event)=>  ClientQueryManager.writeInstanceField(event.ref, 1, StringConstant(text))
     case None =>
   })
   GridPane.setConstraints(eventTextField, 0, 0,2,1)
  val userLabel = new Label("Beteiligte:")
   userLabel.setTooltip(new Tooltip("Beteiligte können aus den Projekt-Adressen mit der Maus\nin diese Liste gezogen werden"))
   GridPane.setConstraints(userLabel, 0, 1)   
   val userListView=new ListView[Address]
   userListView.setItems(contactsList)
   userListView.setFocusTraversable(false)
   userListView.setOnDragOver(handleEvent (event=> 
     if(event.getDragboard().hasContent(CalendarHelper.AddressDragFormat)&&currentEvent.isDefined){       
	     event.getDragboard.getContent(CalendarHelper.AddressDragFormat) match {
	       case add:Address => if(!contactsList.asScala.exists(_.ref==add.ref)){
	      	 event.acceptTransferModes(TransferMode.COPY_OR_MOVE:_*)	 
	      	 event.consume()
	       } 
	       case _=>
	     }     
   })) 
   userListView.setOnDragDropped(handleEvent (event=> {
     var success=false
     if(event.getDragboard().hasContent(CalendarHelper.AddressDragFormat)){       
	     event.getDragboard.getContent(CalendarHelper.AddressDragFormat) match {
	       case add:Address => if(!contactsList.asScala.exists(_.ref==add.ref)){
	         currentEvent match {
	        	 case Some(ce) =>ClientQueryManager.secondUseInstances(List(add.ref), add.owner,new OwnerReference(2,ce.ref), -1)
	        			 success=true
	        	 case _ =>
	         }     	 
	       } 
	       case _=>
	     }     
     }
     event.setDropCompleted(success)
     event.consume()
   })) 
   
   
   
   GridPane.setConstraints(userListView, 0, 2)  
   val colConstr3=new ColumnConstraints
   val emptyConstr=new ColumnConstraints
   colConstr3.setHgrow(Priority.ALWAYS)
   colConstr3.setPrefWidth(280)
   val colConstr4=new ColumnConstraints
   colConstr4.setHalignment(HPos.RIGHT)
   colConstr4.setHgrow(Priority.NEVER)  
   val colConstr2=new ColumnConstraints
   colConstr2.setPrefWidth(120)
   colConstr2.setHgrow(Priority.NEVER) 
   form.getColumnConstraints().setAll(emptyConstr,colConstr2,colConstr2,colConstr3,colConstr4)
   form.setStyle("-fx-border-width: 1px;-fx-border-color:grey;")
   val placeLabel=new Label("Ort:")
   val placeEdit=new ActiveField((text)=> currentEvent match {
     case Some(event)=>  ClientQueryManager.writeInstanceField(event.ref, 4, StringConstant(text))
     case None =>
   })  
   GridPane.setConstraints(placeLabel, 2, 0)
   GridPane.setHalignment(placeLabel,HPos.RIGHT)
   GridPane.setConstraints(placeEdit, 3, 0)   
   placeEdit.setMaxWidth(Short.MaxValue)
   val protocollLabel=new Label("Protokoll Themen:")
   protocollLabel.setTooltip(new Tooltip("Themen anlegen/ändern durch Doppelklick in Liste"))
   GridPane.setConstraints(protocollLabel, 1, 1,2,1)
   val protocollListView=new ListView[Folder]
   protocollListView.setItems(protocollList)   
   protocollListView.setEditable(true)
   protocollListView.setCellFactory (callback(l=> {
     new TextFieldListCell[Folder](Folder.converter){
       override def startEdit():Unit= {
         super.startEdit()
         getGraphic().requestFocus()
       }
     }
   }))
   protocollListView.setOnEditCommit(handleEvent(e=> currentEvent match {
     case Some(event)=>

       val newFolder=e.getNewValue()
       val elem=protocollList.get(e.getIndex())
       //println("setOnEditCommit newFolder:"+newFolder+" "+newFolder.ref+" elem:"+elem)
       val fref=elem.ref match {
         case EMPTY_REFERENCE => new Reference(mod.folderType,ClientQueryManager.createInstance(mod.folderType,Array(OwnerReference(1,event.ref))))
         case oldRef=> oldRef
       }
       ClientQueryManager.writeInstanceField(fref,0, StringConstant(newFolder.name))
     case None =>
   }))
   val selMod: MultipleSelectionModel[Folder] =protocollListView.getSelectionModel()
   selMod.setSelectionMode(SelectionMode.SINGLE) 
   onChanged[Folder](selMod.selectedItemProperty(),(o,n)=> {
     n match {
       case null => protContentArea.setVisible(false) 
       case Folder(EMPTY_REFERENCE,_,_)=>protContentArea.setVisible(false)
       case folder:Folder=>
         protContentArea.setVisible(true)
         protContentArea.setText(folder.details)

     }
   })
   GridPane.setConstraints(protocollListView, 1, 2,2,1)       
   val protContentLabel=new Label("Inhalt:")
   protContentLabel.setTooltip(new Tooltip("Rechts Thema auswählen, dann hier in Textfeld klicken\nzur ausführlichen Beschreibung" ))
   GridPane.setConstraints(protContentLabel, 3, 1)
   val protContentArea=new ActiveTextArea((text)=> {
      protocollListView.getSelectionModel().getSelectedItem() match {
        case null =>
        case Folder(EMPTY_REFERENCE,_,_)=>
        case f:Folder=> ClientQueryManager.writeInstanceField(f.ref,1, StringConstant(text))
      }
   })
   GridPane.setConstraints(protContentArea, 3, 2,2,1)
   val deleteEventButton=new Button("Gesamtes Ereignis Löschen")
   GridPane.setConstraints(deleteEventButton, 4, 3)
   deleteEventButton.setOnAction(handleEvent(e=> currentEvent match {
     case Some(event)=>
       ClientQueryManager.deleteInstance(event.ref)
       loadNoEvent()
     case None =>
   }))
   deleteEventButton.setFocusTraversable(false)
   val deleteContactButton=new Button("Beteiligte(r) raus")
   GridPane.setConstraints(deleteContactButton, 0, 3)
   deleteContactButton.setOnAction(handleEvent (e=> currentEvent match {
     case Some(event) =>
       userListView.getSelectionModel().getSelectedItem() match {
	       case null =>
	       case a:Address if a.ref == EMPTY_REFERENCE   => util.Log.e("Cant delete event"+event+" user "+a+" has null ref")
	       case u:Address => ClientQueryManager.deleteInstance(u.ref,new OwnerReference(1,event.ref))
       }
     case None =>      
   }))
   deleteContactButton.setFocusTraversable(false)
   val deleteProtButton=new Button("Thema löschen")
   GridPane.setConstraints(deleteProtButton, 1, 3,2,1)
   deleteProtButton.setOnAction(handleEvent (e=> currentEvent match {
     case Some(event) =>
       protocollListView.getSelectionModel().getSelectedItem() match {
	       case null =>
	       case Folder(EMPTY_REFERENCE,_,_)=>
	       case f:Folder => ClientQueryManager.deleteInstance(f.ref)
       }
     case None =>      
   }))
   deleteProtButton.setFocusTraversable(false)
   val doneCheck=new CheckBox("Erledigt")
   doneCheck.setTooltip(new Tooltip("Ereignis/Aufgabe ist erledigt"))
   GridPane.setConstraints(doneCheck, 4, 0)
   doneCheck.setOnAction(handleEvent(e=>currentEvent match {
     case Some(event)=> ClientQueryManager.writeInstanceField(event.ref, 5, BoolConstant(doneCheck.isSelected()))
     case None =>
   }))
   form.getChildren.addAll(eventTextField,placeLabel,placeEdit,doneCheck,
       userLabel,protocollLabel,protContentLabel,
       userListView,protocollListView,protContentArea,
       deleteContactButton,deleteProtButton,deleteEventButton)
   
   def loadEvent(event:CalendarEvent): Unit = {
     shutDown()
     form.setVisible(true)          
     currentEvent=Some(event)
     eventSubsID=ClientQueryManager.createSubscription(event.ref,-1)((command,data)=> runInFx{
       command match {
         case NotificationType.sendData|NotificationType.updateUndo|
         NotificationType.fieldChanged=>
           val newEvent=new CalendarEvent(data.head,event.day,event.projID)
           currentEvent=Some(newEvent)
           eventTextField._setText(newEvent.name)
           placeEdit._setText(newEvent.place)
           doneCheck.setSelected(newEvent.done)
           val creatorUserAdress=UserList.list.asScala.find(_.id==newEvent.userID) map {
             user=> Address(EMPTY_REFERENCE,null,"["+user.name+"]","","","","","","","","")
           }
           if(command==NotificationType.sendData)
             contactsSubsID=ClientQueryManager.createSubscription(event.ref, 2)(handleSubscription(contactsList,new Address(_),false,creatorUserAdress,{
          	 protocollSubsID=ClientQueryManager.createSubscription(event.ref, 1)(handleSubscription(protocollList,Folder(_),true,None,{}))
           }))
         case _=>
       }
     })     
   }
   
   def loadNoEvent(): Unit = {
     //println("Unload event ")
     shutDown()
     form.setVisible(false)
   }
   
   
   def shutDown(): Unit = {
     if(eventSubsID > -1) {
       ClientQueryManager.removeSubscription(eventSubsID)
       eventSubsID= -1
     }
     if(contactsSubsID > -1) {
       ClientQueryManager.removeSubscription(contactsSubsID)
       contactsSubsID= -1
     }
     if(protocollSubsID > -1) {
       ClientQueryManager.removeSubscription(protocollSubsID)
       protocollSubsID= -1
     }
     currentEvent=None
     contactsList.clear()
     protocollList.clear()
     form.setVisible(false)
   }
}

