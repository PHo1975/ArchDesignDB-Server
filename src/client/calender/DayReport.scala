package client.calender

import client.comm.ClientQueryManager
import definition.data.{EMPTY_REFERENCE, InstanceData, OwnerReference, Reference}
import definition.expression.{DateConstant, Expression, IntConstant, StringConstant}
import javafx.collections.{FXCollections, ObservableList}
import javafx.geometry.{HPos, Insets, VPos}
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.{KeyCode, KeyEvent, TransferMode}
import javafx.scene.layout.{ColumnConstraints, GridPane, HBox, Priority}

import scala.collection.immutable.IndexedSeq
import scala.jdk.CollectionConverters._




class DayReport (val ul:InstanceData) extends AnyVal {  
  def ref: Reference =ul.ref
  def day: Int =if(ul.fieldValue.isEmpty) {util.Log.e("day=null "+ul.ref);0} else ul.fieldValue.head.toInt
  def weather: Int =ul.fieldValue(1).toInt
  def works: String =ul.fieldValue(2).toString
  def additional: String =ul.fieldValue(3).toString
  def faults: String =ul.fieldValue(4).toString
  def documents: String =ul.fieldValue(5).toString
  def notice: String =ul.fieldValue(6).toString
  override def toString: String ="DayReport "+day+" "+notice
}

class DayReportList (var month:Int,var list:Seq[InstanceData]) {
  def apply(ix:Int):DayReport=if(ix>=0 && ix<list.size)new DayReport(list(ix)) else throw new IllegalArgumentException("Index "+ix+" out of bounds")  
  private def getIndex(day:Int)=list.indexWhere(el=> new DayReport(el).day==day)
  def hasReportForDay(day:Int):Boolean= getIndex(day)>=0
  def getReportForDay(day:Int): DayReport =  apply(getIndex(day))
  def getDate(report:InstanceData)=DateConstant(new DayReport(report).day,month %100,month /100)
  def removeReport(ref:Reference): Unit =list=list.filter(_.ref!=ref)
  def changeReport(newElem:InstanceData): Unit = {
    list=list.filter(_.ref!=newElem.ref):+newElem
  }
  def addReport(newElem:InstanceData): Unit =list= newElem +:list
}

object DayReport {
  val Empty=new DayReport(new InstanceData(EMPTY_REFERENCE,IndexedSeq.empty))
}




class DayReportForm(mod:CalendarModel) {  
  import client.calender.CalendarHelper._
  var companiesSubsID: Int = -1
  var currentDayReport:Option[DayReport]=None
  var companies: ObservableList[Address] =FXCollections.observableList[Address](new java.util.ArrayList[Address])
  
  val form=new GridPane
  form.setHgap(10)
  form.setVgap(3)
  form.setPrefHeight(150d)  
  form.setPadding(new Insets(15))     
  
  val images=List(("sonne.gif",1,"Sonnig"),("wolke.gif",2,"Bewölkt"),("sturm.gif",4,"Stürmisch"),("Frost.gif",8,"Frost"),
      ("regen.gif",16,"Regen"),("mann.gif",32,"Bauleitung war vor Ort"))
  val wButtons: List[ToggleButton] =images.map{ case (im,code,description)=>
    val but=new ToggleButton("",new ImageView(loadImage(im,30)))
    //but.setStyle("-fx-padding:3;")
    but.getStyleClass.add("weather-button")
    but.setTooltip(new Tooltip(description))
    but.setFocusTraversable(false)
    but.setOnAction(handleEvent (e=>
      currentDayReport match {
        case Some(report)=>
          val newWeather= if(but.isSelected()) report.weather | code else report.weather & (~code)
          ClientQueryManager.writeInstanceField(report.ref, 1, IntConstant(newWeather))
        case None => mod.weekTableModel.createDayReport() match {
          case Some(ref)=> ClientQueryManager.writeInstanceField(ref, 1, IntConstant(code))
          case None=>
        }
      }
    ))
    but
  }  
  val buttonBox=new HBox
  buttonBox.setSpacing(2)
  buttonBox.getChildren().addAll(wButtons.asJava)
  GridPane.setConstraints(buttonBox, 0, 0,1,1)

  val companiesLabel = new Label("Firmen vor Ort:")
  companiesLabel.setTooltip(new Tooltip("Anwesende Firmen können aus den Projektadressen\nin diese Liste hineingezogen werden."))
  GridPane.setConstraints(companiesLabel, 0, 1,1,1)
  val companiesListView=new ListView[Address]
  		companiesListView.setItems(companies)
  		companiesListView.setFocusTraversable(false)
  		GridPane.setConstraints(companiesListView, 0, 2,1,4) 
  		companiesListView.setOnDragOver(handleEvent (event=> 
  		if(event.getDragboard().hasContent(CalendarHelper.AddressDragFormat)&&currentDayReport.isDefined){       
  			event.getDragboard.getContent(CalendarHelper.AddressDragFormat) match {
  				case add:Address => if(!companies.asScala.exists(_.ref==add.ref)){
  					event.acceptTransferModes(TransferMode.COPY_OR_MOVE:_*)	 
  					event.consume()
  				} 
  				case _=>
  			}     
  		})) 
   companiesListView.setOnDragDropped(handleEvent (event=> {
     var success=false
     if(event.getDragboard().hasContent(CalendarHelper.AddressDragFormat)){       
	     event.getDragboard.getContent(CalendarHelper.AddressDragFormat) match {
	       case add:Address => if(!companies.asScala.exists(_.ref==add.ref)){
	         currentDayReport match {
	        	 case Some(ce) =>ClientQueryManager.secondUseInstances(List(add.ref), add.owner,new OwnerReference(1,ce.ref), -1)
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
   
   def writeReportField(fieldNr:Byte,value:Expression): Unit = currentDayReport match {
     case Some(report)=>  ClientQueryManager.writeInstanceField(report.ref, fieldNr, value)
     case None => mod.weekTableModel.createDayReport() match {
       case Some(ref)=> ClientQueryManager.writeInstanceField(ref, fieldNr, value)       
       case None=>
     }
   }
  val labelTexts=IndexedSeq("Geleistete Arbeiten:","Zusätzliche Arbeiten:","Mängel:","Übergebene Dokumente:",
      "Notizen:")
  val labels: IndexedSeq[Label] = for(i<-labelTexts.indices; labelText=labelTexts(i)) yield{
     val label=new Label(labelText)
     GridPane.setConstraints(label,1,i)
     label
   }      
   val worksTextField=new IndexedActiveField(0,(text)=>writeReportField(2,StringConstant(text)))
   val additionalTextField=new IndexedActiveField(1,(text)=>writeReportField(3,StringConstant(text)))
   val failTextField=new IndexedActiveField(2,(text)=>writeReportField(4,StringConstant(text)))
   val docTextField=new IndexedActiveField(3,(text)=>writeReportField(5,StringConstant(text)))
   val noteTextField=new IndexedActiveArea(4,(text)=>writeReportField(6,StringConstant(text)))
   GridPane.setConstraints(noteTextField, 2, 4,1,2)
   val fields=IndexedSeq(worksTextField,additionalTextField,failTextField,docTextField,noteTextField)
   for(i<-0 to 4;field=fields(i)) GridPane.setConstraints(field, 2, i)
   val colConstr=new ColumnConstraints
   colConstr.setPrefWidth(230)
   colConstr.setHgrow(Priority.NEVER)
   val colConstr1=new ColumnConstraints
   colConstr1.setMinWidth(210)
   colConstr1.setHgrow(Priority.NEVER)
   colConstr1.setHalignment(HPos.RIGHT)
   val colConstr2=new ColumnConstraints
   colConstr2.setHgrow(Priority.ALWAYS)
   val delCompanyButton=new Button("< Firma raus")
   delCompanyButton.setFocusTraversable(false)
   GridPane.setConstraints(delCompanyButton, 1, 5,1,1,HPos.LEFT,VPos.CENTER)
   delCompanyButton.setOnAction(handleEvent (e=> currentDayReport match {
     case Some(report) =>
       companiesListView.getSelectionModel().getSelectedItem() match {
	       case null =>
	       case u:Address => ClientQueryManager.deleteInstance(u.ref,new OwnerReference(1,report.ref))
       }
     case None =>      
   }))
   val emptyConstr=new ColumnConstraints
   form.getColumnConstraints().setAll(colConstr,colConstr1,colConstr2)
   form.setStyle("-fx-border-width: 1px;-fx-border-color:grey;")
   form.getChildren.addAll(buttonBox,companiesLabel,companiesListView,worksTextField,additionalTextField,failTextField,
       docTextField,noteTextField,delCompanyButton)
   form.getChildren.addAll(labels.asJavaCollection)
  
   def handleKey(e:KeyEvent,func:(Int)=>Unit):Unit= e.getTarget() match{
        case f:IndexedActiveField=> func(f.ix)           
        case o => 
      }     
  
  form.addEventFilter(KeyEvent.KEY_PRESSED,
  handleEvent[KeyEvent](e => e.getCode() match {
    case KeyCode.ENTER|KeyCode.DOWN => handleKey(e,(fieldNr)=>{
      fields(if(fieldNr>3)0 else fieldNr+1).requestFocus()      
    })
    case KeyCode.UP=> handleKey(e,(fieldNr)=>fields(if(fieldNr<1)4 else fieldNr-1).requestFocus() )      
    case o => 
  }))
   
   
  def loadReport(rep:Option[DayReport]): Unit ={
    //println("load report "+rep)
    shutDown()
    currentDayReport=rep
    rep match {
    	case Some(report)=>
        worksTextField._setText(report.works)
        additionalTextField._setText(report.additional)
        failTextField._setText(report.faults)
        docTextField._setText(report.documents)
        noteTextField._setText(report.notice)
        val weather=report.weather
        for(i<- 0 until 6){
          val code=1<<i
          wButtons(i).setSelected((weather & code)>0)
        }
        companiesSubsID=ClientQueryManager.createSubscription(report.ref, 1)(handleSubscription(companies,new Address(_),false,None,{ }))
      case None =>
        worksTextField._setText("")
        additionalTextField._setText("")
        failTextField._setText("")
        docTextField._setText("")
        noteTextField._setText("")
        wButtons.foreach(_.setSelected(false))
    } 
    
  } 
   
  
  def shutDown(): Unit = {
    companies.clear()
    if(companiesSubsID> -1) {
      ClientQueryManager.removeSubscription(companiesSubsID)
      companiesSubsID= -1
    }
  }

}