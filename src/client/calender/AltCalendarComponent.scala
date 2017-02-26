package client.calender

import javafx.beans.property.{SimpleIntegerProperty, SimpleObjectProperty}
import javafx.event.EventHandler
import javafx.geometry.{HPos, Pos}
import javafx.scene.control.{Button, Tooltip}
import javafx.scene.input.{MouseEvent, TransferMode}
import javafx.scene.layout.{ColumnConstraints, GridPane, HBox, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.{Line, Rectangle}
import javafx.scene.text.{Text, TextAlignment}

import client.comm.ClientQueryManager
import definition.data.OwnerReference
import definition.expression.{DateConstant, IntConstant}
import definition.typ.SystemSettings
import util.StrToInt

class AltCalendarComponent(mod:CalendarModel) extends VBox {
  import client.calender.CalendarHelper._
  final val weekRectWidth=189
  final val weekRectHeight=30
  
  val monthProperty=new SimpleIntegerProperty  
  val yearProperty=new SimpleIntegerProperty  
  val highlightWeekProperty= new SimpleIntegerProperty    
  var todayDay=0
  var firstDay:DateConstant=null
  var firstDayColumn=0
  var holidays:Map[DateConstant,String]=Map.empty
  var theseHolidays:Map[Int,String]=Map.empty
  var weeks:List[Text]=Nil
  var days:List[Text]=Nil
  var weekRects:List[Rectangle]=Nil
  onChanged[Number](monthProperty,(o,n)=>update())
  onChanged[Number](yearProperty,(o,n)=>update(o.intValue())) 
  onChanged[Number](highlightWeekProperty,(o,n)=>update())
  
  val t=new Text
  val wochenTage=Array[String]("Mo","Di","Mi","Do","Fr","Sa","So")
  getStyleClass().add("calender")  
  setMinWidth(228)
  setMaxWidth(228)
  val titleLabel=new Text
  titleLabel.setTextAlignment(TextAlignment.CENTER)
  
  val grid=new GridPane
  grid.setHgap(8)
  grid.setVgap(3)
  grid.setAlignment(Pos.CENTER)  
  //grid.setGridLinesVisible(true)
  val gridChildren=grid.getChildren
  val daysOfWeekTx=for(i<-wochenTage.indices;w=wochenTage(i)) yield{
    val tx=new Text(w)
    tx.getStyleClass().add("dayOfWeek")    
    GridPane.setConstraints(tx, i+2, 0)
    gridChildren.add(tx)
    tx
  }
  grid.setOnDragOver(handleEvent(e=>{
    if( e.getDragboard().hasContent(CalendarDragFormat)) e.acceptTransferModes(TransferMode.COPY_OR_MOVE:_*)
    e.consume()
  }))
  
  grid.setOnDragDropped(handleEvent(e=>{
    if( e.getDragboard().hasContent(CalendarDragFormat)) {
      e.getDragboard.getContent(CalendarDragFormat) match {
        case ce:CalendarEvent =>                 
           e.getTarget match {
             case t:Text if t.getId.length > 0 => t.getId match{
               case StrToInt(julian)=>
                 val newDate=DateConstant.asJulian(julian)
                 mod.projectList.findProject(ce.projID) match {
                   case Some(project)=>
                  	 e.getTransferMode() match {
                  	 	case TransferMode.MOVE=>
                        ClientQueryManager.writeInstanceField(ce.ref, 0, new IntConstant(newDate.day))
                        if(newDate.month!=ce.day.month||newDate.year!=ce.day.year) {
	                  	 	  ClientQueryManager.moveInstances(List(ce.ref),new OwnerReference(0, project.getMonthParentRef(ce.day)),
	                  	 	      new OwnerReference(0,project.getMonthParentRef(newDate)), -1)
                  	 	  }
                      case TransferMode.COPY=>
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
  
  val constraints=new ColumnConstraints
  constraints.setHalignment(HPos.CENTER)
  for(i<-0 until 9) grid.getColumnConstraints().add(constraints)  
  
  getChildren.addAll(titleLabel,grid)
  val currWeekRect=new Rectangle(weekRectWidth,weekRectHeight)
  currWeekRect.getStyleClass.add("currWeekRect")
  currWeekRect.setStroke(Color.BLACK)
  currWeekRect.setId("curr")
   
  def setRectListener(nl:EventHandler[MouseEvent])=  setOnMouseReleased(nl)     
  
  def monthUp()= {
    monthProperty.set(if(monthProperty.get==12) {
      yearProperty.set(yearProperty.get+1)
      1
    } else monthProperty.get+1)    
  }
  
  def monthDown()= {
    monthProperty.set(if(monthProperty.get==1) {
      yearProperty.set(yearProperty.get-1)
      12
    } else monthProperty.get-1)   
  }    
  
  def showMonth(d:DateConstant)= {   
    
    monthProperty.set(d.month)
    yearProperty.set(d.year)        
  }  
   
  def update(lastYear:Int= -1):Unit= {
    if(monthProperty.get()>12||monthProperty.get<1) return
    if(lastYear> -1) { // year updated      
      holidays=SystemSettings().getHolidays(yearProperty.get)      
    }
    todayDay=if( yearProperty.get==CalendarHelper.today.year&& monthProperty.get==CalendarHelper.today.month) 
      CalendarHelper.today.day else 0 
    gridChildren.removeAll(weeks:_*)
    weeks=Nil
    gridChildren.removeAll(days:_*)    
    days=Nil
    gridChildren.remove(currWeekRect)
    gridChildren.removeAll(weekRects:_*)
    weekRects=Nil
    firstDay=DateConstant(1,monthProperty.get(),yearProperty.get())
    titleLabel.setText(CalendarHelper.formatDate(firstDay))
    firstDayColumn=firstDay.weekDay
    //println("first day :"+firstDay+" column:"+firstDayColumn)
    if(firstDayColumn<0) firstDayColumn+=7
    val numDays=DateConstant.getDaysInMonth(firstDay.month,firstDay.year)
    val firstWeek=firstDay.weekOfYear 
    val lastWeek=firstDay.addDays(numDays-1).weekOfYear
    val betweenWeek= if(firstWeek>lastWeek) {
      monthProperty.get() match {
        case 1=> firstDay.addDays(0).weekOfYear
        case 12=>firstDay.addDays(numDays-8).weekOfYear
        case _=> throw new IllegalArgumentException("Error in Date Calculation "+firstDay  )
      }      
    } else 0    
    
    val zw=betweenWeek>0
  	var row=0
    val lw=if(zw) betweenWeek+lastWeek else lastWeek    
    for(week<-firstWeek to lw) {
      val weekNumber=if(zw && week>betweenWeek) week-betweenWeek else week 
      val weekTx=new Text(weekNumber.toString)  
      weekTx.getStyleClass.add("weekNr")
      GridPane.setConstraints(weekTx,0,row+2)
      gridChildren.add(weekTx)
      val rect=if(weekNumber==highlightWeekProperty.get) currWeekRect        
      else {
        val r= new Rectangle(weekRectWidth,weekRectHeight)
        r.getStyleClass().add("weekRect")
        weekRects=r::weekRects        
        r
      }
      rect.setId(weekNumber.toString)      
      GridPane.setConstraints(rect,2,row+2,7,1)
      gridChildren.add(rect)
      weeks=weekTx::weeks
      row+=1
    }
    
    theseHolidays=holidays.filter(_._1.month == monthProperty.get).map(el => (el._1.day, el._2))
    val firstJulian=firstDay.julian
    for(day<-1 to numDays) {
      val row=(firstDayColumn+day-1)/7 +2
  	  val col=((firstDayColumn+day-1)%7 )+2
  	  val dayTx=new Text(day.toString)
      dayTx.setTextAlignment(TextAlignment.RIGHT)
      //dayTx.setTextOverrun(OverrunStyle.CLIP)
      dayTx.setId((firstJulian+day-1).toString)
      dayTx.getStyleClass.add(if(todayDay==day)"todayTx" else if(col<8 && !theseHolidays.contains(day))"dayTx" else "weekendDayTx")
      //if(theseHolidays.contains(day)) dayTx.setTooltip(new Tooltip(theseHolidays(day)))
      GridPane.setConstraints(dayTx,col,row)
      gridChildren.add(dayTx)
      days=dayTx::days
    }
  }
}



class CalendarGroup(mod:CalendarModel) extends HBox{
  import client.calender.CalendarHelper._
  val currentWeekProperty=new SimpleObjectProperty[DateConstant]  
  val cals=for(i<-0 until 3) yield new AltCalendarComponent(mod)
  val leftBut=new Button("<")
  leftBut.setTooltip(new Tooltip("Einen Monat zurück"))
  leftBut.setFocusTraversable(false)
  val rightBut=new Button(">")
  rightBut.setTooltip(new Tooltip("Einen Monat weiter"))
  rightBut.setFocusTraversable(false)
  val lineHeight=235
  setSpacing(5)
  getChildren.addAll(leftBut,cals.head,new Line(0,0,0,lineHeight),cals(1),new Line(0,0,0,lineHeight),cals(2),rightBut)
  
  activateDate(today)
  
  val rectListener=handleEvent[MouseEvent](e=>{
    e.getTarget() match {
      case r:Rectangle=>
        r.parentProperty().get().parentProperty().get() match {
          case c:AltCalendarComponent=>
            val newDate=DateConstant.fromWeekNr(r.getId().toInt,c.yearProperty.get())
            activateDate(newDate)
            e.consume()
          case _=>
        }
      case t:Text if t.getId().length > 0 =>
        t.getId match {
          case StrToInt(julian)=>activateDate(DateConstant.asJulian(julian))
          case _=>
        }
        e.consume()
      case _=>  
    }
  })  
  cals.foreach(_.setRectListener(rectListener))
  
  leftBut.setOnAction(handleEvent(e=>{cals.foreach(_.monthDown())} ))
  rightBut.setOnAction(handleEvent(e=>{cals.foreach(_.monthUp())} ))
  
  def activateDate(date:DateConstant)= {    
    cals(1).showMonth(date)    
    cals.head.showMonth(date.subMonth())
    cals(2).showMonth(date.addMonth())
    cals(1).highlightWeekProperty.set(date.weekOfYear)
    currentWeekProperty.set(date)
  }
}