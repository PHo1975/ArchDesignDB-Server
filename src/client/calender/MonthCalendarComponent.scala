package client.calender

import java.util.Calendar
import javafx.beans.property.SimpleIntegerProperty
import javafx.scene.canvas.Canvas
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.text.{Font, Text, TextAlignment}

import definition.expression.DateConstant

class MonthCalendarComponent extends StackPane {
  import client.calender.CalendarHelper._
  val today=new java.util.GregorianCalendar
  val monthProperty=new SimpleIntegerProperty
  monthProperty.set(today.get(Calendar.MONTH)+1)
  val yearProperty=new SimpleIntegerProperty
  yearProperty.set(today.get(Calendar.YEAR))
  val colSpace=27
  val rowSpace=23  
  var firstDay:DateConstant=null
  var firstDayColumn=0
  onChanged[Number](monthProperty,(o,n)=>update())
  onChanged[Number](yearProperty,(o,n)=>update())

  val headerFont=Font.font("Arial",12d)
  val weekFont=Font.font("Arial",12d) 
  val daysFont=Font.font("Arial",15d)
  val t=new Text
  val wochenTage=Array[String]("Mo","Di","Mi","Do","Fr","Sa","So")
  getStyleClass().add("calender")  
  val canvas=new Canvas(290,235)  
  canvas.setOpacity(1)
  
  
  getChildren().add(canvas)
  setMinWidth(295)
  setMaxWidth(295)
  //setOpacity(1)
  update()
  
  def monthUp()= {
    monthProperty.set(if(monthProperty.get==12) {
      yearProperty.set(yearProperty.get+1)
      1
    } else monthProperty.get+1)
    update()
  }
  
  def monthDown()= {
    monthProperty.set(if(monthProperty.get==1) {
      yearProperty.set(yearProperty.get-1)
      12
    } else monthProperty.get-1)
    update()
  }
  
  def update():Unit= {
    val context=canvas.getGraphicsContext2D()    
    context.save()
    context.setTextAlign(TextAlignment.CENTER)
    context.setLineWidth(0.5)
    //context.setStroke(Color.WHITE)    
    if(monthProperty.get()>12||monthProperty.get<1) return
    firstDay=DateConstant(1,monthProperty.get(),yearProperty.get())    
    firstDayColumn=firstDay.weekDay
    println("first day :"+firstDay+" column:"+firstDayColumn)
    if(firstDayColumn<0) firstDayColumn+=7
    val numDays=DateConstant.getDaysInMonth(firstDay.month,firstDay.year)
    /*val thisMonth=today.get(Calendar.MONTH)+1==monthProperty.get()&&
     today.get(Calendar.YEAR)== yearProperty.get()*/
    val firstWeek=firstDay.weekOfYear 
    val lastWeek=firstDay.addDays(numDays-1).weekOfYear
    val betweenWeek= if(firstWeek>lastWeek) {
      monthProperty.get() match {
        case 1=> firstDay.addDays(0).weekOfYear
        case 12=>firstDay.addDays(numDays-8).weekOfYear
        case _=> throw new IllegalArgumentException("Error in Date Calculation "+firstDay  )
      }      
    } else 0
    
    context.clearRect(0,0, canvas.getWidth(),canvas.getHeight)    
  	context.setFont(headerFont)
  	context.setFill(Color.GRAY)
  	context.setTextAlign(TextAlignment.CENTER)
  	for(i<-wochenTage.indices;tagText=wochenTage(i))
  		context.fillText(tagText, colSpace+10+i*colSpace, 20+rowSpace)  
  	val zw=betweenWeek>0
  	var row=0
  	context.setFont(weekFont)
  	context.setTextAlign(TextAlignment.CENTER)
  	val lw=if(zw) betweenWeek+lastWeek else lastWeek
    for(week<-firstWeek to lw) {
      val weekNumber=if(zw && week>betweenWeek) week-betweenWeek else week 
      context.fillText(weekNumber.toString,10,rowSpace*2+20+row*rowSpace)
      row+=1
    }
  	context.setFont(daysFont)
  	context.setFill(Color.BLACK)
  	for(day<-1 to numDays) {
  	  val row=(firstDayColumn+day-1)/7 +2
  	  val col=((firstDayColumn+day-1)%7 )+1
  	  context.fillText(day.toString,10+col*colSpace,20+row*rowSpace)
  	}
  	context.fillText(CalendarHelper.formatDate(firstDay),colSpace*3,13)
  	//context.applyEffect(new DropShadow(0,2,1,Color.GREY))
  	context.restore()
  }
  
}

