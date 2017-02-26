package client.ui

import scala.swing.SimpleSwingApplication
import java.awt.Color
import scala.swing.MainFrame
import scala.swing.{BorderPanel,BoxPanel,Orientation,TextArea}
import scala.swing.Panel
import scala.swing.event.MouseMoved
import java.awt.Dimension
import scala.swing.event.MouseEntered
import scala.swing.event.MouseExited
import scala.swing.SplitPane
import java.awt.Rectangle
import javax.swing.Timer
import java.awt.event.ActionListener
import java.awt.event.ActionEvent


object HorState extends Enumeration{
  val OnlyLeft=Value("only left")
  val Both=Value("Both")
  val OnlyRight=Value("only right")
}

object FlowViewTest extends SimpleSwingApplication {
  val borderWidth=20
  val borderColor=Color.GRAY
  val waitTime=500
  var horState:HorState.Value=HorState.Both
  var oldDivLocation:Int =0
  
  
  class SidePanel(func: ()=>Unit) extends Panel {
    var inside=false
    var enterTime:Long=0
    background=borderColor
    opaque=true
    listenTo(this.mouse.moves)
    preferredSize=new Dimension(borderWidth,borderWidth)
    reactions += {
      case MouseEntered(c,point,modifiers)=>
        if(!inside) {
         inside=true
          enterTime=System.currentTimeMillis()
         val timer=new Timer(waitTime,new ActionListener {
            def actionPerformed(e:ActionEvent)= {
              if(inside)func()
            }
         })
         timer.setRepeats(false)
         timer.start()
       }
      case MouseExited(c,point,modifiers)=>
        if(inside) inside=false
    }
  }
  
  class ColorLabel(tx:String,color:Color)extends TextArea(tx){
    background=color;opaque=true
    minimumSize=new Dimension(50,50)
    preferredSize=minimumSize
    editable=false
  }
  
  
  
  
  
  val topLPane=new SidePanel (()=>
    println("top left flow")
  )
  
  val bottomLPane=new SidePanel(()=>
    println("bottom left flow")
  )
  
  val topRPane=new SidePanel (()=>
    println("top right flow")
  )
  
  val bottomRPane=new SidePanel(()=>
    println("bottom right flow")
  )
  
  val leftLZ=new ColorLabel("Lesezeichen links ",Color.green)
  val leftPath=new ColorLabel("Pfad Links",Color.green.darker().darker)
  val leftTable=new ColorLabel("Tabelle links ",Color.CYAN.brighter)
  val leftPaper=new ColorLabel("Zeichenfläche links ",Color.white)
  
  val leftCenterBox=new BoxPanel(Orientation.Vertical) {
    contents+=leftLZ+=leftPath+=leftTable+=leftPaper
  }
  
  val rightLZ=new ColorLabel("Lesezeichen rechts ",Color.green)
  val rightPath=new ColorLabel("Pfad rechts",Color.green.darker().darker)
  val rightTable=new ColorLabel("Tabelle rechts ",Color.CYAN.brighter)
  val rightPaper=new ColorLabel("Zeichenfläche rechts ",Color.white)
  
  val rightCenterBox=new BoxPanel(Orientation.Vertical) {
    contents+=rightLZ+=rightPath+=rightTable+=rightPaper
  }
  
  
  
  val leftCenterPanel=new BorderPanel{
    add(topLPane,BorderPanel.Position.North)
    add(bottomLPane,BorderPanel.Position.South)
    add(leftCenterBox,BorderPanel.Position.Center)
  }
  
  val rightCenterPanel=new BorderPanel{
    add(topRPane,BorderPanel.Position.North)
    add(bottomRPane,BorderPanel.Position.South)
    add(rightCenterBox,BorderPanel.Position.Center)
  }
  
  val splitter=new SplitPane(Orientation.Vertical,leftCenterPanel,rightCenterPanel){
    this.dividerSize=borderWidth
    this.resizeWeight=0.5f
  }
  
  val leftPane=new SidePanel(()=>{    
    horState match {
      case HorState.Both=>
        horState=HorState.OnlyLeft
        oldDivLocation=splitter.dividerLocation
        rightCenterPanel.visible=false
        //splitter.resizeWeight=1
        splitter.resetToPreferredSizes()
      case HorState.OnlyRight=>
        horState=HorState.Both
        leftCenterPanel.visible=true
        //splitter.resizeWeight=0.5f
        //splitter.resetToPreferredSizes
        splitter.dividerLocation=oldDivLocation
      case _=>
    }
    println("left flow "+horState)
   })
  
  val rightPane=new SidePanel(()=>{    
    horState match {
      case HorState.Both=>
        horState=HorState.OnlyRight
        println(" "+splitter.resizeWeight+" "+splitter.dividerLocation)
        leftCenterPanel.visible=false
        oldDivLocation=splitter.dividerLocation
        //splitter.resizeWeight=0f
        splitter.resetToPreferredSizes()
      case HorState.OnlyLeft=>
        horState=HorState.Both
        rightCenterPanel.visible=true
        //splitter.resizeWeight=0.5f
        //splitter.resetToPreferredSizes
        splitter.dividerLocation=oldDivLocation
      case _=>
    }
    println("right flow "+horState)
   })
  
  
  val mainPanel=new BorderPanel{
    add(leftPane,BorderPanel.Position.West)
    add(rightPane,BorderPanel.Position.East)
    add(splitter,BorderPanel.Position.Center)
  }
  
  val top=new MainFrame ()	{
    title="FlowTest"
    contents=mainPanel  
    bounds=new Rectangle(100,100,700,500)
  }
  
  

}