package management.databrowser

import scala.collection.mutable.ArrayBuffer
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, BoxPanel, Component, Orientation, Swing, ToggleButton}

class MyButton(label:String,val index:Int,val panel:Component) extends ToggleButton(label) {
  focusable=false  
  maximumSize=MainWindow.buttonSize
}


class AccordionComponent extends BorderPanel {
  val topPanel=new BoxPanel(Orientation.Vertical)
  val bottomPanel=new BoxPanel(Orientation.Vertical)
  val centerPanel=new BorderPanel {
    add(Swing.VStrut(10),BorderPanel.Position.North)
    add(Swing.VStrut(10),BorderPanel.Position.South)    
  }
  add(topPanel,BorderPanel.Position.North)
  add(bottomPanel,BorderPanel.Position.South)
  add(centerPanel,BorderPanel.Position.Center)
  
  val panelList=ArrayBuffer[MyButton]()
  
  def addPanel(name:String,panel:Component)= {
    val button=new MyButton(name,panelList.size,panel)
    listenTo(button)
    for(but<-panelList) but.selected=false
    panelList += button
    topPanel.contents.clear()
    topPanel.contents++=panelList
    bottomPanel.contents.clear()
    button.selected=true
    centerPanel.layout(panel)=BorderPanel.Position.Center
    //centerPanel.addCenter(panel)
    revalidate()
    repaint()
  }
  
  def activatePanel(ix:Int)= activateButton(panelList(ix))
  
  
  def activateButton(button:MyButton)= {
    centerPanel.layout(button.panel)=BorderPanel.Position.Center
      for(but<-panelList;if but.index != button.index) but.selected=false
      topPanel.contents.clear()
      topPanel.contents++=panelList.take(button.index+1)
      topPanel.contents.last.asInstanceOf[MyButton].selected=true
      bottomPanel.contents.clear()
      bottomPanel.contents++=panelList.drop(button.index+1)      
      topPanel.revalidate()
      bottomPanel.revalidate()
      repaint()
  }
  
  reactions+= {
    case ButtonClicked(button:MyButton)=>
      activateButton(button)
  }
  
}