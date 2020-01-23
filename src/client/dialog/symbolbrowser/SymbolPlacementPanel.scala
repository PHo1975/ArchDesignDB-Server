package client.dialog.symbolbrowser

import java.awt.Dimension

import client.comm.ClientQueryManager
import client.dialog._
import definition.expression._
import definition.typ.CustomPanel

import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, GridPanel, Orientation, Swing}

class SymbolPlacementPanel extends BoxPanel(Orientation.Vertical) with  CustomPanel {
  @volatile var angle:Double=0d
  @volatile var scale:Double=1d

  def parse(text: String): Double = StringParser.parse(text) match {
    case e:ParserError=> ClientQueryManager.printErrorMessage(e.message);0d
    case ex:Expression=>ex.getValue.toDouble
  }

  val angleEdit: ActiveTextField = (st: String) => angle = parse(st)
  
  val scaleEdit: ActiveTextField = (st: String) => scale = parse(st)
  
  val noRotateBut=new Button("/\\")
  val rotateLeftBut=new Button("<-")
  val rotateRightBut=new Button("->")
  val rotate180But=new Button("\\/")
  val buttonPanel= new GridPanel(1,3)
  opaque=false
  buttonPanel.contents+=noRotateBut+=rotateLeftBut+=rotateRightBut+=rotate180But
  buttonPanel.maximumSize=new Dimension(Short.MaxValue,35)
  buttonPanel.xLayoutAlignment=0.5d
  listenTo(/*angleEdit.keys,scaleEdit.keys,*/noRotateBut,rotateLeftBut,rotateRightBut,rotate180But)
  val pointPanel=new PointAnswerPanel
  val part1=new PanelPart("Winkel",angleEdit)
  val part2=new PanelPart("Zoom",scaleEdit)
  part1.xLayoutAlignment=0.5d
  part2.xLayoutAlignment=0.5d
  
  pointPanel.externPointClickListener= Some((point:VectorConstant)=>{
    DialogManager.addAnswer(SymbolBrowserController.answer, DoubleConstant(angle))
    DialogManager.addAnswer(SymbolBrowserController.answer, DoubleConstant(scale))
    DialogManager.answerGiven(SymbolBrowserController.answer, point)
  })
  xLayoutAlignment=0.5d
  contents+=part1+=buttonPanel+=Swing.VStrut(20)+=part2+=
    pointPanel+=Swing.VStrut(20)
  
  reactions+={
    case ButtonClicked(`noRotateBut`)=> angleEdit.text="0.0";angle=0d
    case ButtonClicked(`rotateLeftBut`)=> angleEdit.text="90.0";angle=90d
    case ButtonClicked(`rotateRightBut`)=> angleEdit.text="-90.0";angle= -90d
    case ButtonClicked(`rotate180But`)=> angleEdit.text="180.0";angle=180d
    //case KeyPressed(_, Key.Escape, _, _) => DialogManager.reset()
  }  
  
  def open():Unit= {
    angleEdit.text=angle.toString
    scaleEdit.text=scale.toString
    pointPanel.loadParamAnswer(SymbolBrowserController.pointAnswer)
  }
  
  def setFocus():Unit= {
    angleEdit.requestFocus()
  }
  
  def shutDown():Unit= {
    
  }
  
  override def name:String="Symbol absetzen"
}