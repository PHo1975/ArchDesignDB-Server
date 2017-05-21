package client.graphicsView

import java.awt.Dimension
import javafx.application.Platform
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.embed.swing.JFXPanel
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.Pos
import javafx.scene.{Group, Scene}
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.scene.layout.Pane

import client.dialog.AbstractViewController

import scala.swing.Component

class InplaceEditPanel(controller:AbstractViewController[_,_]) extends Component {
  
  lazy val panel=new JFXPanel with SuperMixin {}
  override lazy val peer: JFXPanel with SuperMixin = panel
  var currentTextEl:Option[TextElement]=None
  var currentScaler: Scaler = _
  val  tf=new javafx.scene.control.TextField("ABCDE")
  val vb=new Pane    
  var wantCancel=false

  def getEditText: String = tf.getText

  def setPaneSize(ns: Dimension): Unit = vb.setPrefSize(ns.width, ns.height)


  def runLater(func: => Unit): Unit = Platform.runLater(() => {
    func
  })
  
  
  def handleEvent[T<:javafx.event.Event](func:(T)=>Unit)= new EventHandler[T]() {
    def handle(e: T): Unit = func(e)
    }


  def setup(): Unit = runLater {
    createScene()
  } 
  
  
  private def createScene(): Unit = {
    	val g=new Group
    	val sc=new Scene(g,200,100)    	    	
    	tf.setAlignment(Pos.CENTER_LEFT)    	
    	tf.relocate(50,100)
    //tf.setPrefSize(170,50)
    	g.getChildren().add(vb)
    vb.getChildren().add(tf)
    vb.setStyle("-fx-border-color:green;-fx-border-width:1px;")
    tf.setStyle("-fx-border-color:blue;-fx-border-width:1px;")
    	panel.setOpaque(false)      
    	sc.setFill(new javafx.scene.paint.Color(0,0,1,0))    	
    	panel.setScene(sc)
    vb.addEventHandler(MouseEvent.MOUSE_PRESSED, handleEvent[MouseEvent](_ => controller.stopIPEMode()))
      tf.focusedProperty.addListener(new ChangeListener[java.lang.Boolean]{
        def changed(observable:ObservableValue[_ <: java.lang.Boolean] , oldValue:java.lang.Boolean,newValue:java.lang.Boolean): Unit = {
          if(oldValue==true && newValue==false) {
            //println("Focus Lost")
            if(!wantCancel) controller.stopIPEMode()
          } else if(oldValue==false && newValue==true) {          	
          	tf.deselect()
          	tf.positionCaret(tf.getText.length)
          } 
        } 
      })
      tf.setOnAction(handleEvent[ActionEvent](event=> controller.stopIPEMode()))
    tf.setOnKeyTyped(handleEvent[KeyEvent](event => updateSize(event.getCharacter)))
      tf.setOnKeyPressed(handleEvent[KeyEvent](event=> 
        event.getCode() match {
          case KeyCode.ESCAPE =>
            wantCancel=true
            controller.cancelIPEMode()
          case _ =>
        }             
      ))
  }

  def setValues(textEl: TextElement, sc: Scaler): Boolean = {
    //println("IPE set Values:"+textEl+ " size:"+panel.getSize())
    wantCancel=false      
    currentTextEl=Some(textEl)
    currentScaler=sc
    val rscale=sc.relScaleFactor
    val fontHeight=((GraphElemConst.toMM(textEl.font.getSize2D())*sc.scale*rscale)/10000d-1d).toFloat    
    //println("style:"+tf.getStyle)
    val geom=textEl.getGeometry(rscale,textEl.radAngle)	
		val xpos=sc.xToScreen(textEl.position.x+geom.tvx.x+geom.tvy.x)
		val ypos=sc.yToScreen(textEl.position.y+geom.tvx.y+geom.tvy.y)		
		//println("Set Values rscale:"+rscale+" scale:"+sc.scale+" textbounds:"+textEl.textBounds)
    //println("xpos:"+xpos+" ypos:"+ypos)
		val width=GraphElemConst.toMM(textEl.textBounds.width+textEl.textBounds.x)*rscale/10000d*sc.scale
		val height=GraphElemConst.toMM(textEl.textHeight)*rscale/1000d*sc.scale
    //println("IPE width:"+width+" height:"+height+" textBounds:"+textEl.textBounds)
		runLater{
      tf.setText(textEl.text)
      tf.setStyle("-fx-background-insets: 0, 0, 0, 0;-fx-text-fill: black;-fx-font-size: "+fontHeight+";-fx-font-family: '"+textEl.fontName+"';"+
          (if(GraphElemConst.styleIsBold(textEl.style))"-fx-font-weight:bold;"else "")+(if(GraphElemConst.styleIsItalic(textEl.style))"-fx-font-style:italic;"else " "))
  		tf.relocate(xpos-width*0.01,ypos-height*1.05)
  		tf.getTransforms().setAll(new javafx.scene.transform.Rotate(-textEl.textAngle,width*0.01,height*1.05))
      tf.setPrefSize(width + height + 10, height * 1.5 + 5)
    }
		panel.requestFocusInWindow()    		
  }

  def updateSize(key: String): Unit = currentTextEl match {
    case Some(tel) =>
      val rscale=currentScaler.relScaleFactor
      val rect = tel.getInplaceBounds(rscale, tf.getText + key + "i")
      //println("Updatesize inplaceBounds:"+rect+" "+tel.textBounds+" text:"+tf.getText)
      val width=GraphElemConst.toMM(rect.width)*rscale/1000d*currentScaler.scale
      val height=GraphElemConst.toMM(rect.height)*rscale/1000d*currentScaler.scale
      //println("update width:"+width+" height:"+height)
      runLater{
        tf.relocate(currentScaler.xToScreen(rect.x)-width*0.01,currentScaler.yToScreen(rect.y)-height*1.05)
        tf.setPrefSize(width + height + 10, height * 1.5 + 5)
      }
    case _ => println("no textel when update size")
  }
  
}