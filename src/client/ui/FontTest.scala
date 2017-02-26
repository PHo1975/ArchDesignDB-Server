package client.ui

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import java.awt.Rectangle
import scala.swing.Component
import java.awt.Graphics2D
import java.awt.font.FontRenderContext
import java.awt.Font
import java.awt.font.TextLayout
import scala.swing.Panel
import scala.swing.TextField
import java.awt.Dimension
import javax.swing.JTextField
import java.awt.Graphics
import javafx.embed.swing.JFXPanel
import javafx.application.Platform
import javafx.scene.Scene
import javafx.scene.Group
import javafx.scene.control.TextField
import javafx.scene.control.Slider
import javafx.geometry.{Pos,VPos}
import javafx.scene.layout.VBox
import javafx.scene.layout.Pane
import java.awt.Color
import scala.swing.BorderPanel
import scala.swing.Label
import javafx.scene.shape.Line
import javax.swing.JLayeredPane
import scala.swing.TextArea
import scala.swing.Container
import javafx.scene.control.CheckBox
import com.sun.xml.internal.bind.v2.runtime.property.ValueProperty
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue



object FontTest extends SimpleSwingApplication {
  val  tf=new javafx.scene.control.TextField("ABCDE")
  val top=new MainFrame() {
    peer.setBounds(new Rectangle(100,100,600,600))
    
    val ppan=new BorderPanel{
      add (new Label("Hallo"){preferredSize=new Dimension(100,300)},BorderPanel.Position.South)
      add (new TextArea("Das ist \naber\n ein\n komischer Text"),BorderPanel.Position.Center)
      background=Color.CYAN
    }    
    val lpane=new LPane    
    
    ppan.peer.setBounds(new java.awt.Rectangle(0,0,500,450))
    lpane.panel.add(ppan.peer,new java.lang.Integer(1))
    //lpane.preferredSize=new Dimension(400,300)
    lpane.background=Color.red
    lpane.peer.setBounds(0,0,500,450)
    val mc= new MyComp
    mc.peer.setBounds(0,0,500,450)
    lpane.panel.add(mc.peer,new java.lang.Integer(2))
    //lpane.size=new Dimension(400,300)
    contents=lpane
    //peer.setBackground(Color.CYAN)
    
    
    	val m=new ChangeListener[java.lang.Boolean] {
        def changed(ov:ObservableValue[_ <: java.lang.Boolean] , old_val:java.lang.Boolean, new_val:java.lang.Boolean): Unit = {
          mc.visible=new_val      
        }
      }
    
    mc.cb.selectedProperty().addListener(m)
  }
  
  class LPane extends Component {
    lazy val panel=new JLayeredPane 
    override lazy val peer=panel
  }
  
  class MyComp extends Component {
    lazy val panel=new JFXPanel with SuperMixin {}
    
    val cb=new CheckBox("weg")
    override lazy val peer=panel 
    
    Platform.runLater(new Runnable() {
    	override def run(): Unit = {
    		createScene()
    		tf.requestFocus()
    	}
    })
    def createScene(): Unit = {
    	val g=new Group
    	val sc=new Scene(g,200,100)
    	
    	//tf.setRotate(115)
    	
    	tf.setAlignment(Pos.BOTTOM_LEFT)
    	tf.setStyle("-fx-background-insets: 0, 0, 0, 0;-fx-border-insets:0, 0, 0, 0;-fx-border-width:0;fx-border-radius:0;"+
    	    "fx-background-radius:0;-fx-text-fill: red;-fx-font-size: 40;-fx-font-family: 'Times New Roman';-fx-font-style:italic;")    	
    	val vb=new Pane
    	vb.setPrefSize(400,350)
    	//vb.setStyle("-fx-background-color: yellow;")
    	
    	val nt=new javafx.scene.control.Label("Lab1")
    	val n2=new javafx.scene.control.Label("Lab2")
    	val line1 = new Line()
      val line2 = new Line()
      val slider=new Slider(0,360,45)
    	slider.relocate(10,200)
    	slider.setPrefSize(150,30)
    	slider.setOrientation(javafx.geometry.Orientation.HORIZONTAL)
    	tf.relocate(50,100)
    	tf.setPrefSize(170,50)
    	println("Rotate axis:"+tf.getRotationAxis())
    	nt.relocate(50,0)
    	n2.relocate(0,100)
    	g.getChildren().add(vb)
    	//slider.valueProperty.bindBidirectional(tf.rotateProperty())
    	slider.valueProperty.addListener(new javafx.beans.value.ChangeListener[java.lang.Number] () {
    	  def changed(observable:ObservableValue[_ <: java.lang.Number] ,
           oldValue:java.lang.Number , newValue:java.lang.Number)= {
    	    tf.getTransforms().setAll(new javafx.scene.transform.Rotate(newValue.doubleValue(),0,0))
    	  }
    	}) 
    	line1.setStartX(50.0f)
      line1.setStartY(0.0f)
      line1.setEndX(50.0f)
      line1.setEndY(300.0f)
      line2.setStartX(0.0f)
      line2.setStartY(100.0f)
      line2.setEndX(400.0f)
      line2.setEndY(100.0f)
      cb.relocate(0,250)
    	cb.setPrefSize(100,30)
    	vb.getChildren.addAll(nt,n2,cb,line1,line2,slider,tf)
    	//vb.setClip(tf)
    	//vb.relocate()
    	panel.setOpaque(false)
    	//vb.setOpacity(0.4)
    	sc.setFill(new javafx.scene.paint.Color(0,0,1,0))
    	//tf.setFill(new javafx.scene.paint.Color(250,250,100,0.5))
    	panel.setScene(sc)
    	tf.focusedProperty.addListener(new ChangeListener[java.lang.Boolean]{
        def changed(observable:ObservableValue[_ <: java.lang.Boolean] , oldValue:java.lang.Boolean,newValue:java.lang.Boolean): Unit = {
          if(oldValue==false && newValue==true) {
            println("Focus Lost")
            tf.deselect()
            tf.positionCaret(2)
          } 
        } 
      })
    }    

  }
  
  
  
  override def startup(args: Array[String]):Unit = {
    super.startup(args)
    top.bounds=new Rectangle(100,100,600,600)
  }  
  
}

