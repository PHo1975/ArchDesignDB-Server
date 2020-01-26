package client.calender

import javafx.geometry.{Insets, Pos}
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.text.Text
import javafx.stage.{Modality, Stage, StageStyle, Window}

import scala.jdk.CollectionConverters._

class  FxMessageBox(buttonTexts:Seq[String]) extends Stage {  
	initModality(Modality.WINDOW_MODAL)
  initStyle( StageStyle.UTILITY )

  val messageText=new Text
  var result:Option[String]=None
	val buttons: Seq[Button] =buttonTexts map (text=> {
	  val button=new Button(text)
	  button.setOnAction(CalendarHelper.handleEvent(e=> {result=Some(text);this.close()}))
	  button
	})
	val vbox=new VBox
	vbox.setSpacing(15)
	vbox.setPadding(new Insets(10))
	vbox.setAlignment(Pos.TOP_CENTER)	
	val hbox=new HBox
	hbox.getChildren.addAll(buttons.asJavaCollection)
	hbox.setAlignment(Pos.BOTTOM_CENTER)
	hbox.setSpacing(15)
	vbox.getChildren.addAll(messageText,hbox)	
	setScene(new Scene(vbox ))  	
}

object FxMessageBox {
  def showMessage(message:String,title:String,owner:Window,buttonTexts:Seq[String]=Seq("Ok")):Option[String]={ 
	  val box=new FxMessageBox(buttonTexts)
	  box.messageText.setText(message)
	  box.setTitle(title)
	  box.initOwner(owner)
	  box.sizeToScene()
    box.centerOnScreen()
    box.result=None
	  box.showAndWait()
	  box.result
	} 
}