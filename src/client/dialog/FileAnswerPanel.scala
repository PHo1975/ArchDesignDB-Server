package client.dialog

import client.ui.ViewConstants
import definition.expression.StringConstant

import javax.swing.JFileChooser
import scala.swing.Button
import scala.swing.event.ButtonClicked

class FileAnswerPanel extends AnswerPanel{
  val fileButton=new Button("Datei auswählen ...")
  contents+=fileButton
  listenTo(fileButton)
  reactions+= {
    case ButtonClicked(`fileButton`)=>
      val fileChooser=new JFileChooser()
      if(fileChooser.showOpenDialog(this.peer)==JFileChooser.APPROVE_OPTION){
        val file=fileChooser.getSelectedFile
        DialogManager.answerGiven(ansParm,StringConstant(file.getAbsolutePath.replaceFirst(
            ViewConstants.imagePath,"*")) )
      }
      else DialogManager.reset()
  }
}
