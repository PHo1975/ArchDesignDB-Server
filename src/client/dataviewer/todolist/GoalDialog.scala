package client.dataviewer.todolist

import client.comm.ClientQueryManager
import com.sun.glass.events.KeyEvent
import definition.data.{OwnerReference, Reference}
import definition.expression.DateConstant.NULL_DATE
import definition.expression.{DateConstant, StringConstant}

import java.awt.event.ActionEvent
import java.awt.{Dimension, KeyboardFocusManager}
import javax.swing.{JComponent, JOptionPane, KeyStroke}
import scala.swing.event.{ButtonClicked, Key, KeyPressed}
import scala.swing.{BoxPanel, Button, Dialog, Label, Orientation, Swing, TextArea, TextField, Window}

class GoalDialog(owner: Window) extends Dialog(owner) {
  val nameEdit=new TextField()
  val descriptionEdit=new TextArea()
  descriptionEdit.preferredSize = new Dimension(100,200)
  descriptionEdit.peer.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS,null)
  descriptionEdit.peer.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS,null)
  val dateEdit=new TextField()
  val okBut=new Button("OK")
  val cancelBut = new Button("Abbruch")
  var currentGoal:Option[ProjectGoal]=None
  var parentRef:Option[Reference]=None

  peer.getRootPane.registerKeyboardAction((actionEvent: ActionEvent) => visible = false,
    KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE,0),JComponent.WHEN_IN_FOCUSED_WINDOW)
  peer.getRootPane.registerKeyboardAction((actionEvent: ActionEvent) => store(),
    KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,0),JComponent.WHEN_IN_FOCUSED_WINDOW)

  contents=new BoxPanel(Orientation.Horizontal) {
    contents+=Swing.HStrut(20)+=new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Name:") += nameEdit += Swing.HGlue
      } += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Beschreibung:") += Swing.HStrut(40) += Swing.HGlue
      } +=
        descriptionEdit +=
        new BoxPanel(Orientation.Horizontal) {
          contents += new Label("Fertigstellung bis:") += dateEdit
        } += new BoxPanel(Orientation.Horizontal) {
        contents += okBut += Swing.HGlue += cancelBut
      }
      listenTo(descriptionEdit.keys)
      reactions += {
        case ev@KeyPressed(_, Key.Enter, _, _) => if((ev.modifiers& Key.Modifier.Control)>0) {
          ev.peer.consume()
          descriptionEdit.peer.insert("\n",descriptionEdit.caret.position)
        }
        else if(ev.modifiers==0) {
          ev.peer.consume()
          store()
        }
      }
    }
  }
  minimumSize = new Dimension(700,400)
  modal=true

  def create(parent:Reference): Unit ={
    parentRef=Some(parent)
    open(None)
  }

  def open(goal:Option[ProjectGoal]):Unit= {
    goal match {
      case Some(g)=>
        nameEdit.text = g.name
        descriptionEdit.text = g.bemerkung
        dateEdit.text = g.erledigt.toDateString match {
          case "0.0.0" => ""
          case o => o
        }
      case None =>
        nameEdit.text=""
        descriptionEdit.text=""
        dateEdit.text = ""
    }
    currentGoal=goal
    visible = true
    nameEdit.requestFocus()
  }

  def store():Unit= {
     val ref=currentGoal match {
       case Some(goal)=>goal.ref
       case None => parentRef match {
         case Some(p)=>Reference(171, ClientQueryManager.createInstance(171, Array(OwnerReference(1, p))))
         case None => throw new IllegalArgumentException(" Create but ParentRef not defined")
       }
     }
      var doClose:Boolean=true
      ClientQueryManager.writeInstanceField(ref,0,StringConstant(nameEdit.text ))
      ClientQueryManager.writeInstanceField(ref,1,StringConstant(descriptionEdit.text ))
      dateEdit.text match {
        case DateConstant(d)=>
            ClientQueryManager.writeInstanceField(ref,2,d)
        case ""=> if(currentGoal.nonEmpty && currentGoal.get.erledigt.toDateString!="0.0.0")  ClientQueryManager.writeInstanceField(ref,2,NULL_DATE)
        case o=> JOptionPane.showInputDialog(this.peer,"Falsches Datumsformat:"+o );doClose=false
      }
      if(doClose)
      visible = false
    }


  listenTo(okBut,cancelBut)
  reactions+= {
    case ButtonClicked(`okBut`)=>store()
    case ButtonClicked(`cancelBut`)=> visible = false
  }
}
