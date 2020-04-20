/**
 * Author: Peter Started:25.09.2010
 */
package client.dialog

import client.comm.{ClientObjectClass, KeyStrokeManager}
import client.dataviewer.TitlePopupMenu
import client.ui.ViewConstants
import definition.data.Referencable
import definition.typ.{ActionDescription, AllClasses, SelectGroup}
import javax.swing.{BorderFactory, JOptionPane}

import scala.swing.event.ButtonClicked
import scala.swing.{AbstractButton, BoxPanel, Component, Insets, MenuItem, Point, Swing}


object ActionPanel extends BoxPanel(scala.swing.Orientation.Vertical) with SelectListener {
  val emptyInsets=new Insets(0,0,0,0)
  var lastSender:Option[SelectSender]= None
  var groupList:Iterable[SelectGroup[_<:Referencable]]= Nil
  //var lastCommonClass= -1
  var buttons:Seq[StrokableButton]=Nil
  val miniInsets = new Insets(0, 5, 0, 5)
  var lastClassName:String= _
  val endStrut: Component = Swing.VGlue
  endStrut.xLayoutAlignment = 0

  opaque=false
  //background=Color.red

  border=BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(8,0,6,0),
    BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(8, 0, 6, 0), "Funktionen:"))

  xLayoutAlignment=0.5d
  yLayoutAlignment = 0.5d

  def shutDown():Unit = {
    removeButtons()
    contents.clear()
  }


  def hasSelection: Boolean = groupList.nonEmpty && groupList.head.children.nonEmpty

  def getButtonID(but:StrokableButton):Int=but match {
    case asb:ActionStrokeButton=>asb.theAction match {
      case it:ActionDescription=> it.buttonID
      case _=> 0
    }
    case csb:CustomStrokeButton=>csb.buttonID
    case _=>0
  }

  def insertButtons(newButtons: Iterable[StrokableButton]): Unit = {
    contents.clear()
    var groupID = 0
    for (but <- newButtons) {
      if (getButtonID(but) / 10 != groupID) {
        val strut = Swing.VStrut(10 * ViewConstants.fontScale / 100)
        strut.xLayoutAlignment = 0d
        contents += strut
        groupID = getButtonID(but) / 10
      }
      contents += but
    }
    contents += endStrut
  }


  def selectionChanged [T <: Referencable](sender:SelectSender,groups:Iterable[SelectGroup[T]],alsoSelected:Iterable[T]):Unit = {
    //println("Selection Changed sender:"+sender.getClass+" groups:"+groups+" ")
    //removeButtons()
    groupList=groups
    for(ls <-lastSender;if ls != sender) ls.deselect(false)
    if( groupList.isEmpty || groupList.head.children.isEmpty) visible=false
    else {
      val allc=AllClasses.get
      val commonClass=allc.getCommonClassForGroups(groups)
      shutDown()
      if(commonClass>0) {
        val theClass = allc.getClassByID(commonClass).asInstanceOf[ClientObjectClass]

        if (hasSelection && groupList.head.children.head.ref.instance != -1) {
          // ignore invalidate dummy instances
          buttons = theClass.actionButtons.toSeq.sortBy(getButtonID)
          visible = true
          restoreButtonBindings()
          insertButtons(buttons)
          listenTo(buttons: _*)

        } else {
          buttons = Nil
          visible = false
        }
        //lastCommonClass = commonClass
      }
      //Log.w("sel Change but")
      peer.invalidate()
      peer.revalidate()
      repaint()
    }
    lastSender = Some(sender)
  }

  def removeButtons(): Unit = deafTo(buttons: _*)

  def addCustomButtons(custBut:Seq[CustomStrokeButton]):Unit= {
    buttons = buttons ++ custBut
    if (!visible) visible = true
    for (b <- custBut) KeyStrokeManager.registerReceiver(b)
    insertButtons(buttons)
    revalidate()
    repaint()
    listenTo(custBut: _*)
  }

  def restoreButtonBindings():Unit= for(b<-buttons) KeyStrokeManager.registerReceiver(b)

  reactions += {
    case ButtonClicked(but:ActionStrokeButton) => if(groupList!=null) {
      //println("Button clicked " + but.theAction.buttonID + " " + but.theAction.name)
      checkAndExecute(but)
      //DialogManager.startActionDialog(but.theAction, groupList)
    }
    case ButtonClicked(but: CustomStrokeButton) => but.callBack()
  }

  protected def checkAndExecute(asb:ActionStrokeButton): Unit = if(groupList!=null){
    if(asb.theAction.buttonID==1000&&asb.theAction.name=="Objekt Löschen"){
      if(groupList.exists(gr=> gr.children.exists(ch=> ch.ref.typ match {
        case 39=>true
        case 110 => true
        case 202 => true
        case _=> false
      }))) {
        if (JOptionPane.showConfirmDialog(this.peer,"Wollen Sie das Objekt wirklich löschen?","Objekt Löschen",JOptionPane.OK_CANCEL_OPTION)== JOptionPane.OK_OPTION)
          DialogManager.startActionDialog(asb.theAction, groupList)
      } else DialogManager.startActionDialog(asb.theAction, groupList)
    }
    else DialogManager.startActionDialog(asb.theAction, groupList)
  }

  // ******* Support for right click menu

  class CustomMenuItem(val ob: CustomStrokeButton) extends MenuItem(ob.text)
  class ActionMenuItem(val ob:ActionStrokeButton) extends MenuItem(ob.text)

  def showRightMenu(point: Point, component: Component): Unit = if (hasSelection) {
    val popup = new TitlePopupMenu("Actions")
    val menuItems = buttons map (el => {
      val m = el match {
        case csb: CustomStrokeButton => new CustomMenuItem(csb)
        case asb: ActionStrokeButton => new ActionMenuItem(asb)
      }
      m.icon = el.icon
      m
    })
    popup.addButtons(menuItems)
    popup.show(component.peer, point.x, point.y)
    popup.listenTo(menuItems: _*)
    popup.reactions += {
      case ButtonClicked(but: ActionMenuItem) => checkAndExecute(but.ob)
      case ButtonClicked(but: CustomMenuItem) => but.ob.callBack()
    }
  }

  def minimizeButton(button: AbstractButton): Unit = {
    button.peer.putClientProperty("JComponent.sizeVariant", "small")
    button.peer.updateUI()
  }

}	


