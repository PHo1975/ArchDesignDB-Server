/**
 * Author: Peter Started:27.10.2010
 */
package client.dialog

import client.comm.{ClientObjectClass, ClientQueryManager, KeyStrokeManager}
import definition.data.Reference
import definition.typ.AllClasses

import scala.swing.Reactor
import scala.swing.event.ButtonClicked

/** area where "New panels" are placed
 * 
 */

object CreateActionList extends Reactor  {
  var lastContainer:Option[FocusContainer]=None
  var lastOwnerRef:Option[Reference]=None
  var lastPropField:Int= -1
  //private val selGroup=new SelectGroup[Referencable](EMPTY_OWNERREF,Seq[Referencable]())
  //var buttonList:Seq[CreateMenuButton]=Nil
  var actionButtons:Seq[CreateActionMenuButton]=Nil

  reactions += {
    case ButtonClicked(theBut:CreateActionMenuButton) => for (owner<-lastOwnerRef){
      theBut.ccd.action match {
        case Some(action)=>DialogManager.startCreateActionDialog(action,owner,theBut.ccd.childClassID,theBut.propField)
        case _ => throw new IllegalArgumentException("No Action defined in CreateActionButton "+theBut)
      }
    }
    case ButtonClicked(theBut:CreateMenuButton)=> for(owner<-lastOwnerRef) {
      DialogManager.reset()
      for(lc<-CreateActionList.lastContainer) {
        //println("newbuttonslist button clicked "+theBut.text)
        lc.createActionSubmitted(1)
        val formatValues= lc .getCreationFormatValues(theBut.ccd.childClassID)
        ClientQueryManager.executeCreateAction(owner.ref,theBut.ccd.childClassID,theBut.propField,"*",Seq(),formatValues)
      }
    }
  }

  def containerFocused(container:FocusContainer, propField:Int):Unit = {
    //println("Dispatcher Container Focused :"+container.getClass().toString+" ref:"+container.containerRef+" propfield:"+propField)
    val cont=Some(container)
    val newContRef=container.ownerRef.map(_.ref)
  	if(!(cont==lastContainer&&newContRef==lastOwnerRef&&propField==lastPropField)) {
      //println("ContainerFocus the same ")
      //println("container Focused newCont:"+container+" last:"+lastContainer+"\nnewContRef:"+newContRef+" last:"+lastSuperInstRef+"\npropField:"+propField+" last:"+lastPropField)
      if (DialogManager.dialogIsActive) DialogManager.reset()
      shutDown()
      container.ownerRef match {
        case Some(contRef) =>
          val theClass = AllClasses.get.getClassByID(contRef.ref.typ).asInstanceOf[ClientObjectClass]
          if (theClass.propFields.size > propField) {
            //buttonList=theClass.createMenuItems(propField).filter(_.ccd.editorName==container.containerName)
            //listenTo(buttonList:_*)
            actionButtons = theClass.actionCreateMenuItems(propField).filter(_.ccd.editorName == container.containerName)
            listenTo(actionButtons: _*)
            registerActionButtons()
          } else util.Log.e("wrong propField " + propField + " for class " + theClass)

          //println("set selgroup.children "+selGroup.children)
        case None =>
      }
      lastContainer = cont
      lastPropField = propField
      lastOwnerRef = newContRef
    }
  }

  def listenToButtons(buttons: Seq[StrokableButton]): Unit = listenTo(buttons: _*)

  def deafToButtons(buttons: Seq[StrokableButton]): Unit = deafTo(buttons: _*)
    
  def focusLastContainer():Unit= for(cl<-lastContainer) cl.requestFocus()

  def unregisterActionButtons(): Unit = for (b <- actionButtons) KeyStrokeManager.unregisterReceiver(b)

  def registerActionButtons(): Unit = for (b <- actionButtons) KeyStrokeManager.registerReceiver(b)

  def shutDown(): Unit = {
    deafTo(actionButtons:_*)
    unregisterActionButtons()
    actionButtons=Nil
  }
}



