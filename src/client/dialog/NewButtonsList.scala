/**
 * Author: Peter Started:27.10.2010
 */
package client.dialog

import scala.swing.Reactor
import scala.swing.event.ButtonClicked

import client.comm.ClientObjectClass
import client.comm.ClientQueryManager
import client.comm.KeyStrokeManager
import definition.data.EMPTY_OWNERREF
import definition.data.Referencable
import definition.data.Reference
import definition.typ.AllClasses
import definition.typ.SelectGroup

/** area where "New panels" are placed
 * 
 */

object NewButtonsList extends Reactor with ContainerFocusListener {
  var lastContainer:Option[FocusContainer]=None
  var lastSuperInstRef:Option[Reference]=None
  var lastPropField:Int= -1
  private val selGroup=new SelectGroup[Referencable](EMPTY_OWNERREF,Seq[Referencable]())
  //var buttonList:Seq[CreateMenuButton]=Nil
  var actionButtons:Seq[CreateActionMenuButton]=Nil
  
  reactions += {
    case ButtonClicked(theBut:CreateActionMenuButton) => if(selGroup.children.nonEmpty){
      theBut.ccd.action match {       
        case Some(action)=>DialogManager.startCreateActionDialog(action,selGroup,theBut.ccd.childClassID,theBut.propField)          
        case _ => throw new IllegalArgumentException("No Action defined in CreateActionButton "+theBut) 
      }          
    } 
    case ButtonClicked(theBut:CreateMenuButton)=> if(selGroup.children.nonEmpty) {
      DialogManager.reset()
      for(lc<-NewButtonsList.lastContainer) {
        //println("newbuttonslist button clicked "+theBut.text)
        lc.createActionStarted(1)
        val formatValues= lc .getCreationFormatValues(theBut.ccd.childClassID)        
        ClientQueryManager.executeCreateAction(selGroup.children,theBut.ccd.childClassID,theBut.propField,"*",Seq(),formatValues)
      }
    }
  }
  def containerFocused(container:FocusContainer, propField:Int):Unit = {    
    //println("Dispatcher Container Focused :"+container.getClass().toString+" ref:"+container.containerRef+" propfield:"+propField)
    val cont=Some(container)
    val newContRef=container.containerRef.map(_.ref)
  	if(!(cont==lastContainer&&newContRef==lastSuperInstRef&&propField==lastPropField)) {
      //println("ContainerFocus the same ")
      if (DialogManager.dialogIsActive) DialogManager.reset()
      shutDown()
      container.containerRef match {
        case Some(contRef) =>
          val theClass = AllClasses.get.getClassByID(contRef.ref.typ).asInstanceOf[ClientObjectClass]
          if (theClass.propFields.size > propField) {
            //buttonList=theClass.createMenuItems(propField).filter(_.ccd.editorName==container.containerName)
            //listenTo(buttonList:_*)
            actionButtons = theClass.createActionMenuItems(propField).filter(_.ccd.editorName == container.containerName)
            listenTo(actionButtons: _*)
            registerActionButtons()
          } else util.Log.e("wrong propField " + propField + " for class " + theClass)
          selGroup.children = List(contRef)
          //println("set selgroup.children "+selGroup.children)
        case None =>
      }
      lastContainer = cont
      lastPropField = propField
      lastSuperInstRef = newContRef
    }
  }

  def listenToButtons(buttons:Seq[StrokableButton])=listenTo(buttons:_*)

  def deafToButtons(buttons:Seq[StrokableButton])=deafTo(buttons:_*)
    
  def focusLastContainer():Unit= for(cl<-lastContainer) cl.requestFocus()
  
  def unregisterActionButtons()=for(b<-actionButtons) KeyStrokeManager.unregisterReceiver(b)    
  
  def registerActionButtons()= for(b<-actionButtons) KeyStrokeManager.registerReceiver(b)
  
  def shutDown()= {    
    deafTo(actionButtons:_*)
    unregisterActionButtons()
    actionButtons=Nil
  }
}



