/**
 * Author: Peter Started:30.10.2010
 */
package client.dialog

import definition.data.Referencable
import definition.expression.Constant

import scala.collection.mutable

/** 
 *  Container Listeners are notified when a Container of elements is focused
 *  the NewPanel Area can show appropriate CreateActions to create new elements for that container
 */
trait ContainerFocusListener {
	def containerFocused(container:FocusContainer, propField:Int): Unit
}

trait FocusContainer {

  val containerFocusListeners: mutable.HashSet[ContainerFocusListener] = collection.mutable.HashSet[ContainerFocusListener]()
  
  protected var afterCASReceived:Option[ ()=>Unit]=None

  def registerContainerListener(newList: ContainerFocusListener): Unit =
		containerFocusListeners+=newList

	def notifyContainerListeners(propField:Int):Unit = 				
    for(list<-containerFocusListeners)
		  list.containerFocused(this,propField)
		
  
  /** notifies the Container that a createAction was started that can create an object in the container
   * so the container can select it
   * @param numCreatedElements how many new elements were created
   */
  def createActionStarted(numCreatedElements:Int):Unit
  
  def hasCreateActionStarted:Boolean
  
  /** stops the CreateActionStarted mode in the container, so that new elements wont get selected
   * 
   */
  def resetCAS():Unit={
      afterCASReceived=None
  }
  
  def containerName:String
  
  /** Reference of the current super  data object of the container
   * 
   */
  def containerRef:Option[Referencable]
  
  def requestFocus():Unit

  def onCASReceived(func: () => Unit): Unit = afterCASReceived = Some(func)

  protected def casReceived(): Unit = {
    //println("Cas Received "+afterCASReceived)
    for (listener<-afterCASReceived) listener()      
    resetCAS()
    }
  
  
  /** Format Field Values to give to a new created Object
   * @param forType class type of the object to create
   * @return list of (formatfieldNr,FieldValue) 
   */
  def getCreationFormatValues(forType:Int):Seq[(Int,Constant)]=Nil
  
  def actionStopped():Unit
}

trait AbstractFocusContainer extends FocusContainer {
  def createActionStarted(numCreatedElements: Int): Unit = {}
		
	def actionStopped():Unit=requestFocus()
  def hasCreateActionStarted:Boolean=false
}