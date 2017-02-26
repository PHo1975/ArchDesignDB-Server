/**
 * Author: Peter Started:09.10.2010
 */
package client.dialog

import definition.data._
import definition.typ.SelectGroup


/** a  component that sends out selection messages 
 * 
 */

trait SelectSender {	
	val selectListeners=collection.mutable.LinkedHashSet[SelectListener]()
	
	def deselect(notify:Boolean):Unit
	
	def registerSelectListener(listener:SelectListener) =	selectListeners+=listener	
	
	def notifySelectListeners[T <: Referencable](selection:Iterable[SelectGroup[T]]):Unit= 
	  for(s<-selectListeners) 
	    s.selectionChanged(this,selection)
}


/** a component that receives select messages
 * 
 */
trait SelectListener {
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Iterable[SelectGroup[T]],alsoSelected:Iterable[T]=Nil): Unit
}



object EMPTY_GROUP extends SelectGroup[Referencable](EMPTY_OWNERREF,Seq.empty) {
	val list=List(this)
}

