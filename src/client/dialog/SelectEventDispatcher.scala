/**
 * Author: Peter Started:10.11.2010
 */
package client.dialog
import definition.data.Referencable
import definition.data.OwnerReference
import definition.typ.SelectGroup

/** collects all select events and sends them to consumers
 * 
 */
object SelectEventDispatcher extends SelectListener with SelectSender{
	
	var lastSender:Option[SelectSender] = None
	
	def deselect(notify:Boolean) = for(l<-lastSender)l.deselect(notify)
		
	
	def removeSelectListener(listener:SelectListener) = {
		selectListeners -=listener
	}
	
	
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Iterable[SelectGroup[T]],alsoSelected:Iterable[T]) = {
	  //println("sel changed "+sender+" "+groups.mkString(","))
	  for( li <-selectListeners) 
	    li.selectionChanged(sender,groups,alsoSelected)
	  lastSender=Some(sender)
	}

}