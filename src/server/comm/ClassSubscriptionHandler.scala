/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import definition.data.{InstanceData, OwnerReference, Reference}
import util.Log

import scala.collection.mutable

/** handles subscriptions for this class
 * 
 */
class ClassSubscriptionHandler(typID:Int) {
	// subscriptions to single objects
	protected var singleSubsMap: mutable.Map[Reference, List[SingleSubscription]] =mutable.Map[Reference,List[SingleSubscription]]()
	// subscriptions to parents
	protected var propSubsMap: mutable.Map[Reference, List[PropSubscription]] =mutable.Map[Reference,List[PropSubscription]]()
	
	def addSubscription(s:SubscriptionInfo): Unit =
		s match {
			case a:SingleSubscription => addSingleS(a,a.parentRef)
			case b:PropSubscription => addPropS(b)
		}				

	
	def addPathSubscription(p:PathSubscription,ref:Reference): Unit = {
			val list=if(singleSubsMap.contains(ref )) p :: singleSubsMap(ref) else List(p)
			singleSubsMap.put(ref, list)
	}
	
	def removeSubscription(s:SubscriptionInfo): Unit =
		s match {
			case c:PathSubscription => removePathS(c)
			case a:SingleSubscription => removeSingleS(a)
			case b:PropSubscription => removePropS(b)
		}
	
	def removeSinglePathSubs(s:PathSubscription,forElem:Reference):Unit=
		if(singleSubsMap.contains(forElem)) {
			val list=singleSubsMap(forElem)
			singleSubsMap.put(forElem,list.filterNot (_ == s))
		} else Log.e("RemoveSingePathSubs "+s+" forElem:"+forElem+" Cant find elem !!")

	def singleInstanceChanged(newState:InstanceData): Unit =
		if (singleSubsMap.contains(newState.ref))	{
			val list=singleSubsMap(newState.ref)
			//System.out.println("single instance changed subslist:"+list.mkString(", "))
			for(subs <-list)
				subs.connectionEntry.queryHandler.notifyInstanceChanged(subs,newState)
		}
	
	def refreshSubscriptionsFor(parentRef:Reference): Unit =
		if(propSubsMap.contains(parentRef)) {
			val list=propSubsMap(parentRef)
			for(subs <-list)				
					subs.connectionEntry.queryHandler.refreshSubscription(subs)
		}
	
	def childInstanceChanged(ownerRef:Reference,propField:Byte,childInst:InstanceData): Unit =
		//System.out.println("csh single instance changed subslist owner:"+ownerRef+" childInst:"+childInst+" prField:"+propField)
		if(propSubsMap.contains(ownerRef)) {
			val list=propSubsMap(ownerRef)
			for(subs <-list)
				if(subs.propertyField ==propField)
					subs.connectionEntry.queryHandler.notifyInstanceChanged(subs,childInst)
		}

	
	/*def burstNotifyChanges(ownerRef:Reference ,propField:Byte) = {
		if(propSubsMap.contains(ownerRef)) {
			val list=propSubsMap(ownerRef)
			for(subs <-list)
				if(subs.propertyField ==propField)
					subs.user.queryHandler.burstNotifyChanges(subs,ownerRef,propField)
		}
	}*/
	
	def instanceCreated(owner:OwnerReference,newInstance:InstanceData): Unit =
		if(propSubsMap.contains(owner.ownerRef)) {
			val list=propSubsMap(owner.ownerRef)
			for(subs <-list)
				if(subs.propertyField ==owner.ownerField )
					subs.connectionEntry.queryHandler.notifyInstanceAdded(subs,newInstance)
		}
	
	def instanceDeleted(owner:OwnerReference,ref:Reference): Unit =
		if(owner==null) { // check single subscriptions
			if(singleSubsMap.contains(ref)) {
				val list=singleSubsMap(ref)
				for(subs <-list)
					subs.connectionEntry.queryHandler.notifyInstanceDeleted(subs,ref)
				singleSubsMap.remove(ref)	
			}				
			if(propSubsMap.contains(ref)) { // a parent ref of a subscription is deleted
				val list=propSubsMap(ref)
				for(subs <-list)
					subs.connectionEntry.queryHandler.notifyParentDeleted(subs,ref)
				propSubsMap.remove(ref)
			}	
		} // check property subscriptions
		else if( propSubsMap.contains(owner.ownerRef)) {
			val list=propSubsMap(owner.ownerRef)
			for(subs <-list)
				if(subs.propertyField ==owner.ownerField )
					subs.connectionEntry.queryHandler.notifyInstanceDeleted(subs,ref)
		}
	
	// ********************** Internal routines ***********************
	
	private def addSingleS(s:SingleSubscription,ref:Reference) =
		singleSubsMap.put(ref,  if(singleSubsMap.contains(ref )) s :: singleSubsMap(ref)
														else List(s))

	
	private def addPropS(s:PropSubscription) =
		propSubsMap.put(s.parentRef,if(propSubsMap.contains(s.parentRef)) s :: propSubsMap(s.parentRef)
																else List(s))


	private def removeSingleS(s:SingleSubscription) =
		//System.out.println("csm remove singleSubs:"+s)
		if(singleSubsMap.contains(s.parentRef)) {
			val list=singleSubsMap(s.parentRef)
			if(list.contains(s))
				singleSubsMap.put(s.parentRef,list.filterNot(_ ==  s))
		}
		else Log.e("Remove SingleSubscription, Entry for "+s.parentRef+" not found")
	
	private def removePropS(s:PropSubscription) =
		//System.out.println("csm remove propSubs:"+s)
		if(propSubsMap.contains(s.parentRef)) {
		val list=propSubsMap(s.parentRef)
		if(list.contains(s))
			propSubsMap.put(s.parentRef,list.filterNot(_ == s))
		} else Log.e("Remove PropertySubscription, Entry for "+s.parentRef+" not found")

	
	def removePathS(s:PathSubscription): Unit =
		//System.out.println("csm remove pathSubs:"+s)
		for((k,list) <-singleSubsMap.iterator)
			if(list.contains(s))
				 singleSubsMap.put(k,list.filterNot (_ == s))
}

object ClassSubscriptionHandler {
	val ALLFIELDS: Byte = (-2).toByte
}