/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import definition.data.{InstanceData, OwnerReference, Reference}
import util.Log

import scala.collection.mutable

/**
 * 
 */
object CommonSubscriptionHandler {
	val subscriptionList: mutable.HashMap[Int, SubscriptionInfo] = collection.mutable.HashMap[Int,SubscriptionInfo] ()
	var maxID:Int=0
	private val listLock : AnyRef = new Object()
	
	private val _classHandlerMap=mutable.HashMap[Int,ClassSubscriptionHandler]()
	
	def classHandlerMap(typeID:Int): ClassSubscriptionHandler =
		_classHandlerMap.getOrElseUpdate(typeID,new ClassSubscriptionHandler(typeID))
	
	
	/** adds a new Subscription to the common List
	 * 
	 */
	def addSubscription(user:AbstractConnectionEntry,parentRef:Reference,propertyField:Byte):Int =
		listLock.synchronized {
		  maxID+=1
			val newS= if (propertyField == -1)
				new SingleSubscription(user, maxID, parentRef)
			else PropSubscription(user, maxID, parentRef, propertyField)
			subscriptionList(maxID) = newS		
			classHandlerMap(parentRef.typ ).addSubscription(newS)		
			maxID
		}
	
	/** changes the target of a subscription
	 * 
	 */
	def changeSubscription(user:ConnectionEntry,subsID:Int,parentRef:Reference,propertyField:Byte): Unit =  {
	  	if(subscriptionList.contains(subsID)) Log.e("user"+user  + " Want change subscription "+subsID+ " but it's still in use")
	  	listLock.synchronized {
	  		val newS= if (propertyField == -1) new SingleSubscription(user, subsID, parentRef)
				else PropSubscription(user, subsID, parentRef, propertyField)
				subscriptionList(subsID) = newS		
				classHandlerMap(parentRef.typ ).addSubscription(newS)			
	  	}
	  }
	
	
	/** adds a suscription for a path element
	 * 
	 */
	def addPathSubscription(user:AbstractConnectionEntry,pathList:Seq[Reference]): Int =
		listLock.synchronized {
		  maxID+=1
			val subsID=maxID
			val newS= PathSubscription(user, subsID, pathList)
			subscriptionList(maxID)=newS
			// add single Subscriptions to all path elements
			for(el <-pathList)
				classHandlerMap(el.typ).addPathSubscription(newS,el)			
			subsID
		}
	
	/** a path subscription wants to add another child to the path
	 * 
	 */
	def openChild(subsID:Int,newRef:Reference):Seq[Reference] =   listLock.synchronized {
		 subscriptionList.get(subsID) match {
			 case Some(subs:PathSubscription) =>
				 //System.out.println("open child "+newRef)
				 val newList=subs.path :+ newRef
				 val newSubs=subs.updatePath(newList)
				 subscriptionList(subsID)= newSubs
				 classHandlerMap(newRef.typ).addPathSubscription(newSubs,newRef)
				 newList
			 case _ =>
				 Log.e("Subscription "+subsID+" is no path subscription when open Child ref"+newRef)
				 null
		 }		 
	}
	
	/** a path element wants to reduce the path until to a certain element
	 * @param newPos the number of the element in the path that should be the last element
	 */
	def jumpUp(subsID:Int,newPos:Int): Seq[Reference] = listLock.synchronized {
		subscriptionList.get(subsID) match {
			 case Some(subs:PathSubscription) =>
				 //System.out.println("jumpup " + newPos+" oldpath:"+subs.path)
				 if(subs.path.size<=newPos) {Log.e("Wrong jump Pos "+newPos+" when jumping up "+subs.path .mkString("/")+" subsID:"+subsID);null}
				 else {
					 for (i <-(newPos+1) until subs.path.size) { // remove unneeded entries
						 val pRef=subs.path(i)
						 classHandlerMap(pRef.typ).removeSinglePathSubs(subs,pRef)
						// System.out.println("remove "+pRef)
					 }
           val newList=subs.path.take(newPos+1)
					 subscriptionList(subsID)= subs.updatePath(newList )
           newList
				 }
			 case _ => Log.e("Subscription "+subsID+" is no path subscription when jump Up "+subsID);null
		 }
	}
	
	
	def changePath(subsID:Int,newPath:Seq[Reference]): Unit = listLock.synchronized {
		subscriptionList.get(subsID) match {
			 case Some(subs:PathSubscription) =>
				 // remove old subscription entries
				 for(o <- subs.path)
					 classHandlerMap(o.typ).removePathS(subs)
				 // add new entries
				val newSubs=subs.updatePath(newPath)
				 for (n <- newPath)
           classHandlerMap(n.typ).addPathSubscription(newSubs,n)
				 subscriptionList(subsID)= newSubs
			 case _ => Log.e("Subscription "+subsID+" is no path subscription when changing Path to"+newPath.mkString("/"))
		 }
	}
	
		
	
	def removeSubscription(id:Int): Unit =	listLock.synchronized {
			doRemove(id)						
			subscriptionList.remove(id)			
		}
	
	
	def pauseSubscription(id:Int): Unit =	removeSubscription(id)
	
	private def doRemove(id:Int) = 	subscriptionList.get(id) match {
		case Some(p:PathSubscription) =>
			for (el <-p.path)
				classHandlerMap(el.typ ).removeSubscription(p)
		case Some(a:SubscriptionInfo) => classHandlerMap(a.parentRef.typ).removeSubscription(a)
		case None=> throw new IllegalArgumentException("Removing Subscription "+id+" but it is null")
	}

	
	def getSubscriptionInfo(id:Int):SubscriptionInfo =
		listLock.synchronized {
			subscriptionList(id)
		}
	
	def submitBurstInfo(): Unit = {}

	def instanceChanged(newState:InstanceData): Unit = {
		//System.out.println("comsubsman inst changed "+newState.ref+" "+newState)
		classHandlerMap(newState.ref.typ ).singleInstanceChanged(newState)			 
		for(owner <-newState.owners )
			classHandlerMap(owner.ownerRef.typ).childInstanceChanged(owner.ownerRef,owner.ownerField,newState)	
		for(owner <-newState.secondUseOwners )
			classHandlerMap(owner.ownerRef.typ).childInstanceChanged(owner.ownerRef,owner.ownerField,newState)	
	}
	
	def refreshSubscriptionsFor(parentRef:Reference): Unit = {
		classHandlerMap(parentRef.typ ).refreshSubscriptionsFor(parentRef)
	}
	
	
	def instanceCreated(owner:OwnerReference,newInstance:InstanceData): Unit = {
		//System.out.println("subsMan inst created "+ newInstance.ref)
		if(_classHandlerMap.contains(owner.ownerRef.typ))
		classHandlerMap(owner.ownerRef.typ).instanceCreated(owner,newInstance)		
	}
	
	
	def instanceDeleted(owner:OwnerReference,ref:Reference): Unit = {
		//System.out.println("subsMan inst deleted "+ ref)
		classHandlerMap(ref.typ).instanceDeleted(null,ref)//single
		classHandlerMap(owner.ownerRef.typ).instanceDeleted(owner,ref) // prop
	}
	
	def instanceDeletedViaUndo(ref:Reference): Unit =
	  classHandlerMap(ref.typ).instanceDeleted(null,ref)
	
	
	def userLogsOff(userID:AbstractConnectionEntry): Unit = listLock.synchronized {
		//System.out.println("subsMan user log off "+userID)
		for(subs <- subscriptionList.values )			
			if(subs.connectionEntry==userID) {
				removeSubscription(subs.id)				 
			}
	}
	
	def refreshAfterUndo(): Unit = {
	  //println("frefresh after undo")
		for(subs <- subscriptionList.values )
			subs.connectionEntry.queryHandler.refreshSubscription(subs)
	  //println("refresh done ")
	}
}