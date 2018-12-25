/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import definition.data.Reference

/** Contains Information about one subscription
 * 
 */
sealed abstract class SubscriptionInfo(val connectionEntry:AbstractConnectionEntry,val id:Int,val parentRef:Reference)

class SingleSubscription(override val connectionEntry:AbstractConnectionEntry,override  val id:Int,override val parentRef:Reference)
extends SubscriptionInfo(connectionEntry,id,parentRef) {
	override def equals(other: Any): Boolean = other match {
		case that: SingleSubscription => that.canEqual(this) && this.connectionEntry == that.connectionEntry && this.id==that.id && this.parentRef==that.parentRef
		case _ => false
	}
	override def hashCode: Int = 41 * connectionEntry.hashCode + 1041*id+4041*parentRef.hashCode+3
	override def toString: String = "SingleSubs("+id+ " ref: "+parentRef+")"
	def canEqual(that: SingleSubscription) = true  
}
object SingleSubsciption {	
	def apply(user:ConnectionEntry,id:Int,parentRef:Reference) = new SingleSubscription(user,id,parentRef)	
	def unapply(el: SingleSubscription) = Some((el.connectionEntry,el.id,el.parentRef))
}



case class PropSubscription(override val connectionEntry:AbstractConnectionEntry,override val id:Int,override val parentRef:Reference,propertyField:Byte)
extends SubscriptionInfo (connectionEntry,id,parentRef)

/** a subscription for path views
 * 
 */
case class PathSubscription(override val connectionEntry:AbstractConnectionEntry,override  val id:Int,var path:Seq[Reference])
extends SingleSubscription(connectionEntry,id,null) {
	def updatePath(newPath:Seq[Reference]): PathSubscription = {
		path=newPath
		this
	}
	override def toString: String = "PathSubs(id:"+id+ " path: "+path.mkString+")"
}