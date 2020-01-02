/**
 * Author: Peter Started:22.09.2010
 */
package server.storage

import definition.data.{InstanceData, OwnerReference}
import definition.expression.Constant
import definition.typ.{ActionTrait, ParamQuestion}
import server.comm.AbstractUserSocket

import scala.xml.Elem

/** Implementation of an action
 * 
 */

class ActionImpl(val name:String, override val question:Option[ParamQuestion],
								 val func:(AbstractUserSocket,InstanceData,Iterable[(String,Constant)]) => Boolean, val rebound:Boolean=false, val buttonID:Int= 0) extends ActionTrait  {
	def isIterator=false
	def toXML: Elem =   {
  	 <Action  name={name} iter={"0"} reb={if(rebound)"1" else "0"} id={buttonID.toString} >
  	 { question match {
  		 case Some(q)=>q.toXML
  		 case _ =>  		 
  		 }
  	 }
  	 </Action>
   }
}
	
/**
 * @param name name of the iterator action
 * @param question definition of the questions for the action dialog
 * @param func the function to call with parameters(parentRef,childRefs,params)
 * @param rebound should the question be asked repeatedly
 * @param buttonID number to order the buttons
 */
class ActionIterator(val name:String,override val question:Option[ParamQuestion],
	val func:(AbstractUserSocket,OwnerReference,Iterable[InstanceData],Iterable[(String,Constant)]) => Boolean,val rebound:Boolean=false,val buttonID:Int= 0) extends ActionTrait  {
	def isIterator=true	
	def toXML: Elem = {
  	 <Action  name={name} iter={"1"} reb={if(rebound)"1" else "0"} id={buttonID.toString} >
  	 { question match {
  		 case Some(q)=>q.toXML
  		 case _ =>  		 
  		 }
  	 }
  	 </Action> 
   }
}


class CreateActionImpl(val name: String, override val question: Option[ParamQuestion], val func: (AbstractUserSocket, Iterable[InstanceData], Seq[(String, Constant)], Int, Seq[(Int, Constant)]) => Boolean, val rebound: Boolean = false) extends ActionTrait  {
	def isIterator=false
	def buttonID=0
	def toXML: Elem =   {
  	 <Action  name={name} iter={"1"} reb={if(rebound)"1" else "0"} >
  	 { question match {
  		 case Some(q)=>q.toXML
  		 case _ =>  		 
  		 }
  	 }
  	 </Action> 
   }
}