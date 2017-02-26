/**
 * Author: Peter Started:25.09.2010
 */
package runtime.function

import definition.data.InstanceData
import definition.data.OwnerReference
import definition.expression.Constant
import server.comm.{AbstractUserSocket, JavaClientSocket}
import server.storage.ActionIterator
import server.storage.ActionModule
import transaction.handling.TransactionManager

/** Action module for base class
 * 
 */
class BaseClassModule extends ActionModule {
	val deleteAction=new ActionIterator("Objekt Löschen",None,doDelete,false,1000)
  val actions = List(deleteAction) 
  
  def doDelete(u:AbstractUserSocket, parent:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]):Boolean = {
  	for(inst <-data) 	TransactionManager.tryDeleteInstance(inst.ref,Some(parent),None)  	
  	true
  }
	
	def setObjectType(typeID:Int)={}

}