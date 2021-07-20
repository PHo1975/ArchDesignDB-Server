/**
 * Author: Peter Started:25.09.2010
 */
package runtime.function

import definition.data.{InstanceData, OwnerReference, PropertyFieldData}
import definition.expression.Constant
import definition.typ.ActionTrait
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule, StorageManager}
import transaction.handling.{ActionList, DataChangeAction, TransactionManager}

/** Action module for base class
 * 
 */
class BaseClassModule extends ActionModule {
	val deleteAction=new ActionIterator("Objekt Löschen",None,doDelete,false,1000)
  val actions = List(deleteAction) 
  
  def doDelete(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
  	for(inst <-data) 	TransactionManager.tryDeleteInstance(inst.ref,Some(parent),None)  	
  	true
  }
	
	def setObjectType(typeID:Int): Unit ={}

}

class FolderModule extends ActionModule {
	val sortAction=new ActionIterator("Sortieren",None,doSort,false)
	override def actions: Iterable[ActionTrait] = List(sortAction)

	def doSort(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
		for(folderInst <-data){
			for(props<-ActionList.getInstanceProperties(folderInst.ref)){
				val pfield=props.propertyFields(1)
				val dataList=pfield.propertyList.view.map(StorageManager.getInstanceData).sortBy(_.toString().toLowerCase).map(_.ref)
				ActionList.addTransactionData(folderInst.ref,
					DataChangeAction(None,Some(
						props.changeField(1,new PropertyFieldData(pfield.isSingle,dataList.toIndexedSeq))
					))
				)
			}
		}
		true
	}


	override def setObjectType(typeID: Int): Unit = {}
}