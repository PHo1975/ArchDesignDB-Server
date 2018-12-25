package runtime.function

import definition.data.{InstanceData, OwnerReference}
import definition.expression.Constant
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule}



class SpreadSheetCellModule extends ActionModule {
  
  val deleteAction=new ActionIterator("Zellinhalte löschen",None,doDelete,false,1000)
  
  val actions = List(deleteAction) 
  
  def doDelete(u:AbstractUserSocket, parent:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]):Boolean = {
    SpreadSheetProxy.deleteSpreadSheetCells(data)	   	  		
  	true
  }
  
  def setObjectType(typeID:Int)={}

}