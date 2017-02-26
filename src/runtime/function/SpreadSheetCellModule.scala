package runtime.function

import definition.data.InstanceData
import definition.data.OwnerReference
import definition.expression.Constant
import server.comm.{AbstractUserSocket, JavaClientSocket}
import server.storage.ActionIterator
import server.storage.ActionModule



class SpreadSheetCellModule extends ActionModule {
  
  val deleteAction=new ActionIterator("Zellinhalte löschen",None,doDelete,false,1000)
  
  val actions = List(deleteAction) 
  
  def doDelete(u:AbstractUserSocket, parent:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]):Boolean = {
    SpreadSheetProxy.deleteSpreadSheetCells(data)	   	  		
  	true
  }
  
  def setObjectType(typeID:Int)={}

}