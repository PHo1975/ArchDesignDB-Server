package runtime.function

import definition.data.{InstanceData, OwnerReference}
import definition.expression.Constant
import definition.typ.ActionTrait
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule, StorageManager}
import transaction.handling.TransactionManager
import util.Log

/**
 * Created by Kathi on 15.05.2015.
 */
class PlotDesignModule extends ActionModule {
  var designTypeID:Int= -1
  override def actions: Iterable[ActionTrait] =List(cleanup)

  override def setObjectType(typeID: Int): Unit = designTypeID=typeID

  val cleanup=new ActionIterator("Bereinigen",None,doCleanup)

  def doCleanup(u:AbstractUserSocket, owner:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]) =  {
    for(d<-data;layerRef<-StorageManager.getInstPropList(d.ref,1);
        layerInst=StorageManager.getInstanceData(layerRef);plotRef=layerInst.fieldValue(0).toObjectReference){
      if(!StorageManager.instanceExists(plotRef.typ,plotRef.instance))  {
        Log.w("delete LayerRef "+layerRef+" => "+plotRef)
        TransactionManager.tryDeleteInstance(layerRef,None,None)
      }

    }
    true
  }
}
