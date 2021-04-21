package server.ava

import definition.data.InstanceData
import definition.expression.Constant
import definition.typ.{ActionTrait, PanelRemoteQuestion}
import server.comm.AbstractUserSocket
import server.storage.{ActionImpl, ActionModule}

class PSColHeaderActionModule extends ActionModule{

  val importX84Action=new ActionImpl("X84 importieren",Some(PanelRemoteQuestion("client.model.Ava.X84ImportPanel")),doImportX84)
  override def actions: Iterable[ActionTrait] = List(importX84Action)

  override def setObjectType(typeID: Int): Unit = {  }

  def doImportX84(u:AbstractUserSocket, data:InstanceData, param:Iterable[(String,Constant)]):Boolean = {
    true
  }
}
