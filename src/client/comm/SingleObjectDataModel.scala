package client.comm
import definition.data.InstanceData
import definition.data.Reference
import definition.comm.NotificationType
import scala.swing.Swing
import definition.data.Referencable

class SingleObjectDataModel[T](factory:InstanceData=>T,onChanged: ()=>Unit=()=>{},onDelete: ()=>Unit=()=>{}) {
  //var currentInstData:Option[InstanceData];
  var currentData:Option[T]=None
  var subsID:Int = -1
  
  
  
  def load(ref:Referencable)= { 
    shutDown()
    subsID=ClientQueryManager.createSubscription(ref.ref,-1)((command,data)=> Swing.onEDT{
      command match {
    		case NotificationType.sendData|NotificationType.updateUndo|NotificationType.fieldChanged =>
          if(data.size>0){
            currentData=Some(factory(data.head))
            onChanged()
          }
        case NotificationType.instanceRemoved=> currentData=None; onChanged(); onDelete(); shutDown()
        case _=> 
       }
    })  
  } 
  
  def shutDown()= {
    currentData=None
    if(subsID> -1) ClientQueryManager.removeSubscription(subsID)
    subsID= -1
  }

}