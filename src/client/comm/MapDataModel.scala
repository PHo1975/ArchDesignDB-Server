package client.comm
import definition.comm.{KeyAble, NotificationType}
import definition.data.{InstanceData, Reference}

import scala.collection.immutable.TreeMap
import scala.swing.Swing



abstract class MapDataModel[K,T <:KeyAble[K]] {
  var subsID:Int= -1
  val lock=new Object
  var theMap:Map[K,T]=emptyMap()
  
  def shutDown(): Unit = {
    if(subsID> -1 ) {      
      ClientQueryManager.removeSubscription(subsID)
      theMap=emptyMap()
      subsID= -1    	   
    }
  }
  
  def emptyMap():Map[K,T]=collection.immutable.HashMap.empty
  
  def dataToMap(data:IndexedSeq[InstanceData]): Map[K, T] =data.map(id=>{
    		    val elem=elemFactory(id)
    		    (elem.key,elem)
    		  }).toMap
  
  def load(superRef:Reference,propField:Int): Unit ={
    shutDown() 
    //println("MapDataModel load "+superRef+" propField:"+propField)
    subsID=ClientQueryManager.createSubscription(superRef,propField.toByte)((command,data)=> Swing.onEDT{
      lock.synchronized{
    	command match {
    		case NotificationType.sendData =>
          theMap=emptyMap()++dataToMap(data)
          sendData()
        case NotificationType.updateUndo=>
          theMap=emptyMap()++dataToMap(data)
          updateUndo()
        case NotificationType.fieldChanged=>
          val newRef=data.head.ref
          val newElem=elemFactory(data.head)
          theMap.values.find(_.ref==newRef) match{
            case Some(oldElem)=>
              theMap=(theMap- oldElem.key)+((newElem.key,newElem))
            case None => theMap=theMap.updated(newElem.key,newElem)
          }
          fieldChanged(newElem)
        case  NotificationType.childAdded =>
          val newElem=elemFactory(data.head)
          theMap=theMap+((newElem.key,newElem))
          childAdded(newElem)
        case NotificationType.instanceRemoved=>
          val theRef=data.head.ref
          theMap.values.find(_.ref==theRef) match {
            case Some(oldElem)=>
              theMap=theMap- oldElem.key
              instanceRemoved(oldElem)
            case None => // println("InstanceRemoved:"+theRef+" not found in Map !")
          }
        case NotificationType.parentNotExistend=> sendData()
    	 }      
      }
    })
  }
  
  def sendData():Unit
  
  def updateUndo():Unit = {}
  
  def fieldChanged(elem:T):Unit = {}
  
  def childAdded(elem:T):Unit = {}
  
  def instanceRemoved(elem:T):Unit = {}
  
  def elemFactory(data:InstanceData):T
  
  def findKeyByRef(ref:Reference):Option[K]= {
  		theMap.find(_._2.ref==ref) map(_._1)
  }
  
}

abstract class TreeMapDataModel[K,T <:KeyAble[K]](implicit val ord:Ordering[K]) extends MapDataModel[K,T] {
  override def emptyMap(): TreeMap[K, T] =TreeMap[K,T]()
}

