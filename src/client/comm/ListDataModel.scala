package client.comm

import definition.comm.NotificationType
import definition.data.{InstanceData, Referencable, Reference}
import javax.swing.AbstractListModel

import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing

class ListDataModel[T<:Referencable](typeFilter:Seq[Int],factory:InstanceData=>T ) extends AbstractListModel[T] {
  var subsID:Int= -1
  val lock=new Object
  val theList=new ArrayBuffer[T]()  
  
  
  def shutDown(): Unit = {
    if(subsID> -1 ) {      
      ClientQueryManager.removeSubscription(subsID)
      theList.clear()
      subsID= -1         
    }
  }
  
  val noListener: () => Unit = ()=>{}
  
  def appliesFilter(data:Referencable): Boolean = typeFilter.isEmpty || typeFilter.contains(data.ref.typ)
  
  def filtered(data:IndexedSeq[InstanceData]): Seq[InstanceData] = if(typeFilter.isEmpty) data
    else data.view.filter(appliesFilter)
  
  def load(superRef:Reference,propField:Int,loadReadyListener:()=>Unit=noListener): Unit ={
    shutDown() 
    
    subsID=ClientQueryManager.createSubscription(superRef,propField.toByte)((command,data)=> Swing.onEDT{
      lock.synchronized{
      command match {
        case NotificationType.sendData| NotificationType.updateUndo=>
          theList.clear()
          theList ++= filtered(data).map(factory)
          fireContentsChanged(this,0, theList.size-1)
          if(command==NotificationType.sendData)loadReadyListener()

        case NotificationType.fieldChanged=>
          val newElem=factory(data.head)
          if(appliesFilter(newElem.ref)){
            val newRef=newElem.ref
            val changedIx=theList.indexWhere(_.ref==newRef) match{
              case -1=> theList +=newElem;theList.size-1
              case ix=> theList(ix)=newElem;ix
            }
            fireContentsChanged(this,changedIx, changedIx)
          }
        case  NotificationType.childAdded =>
          val newElem=data.head
          if(appliesFilter(newElem)){
            theList+=factory(newElem)
            val pos=theList.size-1
            fireIntervalAdded(this, pos, pos)
          }
        case NotificationType.instanceRemoved=>
          if(appliesFilter(data.head)) {
            val theRef=data.head.ref
            theList.indexWhere(_.ref==theRef) match {
              case -1=> util.Log.e("InstanceRemoved "+theRef+" not found in Array")
              case ix=>
                theList.remove(ix)
                fireIntervalRemoved(this,ix,ix)
            }
          }
        case NotificationType.parentNotExistend=> shutDown()
       } 
      }
    })
  } 
  
  def getElementAt(index:Int) = theList(index)
  def getSize: Int =theList.size
}

class InstanceDataListModel(typeFilter:Seq[Int]) extends ListDataModel[InstanceData](typeFilter,a=>a)
