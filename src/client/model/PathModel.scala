/**
 * Author: Peter Started:16.09.2010
 */
package client.model

import javax.swing.AbstractListModel
import definition.data._
import client.comm._
import definition.comm._
import scala.collection.immutable.IndexedSeq
import javax.swing.SwingUtilities
import scala.swing.Swing

/**
 * 
 */
class PathModel extends AbstractListModel[InstanceData] {
	
	var subsID= -1
	//var dataList:Option[Seq[InstanceData]]= None
	var dataList:Seq[InstanceData]=Seq.empty
	
	def loadPath(newPath:Seq[Reference],readyFunc:(Seq[Reference]) => Unit) = {
	 // var firstLoad=true	
    //println("load path "+newPath.mkString(", "))
		if(subsID== -1) {
			subsID= ClientQueryManager.createPathSubscription(newPath) { 
				(ntype: NotificationType.Value,data:IndexedSeq[InstanceData]) => Swing.onEDT{
				  var dataChanged=true					
					val oldSize=dataList.size			
          //println("notific: "+ntype+" data:"+data)
					ntype match {
						case NotificationType.sendData|NotificationType.updateUndo  =>
							if(InstanceData.compareLists(dataList,data)) dataChanged=false
							if (dataChanged) dataList=data
						case NotificationType.fieldChanged  =>
							//System.out.println("path field changed "+data(0))
							val searchRef=data.head.ref
							dataList=dataList.map(el=> if(el.ref==searchRef) data.head else el)
						case NotificationType.instanceRemoved =>
							val searchRef=data.head.ref
							dataList =dataList.filter(searchRef!= _.ref)
						case NotificationType.parentNotExistend=> util.Log.e("parent does not exist "+newPath.mkString("|")); shutDown()
					}
					if(dataChanged) {
						val newSize=dataList.size
						if (newSize>oldSize) fireIntervalAdded(this,oldSize,newSize-1)
						else if(newSize<oldSize) fireIntervalRemoved(this,newSize,oldSize-1)
						else fireContentsChanged(this,0,newSize)						
						if(ntype==NotificationType.sendData&& readyFunc!=null) 							
							readyFunc(dataList.map( _.ref))											 
					}
				}
			}
		}
		else  // subscription exists already
			ClientQueryManager.pathSubs_changePath(subsID,newPath)		
	}	
	
	
	 def addPathElement(newElement:Reference) ={
		//println("add PathElement "+newElement)
		if(subsID> -1) {
			ClientQueryManager.pathSubs_addPathElement(subsID,newElement)			
		}
	}			
	
	
	 def jumpUp(newPos:Int) = 
		if(subsID> -1) { 
			//System.out.println("Jumping to "+newPos)
			dataList=dataList.take(newPos+1)
			ClientQueryManager.pathSubs_jumpUp(subsID,newPos)
			fireContentsChanged(this,0,dataList.size)
		}		
		
  def getSize(): Int = if(dataList.isEmpty) 0 else dataList.size-1
  

  def getElementAt(index: Int): InstanceData = getInstanceAt(index)
  
  
  def getInstanceAt(index:Int):InstanceData = if(dataList.size>index) dataList(index)  	
     else null
  
  def shutDown() = {
  	if(subsID > -1) {
  		ClientQueryManager.removeSubscription(subsID)
  		dataList=Seq.empty
  		subsID= -1
  	}
  }
  
  def getTitleText:String = {
		dataList.drop(1).mkString("\\ ")		
	}

}