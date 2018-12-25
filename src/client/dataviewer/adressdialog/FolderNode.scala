package client.dataviewer.adressdialog

import client.calender.{Address, CalendarHelper, Callbackable, Folder}
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data.{InstanceData, Referencable, Reference}
import definition.expression.StringConstant
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeModel, TreePath}

import scala.collection.JavaConverters._
import scala.swing.Swing

trait SomeNode{
  self : DefaultMutableTreeNode=>
  def ref: Reference =getUserObject().asInstanceOf[Referencable].ref
  def getUserObject:Object
}


class AddressNode(data:InstanceData)extends DefaultMutableTreeNode(new Address(data)) with SomeNode {
  override def isLeaf=true
} 


class FolderNode(folder:Folder,model:DefaultTreeModel) extends DefaultMutableTreeNode(folder) with SomeNode with Callbackable{
  var subsID: Int = -1
  var loadCallBack:()=>Unit=_
  def load(): Unit = {
	  subsID=ClientQueryManager.createSubscription(folder.ref, 1)((command,data)=> Swing.onEDT{
	    command match {
	      case NotificationType.sendData|NotificationType.updateUndo =>
          removeAllChildren()
          for (d<-data) super.add(new AddressNode(d))
          model.reload(this)
          if(command==NotificationType.sendData) if(loadCallBack!=null)loadCallBack()
        case NotificationType.childAdded =>
          add(new DefaultMutableTreeNode(new Address(data.head)))
          model.reload(this)
        case NotificationType.fieldChanged =>
          val searchRef=data.head.ref
          children().asScala.indexWhere(node=>node.asInstanceOf[DefaultMutableTreeNode].getUserObject().asInstanceOf[Referencable].ref==searchRef) match{
            case -1 => util.Log.e("field changed but not found "+data.head.ref)
            case ix => getChildAt(ix).asInstanceOf[DefaultMutableTreeNode].setUserObject(new Address(data.head))
          }
          model.reload(this)
        case NotificationType.instanceRemoved=>
          val searchRef=data.head.ref
          children().asScala.indexWhere(node=>node.asInstanceOf[DefaultMutableTreeNode].getUserObject().asInstanceOf[Referencable].ref==searchRef) match{
            case -1 => util.Log.e("instance removed but not found "+data.head.ref)
            case ix => remove(ix)
          }
          model.reload(this)
      }
	  })
   }
  
	 def shutDown():Unit={
	   if(subsID> -1)ClientQueryManager.removeSubscription(subsID)
	   subsID= -1	
	   removeAllChildren()
	 }
	 
	 def run(callback:()=>Unit): Unit = {
	   loadCallBack=callback
	   load()
	 }
	 override def isLeaf=false
}

class VirtualNode(folder:Folder,val propField:Int) extends DefaultMutableTreeNode(folder) with SomeNode {
   override def isLeaf=false
}



class ProjectNode (folder:Folder,readyListener:()=>Unit) extends DefaultMutableTreeNode(folder) with SomeNode {
  var model:DefaultTreeModel=_
  var bidderSubsID: Int = -1
  ClientQueryManager.queryInstance(folder.ref,1).headOption match { // client Address
    case Some(data)=> add(new AddressNode(data))
    case None =>
  }
  
  val bidderFolder=new VirtualNode(new Folder(folder.ref,"Unternehmen",""),2)
  val otherFolder=new VirtualNode(new Folder(folder.ref,"Andere",""),3)
  
  def handleCommands(superFolder:VirtualNode,callback:()=>Unit)(command:NotificationType.Value,data:IndexedSeq[InstanceData]): Unit = Swing.onEDT{
    command match {
      case NotificationType.sendData|NotificationType.updateUndo =>
        if(command==NotificationType.updateUndo) removeAllChildren()
        var fchildren:List[FolderNode]=Nil
        for(d<-data){
          val afolder=new FolderNode(Folder(d),model)
          superFolder.add(afolder)
          fchildren=afolder::fchildren
        }
        if(command==NotificationType.sendData) CalendarHelper.callBackLoop(fchildren,()=>{callback()})
        else model.reload(superFolder)
      case NotificationType.childAdded=>
        superFolder.add(new FolderNode(Folder(data.head),model))
        model.reload(superFolder)
      case NotificationType.fieldChanged =>
        val searchRef=data.head.ref
        superFolder.children().asScala.indexWhere(node=>node.asInstanceOf[DefaultMutableTreeNode].getUserObject().asInstanceOf[Referencable].ref==searchRef) match{
          case -1 => util.Log.e("field changed but not found "+data.head.ref)
          case ix => superFolder.getChildAt(ix).asInstanceOf[DefaultMutableTreeNode].setUserObject(Folder(data.head))
        }
        model.reload(superFolder)
      case NotificationType.instanceRemoved=>
        val searchRef=data.head.ref
        superFolder.children().asScala.indexWhere(node=>node.asInstanceOf[DefaultMutableTreeNode].getUserObject().asInstanceOf[Referencable].ref==searchRef) match{
          case -1 => util.Log.e("instance removed but not found "+data.head.ref)
          case ix => superFolder.remove(ix)
        }
        model.reload(superFolder)
    }	
  }
  add(bidderFolder)
  add(otherFolder)  
  val otherSubsID: Int = ClientQueryManager.createSubscription(folder.ref,3)(handleCommands(otherFolder, ()=> loadOthers()))
  
  def loadOthers (): Unit = {
    bidderSubsID=ClientQueryManager.createSubscription(folder.ref,2)(handleCommands(bidderFolder,()=>{
      //println("ready loaded "+getChildCount())
      model.reload()
      readyListener()
    }))
  }
  
  def shutDown(): Unit = {
    ClientQueryManager.removeSubscription(otherSubsID)    
    ClientQueryManager.removeSubscription(bidderSubsID)
    bidderSubsID= -1
	  for(ch<-super.children().asScala)
	     ch match {
	     case f:FolderNode=>f.shutDown()
	     case _=>
	  }
	  removeAllChildren()
  }  
}

class MyTreeModel(prNode:ProjectNode) extends DefaultTreeModel(prNode) {
  override def valueForPathChanged(path:TreePath, newValue:Object): Unit = {
    path.getLastPathComponent() match {
      case dn:DefaultMutableTreeNode=> dn.getUserObject() match {
	      case folder:Folder=>
          ClientQueryManager.writeInstanceField(folder.ref,0,StringConstant(newValue.toString))
        case o=>
	    }
	    case _=>
    }     
    
  }
}
