package client.calender

import client.calender.CalendarHelper._
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data._
import javafx.collections.ObservableList
import javafx.scene.control.TreeItem
import javafx.util.StringConverter

import scala.jdk.CollectionConverters._

trait AdTreeNode extends Referencable{
  def name:String  
  override def toString: String =name
  
}

case class Folder(ref:Reference,name:String,details:String) extends AdTreeNode
object Folder{
  val converter = new StringConverter[Folder]{
    def fromString(st:String)= new Folder(EMPTY_REFERENCE,st,"")
    def toString(f:Folder): String =f.name
  }
  val EMPTY_FOLDER=new Folder(EMPTY_REFERENCE,"","")
  
  def apply(data:InstanceData): Folder =if(data==null) EMPTY_FOLDER
  	else new Folder(data.ref,data.fieldValue.head.toString,data.fieldValue(1).toString)
  
}

case class Address(ref:Reference,owner:OwnerReference,name:String,prename:String,street:String,zip:String,city:String,
    phone:String,fax:String,email:String,pers:String) extends AdTreeNode {
  def this(data:InstanceData)= this(data.ref,data.owners(0),data.fieldValue(1).toString,data.fieldValue.head.toString,data.fieldValue(2).toString,data.fieldValue(3).toString,
      data.fieldValue(4).toString,data.fieldValue(5).toString,data.fieldValue(6).toString,data.fieldValue(7).toString,data.fieldValue(8).toString)
  override def toString: String =prename+" "+name
}



class MyTreeItem[T<: AdTreeNode](n:T) extends TreeItem[T](n){
  def shutDown():Unit= {        
    for(ch<-getChildren.asScala) ch match {
      case tch:MyTreeItem[T]=>tch.shutDown()
      case _=>
    }
    getChildren.clear()
  } 
  
  override def equals(other: Any): Boolean =
		other match {
				case that: MyTreeItem[_] => (that canEqual this) && getValue==that.getValue								
				case _ => false
		}
	
	def canEqual(other: Any): Boolean = other.isInstanceOf[MyTreeItem[_]]
}

class FolderTreeItem(folder:Folder) extends MyTreeItem[AdTreeNode](folder) {
  var subsID: Int = -1
  def load(): Unit = {
    //println("load "+folder.name+" "+folder.ref)
	  subsID=ClientQueryManager.createSubscription(folder.ref, 1)((command,data)=> runInFx{
	    command match {
	      case NotificationType.sendData|NotificationType.updateUndo =>
          val chList=super.getChildren
          chList.clear()
          val list=(data map (d=>new MyTreeItem(new Address(d)))).asInstanceOf [Seq[TreeItem[AdTreeNode]]]
          chList.addAll(list:_*)
          setExpanded(true)
        case NotificationType.childAdded =>
          keepSelectedAddress{ super.getChildren.add(new MyTreeItem(new Address(data.head)))}
        case NotificationType.fieldChanged =>
          val chList=super.getChildren
          val searchRef=data.head.ref
          chList.asScala.indexWhere(_.getValue.ref==searchRef) match{
            case -1 => util.Log.e("field changed but not found "+data.head.ref)
            case ix => keepSelectedAddress{chList.set(ix, new MyTreeItem(new Address(data.head)))}
          }
        case NotificationType.instanceRemoved=>
          val chList=super.getChildren
          val searchRef=data.head.ref
          chList.asScala.indexWhere(_.getValue.ref==searchRef) match{
            case -1 => util.Log.e("instance removed but not found "+data.head.ref)
            case ix => keepSelectedAddress{util.Log.e(" adress removed ix:"+ix+" "+chList.get(ix).getValue.name);chList.remove(ix,ix+1)}
          }
      }
	  })
  }
  
  override def isLeaf(): Boolean = {
    if(subsID== -1 )false
    else super.getChildren.isEmpty()
  }
  
  override def getChildren: ObservableList[TreeItem[AdTreeNode]] = {
    if(subsID== -1) load()
    super.getChildren
  }
  
  override def shutDown(): Unit = {
    super.shutDown()
    ClientQueryManager.removeSubscription(subsID)
    subsID= -1
  }
}

class ProjectTreeItem (folder:Folder) extends MyTreeItem[AdTreeNode](folder) {
  var bidderSubsID: Int = -1
  ClientQueryManager.queryInstance(folder.ref,0).headOption match { // project Adress
    case Some(data)=>super.getChildren.add(new MyTreeItem(new Address(data)))
    case None =>
  }
  ClientQueryManager.queryInstance(folder.ref,1).headOption match { // client Address
    case Some(data)=> super.getChildren.add(new MyTreeItem(new Address(data)))
    case None =>
  }  
  
  
  def handleCommands(callback:()=>Unit)(command:NotificationType.Value,data:IndexedSeq[InstanceData]): Unit = runInFx{
    command match {
      case NotificationType.sendData|NotificationType.updateUndo =>
        //println("Project send data "+data.mkString(","))
        val chList=super.getChildren
        if(command==NotificationType.updateUndo&& chList.size>2)chList.remove(2,chList.size)
        val list= data map (d => new FolderTreeItem(Folder(d)))
        //println("List:" +list)
        chList.addAll(list:_*)
        if(command==NotificationType.sendData) callback()
      case NotificationType.childAdded=>
        keepSelectedAddress{super.getChildren.add(new FolderTreeItem(Folder(data.head)))}
      case NotificationType.fieldChanged =>
        val chList=super.getChildren
        val searchRef=data.head.ref
        chList.asScala.indexWhere(_.getValue.ref==searchRef) match{
          case -1 => util.Log.e("field changed but not found "+data.head.ref)
          case ix => keepSelectedAddress{chList.set(ix, new FolderTreeItem(Folder(data.head)))}
        }
      case NotificationType.instanceRemoved=>
        val chList=super.getChildren
        val searchRef=data.head.ref
        chList.asScala.indexWhere(_.getValue.ref==searchRef) match{
          case -1 => util.Log.e("instance removed but not found "+data.head.ref)
          case ix => keepSelectedAddress{chList.remove(ix,ix+1)}
        }
    }	
  }
  
  val otherSubsID: Int = ClientQueryManager.createSubscription(folder.ref,3)(handleCommands(()=> loadOthers()))
  
  def loadOthers (): Unit = {
    bidderSubsID=ClientQueryManager.createSubscription(folder.ref,2)(handleCommands(()=>{}))
  }
  
  override def shutDown(): Unit = {
    super.shutDown()
    if(otherSubsID!= -1) ClientQueryManager.removeSubscription(otherSubsID)
    if(bidderSubsID!= -1) {
      ClientQueryManager.removeSubscription(bidderSubsID)
      bidderSubsID = -1
    }
  }
}

