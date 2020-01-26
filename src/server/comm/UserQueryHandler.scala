/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import java.io.{DataInputStream, DataOutput}

import definition.comm.{ClientCommands, NotificationType, ServerCommands}
import definition.data.{BlockData, InstanceData, Reference}
import server.storage.StorageManager
import util.Log

import scala.util.control.NonFatal


trait AbstractQueryHandler{
	def userSocket:AbstractUserSocket

	def notifyInstanceChanged(subs:SubscriptionInfo,data:InstanceData): Unit =
		//System.out.println("Notify instance changed "+subs+" changedInst:"+data.ref)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subs.id )
			out.writeInt(NotificationType.fieldChanged.id)
			data.ref.write(out)
			data.writeWithChildInfo(out)
		}

	def notifyInstanceAdded(subs:SubscriptionInfo,data:InstanceData): Unit =
		//System.out.println("Notify instance added "+subs+" "+data.ref+" "+data)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subs.id)
			out.writeInt(NotificationType.childAdded.id )
			//out.writeInt(atPos)
			data.ref.write(out)
			data.writeWithChildInfo(out)
		}

	def notifyBlockAdded(subs:SubscriptionInfo,data:BlockData):Unit= {
		userSocket.sendData(ServerCommands.sendSubscriptionNotification) { out =>
			out.writeInt(subs.id)
			out.writeInt(NotificationType.childAdded.id )
			out.writeInt(data.ref.instance)
			data.write(out)
		}
	}

	def notifyBlockChanged(subs:SubscriptionInfo,data:BlockData):Unit= {
		userSocket.sendData(ServerCommands.sendSubscriptionNotification) { out =>
			out.writeInt(subs.id)
			out.writeInt(NotificationType.fieldChanged.id )
			out.writeInt(data.ref.instance)
			data.write(out)
		}
	}

	def notifyBlockDeleted(subs:SubscriptionInfo,inst:Int):Unit= {
		userSocket.sendData(ServerCommands.sendSubscriptionNotification) { out =>
			out.writeInt(subs.id)
			out.writeInt(NotificationType.instanceRemoved.id )
			out.writeInt(inst)
		}
	}

	def notifyInstanceDeleted(subs:SubscriptionInfo,ref:Reference): Unit =
		//System.out.println("Notify deleted "+subs+" "+ref)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subs.id)
			out.writeInt(NotificationType.instanceRemoved.id )
			ref.write(out)
		}

  def notifyParentDeleted(subs:SubscriptionInfo,ref:Reference): Unit =
    userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
      out.writeInt(subs.id)
      out.writeInt(NotificationType.parentNotExistend.id )
      ref.write(out)
    }


	def refreshSubscription(subs:SubscriptionInfo): Unit = try{
		//System.out.println("refreshing subscription "+subs)
		subs match {
			case e:PropSubscription=> userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
				out.writeInt(subs.id )
				out.writeInt(NotificationType.updateUndo .id)
				sendQueryData(out,subs.parentRef,e.propertyField)
			}
			case e:PathSubscription=> userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
				out.writeInt(subs.id )
				out.writeInt(NotificationType.updateUndo .id)
				writePathElements(out,subs.id,e.path)
			}
			case s:SingleSubscription=> userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
				out.writeInt(s.id )
				out.writeInt(NotificationType.updateUndo .id)
				sendQueryData(out,s.parentRef,-1)
			}
			case o:BlockSubscription=> userSocket.sendData(ServerCommands.sendSubscriptionNotification) { out=>
				out.writeInt(subs.id)
				out.writeInt(NotificationType.updateUndo.id)
				sendBlockQueryData(out,o.parentRef,o.propField)
			}
		}
	} catch {
    case NonFatal(e) => util.Log.e("Refresh subscription", e)
    case other: Throwable => util.Log.e("Refresh subscription", other)
	}

	protected def sendQueryData(out:DataOutput,parentRef:Reference,propertyField:Byte): Unit =
		if (propertyField<0) // only get the parent Instance
			sendInstance(out,parentRef)
		else // get the child instances of the property field
			sendChildren(out,parentRef,propertyField)


	protected def sendInstance(out:DataOutput,ref:Reference): Unit =
		try {
			val inst= StorageManager.getInstanceData(ref)
			out.writeInt(1)
			ref.write(out)
			inst.writeWithChildInfo(out)
		}
		catch {
			case e: Exception =>Log.e("Error sending Instance "+ref+"\n"+e); out.writeInt(0)
		}


	protected def sendChildren(out:DataOutput,parentRef:Reference,propertyField:Byte): Unit =
		try {
			StorageManager.getInstanceProperties(parentRef) match	{
				case Some(props) =>
					if(propertyField>=props.propertyFields.length) {
						Log.e("Error when sending children, Type "+parentRef+
							" does not have property field "+propertyField+" current size: "+props.propertyFields.length)
						out.writeInt(0)
					}
					else {
						val childRefs = props.propertyFields(propertyField).propertyList
						//System.out.println("send Children:"+childRefs.map(_.sToString+","))
						// get all Data before starting to write
						if (childRefs.size < 10 || MainServerSocket.noBulkAction) {
							val instList = getInstances(childRefs)
							out.writeInt(instList.size) // only write size after all children are found
							//System.out.println("readlist "+instList)
							for (i <- instList) {
								i.ref.write(out)
								i.writeWithChildInfo(out)
							}
						}
						else {
							out.writeInt(childRefs.size) // ab bit risky if one child does not exist
							pushInstances(childRefs, out)
						}
					}
				case None => out.writeInt(0)
			}
		}
		catch {
			case e: Exception => Log.e("sind children parentRef:"+parentRef+" propField:"+propertyField,e); out.writeInt(0)
		}


	protected def pushInstances(childRefs:IndexedSeq[Reference],out:DataOutput): Unit = {
		//System.out.println("bulk push" +childRefs.head + " num:"+childRefs.size)
		var bulkStart= 0
		var oldRef=childRefs.head
		for(i <-1 until childRefs.size) {
			val newRef=childRefs(i)
			if(oldRef.typ== newRef.typ && oldRef.instance ==(newRef.instance-1)){
				// subsequent instance
			}
			else { // another instance, break bulk block
				if(bulkStart==i-1 )  // only single instance
					StorageManager.pushInstanceData(oldRef,out)
				else  StorageManager.bulkPushInstanceData(childRefs(bulkStart),oldRef,out)
				bulkStart=i
			}
			oldRef=newRef
		}
		if(bulkStart==childRefs.size-1 ) { // only single instance
			StorageManager.pushInstanceData(oldRef,out)
		}
		else
			StorageManager.bulkPushInstanceData(childRefs(bulkStart),oldRef,out)
		//System.out.println("push ready")
	}

	protected def sendBlockQueryData(out:DataOutput,parentRef:Reference,propertyField:Byte): Unit = try {
		StorageManager.getInstanceProperties(parentRef) match {
			case Some(props) =>
				if (propertyField >= props.propertyFields.length) {
					Log.e("Error when sending block children, Type "+parentRef+
						" does not have property field "+propertyField+" current size: "+props.propertyFields.length)
					out.writeInt(0)
				}
				else {
					val childRefs = props.propertyFields(propertyField).propertyList
					val blocks=childRefs.filter(StorageManager.blockExists).map(StorageManager.getBlockData)
					out.writeInt(blocks.size)
					for (i <- blocks) {
						out.writeInt(i.ref.instance)
						i.write(out)
					}
				}
				case None=> out.writeInt(0)
		}
	} catch {
		case NonFatal(e)=> Log.e("send Block Query "+parentRef+ " "+propertyField,e)
	}



	protected def getInstances(childRefs:IndexedSeq[Reference]):Iterable[InstanceData] =
		if(childRefs.size>10&& !MainServerSocket.noBulkAction) {
			var retList=new collection.mutable.ArrayBuffer[InstanceData]()
			var bulkStart= 0
			var oldRef=childRefs.head
			for(i <-1 until childRefs.size) {
				val newRef=childRefs(i)
				if(oldRef.typ== newRef.typ && oldRef.instance ==(newRef.instance-1)){		/* subsequent instance*/	}
				else { // another instance, break bulk block
					if(bulkStart==i-1 ) { // only single instance
						if(StorageManager.instanceExists(oldRef.typ,oldRef.instance))
							retList += StorageManager.getInstanceData(oldRef)
						else Log.e("Cant find instance "+oldRef)
					}
					else retList ++= StorageManager.bulkGetInstanceData(childRefs(bulkStart),oldRef)
					bulkStart=i
				}
				oldRef=newRef
			}
			if(bulkStart==childRefs.size-1 ) { // only single instance
				if(StorageManager.instanceExists(oldRef.typ,oldRef.instance))
					retList += StorageManager.getInstanceData(oldRef)
				else Log.e("Cant find instance "+oldRef)
			}
			else {
				val bulkList= StorageManager.bulkGetInstanceData(childRefs(bulkStart),oldRef)
				if(childRefs.size==bulkList.size) return  bulkList
				else  retList ++= bulkList
			}
			retList
		}
		else if(childRefs.size>1) try{
			for(cRef <- childRefs) yield StorageManager.getInstanceData(cRef)
		} catch {
			case NonFatal(e)=>
				Log.e(e) // filter out nonexisting instances
				for(cRef <- childRefs;if StorageManager.instanceExists(cRef.typ, cRef.instance))
					yield StorageManager.getInstanceData(cRef)
      case other: Throwable => Log.e("Fatal:" + other); println(other); null
		}
		else if(childRefs.nonEmpty){
			if(StorageManager.instanceExists(childRefs.head.typ,childRefs.head.instance)) IndexedSeq(StorageManager.getInstanceData(childRefs.head))
			else {Log.e("Cant find instance "+childRefs.head);IndexedSeq.empty}
		}
		else IndexedSeq.empty


	protected def writePathElements(out:DataOutput,subsID:Int,refList:Seq[Reference]): Unit =
		try {
			// check if all instances are there
			val instList = for(cRef <- refList) yield StorageManager.getInstanceData(cRef)
			// write data
			out.writeInt(refList.size)
			for(i <- refList.indices){
				refList(i).write(out)
				instList(i).writeWithChildInfo(out)
			}
		} catch {
			case e: Exception => Log.e("writePathElement subsID:"+subsID+" refList:"+refList.mkString(","),e);out.writeInt(0)
		}

	protected def createSubscription(parentRef:Reference,propertyField:Byte,conn:AbstractConnectionEntry): Unit =
		if(StorageManager.instanceExists(parentRef.typ,parentRef.instance)) {
			val subsID=CommonSubscriptionHandler.addSubscription(conn,parentRef,propertyField)
			//System.out.println("adding Subscription for "+parentRef+" field:"+propertyField+ " subsID:"+subsID)
			if (subsID<1) Log.e("new Subscription ID ="+subsID+" for Parent "+parentRef)
			else userSocket.sendData(ServerCommands.acceptSubscription ) { out=>
				out.writeInt(subsID)
				sendQueryData(out,parentRef,propertyField)
				//println("Data sent for "+parentRef+" field:"+propertyField)
			}
		}
		else userSocket.sendData(ServerCommands.acceptSubscription){ out=>
			Log.e("New subscription parent does not exist:"+parentRef)
			out.writeInt(-1)
		}

	protected def createBlockSubscription(parentRef:Reference,propertyField:Byte,conn:AbstractConnectionEntry):Unit ={
		if(StorageManager.instanceExists(parentRef.typ,parentRef.instance)) {
			val subsID=CommonSubscriptionHandler.addBlockSubscription(conn,parentRef,propertyField)
			if(subsID<1) Log.e("new Subscription ID ="+subsID+" for BlockParent "+parentRef)
			else userSocket.sendData(ServerCommands.acceptSubscription ) { out=>
				out.writeInt(subsID)
				sendBlockQueryData(out,parentRef,propertyField)
				//println("Data sent for "+parentRef+" field:"+propertyField)
			}
		}
	}


	protected def createPathSubscription(path:Seq[Reference],connection:AbstractConnectionEntry): Unit = {
		//println("create Pathsubscription "+path.mkString("|"))
		val subsID=CommonSubscriptionHandler.addPathSubscription(connection,path)
		if(subsID<1) Log.e("new Pathsubs wrong id "+subsID)
		//System.out.println("adding Path Subscription for "+pathList.mkString("/")+" subsID:"+subsID)
		else userSocket.sendData(ServerCommands.acceptSubscription ) { out =>
			out.writeInt(subsID )
			writePathElements(out,subsID,path)
		}
	}

	protected def openChild(subsID:Int,newRef:Reference):Unit = {
		val list=CommonSubscriptionHandler.openChild(subsID,newRef)
		if(list!=null) {
			//println("open child subsid:"+subsID+" size:" + list.size)
			userSocket.sendData(ServerCommands.sendSubscriptionNotification) { out =>
				out.writeInt(subsID)
				out.writeInt(NotificationType.sendData.id)
				//System.out.println("pathsubs openchild subsid:"+subsID+" "+list.mkString)
				writePathElements(out, subsID, list)
			}
		}
		else Log.e("open child subsID:"+subsID+" ref:"+newRef+" list==null")
	}


  protected def searchRecursive(rootRef: Reference, searchTerm: String,
                                callBack:(List[InstanceData],InstanceData)=>Unit) : Unit = {
    lazy val groupedMatcher ="""-?^[0-9]{1,3}(\.[0-9]{3})+(\,[0-9]+)?$""".r
    val doubleMatcher ="""-?(\d+[\.,]\d*|\d*[\.,]\d+|\d+)""".r
    val checkNums = searchTerm match {
      case doubleMatcher(_*) => true
      case groupedMatcher(_*) => true
      case _ => false
    }
    //println("Search recursive: root:"+rootRef+" term:"+searchTerm)

		def loopChildren(tree:List[InstanceData])(elem:InstanceData):Unit={
      //println("loop children inst:"+elem.ref+" "+elem+" tree:"+tree.mkString("|"))
      if (elem.containsString(searchTerm, checkNums)) callBack(tree, elem)
      StorageManager.forEachChild(elem.ref, loopChildren(elem :: tree))
		}

    StorageManager.forEachChild(rootRef, loopChildren(Nil))
	}

  private def writeInst(inst: InstanceData, out: DataOutput): Unit = {
    inst.ref.write(out)
    inst.writeWithChildInfo(out)
  }

  def searchForText(rootRef: Reference, text: String): Unit = ThreadPool.runInPool {
    try {
      searchRecursive(rootRef, text, (tree: List[InstanceData], inst: InstanceData) => {
        userSocket.sendData(ServerCommands.sendSearchResult) { out =>
          //println("Searchresult tree:" + tree.mkString("|") + " inst:" + inst)
          out.writeInt(tree.size)
          for (t <- tree.reverse) writeInst(t, out)
          writeInst(inst, out)
        }
      })
      userSocket.sendData(ServerCommands.sendSearchResult)(out => {
        //println("search finish")
        out.writeInt(-1)
      })
    }
    catch {
      case NonFatal(e) => Log.e("Error when search ", e)
        userSocket.sendData(ServerCommands.sendSearchResult)(out => {
          //println("search finish")
          out.writeInt(-1)
        })
    }
  }
}





/** Handles Queries and Subscriptions of a certain user
 * 
 */
class UserQueryHandler(val userSocket: JavaClientSocket) extends AbstractQueryHandler {
	userSocket.registerCommandHandler(ClientCommands.queryInstance)( handleQuery)
	userSocket.registerCommandHandler(ClientCommands.getNextParentOfType)( sendNextParentOfType)
	userSocket.registerCommandHandler(ClientCommands.factQueryInstance)( handleFactQuery)
	userSocket.registerCommandHandler(ClientCommands.startSubscription )(newSubscription)
	userSocket.registerCommandHandler(ClientCommands.changeSubscription )(changeSubscription)
	userSocket.registerCommandHandler(ClientCommands.startPathSubscription )(newPathSubscription)
	userSocket.registerCommandHandler(ClientCommands.stopSubscription  )(stopSubscription)
	userSocket.registerCommandHandler(ClientCommands.pauseSubscription  )(pauseSubscription)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_openChild )(pathSubs_openChild)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_jumpUp  )(pathSubs_jumpUp)
	userSocket.registerCommandHandler(ClientCommands.pathSubs_changePath  )(pathSubs_changePath)
  userSocket.registerCommandHandler(ClientCommands.searchForText)(search)


	

	private def handleQuery(in:DataInputStream): Unit = {
		val parentRef:Reference=Reference(in)
		val propertyField:Byte= in.readByte		
		userSocket.sendData(ServerCommands.sendQueryResponse ) {out=>
		sendQueryData(out,parentRef,propertyField)
		}		
	}
	
	private def sendNextParentOfType(in:DataInputStream): Unit = {
		val childRef:Reference=Reference(in)
		val parentType= in.readInt		
		userSocket.sendData(ServerCommands.sendQueryResponse ) {out=> try{
			StorageManager.getNextParentOfType(childRef, parentType) match{
				  case Some(pref)=>
						val inst=StorageManager.getInstanceData(pref)
						out.writeInt(1)
						pref.write(out)
						inst.writeWithChildInfo(out)
					case None =>  out.writeInt(0)
				}			
			}	catch {
				case e: Exception =>Log.e("Error sending nextParent "+childRef+ " type:"+parentType+"\n"+e);  out.writeInt(0)
			}
		}
	}
	
	private def handleFactQuery(in:DataInputStream): Unit = {
		val parentRef:Reference=Reference(in)
		val propertyField:Byte= in.readByte		
		userSocket.sendData(ServerCommands.sendFactQueryResponse ) {out=>
		sendQueryData(out,parentRef,propertyField)
		}		
	}
	
	private def newSubscription(in:DataInputStream): Unit = {
		val parentRef:Reference=Reference(in)
		val propertyField:Byte= in.readByte	
		createSubscription(parentRef,propertyField,userSocket.connectionEntry)
	}
	
	private def changeSubscription(in:DataInputStream ): Unit = {
		val subsID=in.readInt
		val newRef=Reference(in)
		val newPropField=in.readByte
		//System.out.println("changing Subscription for subsID:"+subsID+ " to:"+newRef)
		CommonSubscriptionHandler.changeSubscription(userSocket.connectionEntry,subsID,newRef,newPropField)
		userSocket.sendData(ServerCommands.sendSubscriptionNotification ) { out =>
			out.writeInt(subsID )
			out.writeInt(NotificationType.sendData .id)
			sendQueryData(out,newRef,newPropField)
		}		
	}
	
	
	private def newPathSubscription(in:DataInputStream): Unit = {
		val count=in.readInt
		val pathList=for(i <-0 until count) yield Reference(in)
		val corrList=pathList.filter(ref=> StorageManager.instanceExists(ref.typ, ref.instance))
		createPathSubscription(corrList,userSocket.connectionEntry)
	}
	
	private def pathSubs_openChild(in:DataInputStream): Unit = {
		val subsID=in.readInt
		val newRef=Reference(in)
		openChild(subsID,newRef)
	}
	
	private def pathSubs_jumpUp(in:DataInputStream):Unit = {
		val subsID=in.readInt
		val newPos=in.readInt
		CommonSubscriptionHandler.jumpUp(subsID,newPos)
	}
	
	private def pathSubs_changePath(in:DataInputStream): Unit = {
		val subsID=in.readInt
		val count=in.readInt
		val pathList=for(i <-0 until count) yield Reference(in)
    val corrList = pathList.filter(ref => StorageManager.instanceExists(ref.typ, ref.instance))
    CommonSubscriptionHandler.changePath(subsID, corrList)
    userSocket.sendData(ServerCommands.sendSubscriptionNotification) { out =>
      out.writeInt(subsID)
      out.writeInt(NotificationType.updateUndo.id)
      writePathElements(out, subsID, corrList)
    }
	}
	
	private def stopSubscription(in:DataInputStream):Unit = {
		val subsID=in.readInt
		//System.out.println("Stop Subscription "+subsID)
		CommonSubscriptionHandler.removeSubscription(subsID)
	}
	
	private def pauseSubscription(in:DataInputStream):Unit = {
		val subsID=in.readInt
		//System.out.println("pause Subscription "+subsID)
		CommonSubscriptionHandler.pauseSubscription(subsID)
	}

  private def search(in: DataInputStream): Unit = {
    val parentRef = Reference(in)
    val searchText = in.readUTF()
    // println("search "+parentRef+" test:"+searchText)
    searchForText(parentRef, searchText)
  }
	 
}