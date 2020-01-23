/**
 * Author: Peter Started:04.09.2010
 */
package client.comm

import java.io._
import java.util.concurrent._

import client.dialog.DialogManager
import client.search.{AbstractSearchResult, SearchFinished, SearchResult}
import definition.comm._
import definition.data._
import definition.expression._
import definition.typ._
import util.Log

import scala.swing.Swing
import scala.util.control.NonFatal

/** manages Queries and Subscriptions
 * 
 */

trait StepListReader{
	def loadStepList(list:Seq[TransStepData]):Unit
}



object ClientQueryManager {

	type UpdateFunc=(NotificationType.Value,IndexedSeq[InstanceData])=>Unit
	
	type FactUpdateFunc[T<: Referencable]=(NotificationType.Value,IndexedSeq[T])=>Unit

  type SearchListener = AbstractSearchResult => Unit


  sealed trait Subscriber
	case class SimpleSubscriber(func: UpdateFunc) extends Subscriber{
    override def toString: String = "SimpleSubscriber " + func.getClass
  }

  case class FactSubscriber[T <: Referencable](factory: SubscriptionFactory[T], func: FactUpdateFunc[T]) extends Subscriber

	private val queryQueue = new SynchronousQueue[IndexedSeq[InstanceData]](true)	
	private val factQueryQueue = new SynchronousQueue[IndexedSeq[Referencable]](true)
	private var queryFactories:List[SubscriptionFactory[_]]=Nil
	
	private val newSubscriberQueue=new ArrayBlockingQueue[Subscriber](8,true)
	private val subscriptionMap=new ConcurrentHashMap[Int,Subscriber]()

  private var stepListReader: StepListReader = _
	
	private val commandResultQueue = new SynchronousQueue[CommandResult](true)
	private val subscriptionAcceptQueue = new SynchronousQueue[Int](true)


  private var sock: ClientSocket = _
  val myPool: ExecutorService = Executors.newCachedThreadPool()
	private var isSetup=false
	private var setupListenerMap=collection.mutable.HashSet[() => Unit]()
	//private var afterSetupListeners=collection.mutable.HashSet[Function0[Unit]]()
	private val storeSettingsListenerMap= collection.mutable.HashSet[() => Unit]()
  private var searchListener: Option[SearchListener] = None

  val EmptyResultListener: CommandResult => Unit = (result: CommandResult) => {}
  
	var undoLockListener:Option[(Boolean, String) => Unit]=None
	
	FunctionManager.setManager(new CommonFuncMan)

  def setClientSocket(newSock: ClientSocket): Unit = {
		sock=newSock
		// register Handler
		sock.registerCommandHandler(ServerCommands.sendQueryResponse)(handleQueryResults)
		sock.registerCommandHandler(ServerCommands.sendFactQueryResponse)(handleFactQueryResults)
		sock.registerCommandHandler(ServerCommands.acceptSubscription)(handleAcceptSubscription)	
		sock.registerCommandHandler(ServerCommands.sendSubscriptionNotification )(handleSubsNotifications)
		sock.registerCommandHandler(ServerCommands.sendCommandResponse )(handleCommandResponse)
		sock.registerCommandHandler(ServerCommands.lockForUndo )(lockForUndo)
		sock.registerCommandHandler(ServerCommands.releaseUndoLock )(releaseUndoLock)
		sock.registerCommandHandler(ServerCommands.sendUndoInformation )(sendUndoInformation)
    sock.registerCommandHandler(ServerCommands.askEnquiry)(askEnquiry)
    sock.registerCommandHandler(ServerCommands.sendSearchResult)(receiveSearchResults)
	}
		
	/** reads instances from the DataBase
	 *  @param ref the parent Reference to be read
	 *  @param propertyField if <0, only the parent Instance is send, if>=0, all children from the given property field are send
	 *  @return the parent instance or the children from the given property field
	 * 	
	 */
	def queryInstance(ref:Reference,propertyField:Byte):IndexedSeq[InstanceData] = 	{		
		sock.sendData(ClientCommands.queryInstance ) {out =>			
			//System.out.println("Sending Query request "+ref + " "+Thread.currentThread)
			ref.write(out)
			out.writeByte(propertyField)
		}
		queryQueue.take
	}
	
	/** reads instances from the DataBase
	 *  @param ref the parent Reference to be read
	 *  @param propertyField if <0, only the parent Instance is send, if>=0, all children from the given property field are send
	 *  @return the parent instance or the children from the given property field
	 * 	
	 */
	def queryInstanceFact[T <:Referencable](ref:Reference,propertyField:Byte,factory:SubscriptionFactory[T]):IndexedSeq[T] = 	queryFactories.synchronized{		
		sock.sendData(ClientCommands.factQueryInstance ) {out =>
		  queryFactories=factory :: queryFactories
			//System.out.println("Sending Query request "+ref + " "+Thread.currentThread)
			ref.write(out)
			out.writeByte(propertyField)
		}
		factQueryQueue.take.asInstanceOf[IndexedSeq[T]]		
	}
	
	/** gets the next parent object of the given type, starting from the given object
	 *  @param ref the object where to start the search
	 *  @param parentType the type of the parent object to look for
	 *  @return the data of the parent, if found, or None otherwise
	 */
	def getNextParentOfType(ref:Reference,parentType:Int):Option[InstanceData] = 	{		
		sock.sendData(ClientCommands.getNextParentOfType ) {out =>			
			ref.write(out)
			out.writeInt(parentType)
		}
		queryQueue.take.headOption
	}
	
	/** creates a new subscription
	 * @param parentRef the reference to the parent object of the seached object(s), 
	 * or the seached object's reference when it's a single subscription
	 * @param propertyField the number of the property field of the parent object, or -1 when only
	 * a single instance should be queried. In that case parentRef is the reference to that single object
	 * @param updateFunc a function to be called when values are updated
	 * @return the subscriptionID
	 * 	
	 */
	def createSubscription(parentRef:Reference,propertyField:Byte)(updateFunc: UpdateFunc):Int = {
	  //println("--------------------------------------")
	  //println("Cr Subs "+parentRef+" prF:"+propertyField+" "+Thread.currentThread.getStackTrace.drop(2).take(10).mkString("\n  "))
		sock.sendData(ClientCommands.startSubscription ) {out =>
			newSubscriberQueue.add( SimpleSubscriber(updateFunc))
			//System.out.println("adding subscription "+parentRef+ " "+Thread.currentThread)
			parentRef.write(out)
			out.writeByte(propertyField)
		}
		subscriptionAcceptQueue.take()
	}
		
	
	/** creates a Subscription with a Factory
	 * 
	 */
	def createFactSubscription[T <:Referencable](parentRef:Reference,propertyField:Byte,factory:SubscriptionFactory[T])
	(updateFunc: FactUpdateFunc[T]):Int = {	
	  //print("Create FactSubscription "+parentRef+" propField:"+propertyField+" "+Thread.currentThread.getName)
		sock.sendData(ClientCommands.startSubscription ) {out =>
			newSubscriberQueue.add( FactSubscriber(factory,updateFunc))
			//System.out.println("adding subscription "+parentRef+ " "+Thread.currentThread)
			parentRef.write(out)
			out.writeByte(propertyField)
		}
		subscriptionAcceptQueue.take()
	}

  def changeSubscription(subsID: Int, newParent: Reference, newPropField: Byte): Unit = {
	 // print("change Subscription id:"+subsID+" newParent:"+newParent+" newpropField:"+newPropField)
		sock.sendData(ClientCommands.changeSubscription ) {out =>
			out.writeInt(subsID)
			newParent.write(out)
			out.writeByte(newPropField)
		}
	}
	
	def createPathSubscription(path:Seq[Reference])(updateFunc:UpdateFunc):Int = {	
	  //print("create Path Subscription path:"+path)
		sock.sendData(ClientCommands.startPathSubscription ) {out =>
			newSubscriberQueue.add( SimpleSubscriber(updateFunc))
			out.writeInt(path.size)
			for(el <-path) el.write(out)
		}
		subscriptionAcceptQueue.take()
	}

  def pathSubs_addPathElement(subsID: Int, childRef: Reference): Unit = {
		sock.sendData(ClientCommands.pathSubs_openChild ) { out =>
			out.writeInt(subsID)
			childRef.write(out)
		}
	}
	
	/** changes the subscription only to the remaining elements
	 * @param newPathPos the number of the element that should be the last one starting with 0
	 */
  def pathSubs_jumpUp(subsID: Int, newPathPos: Int): Unit = {
    //println("jumpup "+newPathPos)
		sock.sendData(ClientCommands.pathSubs_jumpUp  ) { out =>
			out.writeInt(subsID)
			out.writeInt(newPathPos)
		}
	}

  def pathSubs_changePath(subsID: Int, newPath: Seq[Reference]): Unit = {
    sock.sendData(ClientCommands.pathSubs_changePath) { out =>
			out.writeInt(subsID)
			out.writeInt(newPath.size)
			for(p <-newPath) p.write(out)
		}
	}


  def removeSubscription(subsID: Int): Unit = {
	  //println("remove Subscription "+subsID)
	  if(subsID<0) {
      util.Log.e("Trying to remove SubsID :"+subsID+" but ",Thread.currentThread().getStackTrace)
	    Thread.dumpStack()
	  }
	  else {
	  	sock.sendData(ClientCommands.stopSubscription ) {out =>
	  	out.writeInt(subsID)			
	  	}
	  	if(subscriptionMap.containsKey(subsID)) subscriptionMap.remove(subsID)		
	  	else { util.Log.e("ERROR: subscription "+subsID+" not found when removing",Thread.currentThread().getStackTrace)
	  	  Thread.dumpStack()
	  	}
	  }
	}


  def pauseSubscription(subsID: Int): Unit = {
		//Thread.dumpStack
		sock.sendData(ClientCommands.pauseSubscription ) {out =>
			out.writeInt(subsID)			
		}		
	}
  
  	
	def writeInstanceField(ref:Reference,fieldNr:Byte,newValue:Expression,resultListener:CommandResult=>Unit=EmptyResultListener):Unit = runInPool{
		sock.sendData(ClientCommands.writeField ) { out =>
			ref.write(out)
			out.writeByte(fieldNr)
			newValue.write(out)
		}
		val result=commandResultQueue.take()
    resultListener(result)
	}
	
	def writeInstancesField(refs:Iterable[Referencable],fieldNr:Byte,newValue:Expression):Unit = runInPool{
		sock.sendData(ClientCommands.writeMultiFields ) { out =>
		  out.writeInt(refs.size)
		  for(inst<-refs) inst.ref.write(out)
			out.writeByte(fieldNr)
			newValue.write(out)
		}
		commandResultQueue.take()			
	}

  def writeKeyStrokes(): Unit = {
	  sock.sendData(ClientCommands.writeKeyStrokes) { out =>
	    KeyStrokeManager.write(out)
	  }
	}
		
	
	/** creates an instance and returns the instanceID
	 * @param classType the typeID of the new instance
	 * @param owners the owners of the new instance
	 * @return the ID of the new class
	 */
	def createInstance(classType:Int,owners:Array[OwnerReference]):Int = {
		sock.sendData(ClientCommands.createInstance ) { out =>
		   out.writeInt(classType)
		   out.writeInt(owners.length)
		   for(owner <-owners)
		  	 owner.write(out)			
		}
		//System.out.println("create "+Thread.currentThread.getName)
		commandResultQueue.take() match {
			case HasResult(const) => const.toInt
			case NoResult => throw new IllegalArgumentException("no instance ID returned when creating type "+classType)
			case HasError(e)=> throw e
		}
	}
	
	/** creates a list of instances under a certain parent object
	 * 
	 * @param owners the owners of the new instances
	 * @param data a Seq of Tuples (Type,FieldValues), each Tuple for one new instance
	 * @param checkLinks check link references, if false: WARNING: only for constant instance values, 
	 *   link references are not checked/created with this command !!!	 * 
	 */
	def createInstances(owners:Array[OwnerReference], data:Iterable[(Int,Array[Expression])],checkLinks:Boolean=false,
											resultListener:CommandResult=>Unit=EmptyResultListener):Unit = {
	  sock.sendData(ClientCommands.createInstances ) { out =>	    
	    out.writeInt(owners.length)
	    for(owner <-owners)
		  	 owner.write(out)
		  out.writeInt(data.size)
		   out.writeBoolean(checkLinks)
		  for((typ,fields)<-data)  {
		    out.writeInt(typ)
		    out.writeByte(fields.length)
		    for(f<-fields) f.write(out)
		  }  
	  }
	  resultListener(commandResultQueue.take())
	}
	
	
	def copyInstances(refList:Iterable[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int):Unit = {
		copyOrMove(refList,fromOwner,toOwner,atPos,ClientCommands.copyInstances )
	}
	
	def moveInstances(refList:Iterable[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int):Unit = {
		copyOrMove(refList,fromOwner,toOwner,atPos,ClientCommands.moveInstances )
	}
	
	def copyOrMove(refList:Iterable[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int,command:ClientCommands.Value):Unit= runInPool{
		sock.sendData(command ) { out =>
		  out.writeShort(refList.size)
		  for(ref <-refList) ref.write(out)
			fromOwner.write(out)
			toOwner.write(out)
			out.writeInt(atPos)
		}
		commandResultQueue.take() match {			
			case HasResult(const) =>util.Log.e("Copy or Move wrong result "+const)
			case NoResult => // alles ok
			case HasError(e)=> throw e
		}
	}
	
	
	/** creates a second use for the given instance 
	 * @param refList list of instances to use a second time
	 * @param fromOwner current owner
	 * @param toOwner owner where the instances should additinally be used
	 * @param atPos position in the toOwner property list where they should be added; -1 means add at the end
	 */
	def secondUseInstances(refList:Iterable[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int):Unit = runInPool{
		sock.sendData(ClientCommands.secondUseInstances  ) { out =>
		  out.writeShort(refList.size)
		  for(ref <-refList) ref.write(out)
			fromOwner.write(out)
			toOwner.write(out)
			out.writeInt(atPos)
		}
		commandResultQueue.take() match {			
			case HasResult(const) =>util.Log.e("SecondUse result returned:"+const)
			case NoResult => // throw new IllegalArgumentException("Error when second using "+refList.mkString+" from:"+fromOwner+" to:"+toOwner+" ")
			case HasError(e)=> throw e  
		}
	}
	
	/** deletes an instance
	 * @param ref Reference of the instance to delete
	 * @param fromOwner delete from wich owner. If there are second uses at other owners, they stay alive. If fromOwner=EMPTY_, delete from all owners 
	 * 
	 * 
	 */
	def deleteInstance(ref:Reference,fromOwner:OwnerReference=EMPTY_OWNERREF):Unit = runInPool{    
  		sock.sendData(ClientCommands.deleteInstance ) { out =>
  			ref.write(out)
  			fromOwner.write(out)
  		}    
		commandResultQueue.take() 
	}


	def convertInstances(sources:Seq[Reference],targetOwner: OwnerReference,targetType:Int,rule:Array[(Int,Int)]): Unit = runInPool{
		sock.sendData(ClientCommands.convertInstances) { out =>
			out.writeInt(sources.size)
			for(r<-sources)
				r.write(out)
			targetOwner.write(out)
      out.writeInt(targetType)
			out.writeInt(rule.length)
			for((f1,f2)<-rule){
			  out.writeInt(f1)
				out.writeInt(f2)
			}
		}
    commandResultQueue.take()
  }
	
	/** sends a notification to the server to execute the given action with the given parameters
	 * @param instList list of instances that should be modified. For a Create Action, the list of parents
	 * @param actionName name of the action
	 * @param params the parameter values for the action	 *
	 * 
	 */
	def executeAction(owner:OwnerReference,instList:Iterable[Referencable],actionName:String,params:Seq[ResultElement]):Unit= {
    //System.out.println("executeAction owner:"+owner+" instList:"+instList+" action:"+actionName+" p:"+params.mkString)
	  _executeAction(owner,instList,actionName,params)
		commandResultQueue.take() match {
			case HasError(e)=> Log.e("result error "+e)
			case _ =>
		}
	}

  def executeActionResult(owner:OwnerReference,instList:Iterable[Referencable],actionName:String,params:Seq[ResultElement],
                          resultListener:CommandResult=>Unit=EmptyResultListener):Unit= {
    _executeAction(owner,instList,actionName,params)
    resultListener(commandResultQueue.take())
  }

  private def _executeAction(owner:OwnerReference,instList:Iterable[Referencable],actionName:String,params:Seq[ResultElement]):Unit = {
    if(instList.isEmpty) throw new IllegalArgumentException("InstList is empty in action "+actionName+" "+params.mkString("|"))
    runInPool{
      sock.sendData( ClientCommands.executeAction) { out =>
        out.writeInt(instList.size)
        instList foreach(_.ref.write(out))
        out.writeUTF(actionName)
        out.writeInt(params.size)
        params foreach(x => {out.writeUTF(x.paramName); x.result .write(out)})
        //println("execute Action :"+actionName)
        owner.write(out)
      }
    }
  }
	
	def executeCreateAction(parentRef:Reference,newType:Int,propField:Byte,actionName:String,params:Seq[ResultElement],
      formatValues:Seq[(Int,Constant)]):Unit = runInPool{
    //System.out.println("execute create parents:"+parentList.mkString(",")+" newType:" + newType+" ")
		sock.sendData(ClientCommands.executeCreateAction) { out =>
			out.writeInt(1)
			parentRef.write(out)
			out.writeInt(newType)
			out.writeByte(propField)
			out.writeUTF(actionName)
			out.writeInt(params.size)
			params foreach(x => {out.writeUTF(x.paramName); x.result.write(out)})
			out.writeInt(formatValues.size)
			for(ix<-formatValues.indices;x=formatValues(ix))
			  {if(x._2==null) util.Log.e("Format value nr "+ix+" ==null");out.writeInt(x._1); x._2.write(out)}
		}
		commandResultQueue.take()  match {
			case HasError(e)=> Log.e("create result error "+e)
				case _ =>
		}

	}
	
	
	// ************************************* Internal routines *************************************
	
	private def readList(in:DataInputStream):IndexedSeq[InstanceData] = {
			val numData=in.readInt
			for(i <- 0 until numData) yield{
			  val ref=Reference(in)
			  InstanceData.readWithChildInfo(ref, in)
			} 			
		}
	
	private def readListWithFactory[T <: Referencable](in:DataInputStream,factory:SubscriptionFactory[T]):IndexedSeq[T] = {
			val numData=in.readInt
			//println("read with FActory: num:"+numData)
			for(i <- 0 until numData) yield factory.createObject(Reference(in), in)			
		}
	
	private def handleQueryResults(in:DataInputStream): Unit = 	{
		val data=readList(in)
		//System.out.println("Handling Query result data size:"+data.size+ " "+Thread.currentThread)		
		queryQueue.put(data)		
	}
	
	private def handleFactQueryResults(in:DataInputStream): Unit = if(queryFactories.nonEmpty){
	  val factory=queryFactories.head
		queryFactories=queryFactories.tail
	  val newFact=factory.asInstanceOf[SubscriptionFactory[ Referencable]]		  
	  val data=readListWithFactory(in,newFact)	  
	  factQueryQueue.put(data)
	}
	
	
	private def handleAcceptSubscription(in:DataInputStream): Unit = {
		val subsID:Int=in.readInt		
		val subs:Subscriber=newSubscriberQueue.take()
		if(subsID> -1)subscriptionMap.put(subsID,subs) else printErrorMessage("Wrong subsID "+subsID+" for subscriber "+subs)
    
		subscriptionAcceptQueue.put(subsID)
		subs match {			
			case a:SimpleSubscriber =>if(subsID > -1){
				val data=readList(in)				
				runInPool{a.func(NotificationType.sendData,data)}
			} else runInPool{a.func(NotificationType.parentNotExistend,IndexedSeq.empty)}
					
			case b:FactSubscriber[Referencable] @unchecked=> if(subsID > -1){
				val data: IndexedSeq[Referencable] =readListWithFactory(in,b.factory)
				runInPool{b.func(NotificationType.sendData,data)}
			} else runInPool{b.func(NotificationType.parentNotExistend,IndexedSeq.empty)}			
		}			
	}
	
	
	
	private def handleSubsNotifications(in:DataInputStream ) = {
		val substID=in.readInt
		if(!subscriptionMap.containsKey(substID)) {
			Log.e("Handle Subs Notification subsID "+substID+" not found")
			NotificationType(in.readInt) match {
				case NotificationType.fieldChanged|NotificationType.childAdded =>
					InstanceData.readWithChildInfo(Reference(in),in)
				case NotificationType.instanceRemoved =>
					Reference(in)
				case NotificationType.sendData|NotificationType.updateUndo =>
					readList(in)
			}
		} else {
			val subs = subscriptionMap.get(substID)
			subs match {
				case subscriber: SimpleSubscriber => val nt = NotificationType(in.readInt)
					//print("simple "+nt)
					nt match {
						case NotificationType.fieldChanged | NotificationType.childAdded =>
							val inst = InstanceData.readWithChildInfo(Reference(in), in)
							//System.out.println(" field changed:"+inst)
							runInPool(subscriber.func(nt, IndexedSeq(inst)))
						case NotificationType.instanceRemoved =>
							val ref = Reference(in)
							runInPool(subscriber.func(NotificationType.instanceRemoved,
								IndexedSeq(new InstanceData(ref, IndexedSeq(), Array.empty, Array.empty, false)))) // empty instance
						case NotificationType.sendData | NotificationType.updateUndo =>
							val list = readList(in)
							//System.out.println(" send Data:"+list)
							runInPool(subscriber.func(nt, list))
							case NotificationType.parentNotExistend =>
                val ref = Reference(in)
                runInPool(subscriber.func(nt,IndexedSeq(new InstanceData(ref, IndexedSeq(), Array.empty, Array.empty, false))))
					}
				case factSubs: FactSubscriber[Referencable] @unchecked=>
					val nt = NotificationType(in.readInt)
					//print("fact ":+nt)
					nt match {
						case NotificationType.fieldChanged | NotificationType.childAdded =>
							val inst = factSubs.factory.createObject(Reference(in), in)
							//System.out.println(" field changed:"+inst)
							runInPool(factSubs.func(nt, IndexedSeq(inst)))
						case NotificationType.instanceRemoved =>
							val ref = Reference(in)
							runInPool(factSubs.func(NotificationType.instanceRemoved,
								IndexedSeq(factSubs.factory.createEmptyObject(ref)))) // empty instance
						case NotificationType.sendData | NotificationType.updateUndo =>
							val list = readListWithFactory(in, factSubs.factory)
							//System.out.println("  fact send Data:"+list)
							runInPool(factSubs.func(nt, list))
            case NotificationType.parentNotExistend =>
              val ref = Reference(in)
              runInPool(factSubs.func(nt,
                IndexedSeq(factSubs.factory.createEmptyObject(ref)))) // empty instance
          }
				case a => util.Log.e("HandleSubsNotification unknown SubscriberType " + a + " class: " + (if (a != null) a.getClass else "Null") + " for Subscription " + substID)
			}
		}
  }
	
	private def handleCommandResponse(in:DataInputStream ): Unit = {
		val hasError=in.readBoolean
		if(hasError) {
			val error=CommandError.read(in)
			commandResultQueue.put(HasError(error))
			printErrorMessage("Command Response")
			printErrorMessage( error.getMessage)
		}
		else {
			val result= if(in.readBoolean) HasResult(Expression.readConstant(in))
																 else NoResult     
		  commandResultQueue.put(result)
		}
	}


  def askEnquiry(in: DataInputStream): Unit = {
	  //println("Ask Enquiry ")
	  val st=in.readUTF
	  //println("St:"+st)
	  val xm=scala.xml.XML.loadString (st)
	  //println("xm:"+xm)
	  val question=ParamQuestion.fromXML(xm)
	  //println("Question:"+question)
		question match {
			case Some(q)=> Swing.onEDT{ DialogManager.startEnquiryDialog(q) }
			case _ => util.Log.e("cant create ParamQuestion from : "+st)
		}		
	}

  def answerEnquiry(params: Seq[ResultElement]): Unit = {
		sock.sendData(ClientCommands.answerEnquiry) { out =>
			out.writeInt(params.size)
			params foreach(x => {out.writeUTF(x.paramName); x.result.write(out)})
		}
	}	
	
	def registerSetupListener(listener:() => Unit): Unit = {
		if(isSetup){ listener() }// call listener instantly when types are already setup
		else setupListenerMap+=listener // else put it in the list to be notified later
	}	
	
	
	def registerStoreSettingsListener(listener:() => Unit): Unit = {
		storeSettingsListenerMap+=listener
		
	}

  private[comm] def notifySetupListeners(): Unit = Swing.onEDT {
		setupListenerMap.foreach(a => try { a()} catch {
			case NonFatal(e)=> util.Log.e(e)
      case other: Throwable => util.Log.w(other.toString)
    })
		isSetup=true
		println("Notify SetupListeners done")
	}
	
	
	
	
	private[comm] def notifyStoreSettingsListeners(): Unit = {
		storeSettingsListenerMap.foreach(a => try { a()} catch {
			case NonFatal(e)=> util.Log.e(e)
      case other: Throwable => util.Log.e("store settings", other); //System.exit(0)
		})
	}
	
	
	/** runs a given function in a new Thread from the ThreadPool
	 *  to avoid deadlocks
	 */
  def runInPool(a: => Unit): Unit = {
    myPool.execute(() => a)
	}

  def startSearch(rootRef: Reference, searchText: String, listener: SearchListener): Boolean =
    if (searchListener.isDefined) {
      Log.e("cant start Search, search is running")
      false
    }
    else {
      //println("Start Search")
      searchListener = Some(listener)
      sock.sendData(ClientCommands.searchForText) { out =>
        rootRef.write(out)
        out.writeUTF(searchText)
      }
      true
    }

  def receiveSearchResults(in: DataInputStream): Unit = {
    val numTree = in.readInt()
    //println("receive search "+searchListener+" num:"+numTree)
    if (numTree == -1) {
      for (r <- searchListener) Swing.onEDT(r(SearchFinished))
      searchListener = None
    } else {
      val tree = for (i <- 0 until numTree) yield InstanceData.readWithChildInfo(Reference(in), in)
      val inst = InstanceData.readWithChildInfo(Reference(in), in)
      for (r <- searchListener) Swing.onEDT(r(SearchResult(tree, inst)))
    }
  }
			
	// ************************************ UNDO ********************************************
	
	private def lockForUndo(in:DataInputStream ): Unit = {
		val lockUser=in.readUTF
    util.Log.w("LOCK FOR UNDO by User "+lockUser)
		for(l<-undoLockListener)Swing.onEDT{l(true,lockUser)} 
	}
	
	private def releaseUndoLock(in:DataInputStream ): Unit = {
		for(l<-undoLockListener)Swing.onEDT{l(false,"")}
	}
	
	private def sendUndoInformation(in:DataInputStream ): Unit = {
		val stepListSize=in.readInt
		//System.out.println("Sending Undo Information "+stepListSize)
		val stepList=collection.mutable.ArrayBuffer[TransStepData]()
		for(i <-0 until stepListSize) {
			val newElem=TransStepData.read(in) 
			stepList+= newElem
			//System.out.println(newElem.toString)
		}		
		if(stepListReader!=null) Swing.onEDT{stepListReader.loadStepList(stepList.toSeq)}
    util.Log.w("send ready ")
			
	}

  def registerStepListReader(slr: StepListReader): Unit = stepListReader = slr

  def requestUndoData(): Unit = sock.sendData(ClientCommands.requestUndoData) { out => }

  def doUndo(): Unit = sock.sendData(ClientCommands.undo) { out => }

  def stopUndo(): Unit = sock.sendData(ClientCommands.stopUndo) { out => }


  def printErrorMessage(message: String): Unit =
    if (message == null) println("message==null")
    else {
      DialogManager.printError(message)
      val ss = Thread.currentThread().getStackTrace
      val newStack = java.util.Arrays.copyOfRange(ss, 2, Math.min(14, ss.length))
      if (message.length > 1) util.Log.e(message, newStack)
    }

  def getMyUserId: Int = SystemSettings.settings match {
	  case s:definition.typ.ClientSystemSettings=>s.myUserID
	  case _=> -1
	}
}