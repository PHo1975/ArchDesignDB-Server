/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import definition.data._
import definition.expression.{CommonFuncMan, FunctionManager}
import definition.typ.{AllClasses, BlockClass, SystemSettings}
import server.comm.CommonSubscriptionHandler
import server.config.FSPaths
import server.storage.TransLogHandler.recordSize
import transaction.handling.{ActionList, TransactionManager}
import util.Log

import java.io._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

class CopyFileHandler(fileName:String){
	val file=new File(FSPaths.dataDir+fileName)
	val stream=new BufferedOutputStream(new FileOutputStream(file),32768)
	var pos:Long=0L

	def writeBuffer(buffer:Array[Byte], size:Int):Long = {
		val oldPos=pos
		stream.write(buffer,0,size)
		pos+=size
		oldPos
	}

	def shutDown(): Unit =stream.flush()

}

/** manages all file io operations
 * 
 */
object StorageManager {
  protected lazy val folderType: Int =SystemSettings.settings.systemTypes("Folder")
  println("Storageman datapath"+FSPaths.dataDir)
  val undoDeleteList=List(TransType.dataChanged.id,TransType.created.id)
  val undoLinkList=List(TransType.linksChanged.id,TransType.created.id)
  val undoPropList=List(TransType.propertyChanged.id,TransType.created.id)
  val undoCollList=List(TransType.collFuncChanged.id,TransType.created.id)
  protected val dataFileHandler=new BoolContFileHandler[InstanceData]("InstData.dat",InstanceData.read)
  protected val propFileHandler=new ContainerFileHandler("PropData.dat",InstanceProperties.read)
  protected val linkFileHandler=new ContainerFileHandler("ExternLinks.dat",ReferencingLinks.read)
  protected val collFuncFileHandler=new ContainerFileHandler("collFuncs.dat",CollFuncResultSet.read)
	val blockFileHandler=new ContainerFileHandler[BlockData]("Block.dat",null)
  private val fileLock = new Object()
  var shuttedDown=false
  var inited=false
  var serverClassList:Map[Int,ServerObjectClass]=_
	var blockClassList:Map[Int,BlockClass]=_
  protected var _ixHandlerList: mutable.HashMap[Int, ClassIndexHandler] = collection.mutable.HashMap[Int, ClassIndexHandler]()
	protected var _blockIxHandlerList: mutable.Map[Int, BlockIndexHandler] = collection.mutable.HashMap[Int,BlockIndexHandler]()

  def init(scl:ServerClassList ): Unit = {
		println("storagemanager init")
    serverClassList=scl.classList
		blockClassList=scl.blockClassList
  	if(shuttedDown)	shuttedDown=false
  	FunctionManager.setManager(new CommonFuncMan)
  }

	def ixHandler(typ:Int): ClassIndexHandler = _ixHandlerList.getOrElseUpdate(typ,new ClassIndexHandler(serverClassList(typ)))

	def blockHandler(typ:Int):BlockIndexHandler= _blockIxHandlerList.getOrElseUpdate(typ,new BlockIndexHandler(blockClassList(typ)))



  /** creates a new empty instance
   *
   */
  def createInstance(typ: Int, owners: Array[OwnerReference], withStartValues: Boolean): InstanceData = fileLock.synchronized {
  	val hand =ixHandler(typ)
  	val inst=hand.theClass.createInstance(
  		new Reference(typ,hand.createInstance()),owners,withStartValues)
  	inst
  }

	def createBlockInstance(typ:Int): Int = fileLock.synchronized{
	  blockHandler(typ).createInstance()
	}

  /** deletes an instace from the index
   *
   */
  def deleteInstance(typ: Int, inst: Int): Unit = fileLock.synchronized {
  	ixHandler(typ).deleteInstance(inst)
  }

	def deleteBlockInstance(typ:Int,inst:Int):Unit = fileLock.synchronized{
		blockHandler(typ).deleteInstance(inst)
	}

  /** creates an empty Properties object with default values
   * this object will not be stored with this function call
   */
  def createInstanceProperties(ref: Reference): InstanceProperties = fileLock.synchronized {
    ixHandler(ref.typ).theClass.createInstanceProperty(ref)
  }
  
  def getInstPropList(ref:Reference,propField:Int): Seq[Reference] =
    getInstanceProperties(ref) match {
      case Some(props) =>if(props.propertyFields.length>propField)props.propertyFields(propField).propertyList
      else {util.Log.e("getInstProp ref="+ref+" propField:"+propField+" is > stored PropData.size:"+props.propertyFields.length);Seq.empty}
      case None=> Seq.empty
  }
  
  def forEachChild(ref: Reference, func: InstanceData => Unit): Unit = {
    //println("foreach child:"+ref)
    getInstanceProperties(ref) match {
      case Some(props) =>
        val theClass = AllClasses.get.getClassByID(ref.typ)
        val classProps = theClass.propFields.size
        for (prf <- props.propertyFields.indices
             if prf < classProps && !theClass.propFields(prf).hidden;
             propField = props.propertyFields(prf);
             child <- propField.propertyList
             if instanceExists(child.typ, child.instance)) {
          func(getInstanceData(child))
        }
      case None =>
    }
	}

	def instanceExists(typ: Int, inst: Int): Boolean = fileLock.synchronized {
		if (serverClassList.contains(typ)) ixHandler(typ).instanceExists(inst) else false}

	def blockExists(ref:Reference):Boolean= fileLock.synchronized{
		if(blockClassList.contains(ref.typ)) blockHandler(ref.typ).instanceExists(ref.instance) else false
	}

	/** loads an instance from the data file
   *
   */
	def getInstanceData(ref: Reference): InstanceData = fileLock.synchronized {
		val rec=ixHandler(ref.typ).getInstanceRecord(ref.instance )
		if (rec.dataPos < 0 || rec.dataLength==0) throw new
				IllegalArgumentException("get Instance() instance "+ref+" is deleted")
		try {
			val instObj = dataFileHandler.readWithBool(ref, rec.dataPos, rec.dataLength, (rec.propPos != 0) && (rec.propLength != 0))
			instObj
		} catch { case NonFatal(er)=> util.Log.e("loading ref:"+ref,er);null
		case eo:EOFException=>util.Log.e("loading ref:"+ref,eo);null}
	}

	def getBlockData(ref: Reference): BlockData = fileLock.synchronized {
		val handler=blockHandler(ref.typ)
		val dataPos=handler.getDataPos(ref.instance)
		if (dataPos < 0 ) throw new
				IllegalArgumentException("get Block instance "+ref+" is deleted")
		try {
			val block=new BlockData(ref,new Array(handler.theClass.blocksize))
			blockFileHandler.readInBuffer(dataPos,handler.theClass.blocksize).copyToArray(block.data,0,handler.theClass.blocksize)
			block
		} catch { case NonFatal(er)=> util.Log.e("loading ref:"+ref,er);null
		case eo:EOFException=>util.Log.e("loading ref:"+ref,eo);null}
	}

	/** writes an instance to the data file
   *  @param created was this instance created during this transaction and should a created
   *  record be stored in the transaction log
   */
	def writeInstance(data: InstanceData, created: Boolean,writeIndex:Boolean=true): (Long,Int) = fileLock.synchronized {
		val result@(pos,size)=dataFileHandler.writeInstance(data)
		if(writeIndex)
			ixHandler(data.ref.typ).writeData(data.ref.instance, pos, size,created)
		result
	}

  def writeBlock(data:BlockData,created:Boolean): Unit = {
		val (pos,_)=blockFileHandler.writeInstance(data)
		blockHandler(data.ref.typ).writeData(data.ref.instance,pos,created)
		//(pos,size)
	}

	// *************************************************************************************
	//                                       PROPERTIES


	def getInstanceProperties(ref: Reference): Option[InstanceProperties] = fileLock.synchronized {
		val handler=ixHandler(ref.typ)
		val rec=handler.getInstanceRecord(ref.instance)
		if (rec.propPos == 0 && rec.propLength==0) None
		else Some(propFileHandler.readInstance(ref,rec.propPos,rec.propLength))
	}

	def writeInstanceProperties(data: InstanceProperties,writeIndex:Boolean=true):(Long,Int) = fileLock.synchronized {
		val hasChildren = data.hasChildren
		val result@(pos, size) = if (hasChildren) propFileHandler.writeInstance(data)
		else (0L, 0) // if there are no children, delete this property data set
		if (writeIndex) ixHandler(data.ref.typ).writePropertiesData(data.ref.instance, pos, size)
		result
	}

  
  def getReferencingLinks(ref: Reference): Option[ReferencingLinks] = fileLock.synchronized {
  	val handler=ixHandler(ref.typ)
  	val rec=handler.getInstanceRecord(ref.instance)
  	    //System.out.println("recpos "+rec.propPos +" recsize:"+rec.propSize)
  	    if (rec.linkPos == 0 && rec.linkLength==0) None
  	    else
  	    {
  	      val propObj: ReferencingLinks =linkFileHandler.readInstance(ref,rec.linkPos,rec.linkLength )
  	      // not chached yet
  	      Some(propObj)
  	    }
  }

  def writeReferencingLinks(data: ReferencingLinks,writeIndex:Boolean=true,writeLog:Boolean=true): (Long,Int) = fileLock.synchronized {
		val result@(pos, size) =if(data.links.nonEmpty) linkFileHandler.writeInstance(data)
		else (0L,0)
    if(writeIndex) ixHandler(data.ref.typ).writeLinksData(data.ref.instance, pos, size,writeLog)
		result
  }


  def getCollectingFuncData(ref: Reference): Option[CollFuncResultSet] = fileLock.synchronized {
  	val rec=ixHandler(ref.typ).getInstanceRecord(ref.instance)
    if (rec.collPos == 0 && rec.collLength==0) None
    else  {
      val propObj=collFuncFileHandler.readInstance(ref,rec.collPos,rec.collLength)
      // not chached yet
      Some(propObj)
    }
  }

  def writeCollectingFuncData(data: CollFuncResultSet,writeIndex:Boolean=true): (Long,Int) = fileLock.synchronized {
  	val result=collFuncFileHandler.writeInstance(data)
  	if(writeIndex) ixHandler(data.ref.typ).writeCollFuncData(data.ref.instance, result._1, result._2)
		result
  }

  def shutDown(): Unit = fileLock.synchronized {
    if (!shuttedDown) {
      for (i <- _ixHandlerList.valuesIterator) i.shutDown()
			for (i <- _blockIxHandlerList.valuesIterator) i.shutDown()
      dataFileHandler.shutDown()
      propFileHandler.shutDown()
      TransLogHandler.shutDown()
      TransDetailLogHandler.shutDown()
      linkFileHandler.shutDown()
      collFuncFileHandler.shutDown()
      shuttedDown = true
      inited = false
      util.Log.w("Storage shutted down")
    }
  }
  
  // *************************************************************************************
  //                                    L I N K S

  def undoLastStep(): Unit = fileLock.synchronized {
    val startTime=System.currentTimeMillis()
  	val currTrID=TransLogHandler.getTransID
  	val startLogPos=TransLogHandler.getInsertPos-1
  	var currLogPos=startLogPos
    util.Log.w("startUNDO trID:"+currTrID+" startLogPos:"+startLogPos )
  	var currRec=TransLogHandler.readPosition(currLogPos)
  	while(currLogPos>0 && currRec.trID ==currTrID)
  	{
  		//print(" "+currLogPos+"=>"+currRec.toString+"| ")
  		undoTransRec(currRec,currLogPos)
  		currLogPos-=1
  		currRec=TransLogHandler.readPosition(currLogPos)
  	}
  	//println("\nTransLogHandler undo last step "+startLogPos+" - "+currLogPos)
  	TransLogHandler.undoLastStep(startLogPos-currLogPos)
  	//println("TransDetailLogHandler undo last step ")
  	TransDetailLogHandler.undoLastStep()
    util.Log.w("Undo "+(System.currentTimeMillis()-startTime))
  }

  private def undoTransRec(rec:LogIndexSet,currLogPos:Int): Unit = {
  	//var ret:(Int,Long,Int)=null

  	val handler=ixHandler(rec.typ)
    rec.transTyp match {
    	case TransType.deleted|TransType.dataChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoDeleteList)
				handler.writeData(rec.inst,pos, len, trans==TransType.created.id,withLog = false)
				//handler.instCache.removeInstanceData(rec.inst)
			case TransType.created =>
				handler.deleteInstance(rec.inst,withLog = false)
				CommonSubscriptionHandler.instanceDeletedViaUndo(Reference(rec.typ,rec.inst))
				//handler.instCache.removeInstanceData(rec.inst)
			case TransType.linksChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoLinkList)
				if(trans==TransType.created.id) // no info since creation
          handler.writeLinksData(rec.inst,0,0,withLog = false)
        else handler.writeLinksData(rec.inst,pos,len,withLog = false)
			case TransType.propertyChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoPropList)
				if(trans==TransType.created.id) // no info since creation
        {util.Log.e("no prop data found for "+rec.typ+","+rec.inst )
          handler.writePropertiesData(rec.inst,0,0,withLog = false)}
        else handler.writePropertiesData(rec.inst,pos,len,withLog = false)
				//handler.propCache.removeInstanceData(rec.inst)
			case TransType.collFuncChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoCollList)
				if(trans==TransType.created.id) // no info since creation
          handler.writeCollFuncData(rec.inst,0,0,withLog = false)
        else handler.writeCollFuncData(rec.inst,pos,len,withLog = false)
		}
  }
  
  
  // *************************************************************************************
  //                                   Collecting-Functions


	def loadChildren(parent: Reference, ofType: Int, propField: Int): Seq[InstanceData] =
		getInstanceProperties(parent) match {
			case Some(props) =>
				for (child <- props.propertyFields(propField).propertyList; if (ofType == -1) || (child.typ == ofType))
					yield getInstanceData(child)
			case None => Seq.empty
		}

	def getNextParentOfType(childRef:Reference,parentType:Int):Option[Reference]= {
		//println("getNextParent:"+childRef)
		if(!instanceExists(childRef.typ,childRef.instance)) None
		else {
			val inst=getInstanceData(childRef)
			if(inst.owners.isEmpty) None
			else {
				for(owner <-inst.owners) {
					if (owner.ownerRef.typ == parentType) return Some(owner.ownerRef)
					else {
						val ret =getNextParentOfType(owner.ownerRef,parentType)
						if(ret.isDefined) return ret
					}
				}
				for(owner <-inst.secondUseOwners ) {
					if (owner.ownerRef.typ == parentType) return Some(owner.ownerRef)
					else {
						val ret =getNextParentOfType(owner.ownerRef,parentType)
						if(ret.isDefined) return ret
					}
				}
				None
			}
		}
	}

	def isChildOf(child: Reference, parent: Reference): Boolean =
		instanceExists(child.typ, child.instance) && child == parent || {
			val inst=getInstanceData(child)
			for(owner<-inst.owners)
				if(owner.ownerRef==parent) return true
				else if(isChildOf(owner.ownerRef,parent))return true
			for(owner<-inst.secondUseOwners)
				if(owner.ownerRef==parent) return true
				else if(isChildOf(owner.ownerRef,parent))return true
			false
		}

	def getPathToParent(child: Reference, parent: Reference): List[Reference] =
		if(!instanceExists(child.typ,child.instance)|| child==parent ||
			!instanceExists(parent.typ,parent.instance)) Nil
		else {
			val inst=getInstanceData(child)
			for(owner<-inst.owners)
				if(owner.ownerRef==parent) return List(owner.ownerRef)
				else getPathToParent(owner.ownerRef,parent) match {
					case Nil=>
					case o=> return owner.ownerRef::o;
				}
			for(owner<-inst.secondUseOwners)
				if(owner.ownerRef==parent) return List(owner.ownerRef)
				else getPathToParent(owner.ownerRef,parent) match {
					case Nil=>
					case o=> return owner.ownerRef::o;
				}
			Nil
		}

	def searchFoldersForType(parentRef:Reference,propField:Int,childType:Int):Option[Reference]=
		if(!instanceExists(parentRef.typ,parentRef.instance)) None
		else {
			for(p<-getInstanceProperties(parentRef);el<-p.propertyFields(propField).propertyList)
				if(el.typ==childType) return Some(el)
				else if(el.typ==folderType) {
					val ret=searchFoldersForType(el,1,childType)
					if(ret.isDefined) return ret
				}
			None
		}

	/** loads an instance from the data file. When that instance is deleted, tries to find the old state
   *
   */
	def getZombieInstanceData(ref: Reference): InstanceData = fileLock.synchronized {
		if(ref.typ==0) EMPTY_INSTANCE else {
			val rec = ixHandler(ref.typ).getInstanceRecord(ref.instance)
			var pos = rec.dataPos
			var length = rec.dataLength
			if (rec.dataPos == -1 && rec.dataLength == 0) { // deleted
				val (transType, npos, nlength) = TransLogHandler.getLastLivingData(ref.typ, ref.instance,
					TransLogHandler.getInsertPos - 1, List(TransType.created.id, TransType.dataChanged.id))
				//System.out.println("Last Living "+ref+" is: pos:"+npos+" size:"+nlength)
				if (transType == -1) return new InstanceData(ref, IndexedSeq(), Array(), Array.empty, false)
				pos = npos
				length = nlength

			}
			//else if(rec.dataPos<1) System.out.println("get Inst pos<1 :"+ref+" "+rec.dataPos)
			val instObj = dataFileHandler.readWithBool(ref, pos, length, boolValue = false)
			instObj
		}
	}

	def pushInstanceData(ref: Reference, out: DataOutput): Unit = fileLock.synchronized {
		val rec=ixHandler(ref.typ).getInstanceRecord(ref.instance )
		ref.write(out)
		dataFileHandler.pushData(rec.dataPos,rec.dataLength,out )
		out.writeBoolean((rec.propPos !=0)&&(rec.propLength !=0))
	}

	def bulkGetInstanceData(startRef: Reference, endRef: Reference): IndexedSeq[InstanceData] = fileLock.synchronized {
		//System.out.println("bulkget:"+startRef+" - "+endRef)
		ixHandler(startRef.typ).bulkGetInstanceRecords(startRef.instance,endRef.instance,dataFileHandler)
	}

	def bulkPushInstanceData(startRef: Reference, endRef: Reference, out: DataOutput): Unit = fileLock.synchronized {
		//System.out.println("bulkpush:"+startRef+" - "+endRef)
		ixHandler(startRef.typ).bulkPushInstanceRecords(startRef.instance,endRef.instance,dataFileHandler,out)
	}


	def safeFlush():Unit = {
		dataFileHandler.flush()
		propFileHandler.flush()
		linkFileHandler.flush()
		collFuncFileHandler.flush()
		for(h<-_ixHandlerList.valuesIterator) h.flush()
		for(b<-_blockIxHandlerList.valuesIterator) b.flush()
		TransLogHandler.flush()
		TransDetailLogHandler.flush()
	}

  /** reorganizeds the database so that deleted and old entries are removed
   * transaction logs are deleted so undo is not possible anymore
   *
   */
  def reorgDatabase(listener: (Int, String) => Unit): Unit = fileLock.synchronized {
		UsageStatFileHandler.updateStatistics()
    util.Log.w("Reorg translog insertPos:"+TransLogHandler.getInsertPos+" detail:"+TransDetailLogHandler.getInsertPos)
	  TransDetailLogHandler.deleteLogFile()
  	TransLogHandler.deleteLogFile()
  	val copyDataFileHandler=new CopyFileHandler("InstData.dat.cpy")
  	val copyPropFileHandler=new CopyFileHandler("PropData.dat.cpy")
  	val copyLinkFileHandler=new CopyFileHandler("ExternLinks.dat.cpy")
  	val copyCollFuncFileHandler=new CopyFileHandler("collFuncs.dat.cpy")

		val transLogFile=new BufferedOutputStream(new FileOutputStream(TransLogHandler.fileName),32768)
		transLogFile.write(new Array[Byte](4))
		val transLogBufferStream= new MyByteStream(recordSize*4)
		val transLogOutStream=new DataOutputStream(transLogBufferStream)
		var numCombi=0

		var timeReadIX:Long=0
		var timeWriteIX:Long=0
		var timeWriteData:Long=0
		var timeWriteProp:Long=0
		var timeWriteLog:Long=0

		var oldTime=System.currentTimeMillis()

		def measure():Long= {
			val now=System.currentTimeMillis()
			val delta=now-oldTime
			oldTime=now
			delta
		}

		def writeTransLog(transTyp: TransType.Value,typ:Int,inst:Int,dataPos:Long,dataLength:Int):Unit =	{
			transLogOutStream.writeByte(transTyp.id)
			transLogOutStream.writeInt(0)
			transLogOutStream.writeInt(typ)
			transLogOutStream.writeInt(inst )
			transLogOutStream.writeLong(dataPos)
			transLogOutStream.writeInt(dataLength)
			numCombi+=1
		}

  	class MyListener extends RecordListener{
  	    var replaceHandler: ClassIndexHandler = _

			def nextRecord(inst: Int, dataPos: Long, dataLen: Int, propPos: Long, propLen: Int, linkPos: Long, linkLen: Int, collPos: Long, collLen: Int): Unit = {
				timeReadIX+=measure()
				val ndataPos = copyDataFileHandler.writeBuffer(dataFileHandler.readInBuffer(dataPos, dataLen), dataLen)
				timeWriteData+=measure()
				val npropPos = if (propLen < 1) 0 else copyPropFileHandler.writeBuffer(propFileHandler.readInBuffer(propPos, propLen), propLen)
				val nlinkPos = if (linkLen < 1) 0 else copyLinkFileHandler.writeBuffer(linkFileHandler.readInBuffer(linkPos, linkLen), linkLen)
				val ncollPos = if (collLen < 1) 0 else copyCollFuncFileHandler.writeBuffer(collFuncFileHandler.readInBuffer(collPos, collLen), collLen)
				timeWriteProp+=measure()
				replaceHandler.reorgWriteRecord(inst, ndataPos, dataLen, npropPos, Math.max(0, propLen), nlinkPos, Math.max(0, linkLen), ncollPos, Math.max(0, collLen))
				timeWriteIX+=measure()
				transLogBufferStream.reset()
				numCombi=0
				writeTransLog(TransType.created, replaceHandler.theClass.id, inst, ndataPos, dataLen)
				if (propLen > 0) writeTransLog(TransType.propertyChanged, replaceHandler.theClass.id, inst, npropPos, propLen)
				if (linkLen > 0) writeTransLog(TransType.linksChanged, replaceHandler.theClass.id, inst, nlinkPos, linkLen)
				if (collLen > 0) writeTransLog(TransType.collFuncChanged, replaceHandler.theClass.id, inst, ncollPos, collLen)
				transLogFile.write(transLogBufferStream.buffer,0,recordSize*numCombi)
				timeWriteLog+=measure()
			}
		}

  	val recListener=new MyListener
	  var lix=1
		try {
			for (typ <- serverClassList.keysIterator; handler = ixHandler(typ)) {
				print(" "+handler.theClass.name)
				listener(lix, handler.theClass.name)
				timeReadIX=0
				timeWriteIX=0
				timeWriteData=0
				timeWriteProp=0
				timeWriteLog=0
				recListener.replaceHandler = new ClassIndexHandler(handler.theClass, ".reo")
				handler.foreachInstance(recListener)
				println("Loop done readIX:"+timeReadIX+" writeIX:"+timeWriteIX+" writeData:"+timeWriteData+" writeProp:"+timeWriteProp+" writeLog:"+timeWriteLog)
				recListener.replaceHandler.shutDown()
				handler.takeOverFromReorgFile(recListener.replaceHandler.fileName)
				//System.out.print(handler.fileName+" ")
				lix += 1
			}
		} catch {case NonFatal(e)=> Log.e(e);println("e:"+e	) }
  	copyDataFileHandler.shutDown()
  	copyPropFileHandler.shutDown()
  	copyLinkFileHandler.shutDown()
  	copyCollFuncFileHandler.shutDown()
  	dataFileHandler.takeOverReorgFile(copyDataFileHandler.file)
  	propFileHandler.takeOverReorgFile(copyPropFileHandler.file)
  	linkFileHandler.takeOverReorgFile(copyLinkFileHandler.file)
  	collFuncFileHandler.takeOverReorgFile(copyCollFuncFileHandler.file)
		FSPaths.setLastStatTransID(1)
		transLogFile.close()
		TransLogHandler.restoreLogFile()
		TransDetailLogHandler.flush()
    util.Log.w("\nReorg complete\n Translog Insert Pos:"+TransLogHandler.getInsertPos+"\n DetailLogHandler Insert Pos:"+TransDetailLogHandler.getInsertPos)
	}



  def fixInheritance(listener: (Int, String) => Unit): Unit = fileLock.synchronized {
	  var lix=1
	  var ownerList:List[(Int,Long,Int)]=Nil

	  val  myListener =new  RecordListener{
	     def nextRecord(inst:Int,dataPos:Long,dataLen:Int,propPos:Long,propLen:Int,linkPos:Long,linkLen:Int,collPos:Long,collLen:Int): Unit =
         if(propLen>0)   ownerList=(inst,propPos,propLen)::ownerList
	  }
	  //System.out.println("fix inheritance ")
	  for (typ<- serverClassList.keysIterator;handler = ixHandler(typ)){
  	  listener(lix,handler.theClass.name)
  	  handler.foreachInstance(myListener)
  	  for((inst,propPos,propLen)<-ownerList){
  	    val ownerRef=Reference(typ,inst)
	      val propData: InstanceProperties =propFileHandler.readInstance(ownerRef,propPos,propLen)
	      for(pfIx<-propData.propertyFields.indices;pf=propData.propertyFields(pfIx)) {
	        val childList =for(childRef<-pf.propertyList;if instanceExists(childRef.typ, childRef.instance))
	          yield getInstanceData(childRef)
	        if(childList.size<pf.propertyList.size) {
	          propData.propertyFields(pfIx)= new PropertyFieldData(pf.isSingle,childList.map(_.ref))
	          writeInstanceProperties(propData)
	        }
	        for(data<-childList) {
		        val oix=data.owners.indexWhere(_.ownerRef ==ownerRef)
		        if(oix> -1) {
		          val ownerRecord=data.owners(oix)
		          if(ownerRecord.ownerField !=pfIx){
                util.Log.e("wrong ownerField " +ownerRecord+" in ownersRecord of Instance "+data.ref+" should be "+pfIx)
		            data.owners(oix)=new OwnerReference(pfIx,ownerRef)
		            writeInstance(data,created = false)
		          }
		        }
		        else {
	            val suix=data.secondUseOwners.indexWhere(_.ownerRef ==ownerRef)
	            if(suix> -1) {
	              val suOwnerRecord=data.secondUseOwners(suix)
	               if(suOwnerRecord.ownerField !=pfIx) {
                   util.Log.e("wrong ownerField " +suOwnerRecord+" in SUOwnersRecord of Instance "+data.ref+" should be "+pfIx)
	                 val newOwners  =for(i<-data.secondUseOwners.view.indices) yield if(i==suix)new OwnerReference(pfIx,ownerRef) else data.secondUseOwners(i)
	                 writeInstance(data.changeSecondUseOwners(newOwners.toArray),created = false)
	               }
	            }
	            else util.Log.e("Cant find the real Owner "+ownerRef+" in Owner- and SUOwner-Records of child:"+data+" ("+data.ref+")")
		        }
	        }
	      }
  	  }
  	  ownerList=Nil
  	  lix+=1
	  }
	}

  def deleteOrphans(listener: (Int, String) => Unit): Unit = fileLock.synchronized {
	  var lix=1
	  var dList:List[(Int,Long,Int)]=Nil
	  val  myListener =new  RecordListener{
      def nextRecord(inst: Int, dataPos: Long, dataLen: Int, propPos: Long, propLen: Int, linkPos: Long, linkLen: Int, collPos: Long, collLen: Int): Unit =
        if (dataLen > 0) dList = (inst, dataPos, dataLen) :: dList
	  }

	  for (typ<- serverClassList.keysIterator;handler = ixHandler(typ)){
  	  listener(lix,handler.theClass.name)
  	  handler.foreachInstance(myListener)
      util.Log.w("DelOrphans check "+handler.theClass.name+" "+dList.size)
  	  for((inst,dPos,dLen)<-dList) try{
  	    val ref=new Reference(typ,inst)
  	    val instObj=dataFileHandler.readWithBool(ref,dPos,dLen,boolValue = false )
  	    if(instObj.owners.nonEmpty || instObj.secondUseOwners.nonEmpty){
	  	    val newOwners=instObj.owners.filter(aOwner=> {
	  	      if(!instanceExists(aOwner.ownerRef.typ,aOwner.ownerRef.instance)) {util.Log.e("owner "+aOwner+" of "+ref+" does not exist");false}
	  	      else{
		  	      val isIn= getInstanceProperties(aOwner.ownerRef ) match {
		  	        case Some(props)=>props.propertyFields(aOwner.ownerField ).propertyList.contains(ref)
		  	        case None =>false
		  	      }
		  	      if(!isIn)util.Log.e("inst "+ref+" is not owned by "+aOwner)
		  	      isIn
	  	      }
	  	    })
	  	    val newSUOwners=instObj.secondUseOwners.filter(aOwner=> {
	  	      if(!instanceExists(aOwner.ownerRef.typ,aOwner.ownerRef.instance)) {util.Log.e("SU owner "+aOwner+" of "+ref+" does not exist");false}
	  	      else{
		  	      val isIn=getInstanceProperties(aOwner.ownerRef ) match {
		  	        case Some(props)=>props.propertyFields(aOwner.ownerField ).propertyList.contains(ref)
		  	        case None => false
		  	      }
		  	      if(!isIn) util.Log.e("inst "+ref+" is not owned by SU "+aOwner)
	  	        isIn
	  	      }
	  	    })
	  	    if(newOwners.isEmpty&&newSUOwners.isEmpty) {
            util.Log.w("Inst "+instObj+" "+ref+" is an orphan ")
	  	      TransactionManager.tryDeleteInstance(ref,None,None)
	  	      ActionList.commitAllData()
	  	      //System.out.println("delete done ")
	  	    }
	  	    else if(newOwners.length<instObj.owners.length||newSUOwners.length<instObj.secondUseOwners.length){
	  	      var newInst= if(newOwners.length<instObj.owners.length) instObj.changeOwner(newOwners) else instObj
	  	      if(newSUOwners.length<instObj.secondUseOwners.length) newInst=newInst.changeSecondUseOwners(newSUOwners)
            util.Log.w("change "+newInst)
	  	      writeInstance(newInst,created = false)
	  	    }
  	    }
  	  } catch {
  	    case NonFatal(e) => util.Log.e("delete orhpans inst:"+inst+" dpos:"+dPos+" dlen:"+dLen,e)
  	  }
  	  lix+=1
  	  dList=Nil
	  }
    util.Log.w("check done")
	}

  def findOrphans(listener: (Int, String) => Unit): Map[OwnerReference, Iterable[Reference]] = fileLock.synchronized {
	  var lix=1
	  val orphanMap= collection.mutable.HashMap[OwnerReference,collection.mutable.ArrayBuffer[Reference]]()
	  var dList:List[(Int,Long,Int)]=Nil
	  val  myListener =new  RecordListener{
	     def nextRecord(inst:Int,dataPos:Long,dataLen:Int,propPos:Long,propLen:Int,linkPos:Long,linkLen:Int,collPos:Long,collLen:Int): Unit =
         if(dataLen>0)    dList=(inst,dataPos,dataLen)::dList
	  }

    def addOrphan(parentRef: OwnerReference, orphanRef: Reference) = orphanMap.getOrElseUpdate(parentRef, new collection.mutable.ArrayBuffer()).+=(orphanRef)

	  for (typ<- serverClassList.keysIterator;handler = ixHandler(typ)){
  	  listener(lix,handler.theClass.name)
  	  handler.foreachInstance(myListener)
      util.Log.w("Find Orphans check  "+handler.theClass.name+" "+dList.size)
  	  for((inst,dPos,dLen)<-dList) try{
  	    val ref=new Reference(typ,inst)
  	    val instObj=dataFileHandler.readWithBool(ref,dPos,dLen,boolValue = false )
  	    if(instObj.owners.nonEmpty || instObj.secondUseOwners.nonEmpty){
	  	    instObj.owners.foreach(aOwner=> {
	  	      if(!instanceExists(aOwner.ownerRef.typ,aOwner.ownerRef.instance)) {util.Log.e("owner "+aOwner+" of "+ref+" does not exist");false}
	  	      else{
		  	      val isIn: Boolean = getInstanceProperties(aOwner.ownerRef ) match {
		  	        case Some(props)=>props.propertyFields(aOwner.ownerField ).propertyList.contains(ref)
		  	        case None =>false
		  	      }
		  	      if(!isIn){ util.Log.e("inst "+ref+" is not owned by "+aOwner);addOrphan(aOwner,ref)}
		  	      isIn
	  	      }
	  	    })
	  	    instObj.secondUseOwners.foreach(aOwner=> {
	  	      if(!instanceExists(aOwner.ownerRef.typ,aOwner.ownerRef.instance)) {util.Log.e("SU owner "+aOwner+" of "+ref+" does not exist");false}
	  	      else{
		  	      val isIn: Boolean =getInstanceProperties(aOwner.ownerRef ) match {
		  	        case Some(props)=>props.propertyFields(aOwner.ownerField ).propertyList.contains(ref)
		  	        case None => false
		  	      }
		  	      if(!isIn) {util.Log.e("inst "+ref+" is not owned by SU "+aOwner);addOrphan(aOwner,ref)}
	  	        isIn
	  	      }
	  	    })
  	    }
  	  } catch {
  	    case NonFatal(e) => util.Log.e(e.getMessage,e)
  	  }
  	  lix+=1
  	  dList=Nil
	  }
    util.Log.w("check done")
	  orphanMap.toMap
	}

	/** adds a field to each instance of this data Type
	 *  a reorg must be done after calling that function
	 */
  def addField(addClassType: ServerObjectClass, atPos: Int): Unit = fileLock.synchronized {
	  for(ref<-ixHandler(addClassType.id).getInstancesRefs) {
	    val data=getInstanceData(ref).addField(atPos)
	    writeInstance(data,created = false)
	  }
	  val lastFieldName=addClassType.fields(atPos-1).name
	  for(succ<-AllClasses.get.getDirectSuccessorsFor(addClassType.id))
	    addField(succ.asInstanceOf[ServerObjectClass],succ.fields.indexWhere(_.name==lastFieldName)+1)
	}
	
  /** adds an  property field to each Instance of this Data Type
   *  a reorg should be done after calling that function
   *
   */
  def addPropertyField(addClassType: ServerObjectClass): Unit = fileLock.synchronized {
	  //System.out.println("adding PropField to "+addClassType.name)
	  val handler=ixHandler(addClassType.id)
	  for(ref<-handler.getInstancesRefs) {
	    getInstanceProperties(ref) match {
	      case Some(data)=>
					val newData=new InstanceProperties(ref,new PropertyFieldData(false,collection.immutable.IndexedSeq.empty) +: data.propertyFields)
					val (pos,size)= propFileHandler.writeInstance(newData)
					handler.writePropertiesData(ref.instance, pos, size)
				case None =>
	    }
	  }
	  for(succ<-AllClasses.get.getDirectSuccessorsFor(addClassType.id))
	    addPropertyField(succ.asInstanceOf[ServerObjectClass])
	}
	
  def restoreDeletedChildren(owner: Reference, propertyField: Int, rollBackLastPropEntry: Boolean): Unit = fileLock.synchronized {
	  if(rollBackLastPropEntry || (getInstanceProperties(owner) match { // check if propfield is empty
	    case Some(props)=> props.propertyFields(propertyField).propertyList.isEmpty
	    case None => true
	  })) { // restore PropfieldData
      util.Log.w("rolling back last prop entry")
		  val (trans,pos,len)=TransLogHandler.getLastLivingData(owner.typ,owner.instance ,TransLogHandler.getInsertPos-2,
	    			undoPropList)
      util.Log.w("found "+" trans:"+trans+" pos:"+pos+" len:"+len)
	    val handler=ixHandler(owner.typ)
	    if(trans==TransType.created.id) // no info since creation
	    		{util.Log.e("no prop data found for "+owner.typ+","+owner.instance  )
	    		  handler.writePropertiesData(owner.instance ,0,0,withLog = false)}
			else handler.writePropertiesData(owner.instance ,pos,len,withLog = false)
			//handler.propCache.removeInstanceData(owner.instance )
	  }
    for( prop<-getInstanceProperties(owner); childRef <-prop.propertyFields (propertyField).propertyList)
      if(!instanceExists(childRef.typ,childRef.instance )) {
	      print("restore :"+childRef)
	      try {
		      val (ctrans,cpos,clen)=TransLogHandler.getLastLivingData(childRef.typ,childRef.instance ,TransLogHandler.getInsertPos-1,
		    			undoDeleteList,skipDeleteEntries = true)
		    	val chandler=ixHandler(childRef.typ)
		  		chandler.writeData(childRef.instance ,cpos, clen, ctrans==TransType.created.id,withLog = false)
		  		//chandler.instCache.removeInstanceData(childRef.instance )
          util.Log.w(" done.")
	      } catch {case NonFatal(e) => util.Log.e(" error:"+e.getMessage,e)}
      } else util.Log.e("child :"+childRef+" still exists")
	}

	def removeBrokenLinks(listener: (Int, String) => Unit): Unit = fileLock.synchronized {
		var lix = 1
		for (typ <- serverClassList.keysIterator; handler = ixHandler(typ)) {
			listener(lix, handler.theClass.name)
			lix+=1
			val buffer=new ArrayBuffer[ReferencingLinks]()
			handler.foreachInstance(new RecordListener {
				override def nextRecord(inst: Int, dataPos: Long, dataLen: Int, propPos: Long, propLen: Int, linkPos: Long, linkLen: Int, collPos: Long, collLen: Int): Unit = {
          if (dataLen>0 && linkLen >0){
						val ref=Reference(typ,inst)
						val refLinks=linkFileHandler.readInstance(ref,linkPos,linkLen )
						var changed=false
						val newList= refLinks.links.map { case (field, list) =>
							(field, list.filter(exLink =>
								if (!StorageManager.instanceExists(exLink.typ, exLink.inst)) {
									println("Linktarget does not exist Source:" + ref + " field:" + field + " exLink:" + exLink)
									changed = true
									false
								}
								else true))
							}
						if(changed)	buffer+=new ReferencingLinks(ref,newList)
					}
				}
			})
			for(b<-buffer)
				println("Write "+b.ref+" ->"+writeReferencingLinks(b, writeLog = false))
		}
		println("Fixing done.")
	}

  
}