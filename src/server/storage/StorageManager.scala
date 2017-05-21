/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import java.io.{DataOutput, EOFException}

import definition.data._
import definition.expression.{CommonFuncMan, FunctionManager}
import definition.typ.{AllClasses, SystemSettings}
import server.comm.CommonSubscriptionHandler
import server.config.FSPaths
import transaction.handling.{ActionList, TransactionManager}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.util.control.NonFatal



/** manages all file io operations
 * 
 */
object StorageManager {
  private val fileLock = new Object()

  var _ixHandlerList: mutable.HashMap[Int, ClassIndexHandler] = collection.mutable.HashMap[Int, ClassIndexHandler]()
  val dataFileHandler=new BoolContFileHandler[InstanceData]("InstData.dat",InstanceData.read)
  val propFileHandler=new ContainerFileHandler("PropData.dat",InstanceProperties.read)
  val linkFileHandler=new ContainerFileHandler("ExternLinks.dat",ReferencingLinks.read)
  val collFuncFileHandler=new ContainerFileHandler("collFuncs.dat",CollFuncResultSet.read)
  var shuttedDown=false
  var inited=false
  
  val undoDeleteList=List(TransType.dataChanged.id,TransType.created.id)
  val undoLinkList=List(TransType.linksChanged.id,TransType.created.id)
  val undoPropList=List(TransType.propertyChanged.id,TransType.created.id)
  val undoCollList=List(TransType.collFuncChanged.id,TransType.created.id)

  var serverClassList:Map[Int,ServerObjectClass]=_

  def ixHandler(typ:Int): ClassIndexHandler = _ixHandlerList.getOrElseUpdate(typ,new ClassIndexHandler(serverClassList(typ)))

  def init(classList:Map[Int,ServerObjectClass] ): Unit = {
    serverClassList=classList
  	if(shuttedDown)	shuttedDown=false
  	FunctionManager.setManager(new CommonFuncMan)
  }


  def instanceExists(typ: Int, inst: Int): Boolean = fileLock.synchronized {if (serverClassList.contains(typ)) ixHandler(typ).instanceExists(inst) else false}
  
  
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


  lazy val folderType: Int =SystemSettings.settings.systemTypes("Folder")

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
  	val rec=ixHandler(ref.typ).getInstanceRecord(ref.instance )
  	var pos= rec.dataPos
  	var length=rec.dataLength
  	if (rec.dataPos == -1 && rec.dataLength==0) { // deleted
  		val (transType,npos,nlength)=TransLogHandler.getLastLivingData(ref.typ,ref.instance,
  			TransLogHandler.getInsertPos-1,List(TransType.created.id,TransType.dataChanged.id))
  		//System.out.println("Last Living "+ref+" is: pos:"+npos+" size:"+nlength)
  		if(transType== -1) return new InstanceData(ref,IndexedSeq(),Array(),Seq.empty,false)
  		pos=npos;length=nlength
  		
  	}
  	//else if(rec.dataPos<1) System.out.println("get Inst pos<1 :"+ref+" "+rec.dataPos)
  	val instObj=dataFileHandler.readWithBool(ref,pos,length,false )
  	instObj
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
  
      
  /** creates a new empty instance
   * 
   */
  def createInstance(typ: Int, owners: Array[OwnerReference], withStartValues: Boolean): InstanceData = fileLock.synchronized {
  	val hand =ixHandler(typ)
  	val inst=hand.theClass.createInstance(
  		new Reference(typ,hand.createInstance()),owners,withStartValues)  	  	
  	inst  
  }  
  
  
  /** writes an instance to the data file
   *  @param created was this instance created during this transaction and should a created
   *  record be stored in the transaction log 
   */
  def writeInstance(data: InstanceData, created: Boolean): Unit = fileLock.synchronized {
  	val (pos,size)=dataFileHandler.writeInstance(data)
  	val handler= ixHandler(data.ref.typ)
  	handler.writeData(data.ref.instance, pos, size,created)
  }
  
  
  /** deletes an instace from the index
   * 
   */
  def deleteInstance(typ: Int, inst: Int): Unit = fileLock.synchronized {
  	val handler=ixHandler(typ)
  	handler.deleteInstance(inst)
  }

  
  // *************************************************************************************
  //                                       PROPERTIES

  /** creates an empty Properties object with default values
   * this object will not be stored with this function call
   */
  def createInstanceProperties(ref: Reference): InstanceProperties = fileLock.synchronized {
    ixHandler(ref.typ).theClass.createInstanceProperty(ref)
  }


  def getInstanceProperties(ref: Reference): Option[InstanceProperties] = fileLock.synchronized {
  	val handler=ixHandler(ref.typ)
		val rec=handler.getInstanceRecord(ref.instance)
		if (rec.propPos == 0 && rec.propLength==0) None
		else Some(propFileHandler.readInstance(ref,rec.propPos,rec.propLength))
  }
  
    
  def getInstPropList(ref:Reference,propField:Int): Seq[Reference] =
    getInstanceProperties(ref) match {
      case Some(props) =>if(props.propertyFields.length>propField)props.propertyFields(propField).propertyList
      else {util.Log.e("getInstProp ref="+ref+" propField:"+propField+" is > stored PropData.size:"+props.propertyFields.length);Seq.empty}
      case None=> Seq.empty
  }


  def writeInstanceProperties(data: InstanceProperties): Unit = fileLock.synchronized {
  	val hasChildren=data.hasChildren
  	val (pos,size)= if(hasChildren)propFileHandler.writeInstance(data)
  									else (0L,0)// if there are no children, delete this property data set
  	ixHandler(data.ref.typ).writePropertiesData(data.ref.instance, pos, size)
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
  
  // *************************************************************************************
  //                                    L I N K S


  def getReferencingLinks(ref: Reference): Option[ReferencingLinks] = fileLock.synchronized {
  	val handler=ixHandler(ref.typ)
  	val rec=handler.getInstanceRecord(ref.instance)
  	    //System.out.println("recpos "+rec.propPos +" recsize:"+rec.propSize)
  	    if (rec.linkPos == 0 && rec.linkLength==0) None
  	    else
  	    {
  	      val propObj=linkFileHandler.readInstance(ref,rec.linkPos,rec.linkLength )
  	      // not chached yet
  	      Some(propObj)
  	    }
  }


  def writeReferencingLinks(data: ReferencingLinks): Unit = fileLock.synchronized {
  	val (pos,size)=linkFileHandler.writeInstance(data)
    ixHandler(data.ref.typ).writeLinksData(data.ref.instance, pos, size)
  }
  
  
  // *************************************************************************************
  //                                   Collecting-Functions

  def getCollectingFuncData(ref: Reference): Option[CollFuncResultSet] = fileLock.synchronized {
  	val rec=ixHandler(ref.typ).getInstanceRecord(ref.instance)
    if (rec.collPos == 0 && rec.collLength==0) None
    else  {
      val propObj=collFuncFileHandler.readInstance(ref,rec.collPos,rec.collLength)
      // not chached yet
      Some(propObj)
    }
  }


  def writeCollectingFuncData(data: CollFuncResultSet): Unit = fileLock.synchronized {
  	val (pos,size)=collFuncFileHandler.writeInstance(data)
  	ixHandler(data.ref.typ).writeCollFuncData(data.ref.instance, pos, size)
  }


  def shutDown(): Unit = fileLock.synchronized {
    if (!shuttedDown) {
      for (i <- _ixHandlerList.valuesIterator) i.shutDown()
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
  
  private def undoTransRec(rec:LogIndexSet,currLogPos:Int) = {
  	//var ret:(Int,Long,Int)=null
  	
  	val handler=ixHandler(rec.typ)
    rec.transTyp match {    	
    	case TransType.deleted|TransType.dataChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoDeleteList)
				handler.writeData(rec.inst,pos, len, trans==TransType.created.id,false)
				//handler.instCache.removeInstanceData(rec.inst)
			case TransType.created =>
				handler.deleteInstance(rec.inst,false)
				CommonSubscriptionHandler.instanceDeletedViaUndo(Reference(rec.typ,rec.inst))
				//handler.instCache.removeInstanceData(rec.inst)
			case TransType.linksChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoLinkList)
				if(trans==TransType.created.id) // no info since creation
          handler.writeLinksData(rec.inst,0,0,false)
        else handler.writeLinksData(rec.inst,pos,len,false)
			case TransType.propertyChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoPropList)
				if(trans==TransType.created.id) // no info since creation
        {util.Log.e("no prop data found for "+rec.typ+","+rec.inst )
          handler.writePropertiesData(rec.inst,0,0,false)}
        else handler.writePropertiesData(rec.inst,pos,len,false)
				//handler.propCache.removeInstanceData(rec.inst)
			case TransType.collFuncChanged =>
				val (trans,pos,len)=TransLogHandler.getLastLivingData(rec.typ,rec.inst,currLogPos-1,
          undoCollList)
				if(trans==TransType.created.id) // no info since creation
          handler.writeCollFuncData(rec.inst,0,0,false)
        else handler.writeCollFuncData(rec.inst,pos,len,false)
		}
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
  	val copyDataFileHandler=new BoolContFileHandler[InstanceData]("InstData.dat.cpy",InstanceData.read)
  	val copyPropFileHandler=new ContainerFileHandler("PropData.dat.cpy",InstanceProperties.read)
  	val copyLinkFileHandler=new ContainerFileHandler("ExternLinks.dat.cpy",ReferencingLinks.read)
  	val copyCollFuncFileHandler=new ContainerFileHandler("collFuncs.dat.cpy",CollFuncResultSet.read)
  	
  	class MyListener extends RecordListener{
  	    var replaceHandler:ClassIndexHandler=_
  	     def nextRecord(inst:Int,dataPos:Long,dataLen:Int,propPos:Long,propLen:Int,linkPos:Long,linkLen:Int,collPos:Long,collLen:Int): Unit ={
  	      val ndataPos=copyDataFileHandler.writeBuffer(dataFileHandler.readInBuffer(dataPos,dataLen), dataLen)
  	      val npropPos=if(propPos<1&&propLen<1) 0 else copyPropFileHandler.writeBuffer(propFileHandler.readInBuffer(propPos,propLen), propLen)
  	      val nlinkPos=if(linkPos<1&&linkLen<1) 0 else copyLinkFileHandler.writeBuffer(linkFileHandler.readInBuffer(linkPos,linkLen), linkLen)
  	      val ncollPos=if(collPos<1&&collLen<1) 0 else copyCollFuncFileHandler.writeBuffer(collFuncFileHandler.readInBuffer(collPos,collLen), collLen)
  	      replaceHandler.reorgWriteRecord(inst,ndataPos,dataLen,npropPos,Math.max(0,propLen),nlinkPos,Math.max(0,linkLen),ncollPos,Math.max(0,collLen))  
  	      TransLogHandler.dataChanged(TransType.created,replaceHandler.theClass.id,inst,ndataPos,dataLen)
  	      if(propLen>0)TransLogHandler.dataChanged(TransType.propertyChanged,replaceHandler.theClass.id,inst,npropPos,propLen)
  	      if(linkLen>0)TransLogHandler.dataChanged(TransType.linksChanged ,replaceHandler.theClass.id,inst,nlinkPos,linkLen)
  	      if(collLen>0)TransLogHandler.dataChanged(TransType.collFuncChanged ,replaceHandler.theClass.id,inst,ncollPos,collLen)
  	    }
  	  }  	
  	val recListener=new MyListener
	  var lix=1
  	for (typ<- serverClassList.keysIterator;handler = ixHandler(typ)){
  	  listener(lix,handler.theClass.name)
  	  recListener.replaceHandler=new ClassIndexHandler(handler.theClass,".reo")
  	  handler.foreachInstance(recListener)
  	  recListener.replaceHandler.shutDown()
  	  handler.takeOverFromReorgFile(recListener.replaceHandler.fileName)
  	  //System.out.print(handler.fileName+" ")
  	  lix+=1
  	}
  	copyDataFileHandler.shutDown()
  	copyPropFileHandler.shutDown()
  	copyLinkFileHandler.shutDown()
  	copyCollFuncFileHandler.shutDown()
  	
  	dataFileHandler.takeOverReorgFile(copyDataFileHandler.compFileName)
  	propFileHandler.takeOverReorgFile(copyPropFileHandler.compFileName)
  	linkFileHandler.takeOverReorgFile(copyLinkFileHandler.compFileName)
  	collFuncFileHandler.takeOverReorgFile(copyCollFuncFileHandler.compFileName)
		FSPaths.setLastStatTransID(1)
    util.Log.w("\nReorg complete translog:"+TransLogHandler.getInsertPos+" detail:"+TransDetailLogHandler.getInsertPos)
  	//System.out.println("\nReorg completely done")
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
	      val propData=propFileHandler.readInstance(ownerRef,propPos,propLen)     
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
		            writeInstance(data,false)
		          }  
		        }
		        else {	            
	            val suix=data.secondUseOwners.indexWhere(_.ownerRef ==ownerRef)
	            if(suix> -1) {
	              val suOwnerRecord=data.secondUseOwners(suix)
	               if(suOwnerRecord.ownerField !=pfIx) {
                   util.Log.e("wrong ownerField " +suOwnerRecord+" in SUOwnersRecord of Instance "+data.ref+" should be "+pfIx)
	                 val newOwners=for(i<-data.secondUseOwners.indices) yield if(i==suix)new OwnerReference(pfIx,ownerRef) else data.secondUseOwners(i) 
	                 writeInstance(data.changeSecondUseOwners(newOwners),false)
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
  	    val instObj=dataFileHandler.readWithBool(ref,dPos,dLen,false )
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
	  	    else if(newOwners.length<instObj.owners.length||newSUOwners.size<instObj.secondUseOwners.size){
	  	      var newInst= if(newOwners.length<instObj.owners.length) instObj.changeOwner(newOwners) else instObj
	  	      if(newSUOwners.size<instObj.secondUseOwners.size) newInst=newInst.changeSecondUseOwners(newSUOwners)
            util.Log.w("change "+newInst)
	  	      writeInstance(newInst,false)
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
  	    val instObj=dataFileHandler.readWithBool(ref,dPos,dLen,false )
  	    if(instObj.owners.nonEmpty || instObj.secondUseOwners.nonEmpty){
	  	    instObj.owners.foreach(aOwner=> {
	  	      if(!instanceExists(aOwner.ownerRef.typ,aOwner.ownerRef.instance)) {util.Log.e("owner "+aOwner+" of "+ref+" does not exist");false}
	  	      else{
		  	      val isIn= getInstanceProperties(aOwner.ownerRef ) match {
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
		  	      val isIn=getInstanceProperties(aOwner.ownerRef ) match {
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
	    writeInstance(data,false)
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
	    		  handler.writePropertiesData(owner.instance ,0,0,false)}
			else handler.writePropertiesData(owner.instance ,pos,len,false)
			//handler.propCache.removeInstanceData(owner.instance )
	  }
    for( prop<-getInstanceProperties(owner); childRef <-prop.propertyFields (propertyField).propertyList)
      if(!instanceExists(childRef.typ,childRef.instance )) {
	      print("restore :"+childRef)
	      try {
		      val (ctrans,cpos,clen)=TransLogHandler.getLastLivingData(childRef.typ,childRef.instance ,TransLogHandler.getInsertPos-1,
		    			undoDeleteList,true)    	
		    	val chandler=ixHandler(childRef.typ)
		  		chandler.writeData(childRef.instance ,cpos, clen, ctrans==TransType.created.id,false)	
		  		//chandler.instCache.removeInstanceData(childRef.instance )
          util.Log.w(" done.")
	      } catch {case NonFatal(e) => util.Log.e(" error:"+e.getMessage(),e)}
      } else util.Log.e("child :"+childRef+" still exists")
	}
  
}