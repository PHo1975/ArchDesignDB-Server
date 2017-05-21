/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import definition.data._
import definition.expression._
import definition.typ.{AllClasses, DataType}
import server.comm.{CommonSubscriptionHandler, ConnectionEntry, UserList}
import server.storage._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal



/** manages Transaction handling
 * 
 */
object TransactionManager {
	@volatile protected var running = false
  
  val numUndoSteps=10
  var currentUser:Short=0
  var currentActionCode:Short=0
	var currentRef: Reference = _
  var multiInst:Boolean=false
  var logCreateType:Int=0


	val transLock: AnyRef = new Object()

	val delayedActionsList: ArrayBuffer[() => Unit] = collection.mutable.ArrayBuffer[() => Unit]()

	@volatile var undoUserEntry: ConnectionEntry = _

	def isRunning: Boolean = running
	
	// Starts a new Transaction
	private def startTransaction():Unit =	{		
	  if(running ) throw new IllegalArgumentException("An Transaction is still running ")
		running=true
	  //TransLogHandler.incrementTransID();
	  //System.out.println("Start Trans " +TransLogHandler.transID)
	}
	
	// Finishes the Transaction and commits all changes to the database
	private def finishTransaction():Unit = 	{		
		if(!running) throw new IllegalArgumentException("Finish: No transaction running ")
		if(ActionList.isEmpty) {
			//TransLogHandler.resetTransID()
		}else {
      TransLogHandler.increaseTransID()
		  ActionList.commitAllData()
		  TransLogHandler.flush()
		  TransDetailLogHandler.log(TransLogHandler.getTransID,currentUser,currentRef,multiInst,currentActionCode,logCreateType)
		}		
		//System.out.println("Finish Trans "+ TransLogHandler.transID)
		running=false
		currentUser = -1

	}

	def canModify: Boolean = running && undoUserEntry == null
	
	
	// breaks an transaction if an error occurs during the transaction
	 def breakTransaction():Unit =	{
		 ActionList.breakAllData()		
		//TransLogHandler.resetTransID()
		running=false
	 }
	 
	 def requestUndoData(connection:ConnectionEntry):Unit = {
		 if(undoUserEntry!=null||running){ // already undoing
			 connection.thread.denyUndoRequest()
		 } else {
			 undoUserEntry=connection
			 UserList.lockUsersForUndo(connection)
			 val currTransID=TransLogHandler.getTransID	
			 val transStepData=TransDetailLogHandler.readTransStepData(currTransID,currTransID-numUndoSteps)
			 //System.out.println("Sending transStep:"+transStepData.size)
			 connection.thread.sendUndoInformation( transStepData)			 
		 }
	 }
	 
	 def stopUndo(connection:ConnectionEntry):Unit = {
		 if(undoUserEntry==null) util.Log.e(" Undo process not running !")
		 else if (connection!=undoUserEntry) util.Log.e("UNDO user "+undoUserEntry.thread.userEntry.name+
		     " but stopped by "+connection.thread.userEntry.name)
		 else {		   
			 UserList.releaseUsersForUndo(connection)
			 undoUserEntry=null
		 }
	 }
		
	
	
	/** creates a new Instance
	 * @param typ the class id of the new instance
	 * @param notifyRefandColl should referencing links and collFuncs be notified about the new instance
	 * @param notifySubs notify subscribers of the parent inst that the new instance was added
	 */
	def tryCreateInstance(typ:Int,owners:Array[OwnerReference],notifyRefandColl:Boolean,pos:Int= -1,
	                      notifySubs:Boolean=true,withStartValues:Boolean=true):InstanceData =	{	
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		//TODO: Check if the child type is allowed in the owner property fields

		//println("Create typ"+typ+" owners:"+owners.mkString(",")+" "+notifyRefandColl+" pos:"+pos+" notifySubs:"+notifySubs+" withStart:"+withStartValues)
		var newInst=StorageManager.createInstance(typ,owners,withStartValues)	
		var collData:Option[CollFuncResultSet]=None
		var newLinkDataForThis:ReferencingLinks=null
		val copyInfo:Option[CopyMoveInfo]=if(!notifySubs) AddDontNotifyOwners.some  else None
		if(withStartValues) {
			var fieldsChanged=false		
			for(i<-newInst.fieldData.indices;expr=newInst.fieldData(i);if !expr.isNullConstant){
				
				// check new collcalls
				val newCollCalls=expr.getElementList[CollectingFuncCall](DataType.CollFunctionCall,Nil)
				if(newCollCalls.nonEmpty) {
          val collD=collData getOrElse new CollFuncResultSet(newInst.ref, Nil)
					val (acollData,anExpression)=TransactionManager.addCollCalls(collD,
					    newInst.ref, newCollCalls, i.toByte,expr)
					newInst=newInst.setField(i.toByte,anExpression)
					collData=Some(acollData)	
				}
				// check new links
				val newRefList:Seq[FieldReference]=expr.getElementList[FieldReference](DataType.FieldRefTyp,Nil)
				for (nr <- newRefList; if nr.remInst.isEmpty) {
					nr.cachedValue =newInst.fieldValue(nr.remField)	//side effect !!! should be changed !
					fieldsChanged=true									
					if(newLinkDataForThis==null)newLinkDataForThis=		new ReferencingLinks(newInst.ref,Map())
					newLinkDataForThis=newLinkDataForThis.addTargetLink(ExtFieldRef(newInst.ref.typ,newInst.ref.instance ,i.toByte,false),nr.remField)
				}
				// check new parent links
				val newPRefList:Seq[ParentFieldRef]=expr.getElementList[ParentFieldRef](DataType.ParentRefTyp,Nil)
				for(nr<-newPRefList){
					val parentInstRef=owners(nr.ownerIx ).ownerRef 
					val parentValue=ActionList.getInstanceData(parentInstRef).fieldValue(nr.fieldNr)
					nr.setValue(parentValue)
					fieldsChanged=true
					checkCircularRef(newInst.ref,i.toByte,List(new FieldReference(Some(parentInstRef.typ),Some(parentInstRef.instance), nr.fieldNr)),false)
					addRef(newInst.ref,i.toByte,parentInstRef,nr.fieldNr,true)					
				}	
			}	// for
			if(fieldsChanged) newInst=new InstanceData(newInst.ref,newInst.fieldData,newInst.owners) // update caches
		}
		ActionList.addTransactionData(newInst.ref, CreateAction(newInst.ref, Some(newInst), None,
			Option(newLinkDataForThis), collData, copyInfo))
		// notify owners
		for(owner <-owners)
			internAddPropertyToOwner(newInst,owner,pos,!notifySubs)
				
		if(notifyRefandColl)
			for(i <- newInst.fieldData.indices;if !newInst.fieldData(i).isNullConstant)
				passOnChangedValue(newInst,i.toByte,EMPTY_EX,newInst.fieldData(i).getValue,false)
		//SimpleProfiler.measure("notif owner")
		if(withStartValues){ // create AutoCreate children
			AllClasses.get.getClassByID(typ) match {
				case theClass:ServerObjectClass => for (ac <-theClass.ownAutoCreateInfos ) {
					val childInst = tryCreateInstance(ac.childType, Array(new OwnerReference(ac.propField, newInst.ref)), true, -1, false)
					for(sv<-ac.startValues ) // write start Values in child instance
						tryWriteInstanceField(childInst.ref,sv._1,sv._2)
				}
					case _=>
			}

				
		}
		notifyModuleChildAdded(owners,newInst)
		newInst
	}
	
	
	private def notifyModuleChildAdded(owners:Array[OwnerReference],child:InstanceData):Unit= {
	  for(owner<-owners) {
	    val ownerClass=AllClasses.get.getClassByID(owner.ownerRef.typ).asInstanceOf[ServerObjectClass]
	    if(ownerClass.actionModule.notifyChildAdded) {
	      val ownerInst=ActionList.getInstanceData(owner.ownerRef)
	      ownerClass.actionModule.onChildAdded(ownerInst,owner.ownerField,child)
	    }
	  }
	}
	
		
	// internal routine
	private def internAddPropertyToOwner(newInst:InstanceData,owner:OwnerReference,pos:Int,refreshDestination:Boolean=false) = {
		val newProp= (ActionList.getInstanceProperties(owner.ownerRef) match {
				// Property data found				
				case Some(a) => a 
				// so far no Property data there, create an empty one
				case _ => StorageManager.createInstanceProperties(owner.ownerRef)
			} // and add the new child to the prop list
			).addChildInstance(owner.ownerField ,newInst.ref,pos) 
			
		  ActionList.addTransactionData(owner.ownerRef,DataChangeAction(None,Some(newProp),None,None, if(refreshDestination)RefreshDestinationOwner.some else None))		  
	}
	
	private def movePropertyToPos(ref:Reference,owner:OwnerReference,pos:Int) = {
		val newProp= (ActionList.getInstanceProperties(owner.ownerRef) match {
  				// Property data found				
  				case Some(a) => a 
  				// so far no Property data there, something is wrong here
  				case _ => throw new IllegalArgumentException("move pos "+ref+"  but owner "+owner.ownerRef+" has no Property data ")
  			} // and add the new child to the prop list
  			).moveChildInstanceToPos(owner.ownerField ,ref,pos)
  	ActionList.addTransactionData(owner.ownerRef,DataChangeAction(None,Some(newProp),None,None,
  		 Some(RefreshDestinationOwner) ))
	}

	def sortProperty(owner: OwnerReference, fieldNr: Int): Unit = {
		val newProp= (ActionList.getInstanceProperties(owner.ownerRef) match {  								
  				case Some(a) => a 				
  				case _ => throw new IllegalArgumentException("sorting "+owner+", has no Property data ")
  			} 
  			).sortChildrenByField(owner.ownerField ,fieldNr,ActionList)
  	ActionList.addTransactionData(owner.ownerRef,DataChangeAction(None,Some(newProp),None,None,
  		 Some(RefreshDestinationOwner) ))
	}


	def tryWriteInstanceData(data: InstanceData): Unit = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		ActionList.addTransactionData(data.ref, DataChangeAction(Some(data)))
	}
	
	/**
	 *  @param ref Reference of the instance to be changed
	 *  @param fieldNr number of the data field to be changed
	 *  @param newValue the new Value
	 */
	def tryWriteInstanceField(ref:Reference,fieldNr:Byte,newExpression:Expression):Boolean = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		//println("writeField "+ref+" fieldNR:"+fieldNr+" =" +newExpression.getTerm)
		//SimpleProfiler.startMeasure("Change inst")
		var theExpression=newExpression
		val instD=ActionList.getInstanceData(ref)
		if(instD.fieldData(fieldNr)==null)util.Log.e("instD.fieldData("+fieldNr+") = null "+ref)
		// check for FieldReferences		
		//********************************
		val oldRefList=instD.fieldData(fieldNr).getElementList[FieldReference](DataType.FieldRefTyp,Nil)
		val newRefList:Seq[FieldReference]=newExpression.getElementList[FieldReference](DataType.FieldRefTyp,Nil)	
		// set the cache value of the FieldReference expressions in the new Expression
		for(nr<-newRefList){
			val sourceInstRef = resolveLinkRef(ref,nr)
		  val sourceValue=ActionList.getInstanceData(sourceInstRef).fieldValue(nr.remField )
		  nr.cachedValue =sourceValue	//side effect !!! should be changed !
		}				
		// remove all ReferencingLinks data off the removed refs
		val removedRefs=findMissingElements[FieldReference](oldRefList,newRefList)		
		for( r <- removedRefs)
			removeLinkRef(ref,r)
			
	  // add new ReferencinLinks data for added refs
		val addedRefs=findMissingElements[FieldReference](newRefList,oldRefList)		
		for( r <- addedRefs){
			checkCircularRef(ref,fieldNr,List(r qualifyWith ref))			
			addLinkRef(ref,fieldNr,r)
		}
		
		// Check for ParentReferences
		// *********************************
		val oldPRefList=instD.fieldData(fieldNr).getElementList[ParentFieldRef](DataType.ParentRefTyp,Nil)
		val newPRefList=newExpression.getElementList[ParentFieldRef](DataType.ParentRefTyp,Nil)
		for(nr<-newPRefList)
			nr.cachedValue=ActionList.getInstanceData(instD.owners(nr.ownerIx).ownerRef).fieldValue(nr.fieldNr)
		
		val removedPRefs=findMissingElements[ParentFieldRef](oldPRefList,newPRefList)
		for(r <-removedPRefs) 
			removeAnyRef(ref,instD.owners(r.ownerIx).ownerRef,r.fieldNr )
		
		val addedPRefs=findMissingElements[ParentFieldRef](newPRefList,oldPRefList)
		for( r <-addedPRefs) {
			val parent=instD.owners(r.ownerIx).ownerRef
			checkCircularRef(ref,fieldNr,List(new FieldReference(Some(parent.typ),Some(parent.instance), r.fieldNr)))
			addRef(ref,fieldNr,parent,r.fieldNr,true)
		}
			
		
		// Check for CollFunctionCalls 
		//**********************************
		val oldCollCalls= instD.fieldData(fieldNr).getElementList[CollectingFuncCall](DataType.CollFunctionCall,Nil)
		//if( !oldCollCalls.isEmpty) System.out.println("oldCollCalls "+oldCollCalls)
		val newCollCalls=newExpression.getElementList[CollectingFuncCall](DataType.CollFunctionCall,Nil)
		//if( !newCollCalls.isEmpty) System.out.println("newCollCalls "+newCollCalls)
		
		
		if(oldCollCalls.nonEmpty || newCollCalls.nonEmpty) {
			var collData=  ActionList.getCollData(ref) // load current data
			var collDataChanged=false
			// Remove all CollResults of the removed CollCalls
			val removedCalls=findMissingElements(oldCollCalls,newCollCalls)
			if(removedCalls.nonEmpty){ if(collData.isDefined) {
			  	collData=Some(removeCollCalls(collData.get,ref,removedCalls,fieldNr))
			  	collDataChanged=true
				}

			else util.Log.e("Cant find CollData when removing unused Calls "+removedCalls+" ref:"+ref)  }
      //println("Removed Calls:"+removedCalls.mkString(","))
			// add new CollResults		  
			val newCalls=findMissingElements(newCollCalls,oldCollCalls)
			if(newCalls.nonEmpty) {
			  val (acollData,anExpression)=addCollCalls(collData match {
			    case Some(cd)=>cd
			    case None => new CollFuncResultSet(ref,Nil)}, ref, newCalls, fieldNr,newExpression)
			  theExpression=anExpression
			  collData=Some(acollData)
			  collDataChanged=true
			}
			//println("New Calls:"+newCalls.mkString(","))
			// For unchanged calls, copy cached values to calls in theExpression
			val stillCalls=findMissingElements(newCollCalls,newCalls)
			//println("still Calls:"+stillCalls.mkString(","))
			if(stillCalls.nonEmpty) {
				collData match {
					case Some(cd)=> for(st<-stillCalls) {
						val oldCacheValue=cd.getCacheValueForCall(st, fieldNr)
						//println("newExpression:"+theExpression)
						theExpression=setFuncResults(theExpression, st, oldCacheValue)
						//println("OldCacheValue:"+oldCacheValue+" expr:"+theExpression)
					}
					case None => util.Log.e("Cant find Colldate when updating cached values"+stillCalls+" ref:"+ref)
				}		  
			}
			if (collDataChanged) ActionList.addTransactionData(ref, DataChangeAction(None, None, None, collData))
		}    
		 
		  
		//safe the old field value  
		val oldValue=instD.fieldData(fieldNr).getValue   				  
		// set field in instance to new expression
		val newInst=instD.setField(fieldNr,theExpression)
		// store data
		ActionList.addTransactionData(ref, DataChangeAction(Some(newInst)))
		
		// pass on the changed value to referencing instances
		val newValue=theExpression.getValue
		if(newValue!=oldValue) passOnChangedValue(newInst,fieldNr,oldValue,newValue)
		else util.Log.w("ref:"+ref+" field:"+fieldNr+" entered value "+theExpression+" ->"+newValue+" same as "+oldValue)
		//SimpleProfiler.measure("passOn")
		true
	}
	
	
	
	private def checkCircularRef(targetRef:Reference,targetField:Byte,dependingRefs:List[FieldReference],loopParents:Boolean=true):Unit={
		//System.out.println("checking target:"+targetRef+" field:"+targetField+" list:"+dependingRefs.mkString(","))
		//val qualifiedSourceRef=link qualifyWith targetRef
		val targetFieldReference=new FieldReference(Some(targetRef.typ),Some(targetRef.instance),targetField)
		if(dependingRefs.contains(targetFieldReference)) 
			throw new IllegalArgumentException("Circular Reference from "+targetRef+" field:"+targetField+
				" including:\n"+dependingRefs.mkString(","))
		val newDepList=targetFieldReference::dependingRefs
		// check external refs that point to target field		
		for(mrl <- ActionList.getReferencingLinks(targetRef);if mrl.links.contains(targetField);extList=mrl.links(targetField))
			for(extLinkRef<-extList)
				checkCircularRef(Reference(extLinkRef.typ,extLinkRef.inst),extLinkRef.field, newDepList)
		// check collFuncs of parents	 
    if(loopParents) {
      var instData:InstanceData=null			
      try {
      	instData=ActionList.getInstanceData(targetRef)
      } catch {case e:Exception => System.err.println(e); return}
      for(owner <-instData.owners) 
      	checkCircular(owner)
      for(owner <-instData.secondUseOwners) 
      		checkCircular(owner)	
    }
		
		
		def checkCircular(owner:OwnerReference):Unit= {
			val collData=ActionList.getCollData(owner.ownerRef )
			//System.out.println("owner:"+owner+" colldata:"+collData)
			for(cData<-collData)
				for(cFResult <-cData.callResultList
						if cFResult.parentPropField == owner.ownerField &&
							AllClasses.get.getClassByID(targetRef.typ).inheritsFrom(cFResult.childType) &&
							cFResult.childField == targetField)
					checkCircularRef(owner.ownerRef,cFResult.parentField,newDepList)
		}
	}
	
		
	/** notifies all target instances of the source instante that the data field has changed
	 *  @param sourceRef instance that has changed
	 *  @param fieldNr field nr that has changed
	 *  @param newValue the new value of the field 
	 */
	private def passOnChangedValue(newInst:InstanceData,fieldNr:Byte,oldValue:Constant,newValue:Constant,withRefLinks:Boolean=true):Unit ={
		//println("passOnChangedValue newInst:"+newInst+ " fieldNr:"+fieldNr+" oldValue:"+oldValue+" newValue:"+newValue)
		val wishType=newInst.theClass.fields(fieldNr).typ
		val nValue=if(newValue.getType!=wishType)
			Constant.createConversion(newValue,wishType) else newValue // CHANGED
			
		// Check for other instances referencing to this instance
		if(withRefLinks)	
		ActionList.getReferencingLinks(newInst.ref) match {			
			case Some(refLinks) =>
				if (refLinks.links.contains(fieldNr))
				for (r <- refLinks.links(fieldNr)) // get all reflinks for the changed field
				   notifyDataChangeToReferencingInst(r,newInst.ref,fieldNr,nValue) // notify them
			case _ => // go drink a beer
		}
		// Check for parent instances having collectingFunctions on this instance
		for(owner <-newInst.owners ) {
			ActionList.getCollData(owner.ownerRef) match {
				case Some(collData) =>
					notifyCollFunc_ChildChanged(owner ,collData,newInst.ref,fieldNr,oldValue,nValue)
				case _ => // another beer
			}			  
		}
		for(owner <-newInst.secondUseOwners ) {
			ActionList.getCollData(owner.ownerRef) match {
				case Some(collData) =>
					notifyCollFunc_ChildChanged(owner ,collData,newInst.ref,fieldNr,oldValue,nValue)
				case _ => // another beer
			}
		}
		
		// notify module
		val module=AllClasses.get.getClassByID(newInst.ref.typ).asInstanceOf[ServerObjectClass].actionModule
			if(module.notifyFieldChanged){			  
			  module.onFieldChanged(newInst,fieldNr,nValue)
			}
		
		// notify Subscriptions
		//CommonSubscriptionHandler.instanceChanged(newInst)
	}	
	
	private def passOnNewInstanceToCollFuncParents(newInst:InstanceData,ownerList:Seq[OwnerReference]):Unit =	{
		for(owner <-ownerList ) {
			ActionList.getCollData(owner.ownerRef) match {
				case Some(collData) =>
					for(i <- newInst.fieldData.indices;if !newInst.fieldData(i).isNullConstant)
					notifyCollFunc_ChildChanged(owner ,collData,newInst.ref,i.toByte,EMPTY_EX,newInst.fieldData(i).getValue)
				case _ => // another beer
			}
		}
	}
	
	
	private def passOnDeletedInstanceToCollFuncParents(instD:InstanceData,ownerList:Seq[OwnerReference]):Unit = {
		for(owner <-ownerList) 	{
  		//System.out.println(" "+owner.ownerRef)
		  if(StorageManager.instanceExists(owner.ownerRef.typ,owner.ownerRef.instance))
  		ActionList.getCollData(owner.ownerRef) match {
				case Some(collData) =>
					//System.out.println("notify CollFunc Child Deleted, owner:"+owner+" child"+instD.ref)
					notifyCollFunc_ChildDeleted(owner ,collData,instD)
				case b => //System.out.println("check "+owner+" for Colldata:"+b) // more beer !
			}
  	}
	}
	
	// **************************** FieldReference Management **************************************************************
	
	
	/** notifies ONE target instance that the source instance field was modified and stores it 
	 *  @param targetFieldRef instance and field of the target instace that should be notified
	 *  @param sourceRef the instance that has changed
	 *  @param sourceField the field that has changed
	 *  @param newValue the new Value of that field
	 */
	private def notifyDataChangeToReferencingInst(targetFieldRef:ExtFieldRef,sourceRef:Reference,sourceField:Byte,newValue:Constant):Unit = 	{
		//println("notifyDataChangeToReferencingInst targetRef"+targetFieldRef+" sourceRef:"+sourceRef+" sourceField:"+sourceField+" newValue:"+newValue)
		val targetRef=targetFieldRef.getReference
		val targetData=ActionList.getInstanceData(targetRef)
		val theField=targetData.fieldData(targetFieldRef.field )
		val oldRemoteValue=try {
      theField.getValue // result value before changing the cache value
    } catch {case NonFatal(e)=> util.Log.e("target:"+targetFieldRef+" source:"+sourceRef+
      " soucreField:"+sourceField+" theField:"+theField.getTerm,e);EMPTY_EX}
		
		if(targetFieldRef.isParentRef ) { // parentReference
			val refList=theField.getElementList[ParentFieldRef](DataType.ParentRefTyp,Nil)
			for (fRef <-refList){
				val theParentRef=targetData.owners(fRef.ownerIx ).ownerRef
				if(theParentRef==sourceRef && fRef.fieldNr==sourceField)
					fRef.setValue(newValue)
			}
		}
		else { // fieldReference
			val refList=theField.getElementList[FieldReference](DataType.FieldRefTyp,Nil) // all field references in the term
			// find the reference(s) that point the the source instance
			for(fRef <-refList; // check all references in the term
			if resolveLinkRef(targetRef, fRef) == sourceRef && // if the reference points to the source instance
				fRef.remField == sourceField) { // and to the source field
				fRef.setCachedValue(newValue)			
			}
		}
		//targetData.regenFieldCache(targetFieldRef.field)
		// now calculate the new value of this field
		//resetFuncExpressions(theField)
		val newRemoteValue=theField.getValue
		val newTargetData=targetData.setField(targetFieldRef.field,theField)
		ActionList.addTransactionData(targetRef, DataChangeAction(Some(newTargetData)))
		if(newRemoteValue!=oldRemoteValue){ // if the value has changed after the change of the referenced field
			//pass on the changed value of the target instance to it's targets
			passOnChangedValue(newTargetData,targetFieldRef.field,oldRemoteValue,newRemoteValue)			
		}
		// save the target instance with the new cached value (could be optimized when cachevalue does not change)
		
	}
	
		
	/**  resolves the real source ref, when the type or instance field is not set in a FieldReference
	 * @param targetRef the "this" instance what contains the field with the FieldReference
	 * @param sourceRef the FielReference pointing to the source. When this reference dont have type or instance
	 * information, take the targetRef information
	 */
	private def resolveLinkRef(targetRef:Reference,sourceRef:FieldReference):Reference ={
	  if(sourceRef.remType.isEmpty ) 		{
			if(sourceRef.remInst.isEmpty) targetRef // reference to a fild in THIS instance, so no more action here
			else new Reference(targetRef.typ,sourceRef.remInst.get)
		}
		else new Reference(sourceRef.remType.get,sourceRef.remInst.get)
	}
	
	/** removes the external target link information from the source instance
	 *  @param targetRef reference to the target object pointing to the source
	 *  @param sourceRef the source instance where the target info is to be deleted 
	 */
	private def removeLinkRef(targetRef:Reference,sourceRef:FieldReference):Unit ={				
		val sourceInst=resolveLinkRef(targetRef,sourceRef)
		removeAnyRef(targetRef,sourceInst,sourceRef.remField )		
	}
	
	
	private def removeAnyRef(targetRef:Reference,sourceInst:Reference,sourceField:Byte):Unit = {
		val newLinkData= ( ActionList.getReferencingLinks(sourceInst) match {
			case Some(linkData) => linkData
			case _ => throw new IllegalArgumentException ("Error: Cant find Referencing links for "+sourceInst + 
				" when trying to remove FieldReference in "+targetRef)
		} ).removeTargetLink(targetRef,sourceField)
		ActionList.addTransactionData(sourceInst,DataChangeAction(None,None,Some(newLinkData)))	
	}
	
	/** adds new external link information to the source instance
	 * @param targetRef the new target instance pointing to the source instance
	 * @param targetField in what field in the target instance is the new FieldReference
	 * @param sourceRef the FieldReference pointing to the source instance 
	 */
	private def addLinkRef(targetRef:Reference,targetField:Byte,sourceRef:FieldReference):Unit = {		
		val qualifiedSourceRef = resolveLinkRef(targetRef,sourceRef) // resolve missing type and instance information
		//System.out.println("addLinkRef targetRef:"+targetRef+" targetField:"+targetField+" sourceRef:"+
		//	sourceRef+ " qualSourceRef:"+qualifiedSourceRef)
		addRef(targetRef,targetField,qualifiedSourceRef,sourceRef.remField ,false)	
	}
	
	private def addRef(targetRef:Reference,targetField:Byte,sourceRef:Reference,sourceField:Byte,isParentRef:Boolean):Unit = {
		val newLinkData= ( ActionList.getReferencingLinks(sourceRef) match {
			case Some(linkData) => /*System.out.println("Linkdata already there:"+linkData);*/ linkData
			case _ => new ReferencingLinks(sourceRef,Map())				
		} ).addTargetLink(ExtFieldRef(targetRef.typ,targetRef.instance ,targetField,isParentRef),sourceField)
		ActionList.addTransactionData(sourceRef,DataChangeAction(None,None,Some(newLinkData)))
	}
	
	
	//**************************************  CollectingFunction management **************************************
	
	/** removes  function call results from the result set
	 * @param collData the current CollFuncResult Set for that instance
	 * @param ref Reference of the parent instace
	 * @param oldCollCalls the list of calls to be removed
	 * @param pFieldNr the field where the calls were stored
	 * 
	 */
	private def removeCollCalls(cData:CollFuncResultSet,ref:Reference,oldCollCalls:Seq[CollectingFuncCall],pFieldNr:Byte):CollFuncResultSet ={
	  var collData=cData		
		for(r <- oldCollCalls)
		   collData=collData.removeCollCall(r,pFieldNr)
		collData		
	}
	
	/** adds new collFuncCalls to the collFuncResultSet
	 *  @param collData current version of the ResultSet
	 *  @param ref Reference of the parent instance
	 *  @param newCollCalls the calls inside of a parentfield that are to be added
	 *  @param pfieldNr in what field were the calls added
	 *  @return tuple: (the new result set, the updated expression)
	 */
	def addCollCalls(collData:CollFuncResultSet,ref:Reference,newCollCalls:Seq[CollectingFuncCall],
	                         pFieldNr:Byte, newTerm:Expression):(CollFuncResultSet,Expression) ={				
		// get the collData
	  //rintln("AddCollCall "+collData+" ref:"+ref+" newCalls:"+newCollCalls.mkString(", "))
		var theTerm=newTerm
		var theCollData= collData
		// for each new call  
		for(nc <- newCollCalls) { // add it to the resultset
		   val (acollData,resultValue)=theCollData.addCollCall(nc,pFieldNr)
		   theCollData=acollData 
		   theTerm=setFuncResults(theTerm,nc,resultValue) // and put the result into the call expression
		}  
		(theCollData,theTerm)		
	}
	
	/** puts the results of a collfunc into the call expression 
	 * @param newTerm the term containing the funcCall that should be replaced with a call that has the result set
	 * @param call the new function call we are looking for
	 * @param newValue the new value that should be given to the call
	 * @return the updated expression
	 */
	private def setFuncResults(newTerm:Expression,call:CollectingFuncCall,newValue:Constant ):Expression=
		newTerm.replaceExpression {
			case fc: CollectingFuncCall => if (fc == call) fc.setValue(newValue) else fc
			case a => a
		}

	
	/** notifies the owner that the value of a CollFuncCall has changed
	 * @param ownerInst the InstanceData of the owner
	 * @param collData the collFuncData of the functionCall that should be changed
	 * @param newValue the new result of the functionCall that should be set
	 * @return a new version of the owner's InstanceData
	 */
	private def updateOwnerCollFunc(ownerInst:InstanceData,collData:CollFuncResult,newValue:Constant):InstanceData = {
		//System.out.println("updateOwnerCollFunc "+ownerInst.fieldData(collData.parentField ))
		val newExpression=ownerInst.fieldData(collData.parentField).replaceExpression {
			case fc: CollectingFuncCall =>
				//System.out.println("UpdateOwnerCollFunc fc:"+fc+ " colldata:"+collData+" newValue:"+newValue)
				if (collData.fitsToFuncCall(fc, collData.parentField)) fc.setValue(newValue)
				else fc
			case a => a
		}
		 //System.out.println(" newExpr:"+newExpression)  
		 ownerInst.setField(collData.parentField,newExpression)
	}	
	
	
	/** notifies one parent instance that a child was changed, eventually calculates the new values and stores the changes
	 * @param owner Reference to the owner instance
	 * @param collData the CollFuncResultSet of the owner instance
	 * @param childRef the Reference of the changed child
	 * @param childField the number of the field that was changed
	 * @param oldValue the old value of the field
	 * @param newValue the new value of the field
	 * 
	 */
	private def notifyCollFunc_ChildChanged(owner:OwnerReference,collData:CollFuncResultSet,childRef:Reference,childField:Byte,
	                                       oldValue:Constant,newValue:Constant):Unit= {
		val myClass=AllClasses.get.getClassByID(childRef.typ)
		
		var fieldMatchSet:Set[Int]=Set.empty
		// check if the child matches to any of the collFuncResults 
		
		for(res <-collData.callResultList ){
			//System.out.println("myclass "+ myClass+ " res.childType:"+res.childType+ " " +myClass.inheritsFrom(res.childType))
			if(res.childField ==childField && res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
		  		 myClass.inheritsFrom(res.childType)) fieldMatchSet=fieldMatchSet+ res.parentField
		}		   
		
		if(fieldMatchSet.nonEmpty) {  // if yes, update the changes
			//System.out.println("fieldMatchSet: "+fieldMatchSet)
			var parentInstData=ActionList.getInstanceData(owner.ownerRef)
			val oldParentValues:Array[Constant]= Array.ofDim[Constant](parentInstData.fieldData .size)
				for(i <-fieldMatchSet) oldParentValues(i)= parentInstData.fieldValue(i)
			val newCollDataList= for (res <- collData.callResultList)	yield
			if (res.childField == childField && res.parentPropField == owner.ownerField && // if we have a matching collresult
					myClass.inheritsFrom(res.childType)) {
					val (newRes, value) = collData.childChanged(res, childRef, oldValue, newValue)
					parentInstData = updateOwnerCollFunc(parentInstData, newRes, value)
					newRes
				}
				else res
		  val newResultSet=new CollFuncResultSet(owner.ownerRef,newCollDataList)
			ActionList.addTransactionData(owner.ownerRef, DataChangeAction(Some(parentInstData), None, None, Some(newResultSet)))
		  // pass on to owners
		  for(fieldNr <-fieldMatchSet;if parentInstData.fieldData(fieldNr).getValue != oldParentValues(fieldNr))
		  	passOnChangedValue(parentInstData,fieldNr.toByte,oldParentValues(fieldNr),parentInstData.fieldData(fieldNr).getValue)
		}		
	}
	
	
	private def notifyCollFunc_ChildDeleted(owner:OwnerReference,collData:CollFuncResultSet,childInstance:InstanceData):Unit = {
		val myClass=AllClasses.get.getClassByID(childInstance.ref.typ)
		var fieldMatchSet:Set[Int]=Set()
		for(res <-collData.callResultList )
		{
			//System.out.println("res :"+res)
			if( res.parentPropField ==owner.ownerField && // if we have a matching collresult		  		
		  		 myClass.inheritsFrom(res.childType)) fieldMatchSet=fieldMatchSet+ res.parentField
		}
		if(fieldMatchSet.nonEmpty) {
			//System.out.println("matches")
			var parentInstData=ActionList.getInstanceData(owner.ownerRef)
			if (parentInstData!=null)
			{
				val oldParentValues:Array[Constant]= Array.ofDim[Constant](parentInstData.fieldData .size)
				for(i <-fieldMatchSet) oldParentValues(i)= parentInstData.fieldValue(i)
				val newCollDataList= for (res <- collData.callResultList)
						yield if (res.parentPropField == owner.ownerField && // if we have a matching collresult
							myClass.inheritsFrom(res.childType)) {
							val (newRes, value) = collData.childDeleted(res, childInstance.ref, childInstance.fieldValue(res.childField))
							//System.out.println("ChildDeleted newRes:"+newRes+ " new Value:"+value)
							parentInstData = updateOwnerCollFunc(parentInstData, newRes, value)
							newRes
						}
						else res
						val newResultSet=new CollFuncResultSet(owner.ownerRef,newCollDataList)
				ActionList.addTransactionData(owner.ownerRef, DataChangeAction(Some(parentInstData), None, None, Some(newResultSet)))
				// pass on to owners
				for(fieldNr <-fieldMatchSet;if parentInstData.fieldData(fieldNr).getValue != oldParentValues(fieldNr))
					passOnChangedValue(parentInstData,fieldNr.toByte,oldParentValues(fieldNr),parentInstData.fieldData(fieldNr).getValue)
			}
		}
	}
	
	
	
	// Checks the  lists of the new value and the old value, what elements were removed in the new one	
	private def findMissingElements[T <: Expression](oldList:Seq[T],newList:Seq[T]) =	
		oldList.filter(!newList.contains(_))
		
	
	/** Delete the instance
	 * if fromOwner is a regular owner: if there are SU Owners, the first SUOwner takes over the place of the from owner
	 * if there are no SUOwners, the instance is deleted completely
	 * if fromOwner is a SU-Owner, the instance is only deleted from that owner
	 * 
	 * @param ref the instance to be deleted
	 * @param fromOwner the ownerReference this instance should be deleted from, if None, delete the instance completely
	 * @param dontNotifyOwner When notifying the parents of that instance, ignore the given parent
	 */
	def tryDeleteInstance(ref:Reference,fromOwner:Option[OwnerReference],dontNotifyOwner:Option[Reference]):Boolean =	{
		//System.out.println("delete instance "+ref+" from Owner:"+fromOwner)
  	if(!canModify ) throw new IllegalArgumentException("try delete No transaction defined ")
  	val instD=ActionList.getInstanceData(ref)
  	if (instD==null) {
      util.Log.e("Delete Instance, cant find: "+ref)
  		return false
  	}
		
  	// mark this instance as deleted  	
  	if(fromOwner.isDefined) { // only delete from a single owner  		
  		if(instD.secondUseOwners.contains(fromOwner.get))
  		{ // delete from seconduser list, dont change the instance beyond that
  			internRemovePropertyFromOwner(ref,fromOwner.get)
  			passOnDeletedInstanceToCollFuncParents(instD,fromOwner.toList)
  			val newSU=instD.secondUseOwners.filterNot(_ == fromOwner.get)
  			val newInst=instD.changeSecondUseOwners(newSU)
				ActionList.addTransactionData(ref, DataChangeAction(Some(newInst), None, None, None, None, Some(fromOwner.get))) // store that target
  			return true
  		} 
  		else if(instD.owners .contains(fromOwner.get)) {
  			if(instD.secondUseOwners.nonEmpty){ // deletion from standard user list, but secondusers still use it
  				internRemovePropertyFromOwner(ref,fromOwner.get)
  				passOnDeletedInstanceToCollFuncParents(instD,fromOwner.toList)
  				val newOwners=instD.owners.filterNot(_ ==fromOwner.get) :+ instD.secondUseOwners.head
  				val newSUOwners=instD.secondUseOwners.drop(1)
  				// raise one of the su owners to a general owner
  				val newInst=instD.clone(ref,newOwners,newSUOwners)
					ActionList.addTransactionData(ref, DataChangeAction(Some(newInst), None, None, None, None, Some(fromOwner.get))) // store that target
  				return true
  			}  			
  			// else do the standard procedure and delete the instance fully
  		} // can not find fromOwner
  		else if(instD.secondUseOwners.nonEmpty) throw new IllegalArgumentException("cant delete "+ref+" from unknown owner "+fromOwner.get+" secondUseOwners:"+instD.secondUseOwners.mkString)
  		else{
        util.Log.e("cant delete "+ref+" from unknown owner "+fromOwner.get+", no SecondUsers found, deleting clompletely")
  		  if(dontNotifyOwner match {case Some(dno)=> fromOwner.get.ownerRef!=dno;case _ =>true}) 			
  			internRemovePropertyFromOwner(ref,fromOwner.get)
  		} 
  	}// fromowner.isdefined
  	  // no fromOwner defined => remove from all Owners
  	for(owner <-instD.owners)  	  
  		if(dontNotifyOwner match {case Some(dno)=> owner.ownerRef!=dno;case _ =>true}) 			
  			internRemovePropertyFromOwner(ref,owner)
  	for(owner <-instD.secondUseOwners)  	  
  		if(dontNotifyOwner match {case Some(dno)=> owner.ownerRef!=dno;case _ =>true}) 			
  			internRemovePropertyFromOwner(ref,owner)
  	passOnDeletedInstanceToCollFuncParents(instD,instD.owners)
  	passOnDeletedInstanceToCollFuncParents(instD,instD.secondUseOwners)


		ActionList.addTransactionData(ref, DeleteAction(instD))
  	
  	// remove link information at external source instances
  	for(afield <-instD.fieldData) // check all fields for FieldReferences
  	{
  		// fieldReferences
  		val refList= afield.getElementList[FieldReference](DataType.FieldRefTyp,Nil) // get all FielReferences from that term
  		for(fref <-refList) // remove all external links
  			removeLinkRef(ref,fref)
  		// parentReferences
  		if(dontNotifyOwner.isEmpty) {
  			val prefList=afield.getElementList[ParentFieldRef](DataType.ParentRefTyp,Nil)
  			for(fref <-prefList)
  				removeAnyRef(ref,instD.owners(fref.ownerIx ).ownerRef,fref.fieldNr)  		
  		}
  	}
  	// remove link information from external target instances pointing here
  	ActionList.getReferencingLinks(ref) match {			
  		case Some(refLinks) =>
				for ((fieldNr,list) <- refLinks.links) //  walk through all fields
          for(aref <- list;if !aref.isParentRef ) // wall through the list of fieldRefs for a field
          {
            val targetRef=aref.getReference
            if(targetRef!=ref) // ignore if the link points from the same instance
            {
              val targetData=ActionList.getInstanceData(targetRef)
              if(targetData!=null) {
                val theField=targetData.fieldData(aref.field). // get the term that contains the linkref
                  replaceExpression {
										case aFieldRef: FieldReference =>
											val relink = resolveLinkRef(targetRef, aFieldRef)
											//System.out.println("checking ref:"+aFieldRef+" resolved:"+relink+" with:"+ref);
											if (relink == ref) aFieldRef.cachedValue // replace the reference with the cached value
											else aFieldRef // wrong reference, leave it

										case other => other // dont change any other elements
                }
								ActionList.addTransactionData(targetRef, DataChangeAction(Some(targetData.setField(aref.field, theField)))) // store that target
              }
            }
          }
			case _ => // go drink a beer
  	}	
  	
  	//TODO delete instance from caches !!!
  	
  	// delete Children
  	ActionList.getInstanceProperties(ref) match {
  		case Some(propdat) => 
  		val myRef=Some(ref)
				for (pfieldIx <-propdat.propertyFields.indices) {
          val oRef=Some(new OwnerReference(pfieldIx.toByte,ref))
          for( child <-propdat.propertyFields(pfieldIx).propertyList )
            try {
              tryDeleteInstance(child,oRef,myRef)
            } catch {
              case NonFatal(e) => util.Log.e("Error when deleting child "+child,e)
            }
          }
			case None =>
  	}
  	
  	// notify listeners in owners
  	for(owner <-instD.owners) {
  	  val ownerClass=AllClasses.get.getClassByID(owner.ownerRef.typ).asInstanceOf[ServerObjectClass]
	    if(ownerClass.actionModule.notifyChildRemoved) {
	      val ownerInst=ActionList.getInstanceData(owner.ownerRef)
	      if(ownerInst!=null) // if the owner instance still exists
	      		ownerClass.actionModule.onChildRemoved(ownerInst,owner.ownerField,instD.ref)
	    }
  	}  	
  	true
  }
	
	
	
	def tryMoveMultiInstances(subRefs:Seq[Reference],fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int):Unit = {
		//System.out.println(" move Instances: "+subRefs.mkString(",")+ " from:"+fromOwner+ " to:"+toOwner+" pos:"+atPos)
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		var pos=atPos
	  for(ref <-subRefs) {
	  	tryMoveInstance(ref,fromOwner,toOwner,pos)
	  	if(atPos> -1) pos +=1
	  }
	}
	
	/** moves one instance to another owner
	 *  @param subRef Instance to be moved
	 *  @param fromOwner old owner who will loose the subinstance
	 *  @param toOwner new owner who will get the subinstance
	 */
	def tryMoveInstance(subRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference,pos:Int):Unit = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		if(checkIsChildOf(toOwner.ownerRef,subRef))throw new IllegalArgumentException("Move not possible. ToOwner "+toOwner+" is child of moving Instance "+fromOwner)
		// change the property information		
		val instData=ActionList.getInstanceData(subRef)
		val differentOwners= fromOwner!=toOwner
		if(differentOwners) {
			internAddPropertyToOwner(instData,toOwner,pos,true)		
			internRemovePropertyFromOwner(subRef,fromOwner,differentOwners)
			// change the owner ref inside of the instance data
			var newInst= instData.changeSingleOwner(fromOwner,toOwner)
			// parentRefs
			val changedOwnerPos=instData.owners.indexOf(fromOwner)
			for(i <- newInst.fieldData.indices;theField=newInst.fieldData(i);if !theField.isNullConstant) {
				val prefList=theField.getElementList[ParentFieldRef](DataType.ParentRefTyp,Nil)
				for(rf <-prefList;if rf.ownerIx == changedOwnerPos) { // find all parent Refs that are related to the changed owner
					 removeAnyRef(subRef,fromOwner.ownerRef ,rf.fieldNr )// remove from fromOwner					 
					 addRef(subRef,i.toByte,toOwner.ownerRef,rf.fieldNr ,true)// add to toOwner
					 val toOwnerData=ActionList.getInstanceData(toOwner.ownerRef)
					 val newFieldValue=theField.replaceExpression {
						 // change link
						 case frEx: ParentFieldRef =>
							 if (frEx == rf) frEx.setValue(toOwnerData.fieldValue(rf.fieldNr))
							 frEx
						 case ex => ex
					 }
					 newInst=newInst.setField(i.toByte, newFieldValue)
				 }
			}
			// store
			ActionList.addTransactionData(subRef, DataChangeAction(Some(newInst), None, None, None, Some(ChangeDontNotifyOwners)))
			passOnNewInstanceToCollFuncParents(newInst,List(toOwner))
			passOnDeletedInstanceToCollFuncParents(instData,List(fromOwner))
			
		} else {
			movePropertyToPos(subRef,fromOwner,pos)
		}
	}
	
	/** checks if the Reference given as toOwner is a child of source
	 * 
	 */
	private def checkIsChildOf(toOwner:Reference,source:Reference):Boolean= {
		//System.out.println("check Child toOwner:"+toOwner+" source:"+source)
		if(toOwner==source)return true
		val instData=ActionList.getInstanceData(toOwner )
    //if(instData==null) {System.err.println("check is child Of toOwner "+toOwner+" does not exist");return false}
		for(owner <-instData.owners)
			if(owner.ownerRef.instance==0) {util.Log.e("wrong ownerRef "+owner.ownerRef+" in "+toOwner);return false}
			else if(checkIsChildOf(owner.ownerRef,source)) return true	
		for(owner <-instData.secondUseOwners)
		  if(owner.ownerRef.instance==0) {util.Log.e("wrong secondownerRef "+owner.ownerRef+" in "+toOwner);return false}
		  else if(checkIsChildOf(owner.ownerRef,source)) return true	
		false
	}


	def tryCopyMultiInstances(instList: Seq[Reference], fromOwner: OwnerReference, toOwner: OwnerReference, atPos: Int): Int = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")		
		var pos=atPos
		var lastInst:Int=0		
		for(ref <-instList) {
			lastInst=tryCopyInstance(ref,fromOwner,toOwner,pos,true)
			if(atPos> -1) pos +=1
		}
		lastInst
	}
	
	
	/** tries to copy the given instance from fromOwner to toOwner
	 * @return instanceid of the new instance
	 * 
	 */
	private def tryCopyInstance(instRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int,
	                    collNotifyOwners:Boolean):Int = {
		//System.out.println("try copy instance:"+instRef+" "+atPos+" "+collNotifyOwners)
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")
		val instID=internTryCopyInstance(instRef,fromOwner,toOwner,atPos,collNotifyOwners)
		if(instID==0) util.Log.e("instID==0 when trying to copy "+instRef+" from:"+fromOwner+" to:"+toOwner)
		else copyChildren(instRef,new Reference(instRef.typ,instID))
		instID
	}
	
	
	private def internTryCopyInstance(instRef:Reference,fromOwner:OwnerReference,toOwner:OwnerReference,atPos:Int,
	                    collNotifyOwners:Boolean):Int = {
		//System.out.println("intern tryp Copy Instance:"+instRef+" fromOwner:"+fromOwner+" toOwner:"+toOwner)
		if(checkIsChildOf(toOwner.ownerRef,instRef))throw new IllegalArgumentException("Copy not possible. ToOwner "+toOwner+" is child of copying Instance "+fromOwner)		
			
		val instD=ActionList.getInstanceData(instRef)
		
		if(!instD.owners.contains(fromOwner)) {		  
			if(!instD.secondUseOwners.contains(fromOwner)) throw new IllegalArgumentException("Copy: instance "+instRef+" is not owned by "+ fromOwner)
			else if(!collNotifyOwners){
				// is a child as second use
				//System.out.println("copy as second use:"+instRef+" from:"+fromOwner+" to:"+toOwner)
			  if(ActionList.isInstanceCopied(instRef)){ // is the source of the second use also copied(before)
			    val newSourceRef=ActionList.getInstanceData(ActionList.getCopyForRef(instRef))
			    trySecondUseInstances(List(newSourceRef.ref),newSourceRef.owners.head,toOwner,-1,false) // create second use from copy target
			  }
			  else trySecondUseInstances(List(instRef),fromOwner,toOwner,-1,false) // create second use from copy source
			  // dont notify coll owners		
			}
			return 0
		}
		if(ActionList.isInstanceCopied(instRef)) {
      util.Log.e("Already copied :"+instRef)
			return 0 // has already been copied
		}			
		
		// get the other owners of that instance, apart from "fromOwner", and add the new owner toOwner
		val fromOwnerPos=instD.owners .indexWhere(_ == fromOwner)
		if(fromOwnerPos<0) throw new IllegalArgumentException("Can not find fromOwner "+fromOwner +" when copy "+instRef)
		val newOwners=instD.owners.clone()
		newOwners(fromOwnerPos)=toOwner		
		// replace other owners that have already been copied
		//TODO: replace link targets in the same way
		for(oIx <-newOwners.indices;oldRef=newOwners(oIx).ownerRef) 
			if(ActionList.isInstanceCopied(oldRef )){
			  val newRef=ActionList.getCopyForRef(oldRef)
			  newOwners(oIx)=new OwnerReference(newOwners(oIx).ownerField,newRef)
			}
		
		var createInst=tryCreateInstance(instRef.typ ,newOwners,false,atPos,false,false) // new instance created by DB
		createInst=instD.clone(createInst.ref,newOwners,Seq.empty)
		// Register FieldReferences to other instances		
		for(i <- instD.fieldData.indices;theField=instD.fieldData(i);if !theField.isNullConstant)
		{			
			// FieldReferences
			val refList=theField.getElementList[FieldReference](DataType.FieldRefTyp,Nil)			
			for(rf <-refList){				 
				 val qualifiedSourceRef = resolveLinkRef(createInst.ref,rf)
				 if(ActionList.isInstanceCopied(qualifiedSourceRef)){ // the source of the link was also copied, change link to the copy					 
					 val newSource=ActionList.getCopyForRef(qualifiedSourceRef)
					 //System.out.println("Copied source Ref found: "+qualifiedSourceRef+" was copied to :"+newSource)
					 createInst=createInst.setField(i.toByte,
					   createInst.fieldData(i).replaceExpression {
							 // change link
							 case frEx: FieldReference => if (frEx == rf) {
								 val newRef = new FieldReference(frEx.remType, Some(newSource.instance), frEx.remField, frEx.cachedValue)
								 //println("Expression found , newRef:"+newRef)
								 addLinkRef(createInst.ref, i.toByte, newRef) // add target information to copied source
								 newRef // return a changed ref
							 } else frEx

							 case ex => ex
						 }
					 )
					 //println(" new Expression "+createInst.fieldData(i))
				 }
				 else addLinkRef(createInst.ref,i.toByte,rf) // only add the target information				 
			}
			// parentReferences
			val prefList=theField.getElementList[ParentFieldRef](DataType.ParentRefTyp,Nil)
			for(rf <-prefList;if(rf.ownerIx ==fromOwnerPos)|| ActionList.isInstanceCopied(newOwners(rf.ownerIx).ownerRef )){
			  val sourceRef=if(rf.ownerIx ==fromOwnerPos) toOwner.ownerRef else ActionList.getCopyForRef(newOwners(rf.ownerIx).ownerRef)
				//val sourceRef=newOwners(rf.ownerIx).ownerRef
				val toOwnerData=ActionList.getInstanceData(sourceRef)
				addRef(createInst.ref,i.toByte,sourceRef,rf.fieldNr ,true)
				createInst=createInst.setField(i.toByte,
					createInst.fieldData(i).replaceExpression {
						// change link
						case frEx: ParentFieldRef =>
							if (frEx == rf) frEx.setValue(toOwnerData.fieldValue(rf.fieldNr))
							frEx
						case ex => ex
					}
				)
			}
		}	
		// check if link targets to this instance were also copied
		ActionList.getReferencingLinks(instRef) match {
			case Some(rl) =>  for((field,lrefs) <- rl.links;alRef <-lrefs; if !alRef.isParentRef) {
				val targetRef=alRef.getReference
				if(ActionList.isInstanceCopied(targetRef)){ // target was also copied
					val targetCopyRef=ActionList.getCopyForRef(targetRef)
					val targetCopyData=ActionList.getInstanceData(targetCopyRef)
					val newFieldValue=targetCopyData.fieldData(alRef.field ). // get the term that contains the linkref
					replaceExpression{// go through all elements and searches fieldRef elements {
						case aFieldRef: FieldReference =>
							val sourceRef = resolveLinkRef(targetCopyRef, aFieldRef)
							//System.out.println("checking ref:"+aFieldRef+" resolved:"+relink+" with:"+ref);
							if (sourceRef == instRef) {
								// if the target pointed to the original instance, replace with new fieldReference pointing to copy
								val newFr = new FieldReference(aFieldRef.remType, Some(createInst.ref.instance), aFieldRef.remField, aFieldRef.cachedValue)
								addLinkRef(targetCopyRef, alRef.field, newFr)
								removeLinkRef(targetCopyRef, aFieldRef)
								newFr
							}
							else aFieldRef // wrong reference, leave it
						case other => other // dont change any other elements
					}
					// store copied target instance again with new reference
					ActionList.addTransactionData(targetCopyRef, DataChangeAction(Some(targetCopyData.setField(alRef.field, newFieldValue))))
				}
			}			
			case _ =>
		}
		
		// get coll data
		var collData=ActionList.getCollData(instRef)
		for(c <-collData)
			collData=Some(c.changeReference(createInst.ref))
		// store new instance	
		ActionList.addTransactionData(createInst.ref, DataChangeAction(Some(createInst), None, None, collData))
		// notify owners
		if(collNotifyOwners)
		passOnNewInstanceToCollFuncParents(createInst,createInst.owners)
		else if(instD.owners.length>1){
			// if there are multiple owners, only notify other owners that are not "fromOwner"
			passOnNewInstanceToCollFuncParents(createInst,instD.owners.filterNot(
				anOwner => (anOwner == fromOwner)|| ActionList.isInstanceCopied(anOwner.ownerRef)) )
		}		
		
		ActionList.notifyInstanceCopied(instRef,createInst.ref)	
		//
		createInst.ref.instance 
	}
	
	
	private def copyChildren(instRef:Reference,createdInstRef:Reference):Unit = {
	  if(createdInstRef.instance ==0) throw new IllegalArgumentException("Copy Children createdInstRef="+createdInstRef)
		// now copy all owned child instances
    val prFieldDefs=AllClasses.get.getClassByID(instRef.typ).propFields
		ActionList.getInstanceProperties(instRef) match {
			case Some(prop) => // if this instances has child instances
				// copy first level children
				for(fieldNr <- prop.propertyFields.indices;if !prFieldDefs(fieldNr).volatile){ // go through all prop fields
				  val crOref=new OwnerReference(fieldNr.toByte,createdInstRef)
				  val instOref=new OwnerReference(fieldNr.toByte,instRef)
					val fieldData=prop.propertyFields(fieldNr) // get the data for the prop field
					for(childRef <-fieldData.propertyList;if StorageManager.instanceExists(childRef.typ,childRef.instance )) // go through all children of that field
						internTryCopyInstance(childRef,instOref, // copy them to the new instance
							crOref,-1,false)
				}
				// copy children of children
				for(fieldNr <- prop.propertyFields.indices;if !prFieldDefs(fieldNr).volatile) // go through all prop fields
				{
					val fieldData=prop.propertyFields(fieldNr) // get the data for the prop field
					for(childRef <-fieldData.propertyList;if StorageManager.instanceExists(childRef.typ,childRef.instance) ) // go through all children of that field
						if(ActionList.isInstanceCopied(childRef))
						  copyChildren(childRef,ActionList.getCopyForRef(childRef))
						// otherwise the child was a second use and not copied
				}
			case _ => // if no children, do nothing
		}
	}
	                    	
	
	/** creates links of the given instances into the new Owner
	 * @param instlist instances to link
	 * @param fromOwner current owner
	 * @param toOwner new owner where the instances should be linked at
	 * @param atPos position where they should be linked at
	 * @param collNotifyOwners should owners be notified of the change, is used when called from copy
	 * 
	 */
	def trySecondUseInstances(instList:Iterable[Reference], fromOwner:OwnerReference, toOwner:OwnerReference,
														atPos: Int, collNotifyOwners: Boolean = true): Unit = {
		if(!canModify ) throw new IllegalArgumentException("No transaction defined ")	
		if(fromOwner==toOwner) throw new IllegalArgumentException("cant make second use with same owner "+fromOwner)
		var pos=atPos
		val toOwnerModule=AllClasses.get.getClassByID(toOwner.ownerRef.typ).asInstanceOf[ServerObjectClass].actionModule
		val toOwnerModuleWantsNotify=toOwnerModule.notifyChildAdded
		val toOwnerInst=if(toOwnerModuleWantsNotify)ActionList.getInstanceData(toOwner.ownerRef)  else null		
		for(instRef <-instList) {
			if(checkIsChildOf(toOwner.ownerRef,instRef))throw new IllegalArgumentException("Second use not possible. ToOwner "+toOwner+" is child of linking Instance "+instRef)
			val instD=ActionList.getInstanceData(instRef)
			if(!instD.owners.contains(fromOwner)&& !instD.secondUseOwners.contains(fromOwner)) throw new IllegalArgumentException("Link: instance "+instRef+" is not owned by "+ fromOwner)
			if(instD.owners.contains(toOwner)||instD.secondUseOwners.contains(toOwner)) throw new IllegalArgumentException("Second use not possible: child "+instD.ref+" is already owned by "+toOwner)
			val newSUOwners=instD.secondUseOwners :+toOwner
			val newInst=instD.changeSecondUseOwners(newSUOwners)
			internAddPropertyToOwner(newInst,toOwner,pos,true)
			if(collNotifyOwners)
				passOnNewInstanceToCollFuncParents(newInst,Seq(toOwner))
			ActionList.addTransactionData(instRef, DataChangeAction(Some(newInst), None, None, None))
			if(toOwnerModuleWantsNotify) {
			  toOwnerModule.onChildAdded(toOwnerInst,toOwner.ownerField,instD)
			}
			if(atPos> -1) pos +=1
		}	  
	}
	
	
	// internal routine
	 def internRemovePropertyFromOwner(subRef:Reference,fromOwner:OwnerReference,notifyOwner:Boolean=false):Unit=	
	  if(StorageManager.instanceExists(fromOwner.ownerRef.typ, fromOwner.ownerRef.instance)){
		val newProp= (ActionList.getInstanceProperties(fromOwner.ownerRef) match {
  				// Property data found				
  				case Some(a) => a 
  				// so far no Property data there, something is wrong here
  				case _ => throw new IllegalArgumentException("Delete "+subRef+" /Notify Owner but owner "+fromOwner.ownerRef+" has no Property data ")
  			} // and add the new child to the prop list
  			).removeChildInstance(fromOwner.ownerField ,subRef)
  	ActionList.addTransactionData(fromOwner.ownerRef,DataChangeAction(None,Some(newProp),None,None,
			if (notifyOwner) Some(RemoveNotifySourceOwners(fromOwner, List(subRef))) else None))
  	//  notify subscriptions
  	//CommonSubscriptionHandler.instanceDeleted(fromOwner,subRef)
	}


	def runDelayed(func: () => Unit): delayedActionsList.type = {
	  delayedActionsList+=func
	}

	def canReorg: Boolean = TransDetailLogHandler.getInsertPos > 0
	
	/**
	 *  encapsulates a transaction so that StartTransaction and Finishtransaction/BreakTransaction
	 *  will always be executed
	 *  @param userID id of the user that started the transaction
	 *  @param actionCode code for the Action , to be used for the transaction log
	 *  @param ref Reference of the first instance that is changed, for the transaction log
	 *  @param multi are there multiple instances changed in this transaction - for the transaction log
	 *  @param f a function that does all the work for this transaction
	 */
	def doTransaction(userID: Short, actionCode: Short, ref: Reference, multi: Boolean, createType: Int, f: => Unit): Option[Exception] = transLock.synchronized {
    //println("Try transaction "+ActionNameMap.getActionName(actionCode)+ " "+ref)	  
		startTransaction()		
    currentUser=userID
    currentActionCode=actionCode
    currentRef=ref
    multiInst=multi
    logCreateType=createType
    delayedActionsList.clear()
    var success=true
    try {
        f        
        for(d<-delayedActionsList) 
          d.apply() // run delayed actions          
		} catch {
			case e: Exception => util.Log.e("transaction " + ref + " code:" + actionCode, e); success = false; breakTransaction(); return Some(e)
			case NonFatal(e) => util.Log.e("transaction " + ref + " code:" + actionCode, e)
		}

    if(success) try {
			finishTransaction()
		} catch {
			case e: Exception => util.Log.e("transaction finish ", e); success = false; breakTransaction(); return Some(e)
			case NonFatal(e) => util.Log.e("transaction finish " + ref + " code:" + actionCode, e)
		}
			//println("Transaction done  ["+ActionNameMap.getActionName(actionCode)+ " "+ref+"] "+(System.currentTimeMillis()-cm))
		None
	}

	/** notifies that an user logged of, so cancel a running transaction of that user
	*
*/
	def userLogsOff(userID: Short): Unit = if (running && userID == currentUser) {
		breakTransaction()
	}
	
	def doFixingJob[A](listener:(Int,String)=>Unit,job: ((Int,String)=>Unit)=>A):Option[A]={
	   if(running ) throw new IllegalArgumentException("Reorg:An Transaction is still running ")
	   UserList.disconnectUsers()	  
		running=true
		var result:Option[A]=None
	  try {
	    result =Some(job(listener))
	  }
	  finally {
	    running=false
		}
	  result
	}

	def doReorgDB(listener: (Int, String) => Unit): Option[Unit] = doFixingJob(listener, StorageManager.reorgDatabase)

	def doFixInheritance(listener: (Int, String) => Unit): Option[Unit] = doFixingJob(listener, StorageManager.fixInheritance)

	def doDeleteOrphans(listener: (Int, String) => Unit): Option[Unit] = doFixingJob(listener, StorageManager.deleteOrphans)

	def doFindOrphans(listener: (Int, String) => Unit): Option[Map[OwnerReference, Iterable[Reference]]] = doFixingJob[Map[OwnerReference, Iterable[Reference]]](listener, StorageManager.findOrphans)


	def doUndo(connection: ConnectionEntry): Unit = {
		if(undoUserEntry!=null && connection==undoUserEntry) {
			//System.out.println("do undo "+TransLogHandler.transID)
			StorageManager.undoLastStep()
			CommonSubscriptionHandler.refreshAfterUndo()	
			UserList.releaseUsersForUndo(connection)
			undoUserEntry=null
		}
	}

}


