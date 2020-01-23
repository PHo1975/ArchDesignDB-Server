/**
 * Author: Peter Started:27.07.2010
 */
package transaction.handling

import definition.data._
import server.comm.{CommonSubscriptionHandler, UserList}
import server.storage.{CollFuncResultSet, StorageManager}

import scala.collection.mutable
import scala.util.control.NonFatal
//import scala.collection.immutable.ListMap

/** manages the List of actions of a transaction
 * 
 * 
 */



object ActionList extends DataRetriever {
  var theList: mutable.LinkedHashMap[Reference, TransactionData] = collection.mutable.LinkedHashMap[Reference, TransactionData]()
	protected var _bufferUpdates=false
  
	val copiedInstances: mutable.HashMap[Reference, Reference] =collection.mutable.HashMap[Reference,Reference]()
  
	def isEmpty: Boolean =theList.isEmpty
	
	def breakAllData(): Unit = {
		//System.out.println("Break")
		for(trans <- theList.valuesIterator)
			trans match {
				case CreateAction(ref,_,_,_,_,_) => // delete the Instances that got created during the try phase
					try {
						StorageManager.deleteInstance(ref.typ, ref.instance)
					} catch {case NonFatal(e) => util.Log.e("BreakAllData error ",e)}
				case _ =>
		}
		reset()
  }

  def isBufferingUpdates: Boolean =_bufferUpdates

  def reset(): Unit = {
      theList.clear
      copiedInstances.clear
      _bufferUpdates=false
    }

  def notifyInstanceCopied(oldRef:Reference,copyRef:Reference):Unit = copiedInstances(oldRef)=copyRef

  def isInstanceCopied(ref:Reference):Boolean= copiedInstances.contains(ref)

  def getCopyForRef(oldRef:Reference): Reference =copiedInstances(oldRef)


  def commitAllData(): Unit = {
    //System.out.println("commit:"+theList.mkString("|"))
    var hasMoveOrCopy:Boolean=false
    _bufferUpdates=true
    try {
      //System.out.println("actions:\n"+theList.mkString("\n"))
      for(trans <- theList.valuesIterator)
        trans match {
          case CreateAction(_, instData, propData, linkData, collData, cmi) => // Instances get created during the Try-Phase of the transaction
            val (dataPos,dataLength)=instData match {
              case Some(data)=>
              val sendData =if(propData.isDefined && propData.get.hasChildren !=data.hasChildren) data.setHasChildren(propData.get.hasChildren)
                 else data
              cmi match {
                   case Some(AddDontNotifyOwners) =>
                   case _ => for(owner <-data.owners) // in all other cases notify owners
                                CommonSubscriptionHandler.instanceCreated(owner,sendData)
                 }
              StorageManager.writeInstance(data,created = true,writeIndex = false) // if instdata is defined, write
              case None => throw new IllegalArgumentException("No Data for Creation")
            }

            val (propPos,propSize)=propData match {
              case Some(pdata)=>
              StorageManager.writeInstanceProperties(pdata,writeIndex = false) // if propdata is defined, write
              case None => (0L,0)
            }
            val (collPos,collSize)=collData match {
              case Some(c)=>  StorageManager.writeCollectingFuncData (c,writeIndex = false)
              case None => (0L,0)
            }
            val (linkPos,linkSize)=linkData match {
              case Some(l)=>  StorageManager.writeReferencingLinks(l,writeIndex = false)
              case None => (0L,0)
            }
            for(d<-instData)
            StorageManager.ixHandler(d.ref.typ).writeAllFields(d.ref.instance,dataPos,dataLength,propPos,propSize,linkPos,linkSize,collPos,collSize,created = true)


          case DataChangeAction(instData, propData, linkData, collData, cmi, deleteFromUser) =>
            for(i <- instData){
              val sendData = if(propData.isDefined && propData.get.hasChildren !=i.hasChildren) i.setHasChildren(propData.get.hasChildren)
                 else i
              cmi match {
                   case Some(ChangeDontNotifyOwners) =>
                   case _ => CommonSubscriptionHandler.instanceChanged(sendData)
                 }
              StorageManager.writeInstance(i,created = false); // if instance is defined, write
            }
            //println("change commit prop:"+propData+" coll:"+collData+" link:"+linkData)
            for(p <- propData){
              StorageManager.writeInstanceProperties(p) // if properties ...
               // child was copied or moved here
              cmi match {
                case Some(RefreshDestinationOwner) => hasMoveOrCopy=true
                case Some(RemoveNotifySourceOwners(fromOwner,childList))=>
                for(child<-childList) CommonSubscriptionHandler.instanceDeleted(fromOwner,child)
                case _ =>
              }

            }
            for(l <- linkData) StorageManager.writeReferencingLinks(l)
            for(c <- collData) StorageManager.writeCollectingFuncData(c)
            for(d <- deleteFromUser) CommonSubscriptionHandler.instanceDeleted(d,instData.get.ref)

          case DeleteAction(inst) =>
            StorageManager.deleteInstance(inst.ref.typ,inst.ref.instance )
            for(owner <-inst.owners)
              CommonSubscriptionHandler.instanceDeleted(owner,inst.ref)
            for(owner <-inst.secondUseOwners)
              CommonSubscriptionHandler.instanceDeleted(owner,inst.ref)
          case CreateBlock(ref, data) =>
          StorageManager.writeBlock(new BlockData(ref,data),created = true)
          case ChangeBlock(ref, data)=>
          StorageManager.writeBlock(new BlockData(ref,data),created = false)
          case DeleteBlock(ref)=>
          StorageManager.deleteBlockInstance(ref.typ,ref.instance)
      }
      // notify property changes for move and copy
      if(hasMoveOrCopy ) {
        for(trans <- theList.valuesIterator)
         trans match {
          case DataChangeAction(_,Some(pr),_,_,Some(RefreshDestinationOwner),_) =>
            CommonSubscriptionHandler.refreshSubscriptionsFor(pr.ref)
          case _ =>
        }
      }
      //SimpleProfiler.finish("commit "+theList.size)
      //util.Profile.measure("write al done")
      UserList.flushTransactionBuffers()
      reset()
    } catch { case e:Exception => util.Log.e(e); TransactionManager.breakTransaction() }
    _bufferUpdates=false
  }


  def addTransactionData (ref:Reference,newRec:TransactionData): Unit = {
    //System.out.println("add Transdata "+ref+"|"+newRec)
    if (theList.contains(ref))
      theList(ref) match { // is there already an transaction data for this instance ?
        case a:CreateAction => newRec match {
          // a createAction is already there. What action shall be added ?
          case  DataChangeAction(in,pr,li,co,cmi,_ ) =>
            if (in.isDefined) a.newInstData = in // add the new data to the createAction
            if (pr.isDefined) a.newPropData = pr
            if (li.isDefined)/* if(a.newLinksData!=None) throw new IllegalArgumentException("Cant add external links to created Instance "+ref+" "+li+" oldlinks:"+a.newLinksData )
            else*/ a.newLinksData =li
            if (co.isDefined) a.newCollData= co
            if(cmi.isDefined) a.cmi=if(a.cmi.isDefined) Some(a.cmi.get.replaceWith(cmi.get)) else cmi
            //if(pos != -1) a.atPosition=pos
          case x: DeleteAction => throw new IllegalArgumentException("Delete after create for "+ref)
          case x: CreateAction =>  throw new IllegalArgumentException("Create after create for "+ref)
          case f=> throw new IllegalArgumentException("wrong Action Type "+f)
        }

        case b:DataChangeAction => newRec match {
          // a DataChange action is already there. What action shall be added ?
          case  DataChangeAction(in,pr,li,co,cmi,deFrOw) =>
            if (in.isDefined) b.newInstData = in // add the new data to the createAction
            if (pr.isDefined) b.newPropData = pr
            if (li.isDefined) b.newLinksData = li
            if (co.isDefined) b.newCollData= co
            if(cmi.isDefined) b.cmi=if(b.cmi.isDefined) Some(b.cmi.get.replaceWith(cmi.get)) else cmi
            if(deFrOw.isDefined) b.deleteFromOwner=deFrOw
          case x: DeleteAction => theList += (ref ->newRec) // replace the datachange action with the delete
          case x: CreateAction =>  throw new IllegalArgumentException("Creating an existing instance "+ref)
          case f=> throw new IllegalArgumentException("wrong Action Type "+f)
        }
        case a:DeleteAction => // drop the new action when the instance should already be deleted

        case _:CreateBlock=> newRec match {
          case x:ChangeBlock=> theList+= (ref -> CreateBlock(x.ref,x.data))
          case w=> throw new IllegalArgumentException("wrong Action type: "+w)
        }
        case _:ChangeBlock=> newRec match {
          case d:DeleteBlock=> theList += (ref -> newRec)
          case c:ChangeBlock => theList += (ref -> newRec)
          case w=> throw new IllegalArgumentException("wrong Action type: "+w)
        }
        case _:DeleteBlock => // drop the new action when the block should already be deleted
      }
    // no data for that ref yet, add the new TransactionData
    else theList += (ref -> newRec)
  }

  /** checks if there is data  in the actionlist
   *  if yes, return it, else get the data from the DB
   */
  def getInstanceData(ref:Reference):InstanceData = {
    if (theList.contains(ref))
      theList(ref)  match {
      case CreateAction(_,Some(data),_,_,_,_) => data
      case DataChangeAction(Some(data),_,_,_,_,_) => data
      case d:DataChangeAction=> if(StorageManager.instanceExists(ref.typ, ref.instance))
        StorageManager.getInstanceData(ref) else throw new IllegalArgumentException("ActionList get Instance cant find instance:"+ref)
      case a:DeleteAction => null
      case w => throw new IllegalArgumentException("wrong Action type: "+w)
    }
    else if(StorageManager.instanceExists(ref.typ, ref.instance))
      StorageManager.getInstanceData(ref)
    else throw new IllegalArgumentException("ActionList get Instance cant find instance:"+ref)
  }


  def getInstanceProperties(ref:Reference):Option[InstanceProperties] = {
    if (theList.contains(ref))
      theList(ref) match {
      case CreateAction(_,_, a @ Some(_),_,_,_) =>  a
      case DataChangeAction(_,a @ Some(_),_,_,_,_) =>  a
      case _ => StorageManager.getInstanceProperties(ref) // ignore
    }
    else StorageManager.getInstanceProperties(ref)
  }

  /** checks if there are data for referencing links in the actionlist
   *  if yes, return them, else get the data from the DB
   */
  def getReferencingLinks(ref:Reference):Option[ReferencingLinks] = {
    if(theList.contains(ref))
      theList(ref) match {
         case CreateAction(_,_,_, a @ Some(_),_,_) =>  a
         case DataChangeAction(_,_,a@ Some(_),_,_,_) => a
         case _ => StorageManager.getReferencingLinks(ref)
      }
    else StorageManager.getReferencingLinks(ref)
  }

  def getCollData(ref:Reference):Option[CollFuncResultSet] =   {
    if (theList.contains(ref))
      theList(ref) match {
      case CreateAction(_,_,_,_,a@ Some(_),_) =>  a
      case DataChangeAction(_,_,_,b @ Some(_),_,_) => b
      case _ =>StorageManager.getCollectingFuncData(ref)// ignore
    }
    else StorageManager.getCollectingFuncData(ref)
  }


}