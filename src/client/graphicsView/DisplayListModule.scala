/**
 * Author: Peter Started:06.10.2010
 */
package client.graphicsView

import server.storage.{ActionModule,ActionImpl,ActionIterator}
import definition.typ._
import definition.expression._
import definition.data._
import java.io._
import transaction.handling.{TransactionManager,SessionManager}
import server.comm.{AbstractUserSocket, JavaClientSocket}
import scala.collection.Iterator
import server.storage.StorageManager
import runtime.function.GraphElemModule
import runtime.function.TypeInfos
import transaction.handling.ActionList
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
 * 
 */

sealed trait ChangeInfo

object NoChange extends ChangeInfo
case class Delete(ref:Reference) extends ChangeInfo
case class Change(line:LineElement) extends ChangeInfo

class DisplayListModule extends ActionModule {

  var lineTyp= -1
  var dlistTyp= -1
  
  SessionManager.registerSetupListener(() => {
		lineTyp=AllClasses.get.getClassIDByName("LineElem")
	})

	def setObjectType(typeID:Int)= {
    dlistTyp=typeID	
  }

	val cleanupAction=new ActionImpl("säubern",None,doCleanup)

	def actions = List(cleanupAction) 
	

	def doCleanup(u:AbstractUserSocket, data:InstanceData, param:Seq[(String,Constant)]):Boolean = {
	  val owner=Some(new OwnerReference(0.toByte,data.ref))	
	  val vertVect=new VectorConstant(0,1,0)
	  val horVect=new VectorConstant(1,0,0)
	  val linesMap=collection.mutable.HashMap[VectorConstant,ArrayBuffer[LineElement]]()	 // groups of parallel lines   
	    
		for (props<-StorageManager.getInstanceProperties(data.ref);elRef<-props.propertyFields.head.propertyList ){
		  val elData=try {StorageManager.getInstanceData(elRef)} catch {case NonFatal(e)=>util.Log.e("cleanup",e); null
			case other:Throwable =>println(other);System.exit(0);null}
		  if(elData==null ) TransactionManager.internRemovePropertyFromOwner(elRef, owner.get, false)
		  else elRef.typ match {
		    case TypeInfos.lineElemType =>
					var p1=elData.fieldValue (3).toVector
					var p2=elData.fieldValue (4).toVector
					if (Math.abs((p1-p2).toDouble)<GraphElemModule.nearNullTreshold ) {
            //println("deleting short line "+elRef+" "+p1+"-  "+p2)
            TransactionManager.tryDeleteInstance(elRef, owner,None)
          }
          else {
            p1=p1.alignValues match {
              case Some(newValue)=>TransactionManager.tryWriteInstanceField(elRef, 3, newValue);newValue
              case _=> p1
            }
            p2=p2.alignValues match {
              case Some(newValue)=>;TransactionManager.tryWriteInstanceField(elRef, 4, newValue);newValue
              case _=> p2
            }
            val line=new LineElement(elRef,elData.fieldValue.head.toInt,elData.fieldValue(1).toInt,
                elData.fieldValue(2).toInt,p1,p2)
            val keyVect=if(p1.x==p2.x) vertVect else if(p1.y==p2.y) horVect else if(p1.x<p2.x) (p2-p1).unit else (p1-p2).unit
            linesMap.getOrElseUpdate(keyVect,new ArrayBuffer[LineElement])+=line
          }
				case TypeInfos.arcElemType =>
					if(Math.abs(elData.fieldValue(4).toDouble)<GraphElemModule.nearNullTreshold){
            //println("delete Arc:"+elRef+" diameter:"+elData.fieldValue(4).toDouble)
            TransactionManager.tryDeleteInstance(elRef,owner,None)
            }
          else {
          val sa=elData.fieldValue(5).toDouble
          if(sa< -360d||sa> 360){
            //println("fix arc"+elRef+" startangle:"+sa+" ")
            TransactionManager.tryWriteInstanceField(elRef, 5, new DoubleConstant(sa%360d))
          }
          val se=elData.fieldValue(6).toDouble
          if(se< -360d||se> 360){
            //println("fix arc"+elRef+" endangle:"+se+" ")
            TransactionManager.tryWriteInstanceField(elRef, 6, new DoubleConstant(se%360d))
          }
          }
				case TypeInfos.polyElemType =>
					val poly=elData.fieldValue(3).toPolygon
					if(Math.abs(poly.getAreaValue)<GraphElemModule.nearNullTreshold){
            //println("delete Poly:"+elRef)
            TransactionManager.tryDeleteInstance(elRef,owner,None)
          }
				case TypeInfos.textElemType=>
					val tx=elData.fieldValue(1).toString
					if(tx.trim.isEmpty) {
            //println("deleteText:"+elRef)
            TransactionManager.tryDeleteInstance(elRef,owner,None)
          }
				case _=>
		  }
		} 
		// check overlapping lines
	  for((dirVector,lineList)<-linesMap){ // iterate over groups of parallel lines
	    //println("\n\n lines parallel to "+dirVector+" :")
	    val commonRayMap=collection.mutable.HashMap[LineElement,ArrayBuffer[LineElement]]()
	    for(line<-lineList)
	      commonRayMap.iterator.find{case(key,list)=> line.onSameRayWith(key)} match {
	        case Some((key,list))=>list+=line 
	        case _=> commonRayMap(line)=ArrayBuffer(line)
	      }
	    for((key,lList)<-commonRayMap){
	      //println("\nLines on same Ray with "+key+" :")
	      //println(lList.mkString(" , "))
	      val changeList=checkOverlappingLines(lList)
	      //rintln("changed:"+changeList.mkString(", "))	 
	      for(ch<-changeList) ch match {
	        case Delete(ref)=> TransactionManager.tryDeleteInstance(ref,owner,None)
	        case Change(newLine)=>
						TransactionManager.tryWriteInstanceField(newLine.ref, 3, newLine.startPoint )
						TransactionManager.tryWriteInstanceField(newLine.ref, 4, newLine.endPoint )
					case _=>
	      }
	    }
	      
	  }		  
		true
	}
	
	def isLineDeleted(line:LineElement)=line.ref.typ==0
	
	/** combines the overlap check between all lines from the list
	 *  @return true if some lines were changed
	 * 
	 */
	def checkOverlappingLines(rayList:ArrayBuffer[LineElement]):List[ChangeInfo]= {
	  //var changed=false
	  if(rayList.size<2) Nil
    else {
      val lineBuffer = ArrayBuffer[LineElement]()
      var changeList: List[ChangeInfo] = Nil
      for (checkLine <- rayList) {
        var theLine = checkLine
        for (ix <- lineBuffer.indices; aLine = lineBuffer(ix)
             if !isLineDeleted(aLine) && !isLineDeleted(theLine) && theLine.rangeOverlaps(aLine)) {
          val (res1, res2) = handleOverlap(aLine, theLine)
          res1 match {
            case Delete(ref) => lineBuffer(ix) = GraphElemConst.DELETED_LINE; changeList = res1 :: changeList
            case Change(line) => lineBuffer(ix) = line; changeList = res1 :: changeList
            case _ =>
          }
          res2 match {
            case Delete(ref) => theLine = GraphElemConst.DELETED_LINE; changeList = res2 :: changeList
            case Change(line) => theLine = line; changeList = res2 :: changeList
            case _ =>
          }
        }
        if (!isLineDeleted(theLine)) lineBuffer += theLine else {util.Log.w("  CheckLine deleted:" + checkLine);}
      }
      changeList.reverse
    }
	}
	
	private def handleOverlap(bufferLine:LineElement, checkLine:LineElement):(ChangeInfo,ChangeInfo)= {
	  if(bufferLine.lineStyle ==checkLine.lineStyle && bufferLine.lineWidth ==checkLine.lineWidth && 
	      bufferLine.color ==checkLine.color) // verschmelzen
	    return(Delete(bufferLine.ref),Change(checkLine.copy(
	        nstartPoint=VectorConstant.min(bufferLine.minPoint,checkLine.minPoint),
	        nendPoint=VectorConstant.max(bufferLine.maxPoint,checkLine.maxPoint))) )	        
	  if(bufferLine.lineStyle ==0&& checkLine.lineWidth <= bufferLine.lineWidth ) 
	    (NoChange,cutLine(checkLine,bufferLine))
	  else if(checkLine.lineStyle ==0&& bufferLine.lineWidth <= checkLine.lineWidth ) 
	    (cutLine(bufferLine,checkLine),NoChange)
	  else (NoChange,NoChange)
	}
	
	/** cuts l1 by l2
	 *  @return the reduced l1 
	 */
	private def cutLine(l1:LineElement,l2:LineElement):ChangeInfo={
	  val exceedMin=l1.minPoint<l2.minPoint
	  val exceedMax=l1.maxPoint>l2.maxPoint
	  if(exceedMin&&exceedMax)  NoChange // cut line exceeds on both ends over measure line, keep it
    else {
      if (exceedMin) createChange(l1, l1.minPoint, l2.minPoint)
      else if (exceedMax) createChange(l1, l2.maxPoint, l1.maxPoint)
      else Delete(l1.ref) // no exceed = lines are the same
    }
	}
	
	private def createChange(old:LineElement,startPoint:VectorConstant,endPoint:VectorConstant)= {
	  if(old.startPoint ==startPoint&&old.endPoint==endPoint) NoChange else Change(old.copy(nstartPoint=startPoint,nendPoint=endPoint))
	}
	
	
	
}