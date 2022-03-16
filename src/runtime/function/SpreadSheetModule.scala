package runtime.function

import client.spreadsheet._
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.{Constant, EMPTY_EX, FieldReference, IntConstant, StringConstant}
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule}
import transaction.handling.{ActionList, TransactionManager}
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

class MyActionIterator(nname:String,nfunc:(AbstractUserSocket,OwnerReference,Iterable[InstanceData],Iterable[(String,Constant)]) => Boolean) extends
  ActionIterator(nname,None,nfunc)  {
    override def hiddenFromPanel=true
  }

class SpreadSheetModule extends ActionModule{
  import runtime.function.SpreadSheetProxy._
  val deleteCellsAction=new MyActionIterator("deleteCells",doDelete)  
  val addColumnsAction=new MyActionIterator("addColumns",doAddColumns)
  val removeColumnsAction=new MyActionIterator("removeColumns",doRemoveColumns)
  val addRowsAction=new MyActionIterator("addRows",doAddRows)
  val removeRowsAction=new MyActionIterator("removeRows",doRemoveRows)
  val insertCellsAction=new MyActionIterator("insertCells",doInsertCells)
  val setResultCellAction=new MyActionIterator("setResultCell",doSetResultCell)
  val cleanupAction=new ActionIterator("Aufräumen",None,doCleanUp)
  var theTypeID:Int= -1
   val actions = List(cleanupAction,deleteCellsAction,addColumnsAction,removeColumnsAction,addRowsAction,removeRowsAction,insertCellsAction,
       setResultCellAction)
  
  def doDelete(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean = {
    SpreadSheetProxy.deleteSpreadSheetCells(data)
  	true
  }
  
  def doAddColumns(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean = if(param.size==2){
    val startCol=param.head._2.toInt
    val endCol=param.last._2.toInt
    val range=startCol to endCol
    val size=endCol-startCol+1
    val proxy=new SpreadSheetProxy(data.head.ref)
    val oldColumnData=proxy.createColumnDataTree.toMap
    
    proxy.iterateCells( (col,row)=> {
          if(col<startCol) NoChange
          else ChangeCol(col+size)
    })          
    proxy.iterateRanges {
      case ColsSelection(oldCols) => insertRange(oldCols, range, asCol = true)
      case RangeSelection(oldCols, _) => insertRange(oldCols, range, asCol = true)

      case SingleCellSelection(col, _) => if (col >= startCol) ChangeCols(Range(col + size, col + size))
      else NoChange
      case _ => NoChange
    }
    
    for(col<-startCol to endCol){    
          val newInst=TransactionManager.tryCreateInstance(SpreadSheetProxy.SSColumnType,proxy.colDataOwnerArray,notifyRefandColl = false, -1)
          TransactionManager.tryWriteInstanceField(newInst.ref,0,IntConstant(col))
          if(oldColumnData.contains(col))
            TransactionManager.tryWriteInstanceField(newInst.ref,2,oldColumnData(col).fieldValue(2))        
    }
  	true
  } else false
  
  def doRemoveColumns(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean = if(param.size==2){
    val startCol=param.head._2.toInt
    val endCol=param.last._2.toInt
    val range=startCol to endCol
    val size=endCol-startCol+1
    //println("remove column start"+startCol+" end:"+endCol+" size:"+size)
    val proxy=new SpreadSheetProxy(data.head.ref)
    proxy.iterateCells((col,row)=> {
          if(col<startCol) NoChange
          else if(col<=endCol) Delete
          else ChangeCol(col-size)
        })
    proxy.iterateRanges {
      case ColsSelection(oldCols) => removeRange(oldCols, range, asCol = true)
      case RangeSelection(oldCols, _) => removeRange(oldCols, range, asCol = true)
      case SingleCellSelection(col, _) => if (col >= startCol) {
        if (col <= endCol) Delete
        else ChangeCols(Range(col - size, col - size))
      } else NoChange
      case _ => NoChange
    }
  	true
  } else false
  
  def doAddRows(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean = if(param.size==2){
    val startRow=param.head._2.toInt
    val endRow=param.last._2.toInt
    val range=startRow to endRow
    val size=endRow-startRow+1
    val proxy=new SpreadSheetProxy(data.head.ref)
    proxy.iterateCells((col,row)=> {
          if(row < startRow) NoChange
          else ChangeRow(row+size)
        })
    proxy.iterateRanges {
      case RowsSelection(oldRows) => insertRange(oldRows, range, asCol = false)
      case RangeSelection(_, oldRows) => insertRange(oldRows, range, asCol = false)
      case SingleCellSelection(_, row) => if (row >= startRow) ChangeRows(Range(row + size, row + size))
      else NoChange
      case _ => NoChange
    }
  	true
  } else false
  
  def doRemoveRows(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean = {
     val startRow=param.head._2.toInt
    val endRow=param.last._2.toInt
    val range=startRow to endRow
    val size=endRow-startRow+1
    val proxy=new SpreadSheetProxy(data.head.ref)
     proxy.iterateCells((col,row)=> {
          if(row<startRow) NoChange
          else if(row<=endRow) Delete
          else ChangeRow(row-size)
        })
        proxy.iterateRanges {
          case RowsSelection(oldRows) => removeRange(oldRows, range, asCol = false)
          case RangeSelection(_, oldRows) => removeRange(oldRows, range, asCol = false)
          case SingleCellSelection(_, row) => if (row >= startRow) if (row <= endRow) Delete
          else ChangeRows(Range(row - size, row - size))
          else NoChange
          case _ => NoChange
        }
  	true
  }
  
  
  def doInsertCells(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],para:Iterable[(String,Constant)]):Boolean = {
    val param=para.toSeq
    val proxy=new SpreadSheetProxy(data.head.ref)
    val startCol=param.head._2.toInt
    val endCol=param(1)._2.toInt
    val startRow=param(2)._2.toInt
    val endRow=param(3)._2.toInt
    val numCols=endCol-startCol+1
    val deltaX=param(4)._2.toInt
    val deltaY=param(5)._2.toInt    
    val cellMap=collection.mutable.HashMap[(Int,Int),InstanceData]()
    var setInMap=false    
    val aquireFunc:(Int,Int)=>InstanceData= if(param.size<16)proxy.aquireCellAt else {
      setInMap=true
      proxy.cellRefs match {
        case Some(refList)=> cellMap++= refList.map(ref=>{
          val inst=proxy.getInst(ref)
          ((proxy.colFieldValue(inst),proxy.rowFieldValue(inst)),inst)
         }).toMap
        case _=>
      }
      def myFunc(a:Int,b:Int):InstanceData= cellMap.getOrElseUpdate((a,b),proxy.createCellAt(a,b))        
      
      myFunc
    }    
    // loop all cells
    for(row<-startRow to endRow;col<-startCol to endCol;
    newValString=param(col-startCol+(row-startRow)*numCols +6)._1
        if newValString.nonEmpty) {
      //print("col:"+col+" row:"+row+" valstring:"+newValString)
      val instance=aquireFunc(col,row)
      val oldCollFuncs=ArrayBuffer[InstanceData]()++proxy.findCollFuncsFor(instance.ref)
    	val expression=try {parser.parse(newValString) } catch {case NonFatal(_)=>StringConstant(newValString)}
    	val replacedExpression=expression.replaceExpression {
        case v: SSVariable => proxy.resolveVariable(v, deltaX, deltaY, aquireFunc)
        case s: SSCollProxy => proxy.resolveCollFunc(instance.ref, s.delta(deltaX, deltaY), oldCollFuncs)
        case o => o
      }
    	//println(" Ex:"+expression+" "+instance.ref)
    	for (o<-oldCollFuncs)
    	  TransactionManager.tryDeleteInstance(o.ref,None,None)
    	TransactionManager.tryWriteInstanceField(instance.ref,2,replacedExpression)
    	if(setInMap) cellMap((col,row))=instance
    }
  	true
  }
  
  
  def doSetResultCell(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean = if(param.size==2){
    val proxy=new SpreadSheetProxy(data.head.ref)
    val col=param.head._2.toInt
    val row=param.last._2.toInt
    val targetCell=proxy.aquireCellAt(col,row)    
	  TransactionManager.tryWriteInstanceField(data.head.ref,0,
	      new FieldReference(Some(SSDoubleCellType),Some(targetCell.ref.instance),2.toByte))
    true
  } else false
  
  
  
  def setObjectType(typeID:Int): Unit = theTypeID=typeID
  
  protected def insertRange(oldRange:Range,newRange:Range,asCol:Boolean):RangeIteratorResult=if(oldRange.end >=newRange.start) {
      val range= if(oldRange.start<newRange.start) Range(oldRange.start,oldRange.end+newRange.size)
      else Range(oldRange.start+newRange.size,oldRange.end+newRange.size)
      if(asCol)ChangeCols(range)else ChangeRows(range)
    }
    else NoChange

   protected def removeRange(oldRange:Range,newRange:Range,asCol:Boolean):RangeIteratorResult=if(oldRange.end >=newRange.start) {
    //print("RemoveRange oldRange:"+oldRange+" newRange:"+newRange+" asCol:"+asCol)
    val range=if(oldRange.end<newRange.end){
      if(oldRange.start>=newRange.start) return Delete
      else{Range(oldRange.start,newRange.start-1)}
    }
    else if(oldRange.start<newRange.start) {Range(oldRange.start,oldRange.end-newRange.size)}
    else {Range(if(oldRange.start<newRange.end)newRange.start else oldRange.start-newRange.size,oldRange.end-newRange.size)}
    if(asCol)ChangeCols(range)else ChangeRows(range)  
  } else NoChange


  def doCleanUp(u:AbstractUserSocket,parent:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean = {
    val proxy=new SpreadSheetProxy(data.head.ref)
    Log.e("cleanup:")
    for(cr<-proxy.cellRefs;cellRef<-cr){
      ActionList.getReferencingLinks(cellRef) match {
        case None =>
        case Some(rl) =>
          val cell = proxy.getInst(cellRef)
          if(rl.links.size==1 && rl.links.head._2.isEmpty) {
            Log.e("Empty Cell " + cellRef + " " + cell.fieldValue(0).toInt + "," + cell.fieldValue(1).toInt + " > " + cell.fieldData(2)+" rl:"+rl.links.mkString("| "))
            TransactionManager.tryDeleteInstance(cellRef,None,None)
          }

      }
    }
    true
  }
}




object Inst{
  def unapply(ref:Reference): Option[InstanceData] =Some(ActionList.getInstanceData(ref))
}

