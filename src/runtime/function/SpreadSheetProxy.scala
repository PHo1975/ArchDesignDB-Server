package runtime.function
import definition.expression.FieldReference
import client.spreadsheet.All_SELECTION
import client.spreadsheet.RangeSelection
import transaction.handling.TransactionManager
import scala.collection.mutable.ArrayBuffer
import transaction.handling.ActionList
import client.spreadsheet.ColsSelection
import client.spreadsheet.SSCollProxy
import client.spreadsheet.RowsSelection
import client.spreadsheet.NO_SELECTION
import definition.data.OwnerReference
import client.spreadsheet.SpreadSheetUtil
import definition.data.InstanceData
import client.spreadsheet.SSVariable
import client.spreadsheet.SingleCellSelection
import definition.expression.StringConstant
import definition.expression.Expression
import definition.data.Reference
import client.spreadsheet.SpreadSheetRange
import definition.expression.CollectingFuncCall
import definition.expression.IntConstant
import definition.expression.ObjectReference
import client.spreadsheet.SpreadSheetParser
import definition.expression.EMPTY_EX
import client.spreadsheet.CellIteratorResult
import client.spreadsheet.{Delete,ChangeRow,ChangeCol,NoChange,ChangeRows,ChangeCols,RangeIteratorResult}
import client.spreadsheet.SpreadSheetFormatRange

class SpreadSheetProxy(val ref:Reference) {
  import runtime.function.SpreadSheetProxy._
  lazy val cellOwnerRef=new OwnerReference(4,ref)
  lazy val cellOwnerArray=Array(cellOwnerRef)
  lazy val collOwnerRef=new OwnerReference(5,ref)
  lazy val collOwnerArray=Array(collOwnerRef)
  lazy val colDataOwnerRef=new OwnerReference(1,ref)
  lazy val colDataOwnerArray=Array(colDataOwnerRef)
  
  
  def cellRefs= ActionList.getInstanceProperties(ref).map(_.propertyFields(4).propertyList)
  def colDataRefs= ActionList.getInstanceProperties(ref).map(_.propertyFields(1).propertyList)
  def collFuncRefs=ActionList.getInstanceProperties(ref).map(_.propertyFields(5).propertyList)
  def formatRefs=ActionList.getInstanceProperties(ref).map(_.propertyFields(3).propertyList)
  def formats= formatRefs match {
    case Some(list)=> for (fref<-list;data=ActionList.getInstanceData(fref)) yield{  
       new SpreadSheetFormatRange(data)
     }
    case None=>IndexedSeq.empty
  }
  
  
  def getInst(ref:Reference)=ActionList.getInstanceData(ref)
  def colFieldValue(data:InstanceData)= data.fieldValue.head.toInt
  def rowFieldValue(data:InstanceData)= data.fieldValue(1).toInt
  def targetFieldValue(data:InstanceData)=data.fieldValue.head.toObjectReference
 
  def findCellValueAt(col:Int,row:Int):Option[InstanceData]= cellRefs flatMap(
      _.collectFirst({case Inst(a) if colFieldValue(a) == col && rowFieldValue(a) == row =>a}))
  
        
  def findCollFuncsFor(targetRef:Reference) = collFuncRefs match {
    case Some(list)=> list.collect({case Inst(a) if targetFieldValue(a) == targetRef =>a})
    case None=> Seq.empty
  }  
      
  
  def createCellAt(col:Int,row:Int):InstanceData= {
    val inst=TransactionManager.tryCreateInstance(SSDoubleCellType,cellOwnerArray,false,-1,true,false).
    setField(0,new IntConstant(col)).setField(1,new IntConstant(row))    
    TransactionManager.tryWriteInstanceData(inst)
    
    collFuncRefs match {
      case Some(list)=>for(collRef<-list) 
        if(collIsDefinedAt(getInst(collRef),col,row))
          TransactionManager.trySecondUseInstances(List(inst.ref),cellOwnerRef,new OwnerReference(0.toByte,collRef),-1)
      case None =>
    }  
    
    inst
  } 
  
  def aquireCellAt(col:Int,row:Int):InstanceData=findCellValueAt(col,row) match {
    case Some(oldData)=> oldData
    case None=> createCellAt(col,row)
  }
  
  def resolveVariable(v:SSVariable,deltaX:Int,deltaY:Int,aquireFunc:(Int,Int)=>InstanceData=aquireCellAt):Expression={  
    val (col,row)= SpreadSheetUtil.lettersToCoords(v.name)	  
	  if(col+deltaX<0 ||row+deltaY<0) return new StringConstant("[negativer Index]")
    val targetCell=aquireCellAt(col+deltaX,row+deltaY)    
	  new FieldReference(Some(SSDoubleCellType),Some(targetCell.ref.instance),2.toByte)  	  
  }
  
  def resolveCollFunc(cellRef:Reference,s:SSCollProxy,oldCols:ArrayBuffer[InstanceData]):Expression= {
    oldCols.find(s.equalsData) match {
      case Some(oldData)=>
        oldCols-=oldData
        new FieldReference(Some(oldData.ref.typ),Some(oldData.ref.instance),1.toByte)
      case None=>
        var inst=TransactionManager.tryCreateInstance(SSCollDataType,collOwnerArray,true,-1)
        TransactionManager.tryWriteInstanceField(inst.ref,0.toByte,new ObjectReference(cellRef))
        TransactionManager.tryWriteInstanceField(inst.ref,1.toByte,new CollectingFuncCall(s.name,0.toByte,SSCellType,2.toByte))
        val rangeValues=s.range.toConstants
        for(i<-0 until 4)
          TransactionManager.tryWriteInstanceField(inst.ref,(2+i).toByte,rangeValues(i))
        val sourceCells=filterCells(s.range)
        // println("s.range:"+s.range)
        // println("CollRef Source cells :"+sourceCells.mkString(","))
        if(sourceCells.nonEmpty)
          TransactionManager.trySecondUseInstances(sourceCells,cellOwnerRef,new OwnerReference(0,inst.ref),-1)

        new FieldReference(Some(inst.ref.typ),Some(inst.ref.instance),1.toByte)
    }    
  }
  
  def filterCells(r:SpreadSheetRange):Iterable[Reference]= r match {
    case All_SELECTION => cellRefs.toSeq.flatten
    case NO_SELECTION=> Seq.empty
    case RowsSelection(rows)=>cellRefs match {
      case Some(list)=> list.filter(ref =>{rows.contains(rowFieldValue(getInst(ref))) })
      case None => Seq.empty
    }
    case ColsSelection(cols)=>cellRefs match {
      case Some(list)=> list.filter(ref =>{cols.contains(colFieldValue(getInst(ref))) })
      case None => Seq.empty
    }
    case RangeSelection(cols,rows)=>cellRefs match {
      case Some(list)=> list.filter(ref =>{val inst=getInst(ref)
        cols.contains(colFieldValue(inst)) && rows.contains(rowFieldValue(inst)) })
      case None => Seq.empty
    }
    case SingleCellSelection(col,row)=>cellRefs match {
      case Some(list)=> list.filter(ref =>{val inst=getInst(ref)
        col==colFieldValue(inst) && row==rowFieldValue(inst) })
      case None => Seq.empty
    }   
  }
  
  def iterateCells(iterator:(Int,Int)=>CellIteratorResult):Unit= {
    for(list<-cellRefs) {    	
    	  val deleteList=ArrayBuffer[InstanceData]()
    		for(ref<-list){    		  
    			val data=getInst(ref)      
    		  iterator(colFieldValue(data),rowFieldValue(data)) match {
    				case Delete => deleteList+=data
    				case ChangeRow(newRow)=> TransactionManager.tryWriteInstanceField(ref,1.toByte,new IntConstant(newRow))
    				case ChangeCol(newCol)=> TransactionManager.tryWriteInstanceField(ref,0.toByte,new IntConstant(newCol))
    				case NoChange =>
    			}
    		}
    	  if(deleteList.nonEmpty) deleteSpreadSheetCells(deleteList)
      }
    
    for((col,inst)<-createColumnDataTree) iterator(col,-1) match {
        case Delete => TransactionManager.tryDeleteInstance(inst.ref,None,None)
    		case ChangeRow(newRow) =>
      	case ChangeCol(newCol) => TransactionManager.tryWriteInstanceField(inst.ref,0.toByte,new IntConstant(newCol))
    		case NoChange =>
    }    
  }
  
  def createColumnDataTree=(for(list<-colDataRefs) yield 
      collection.immutable.TreeMap[Int,InstanceData]() ++ list.map(ref=> {
        val inst=getInst(ref); (inst.fieldValue.head.toInt,inst)})).toSeq.flatten
  
  def iterateCollRanges(iterator:(SpreadSheetRange)=>RangeIteratorResult):Unit={    
      for(list<-collFuncRefs;ref<-list){
        val data=getInst(ref)        
            iterator(SpreadSheetRange.apply(data.fieldValue(2).toInt,data.fieldValue(4).toInt,
            data.fieldValue(3).toInt,data.fieldValue(5).toInt)) match {
        	case Delete =>
            TransactionManager.tryWriteInstanceField(ref,1.toByte,EMPTY_EX)
            TransactionManager.tryWriteInstanceField(ref,2.toByte,new IntConstant(-1))
          case ChangeRows(newRows)=>
            TransactionManager.tryWriteInstanceField(ref,3.toByte,new IntConstant(newRows.start))
            TransactionManager.tryWriteInstanceField(ref,5.toByte,new IntConstant(newRows.end))
          case ChangeCols(newCols)=>
            TransactionManager.tryWriteInstanceField(ref,2.toByte,new IntConstant(newCols.start))
            TransactionManager.tryWriteInstanceField(ref,4.toByte,new IntConstant(newCols.end))
          case NoChange=>
        }
      }
  }
  
  def iterateFormatRanges(iterator:(SpreadSheetRange)=>RangeIteratorResult):Unit={ 
    for(list<-formatRefs;ref<-list;data=getInst(ref)){
      val range=SpreadSheetRange(data.fieldValue.head.toInt,data.fieldValue(2).toInt,
          data.fieldValue(1).toInt,data.fieldValue(3).toInt)
      iterator (range) match {
       case ChangeRows(newRows)=>range match {
	        case RowsSelection(_)|RangeSelection(_,_)=> TransactionManager.tryWriteInstanceField(ref,1,IntConstant(newRows.start))
            TransactionManager.tryWriteInstanceField(ref,3,IntConstant(newRows.end))
          case SingleCellSelection(_,_)=>
            val value=IntConstant(newRows.start)
            TransactionManager.tryWriteInstanceField(ref,1,value)
            TransactionManager.tryWriteInstanceField(ref,3,value)
          case _=>
      }
      case ChangeCols(newCols)=>range match {
	        case ColsSelection(_)|RangeSelection(_,_)=>  TransactionManager.tryWriteInstanceField(ref,0,IntConstant(newCols.start))
	        												TransactionManager.tryWriteInstanceField(ref,2,IntConstant(newCols.end))
	        case SingleCellSelection(_,_)=>
            val value=IntConstant(newCols.start)
            TransactionManager.tryWriteInstanceField(ref,0,value)
            TransactionManager.tryWriteInstanceField(ref,2,value)
          case _=>
      }
      case Delete =>TransactionManager.tryDeleteInstance(ref,None,None) 
      
      case NoChange =>
      }  
    }
  }  
  
  def iterateRanges(iterator:(SpreadSheetRange)=>RangeIteratorResult) :Unit  = {
     iterateCollRanges(iterator) 
     iterateFormatRanges(iterator)
  }  
  
  def collIsDefinedAt(collData:InstanceData,col:Int,row:Int):Boolean= {
    val startCol=collData.fieldValue(2).toInt
    val endCol=collData.fieldValue(4).toInt
    val startRow=collData.fieldValue(3).toInt
    val endRow=collData.fieldValue(5).toInt
    startCol<=col&&col<=endCol&&startRow<=row&&row<=endRow
  }  
}


object SpreadSheetProxy {
  val parser=new SpreadSheetParser
  val SSCellType=505
  val SSColumnType=503
  val SSDoubleCellType=506 
  val SSFormatType=510
  val SSCollDataType=511
  
  lazy val nullString= new StringConstant("")
  
  def deleteSpreadSheetCells(data:Seq[InstanceData]):Unit= {
    if(data.size==0) return
    // assumption that all data are in the same spread sheet    
    val proxy=new SpreadSheetProxy(data.head.owners.head.ownerRef)
    val collFuncList=proxy.collFuncRefs.toSeq.flatten.map(proxy.getInst)
    for(inst <-data){      
      // delete collFuncData that shows to this cell      
      collFuncList.filter(_.fieldValue.head.toObjectReference==inst.ref).foreach(i=>TransactionManager.tryDeleteInstance(i.ref,None,None))
      // check if there are ReferencingLinks to this cell
      if(ActionList.getReferencingLinks(inst.ref) match {			
	  		case Some(refLinks) => refLinks.links.exists(_._2.exists(!_.isParentRef))  		
	  		case None => false
      }) {
        TransactionManager.tryWriteInstanceField(inst.ref, 2.toByte,nullString)
      }
  		// delete also second uses of the selected cells
      else TransactionManager.tryDeleteInstance(inst.ref,None,None)
  	} 
  }
  
}