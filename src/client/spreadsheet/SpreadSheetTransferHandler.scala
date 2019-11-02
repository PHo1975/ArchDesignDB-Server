package client.spreadsheet
import java.awt.datatransfer.{DataFlavor, Transferable, UnsupportedFlavorException}
import java.nio.charset.Charset

import client.comm.ClientQueryManager
import client.dataviewer.{InstanceSelection, TableHeaderMap}
import definition.data.{EMPTY_OWNERREF, Reference, ResultElement}
import definition.expression.{EMPTY_EX, Expression, IntConstant, Variable}
import javax.swing.{JComponent, JTable, TransferHandler}

import scala.swing.Swing

class SpreadSheetTransferHandler(controller:SpreadSheetController) extends TransferHandler {
  var isExporting:Boolean=false
  var selfImportBounds:Option[(Int, Int)]=None
  
  override def getSourceActions(c:JComponent):Int ={ TransferHandler.COPY_OR_MOVE +TransferHandler.LINK}  	    
  
  override def createTransferable(c:JComponent ): SpreadSheetTransferable = {
    isExporting=true
    selfImportBounds=None
    SpreadSheetTransferable(controller)
  }
  
  override def canImport(support: TransferHandler.TransferSupport):Boolean = {
    support.getDropLocation match {
      case tabLoc:JTable.DropLocation =>
        val tabLoc=support.getDropLocation.asInstanceOf[JTable.DropLocation]
        if( tabLoc.getColumn>controller.tableModel.getColumnCount-2)  false
else {
val flavors = support.getDataFlavors
support.isDataFlavorSupported(SpreadSheetTransferable.flavor) ||
support.isDataFlavorSupported(InstanceSelection.flavor) ||
support.isDataFlavorSupported(SpreadSheetTransferable.htmlFlavor) ||
support.isDataFlavorSupported(SpreadSheetTransferable.plainFlavor)
}
      case _=> false
    }
  }
  
  private def addMissingCols(maxColNr:Int): Unit = if(maxColNr>controller.tableModel.getColumnCount-2){
  	 val startCol=controller.tableModel.getColumnCount-2  		      	
  	 for(i <-0 until (maxColNr-(controller.tableModel.getColumnCount-2)))
  		 controller.tableModel.createColumnData(startCol+i,SpreadSheet.defaultColumnWidth)
   } 
  
  
  override def importData(info:TransferHandler.TransferSupport):Boolean  = {
    if (!info.isDrop) false else {
      //println("import Data "+info.getDropAction())
      val tabLoc = info.getDropLocation.asInstanceOf[JTable.DropLocation]
      val dropCol = tabLoc.getColumn
      if (dropCol < 0) return false
      val dropRow = tabLoc.getRow
      //val action = info.getDropAction
      val flavors = info.getDataFlavors

      if (flavors.contains(SpreadSheetTransferable.flavor)) {
        val data = info.getTransferable.getTransferData(SpreadSheetTransferable.flavor).asInstanceOf[SpreadSheetTransferable]
        if (isExporting) {
          //println("Self import ")
          selfImportBounds = Some((dropCol, dropRow))
        }
        if (data.numCols + dropCol > controller.tableModel.getColumnCount - 2) {
          val startCol = controller.tableModel.getColumnCount - 2
          //println("create missing cols "+(data.numCols + dropCol-(controller.tableModel.getColumnCount-2))+" from "+startCol)
          for (i <- 0 until (data.numCols + dropCol - (controller.tableModel.getColumnCount - 2)))
            controller.tableModel.createColumnData(startCol + i, SpreadSheet.defaultColumnWidth)
        }
        val deltaX = dropCol - data.startCol
        val deltaY = dropRow - data.startRow
        //println("deltax:"+deltaX+ " deltaY:"+deltaY)

        ClientQueryManager.executeAction(EMPTY_OWNERREF, List(controller.spreadSheetRef), "insertCells",
          Seq(ResultElement("startCol", IntConstant(dropCol)), ResultElement("endCol", IntConstant(dropCol + data.numCols - 1)),
            ResultElement("startRow", IntConstant(dropRow)), ResultElement("endRow", IntConstant(dropRow + data.numRows - 1)), ResultElement("dx:", IntConstant(deltaX)), ResultElement("dy:", IntConstant(deltaY))) ++
            (data.cellValues map {
              case Some(CellTuple(exp, form)) => ResultElement(exp.getTerm, EMPTY_EX)
              case None => ResultElement("", EMPTY_EX)
            }))
        Swing.onEDT(controller.tableModel.fireTableStructureChanged())
        true
      }
      else if (flavors.contains(InstanceSelection.flavor)) {
        info.getTransferable.getTransferData(InstanceSelection.flavor) match {
          case data: InstanceSelection => if (data.selection.nonEmpty) {
            val colModel = TableHeaderMap.getColumnModel(data.selection.head.typ)
            val numCols = colModel.getColumnCount - 1
            addMissingCols(dropCol + numCols - 1)
            ClientQueryManager.runInPool {
              val lines = for (ref <- data.selection; instData = ClientQueryManager.queryInstance(ref, -1).head;
                               colIx <- 0 until numCols; modelIndex = colModel.getColumn(colIx + 1).getModelIndex)
              yield ResultElement(instData.fieldValue(modelIndex - 1).toString, EMPTY_EX)
              println("Lines:" + lines.mkString(" | "))
              ClientQueryManager.executeAction(EMPTY_OWNERREF, List(controller.spreadSheetRef), "insertCells",
                Seq(ResultElement("startCol", IntConstant(dropCol)),
                  ResultElement("endCol", IntConstant(dropCol + numCols - 1)), ResultElement("startRow", IntConstant(dropRow)), ResultElement("endRow", IntConstant(dropRow + data.selection.length - 1)),
                  ResultElement("dx:", IntConstant(0)), ResultElement("dy:", IntConstant(0))) ++ lines)
            }
          }
          case other => println(other.getClass)
        }
        true
      }
      else if (flavors.contains(DataFlavor.stringFlavor)) {
        info.getTransferable.getTransferData(SpreadSheetTransferable.plainFlavor) match {
          case buffer: Array[Byte] =>
            val inString = new String(buffer, "UTF-8")
            val lines: Array[Array[String]] = inString.split("\n").map(_.split("\t"))
            val maxCols = lines.maxBy(_.length).length
            addMissingCols(maxCols + dropCol)
            //println("importiere CSV spalten:"+maxCols+" zeilen:"+lines.size)
            val fullData: Array[ResultElement] = lines.flatMap(line => line.padTo(maxCols, "")).map(ResultElement(_,EMPTY_EX))
            ClientQueryManager.executeAction(EMPTY_OWNERREF, List(controller.spreadSheetRef), "insertCells",
              Seq(ResultElement("startCol", IntConstant(dropCol)), ResultElement("endCol", IntConstant(dropCol + maxCols - 1)),
                ResultElement("startRow", IntConstant(dropRow)), ResultElement("endRow", IntConstant(dropRow + lines.length - 1)),
                ResultElement ("dx:", IntConstant(0)),ResultElement ("dy:", IntConstant(0))) ++ fullData)
          case other => println(other.getClass)
        }
        true
      } else false
    }
  }

  override def exportDone(source:JComponent,trans: Transferable,action:Int):Unit = {
    //println("Export Done "+action+" source:"+source.getClass()+" trans:"+trans)
    action match {
      case TransferHandler.COPY => //System.out.println("Source Copied")
      case TransferHandler.MOVE =>
        trans match {
          case data:SpreadSheetTransferable=>
            val cellList= selfImportBounds match {
              case Some((impStartCol,impStartRow))=>
                (for(col<-data.startCol until data.startCol+data.numCols;row<-data.startRow until data.startRow+data.numRows
                     if (col < impStartCol || col >= impStartCol + data.numCols) ||
                       row < impStartRow || row >= impStartRow + data.numRows) yield
                  controller.tableModel.getCell(col,row) map(_.ref)).flatten

              case _ => (for(ri<-0  until data.numRows;ci<-0 until data.numCols) yield
                controller.tableModel.getCell(ci+data.startCol,ri+data.startRow) map(_.ref)).flatten
            }
            if(cellList.nonEmpty)ClientQueryManager.executeAction(EMPTY_OWNERREF,cellList,"deleteCells",Seq.empty )
          case o=> util.Log.e("unknown Transferable "+o)
        }
      //System.out.println("Source Moved")
      case TransferHandler.LINK => // System.out.println("Source Linked")
      case TransferHandler.NONE => //System.out.println("no action")
    }
  }
  isExporting=false
}



@SerialVersionUID(15561L) case class CellTuple(value:Expression,format:SpreadSheetFormat)



@SerialVersionUID(14277L) 
class SpreadSheetTransferable(val ssRef:Reference,val startCol:Int,val startRow:Int,val numCols:Int,val numRows:Int,val cellValues:Array[Option[CellTuple]]) 
  extends Transferable with Serializable {
  
  def getTransferData(flavor:DataFlavor ) = {
		if(flavor.equals(SpreadSheetTransferable.flavor)) this
		//else if (flavor.equals(SpreadSheetTransferable.flavors(2))) 
	    // toString	
	     else if(flavor.equals(SpreadSheetTransferable.plainFlavor))
	       toCSV.getBytes(Charset.forName("UTF-8"))
	 else  throw new UnsupportedFlavorException(flavor)
  }
	
	
	def getTransferDataFlavors: Array[DataFlavor] = {
		SpreadSheetTransferable.flavors
	}
	
	def isDataFlavorSupported(flavor:DataFlavor):Boolean ={
	  //println("is flavor supported "+flavor.getHumanPresentableName()+" mime:"+flavor.getMimeType()+" prim: "+flavor.getPrimaryType()+" sub:"+flavor.getSubType()+" "+flavor.getRepresentationClass())
	  SpreadSheetTransferable.flavors.contains(flavor)} 		
	
  override def toString: String = {val ret= (for(row<-0 until numRows) yield {
      (  for(col<-0 until numCols) yield cellValues(row*numCols+col) match {
          	case Some(CellTuple(expression,format))=>expression match {
          	  case v:Variable if v.module == SpreadSheet.spreadSheetModulName =>
          	  case o=>o.getValue.toString
          	}
          	case _=> ""
          }
       ).mkString("\t")       
   }).mkString("\r\n")
   //println("toString called")
   ret
   }
  
   def toCSV: String = (for(row<-0 until numRows) yield {
      (  for(col<-0 until numCols) yield cellValues(row*numCols+col) match {
          	case Some(CellTuple(expression,format))=>'"'+expression.getValue.toString+'"'
          	case _=> "\"\""
          }
       ).mkString(";")       
   }).mkString("\n")   
  
}



object SpreadSheetTransferable {
  def apply(controller:SpreadSheetController): SpreadSheetTransferable = {
    val selection=controller.selection
    val(startCol,startRow,numCols,numRows)=selection match {
      case All_SELECTION=>(0,0,controller.tableModel.getColumnCount-2,controller.tableModel.getRowCount-SpreadSheet.rowOverhead)
      case NO_SELECTION=>(0,0,0,0)
      case RowsSelection(rows)=>(0,rows.start,controller.tableModel.getColumnCount-2,rows.size)
      case ColsSelection(cols)=>(cols.start,0,cols.size,controller.tableModel.getRowCount-SpreadSheet.rowOverhead)
      case RangeSelection(cols,rows)=>(cols.start,rows.start,cols.size,rows.size)
      case SingleCellSelection(col,row)=>(col,row,1,1)
    }
    val cellValues=for(row<-0 until numRows;col<-0 until numCols) 
      yield controller.tableModel.getCell(col+startCol,row+startRow) map(cellValue=>
        CellTuple(controller.replaceFieldReferences(cellValue.exp,allFieldRefs = true),controller.formatList.getCommonFormatForCell(col,row)))
    //println("startCol:"+startCol+" startRow:"+startRow+" numCols:"+numCols+" numRows:"+numRows)
    //println("Cells= "+cellValues.mkString(", "))
    new SpreadSheetTransferable(controller.spreadSheetRef,startCol,startRow,numCols,numRows,cellValues.toArray)
  }
  
  val flavor= new DataFlavor(classOf[SpreadSheetTransferable],"SpreadSheetCells")
	
  val plainFlavor=new DataFlavor("text/plain; charset=UTF-8; class=\"[B\"", "Plain Text")
  
  val htmlFlavor=new DataFlavor("text/html; charset=UTF-8; class=\"[B\"", "HTML Text")
  
	val flavors= Array(flavor,plainFlavor/*,DataFlavor.stringFlavor*/)
}