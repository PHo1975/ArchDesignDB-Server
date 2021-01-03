package client.spreadsheet

import client.comm._
import client.dialog.Toast
import client.ui.{ClientApp, ViewConstants}
import definition.comm.NotificationType
import definition.data.{EMPTY_OWNERREF, InstanceData, Reference, ResultElement}
import definition.expression.{CollectingFuncCall, EMPTY_EX, Expression, IntConstant}
import util.Log

import javax.swing.event.TableModelEvent
import javax.swing.table.{AbstractTableModel, DefaultTableColumnModel, TableColumn}
import javax.swing.{JComponent, JTextArea}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

class SpreadSheetTableModel(controller:SpreadSheetController) extends AbstractTableModel {

  
  class MyTableColumn(modelIndex:Int,startWidth:Int) extends TableColumn(modelIndex) {
    var widthChanged=false
    super.setWidth(startWidth)

    override def setWidth(nWidth: Int): Unit = if (controller.table.peer.isValid) {
      super.setWidth(nWidth)
      widthChanged=true
    } //else println("want set Width but not valid "+modelIndex)
    def initWidth(nWidth: Int): Unit = {
      super.setWidth(nWidth)
      widthChanged=false
    }
  }
  
  
  
  class MyColumnModel extends DefaultTableColumnModel{
    def clear(): Unit = tableColumns.clear()

    def insertAt(col: TableColumn, pos: Int): Unit = tableColumns.add(pos, col)

    def getMyColumn(i: Int): MyTableColumn = getColumn(i).asInstanceOf[MyTableColumn]

    def hasWidthChanged: Boolean = this.getColumns.asScala.exists {
      case mc: MyTableColumn => mc.widthChanged;
      case _ => false
    }
  } 
  
  val columnModel=new MyColumnModel
  
  
  val columnData=new TreeMapDataModel[Int,SpreadSheetColumnData](){
    def elemFactory(data:InstanceData)=new SpreadSheetColumnData(data,controller)

    def sendData(): Unit = {
      myFireTableStructureChanged()
      loadCells()
    }

    override def updateUndo(): Unit = updateColumns()

    override def fieldChanged(elem: SpreadSheetColumnData): Unit = {
      //println("column changed "+elem)
      updateColumns()//(elem)
    	myFireTableStructureChanged()
    }

    override def childAdded(elem: SpreadSheetColumnData): Unit = fieldChanged(elem)

    override def instanceRemoved(elem: SpreadSheetColumnData): Unit = {
      //println("column instacne removed "+elem)
      updateColumns()   
    }
  }

  def loadColumnData(): Unit = {
    columnData.load(controller.spreadSheetRef,1)
  }
  
 val collFuncList=new MapDataModel[Reference,SpreadSheetCollFuncData]() {
   def elemFactory(data: InstanceData) = SpreadSheetCollFuncData(data.ref, data.fieldValue.head.toObjectReference, data.fieldData(1) match {
     case c: CollectingFuncCall => Some(c)
     case _ => None
   }, SpreadSheetRange.apply(data.fieldValue(2).toInt, data.fieldValue(4).toInt, data.fieldValue(3).toInt, data.fieldValue(5).toInt))

   def sendData(): Unit = loadColumnData()
  }


  val colCellList: mutable.HashMap[Int, SpreadSheetColCellList] = collection.mutable.HashMap[Int, SpreadSheetColCellList]()
  var cellSubsID: Int = -1

  override def getColumnClass(col:Int):java.lang.Class[_] =   classOf[Expression]  
  
  def getRowCount: Int = { maxRowNum+SpreadSheet.rowOverhead }

  def getColumnCount: Int = if(columnData.theMap.isEmpty)1 else
    math.max(columnData.theMap.keys.max,if(colCellList.isEmpty) 0 else colCellList.keys.max)+2
  

  def getCell(col:Int,row:Int):Option[SpreadSheetCell]= colCellList.synchronized{ 
    colCellList.get(col).flatMap(_.cellMap.get(row))    
  }

  def getValueAt(row: Int, col: Int): Object = {
    if(col==getColumnCount -1) null
    else if(colCellList.contains(col)){      
      colCellList(col).getValue(row)
    } else null
  }

  def getColumns(range: Range): Iterable[SpreadSheetColumnData] = columnData.theMap.view.filterKeys(range.contains).values

  override def isCellEditable(row: Int, col: Int): Boolean = col < getColumnCount - 1 && col >= 0


  override def setValueAt(value: Object, row: Int, col: Int): Unit = if (col > -1) {
    val ncol=IntConstant(col)
    val nrow=IntConstant(row)
    val nnul=IntConstant(0)

    def showError(res:CommandResult): Unit = res match {
      case HasError(err: Exception) =>
        Swing.onEDT({
          controller.table.peer.editCellAt(row, controller.table.peer.convertColumnIndexToView(col))
          Swing.onEDT({
            if (value != null)
              controller.table.peer.getEditorComponent match {
                case m: JTextArea => m.setText(value.toString)
                  try {
                    new Toast(err.getMessage, controller.table.peer.getEditorComponent.asInstanceOf[JComponent], ClientApp.top).visible = true
                  } catch {case NonFatal(e)=> util.Log.e("Error toast" +e)}
                case o => util.Log.e("Other Editor:" + o + " " + o.getClass)
              }
          })
        })
      case _ =>
    }

    ClientQueryManager.executeActionResult(EMPTY_OWNERREF,List(controller.spreadSheetRef),"insertCells",
        Seq(ResultElement("startCol",ncol),ResultElement("endCol",ncol),ResultElement("startRow",nrow),ResultElement("endRow",nrow),
          ResultElement("dx",nnul),ResultElement("dy",nnul),ResultElement(value.toString,EMPTY_EX)),showError)
  }


  def maxRowNum: Int = if (colCellList.isEmpty) 0 else colCellList.values.maxBy(_.maxRowNum).maxRowNum
  
  
  def findCellByRef(ref:Reference):Option[SpreadSheetCell]=colCellList.synchronized{
    util.CollUtils.iterHeadOption(colCellList.valuesIterator.flatMap(_.findCellByRef(ref)))
  }


  protected def addCells(cells: Seq[SpreadSheetCell]): Unit = colCellList.synchronized {
    colCellList.clear()
    for(c<-cells) addCell(c,notify = false)
  }


  def getCellList(col: Int): SpreadSheetColCellList = colCellList.synchronized {colCellList.getOrElseUpdate(col, new SpreadSheetColCellList(col, controller))}
  
  
  //TODO: optimize this function
  def removePreviousCellData(cell:SpreadSheetCell):Unit = colCellList.synchronized{
    var runIt=false
    val iter=colCellList.valuesIterator
    while(iter.hasNext&& runIt){
      val collData=iter.next()
      collData.findCellByRef(cell.ref) match {
        case Some(oldCell) if oldCell.col != cell.col || oldCell.row != cell.row =>
          collData.removeCell(oldCell.row)
          if(collData.cellMap.isEmpty && cell.col!=oldCell.col) colCellList-= collData.col
          runIt=false
        case _=>
      }
    }
  }
  
  def addCell(cell:SpreadSheetCell,notify:Boolean):Unit= colCellList.synchronized{
    //println("add cell "+cell+" "+notify)
    if(cell.col> -1) { // ignore created instances with column== -1
	    getCellList(cell.col).addCell(cell) 
	    if(notify) myFireTableChanged()    
    }
  }
  
  
  def loadCells():Unit = {
    //println("load cells, columns:"+columnData.mkString)
    cellSubsID=ClientQueryManager.createFactSubscription(controller.spreadSheetRef,4,SpreadSheet.factory)((command,data)=> Swing.onEDT{
      colCellList.synchronized{
       command match {
         case NotificationType.sendData =>
           //println("send cell data ")
           addCells(data)
           updateColumns()
           controller.loadFormats()
         case NotificationType.updateUndo=>
           addCells(data)
           myFireTableChanged()
         case NotificationType.fieldChanged=>
           //println("Cell field Changed:"+data(0))
           removePreviousCellData(data.head)
           val notify=data.head.row>maxRowNum
           addCell(data.head,notify = true)
           if(notify) myFireTableChanged()
         case NotificationType.instanceRemoved=>
           val dref=data.head.ref
           colCellList.values.find(_.removeCell(dref)) match {
             case Some(column)=>
               if(column.cellMap.isEmpty)colCellList-=column.col
               myFireTableChanged()
             case None => //println("not in list" +data(0))
           }
         case NotificationType.childAdded=> addCell(data.head,notify = true)
         case NotificationType.parentNotExistend=> Log.e("load Cells "+controller.spreadSheetRef )
       }
      }
    })
  }
  
  
  def myFireTableChanged():Unit=  {
    val event=new TableModelEvent(this,0,getRowCount() - 1, TableModelEvent.ALL_COLUMNS, TableModelEvent.UPDATE)
    fireTableChanged(event); //changeType
    controller.firstColumnTableModel.fireTableChanged(event)
  }
  
  def myFireTableStructureChanged():Unit = {
    fireTableStructureChanged()
    controller.firstColumnTableModel.fireTableStructureChanged()
  }
        
  
  
  
  def clearAllCells():Unit= colCellList.synchronized{
    for (col<-colCellList.values) col.clear()
    columnModel.clear()
  }
  
  def shutDown():Unit=colCellList.synchronized{    
    //writeColumnWidths()
    columnData.shutDown()
    clearAllCells()      
    
    if(cellSubsID> -1 ) {
      colCellList.clear()
      ClientQueryManager.removeSubscription(cellSubsID)
      cellSubsID= -1
    }
    collFuncList.shutDown()
  }
  
  def updateColumns():Unit= {    
    columnModel.clear()
    val cc=getColumnCount-1
    for(i<-0 until cc) {
      val nColumn = new MyTableColumn(i, SpreadSheet.defaultColumnWidth * ViewConstants.fontScale / 100)
    	columnData.theMap.get(i) match {
    		case Some(col)=>
          setColDataToModel(col,nColumn)
        case _=> nColumn.setHeaderValue(SpreadSheetUtil.columnIdToLetter(i))
    	}
    	columnModel.addColumn(nColumn)
    }
    val lastColumn = new MyTableColumn(cc, SpreadSheet.lastColumnWidth * ViewConstants.fontScale / 100)
    lastColumn.setResizable(false)
    lastColumn.setHeaderValue('+')
    lastColumn.setMaxWidth(SpreadSheet.lastColumnWidth * ViewConstants.fontScale / 100)
    columnModel.addColumn(lastColumn)
    myFireTableStructureChanged()
  }
  
  
  def updateColumn(colData:SpreadSheetColumnData):Unit= {
    if(colData.col== columnModel.getColumnCount-1){
      val newCol = new MyTableColumn(colData.col, SpreadSheet.defaultColumnWidth * ViewConstants.fontScale / 100)
      setColDataToModel(colData,newCol)
      columnModel.insertAt(newCol,colData.col)
      val lastIx=columnModel.getColumnCount-1
      columnModel.getColumn(lastIx).setModelIndex(lastIx)
    }
    else if(colData.col>columnModel.getColumnCount-1) updateColumns()
    else setColDataToModel(colData,columnModel.getColumn(colData.col))    
  }
  
  
  protected def setColDataToModel(colData:SpreadSheetColumnData,nCol:TableColumn):Unit= {
  //println("setColDataToModel "+colData+" " +nCol)
    val nColumn=nCol.asInstanceOf[MyTableColumn]
    nColumn.setModelIndex(colData.col)
    nColumn.setHeaderValue(if(colData.name.length>0) colData.name else SpreadSheetUtil.columnIdToLetter(colData.col))
    if(colData.width>0){
      nColumn.initWidth(colData.width * ViewConstants.fontScale / 100)
      nColumn.setPreferredWidth(colData.width * ViewConstants.fontScale / 100)
    } else{
      nColumn.initWidth(SpreadSheet.defaultColumnWidth * ViewConstants.fontScale / 100)
      nColumn.setPreferredWidth(SpreadSheet.defaultColumnWidth * ViewConstants.fontScale / 100)
    }
  }


  def writeColumnWidths(): Unit = {
      //println("writeColWidths"+ getColumnCount)
      for(i<-0 until math.min(getColumnCount(),columnModel.getColumnCount())-1){
        val theCol=columnModel.getMyColumn(i)
        val width=columnModel.getColumn(i).getWidth()       
        if(theCol.widthChanged ) {
          //println("Write width col "+i+" w:"+theCol.getWidth())
          columnData.theMap.get(i) match{
            case Some(colData) => colData.writeWidth(width * 100 / ViewConstants.fontScale)
            case None => createColumnData(i, width * 100 / ViewConstants.fontScale)
          }
          theCol.widthChanged=false
        }
      }     
    }  
  
  def createColumnData(col:Int,width:Int):Unit={
    ClientQueryManager.createInstances(Array(controller.columnOwnerRef),mutable.Seq((SpreadSheet.spreadSheetColumnType,
      Array(IntConstant(col), EMPTY_EX, IntConstant(width), EMPTY_EX))))
  }

}