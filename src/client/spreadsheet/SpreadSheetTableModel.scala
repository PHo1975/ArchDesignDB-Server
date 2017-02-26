package client.spreadsheet

import javax.swing.table.AbstractTableModel
import client.comm.ClientQueryManager
import definition.data.Reference
import definition.comm.NotificationType
import definition.data.InstanceData
import javax.swing.table.DefaultTableColumnModel
import javax.swing.table.TableColumn
import definition.expression.IntConstant
import definition.expression.StringConstant
import definition.expression.EMPTY_EX
import definition.expression.Expression
import javax.swing.event.TableModelEvent
import client.comm.MapDataModel
import client.comm.TreeMapDataModel
import definition.expression.CollectingFuncCall
import definition.data.EMPTY_OWNERREF
import scala.swing.Swing
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeEvent
import scala.collection.JavaConversions.enumerationAsScalaIterator

class SpreadSheetTableModel(controller:SpreadSheetController) extends AbstractTableModel {
  
  /*val myPropertyChangeListener=new PropertyChangeListener(){
     def propertyChange(ev:PropertyChangeEvent)= {
       ev.getSource match {
         case mc:MyTableColumn=> println("col:"+mc.getIdentifier()+" name:"+ev.getPropertyName()+" value:"+ev.getNewValue())  
         case _=>
       }
       
     }
  }*/
  
  class MyTableColumn(modelIndex:Int,startWidth:Int) extends TableColumn(modelIndex) {
    var widthChanged=false
    super.setWidth(startWidth)
    override def setWidth(nWidth:Int)= if(controller.table.peer.isValid()){      
      super.setWidth(nWidth)
      widthChanged=true
    } //else println("want set Width but not valid "+modelIndex)
    def initWidth(nWidth:Int)= {
      super.setWidth(nWidth)
      //println("Init Width "+modelIndex)
      widthChanged=false
    }
    //addPropertyChangeListener(myPropertyChangeListener)
  }
  
  
  
  
  class MyColumnModel extends DefaultTableColumnModel{
    def clear()=tableColumns.clear()
    def insertAt(col:TableColumn,pos:Int)=  tableColumns.add(pos,col)
    def getMyColumn(i:Int)=getColumn(i).asInstanceOf[MyTableColumn]
    def hasWidthChanged:Boolean= this.getColumns().exists {
      case mc: MyTableColumn => mc.widthChanged;
      case _ => false
    }
  } 
  
  val columnModel=new MyColumnModel
  
  
  val columnData=new TreeMapDataModel[Int,SpreadSheetColumnData](){
    def elemFactory(data:InstanceData)=new SpreadSheetColumnData(data,controller)
    def sendData() = {
      myFireTableStructureChanged()
      loadCells()
    }
    override def updateUndo()= updateColumns()
    override def fieldChanged(elem:SpreadSheetColumnData) = {
      //println("column changed "+elem)      
      updateColumns()//(elem)
    	myFireTableStructureChanged()
    }    
    override def childAdded(elem:SpreadSheetColumnData) = fieldChanged(elem)
    override def instanceRemoved(elem:SpreadSheetColumnData)={
      //println("column instacne removed "+elem)
      updateColumns()   
    }
  } 
  
  def loadColumnData()= {
    columnData.load(controller.spreadSheetRef,1)
  }
  
 val collFuncList=new MapDataModel[Reference,SpreadSheetCollFuncData]() {
    def elemFactory(data:InstanceData)=new SpreadSheetCollFuncData(data.ref,data.fieldValue.head.toObjectReference,data.fieldData(1) match {
      case c:CollectingFuncCall=>Some(c)
      case _=> None
    },SpreadSheetRange.apply(data.fieldValue(2).toInt,data.fieldValue(4).toInt,data.fieldValue(3).toInt,data.fieldValue(5).toInt))
    def sendData() = loadColumnData()    
  }
  
  
  val colCellList=collection.mutable.HashMap[Int,SpreadSheetColCellList]()  
  var cellSubsID= -1

  override def getColumnClass(col:Int):java.lang.Class[_] =   classOf[Expression]  
  
  def getRowCount(): Int = { maxRowNum+SpreadSheet.rowOverhead }

  def getColumnCount(): Int = if(columnData.theMap.isEmpty)1 else 
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
  
  def getColumns(range:Range)=columnData.theMap.filterKeys(range.contains).values
  
  override def isCellEditable(row:Int,col:Int)=col<getColumnCount-1&& col >=0
   
  
  override def setValueAt(value:Object , row:Int , col:Int )= if(col> -1){ 
    val ncol=IntConstant(col)
    val nrow=IntConstant(row)
    val nnul=IntConstant(0)
    ClientQueryManager.executeAction(EMPTY_OWNERREF,List(controller.spreadSheetRef),"insertCells",
        Seq(("startCol",ncol),("endCol",ncol),("startRow",nrow),("endRow",nrow),("dx",nnul),("dy",nnul),(value.toString,EMPTY_EX)))
  }
  
  
  
  def maxRowNum= if(colCellList.isEmpty)0 else colCellList.values.maxBy(_.maxRowNum).maxRowNum  
  
  
  def findCellByRef(ref:Reference):Option[SpreadSheetCell]=colCellList.synchronized{
    util.CollUtils.iterHeadOption(colCellList.valuesIterator.flatMap(_.findCellByRef(ref)))
  }
    
  
  protected def addCells(cells:Seq[SpreadSheetCell])= colCellList.synchronized{
    colCellList.clear()
    for(c<-cells) addCell(c,false)
  }
  
  
  def getCellList(col:Int)=colCellList.synchronized{colCellList.getOrElseUpdate(col,new SpreadSheetColCellList(col,controller))}
  
  
  //TODO: optimize this function
  def removePreviousCellData(cell:SpreadSheetCell):Unit =colCellList.synchronized{ 
    colCellList.values.foreach(collData =>collData.findCellByRef(cell.ref) match {
      case Some(oldCell) if oldCell.col != cell.col || oldCell.row != cell.row =>
        //println("Remove previous "+cell+" was:"+oldCell);
        collData.removeCell(oldCell.row)
        if(collData.cellMap.isEmpty && cell.col!=oldCell.col) colCellList-= collData.col
        return
      case _=> 
    })   
  }
  
  def addCell(cell:SpreadSheetCell,notify:Boolean):Unit= colCellList.synchronized{ 
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
           addCell(data.head,true)
           if(notify) myFireTableChanged()
         case NotificationType.instanceRemoved=>
           val dref=data.head.ref
           colCellList.values.find(_.removeCell(dref)) match {
             case Some(column)=>
               if(column.cellMap.isEmpty)colCellList-=column.col
               myFireTableChanged()
             case None => //println("not in list" +data(0))
           }
         case NotificationType.childAdded=> addCell(data.head,true)
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
    	val nColumn=new MyTableColumn(i,SpreadSheet.defaultColumnWidth)
    	columnData.theMap.get(i) match {
    		case Some(col)=>
          setColDataToModel(col,nColumn)
        case _=> nColumn.setHeaderValue(SpreadSheetUtil.columnIdToLetter(i))
    	}
    	columnModel.addColumn(nColumn)
    }
    val lastColumn=new MyTableColumn(cc,SpreadSheet.lastColumnWidth)
    lastColumn.setResizable(false)
    lastColumn.setHeaderValue('+')
    lastColumn.setMaxWidth(SpreadSheet.lastColumnWidth)
    columnModel.addColumn(lastColumn)
    myFireTableStructureChanged()
  }
  
  
  def updateColumn(colData:SpreadSheetColumnData):Unit= {
    if(colData.col== columnModel.getColumnCount()-1){
      val newCol=new MyTableColumn(colData.col,SpreadSheet.defaultColumnWidth)
      setColDataToModel(colData,newCol)
      columnModel.insertAt(newCol,colData.col)
      val lastIx=columnModel.getColumnCount()-1
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
    	nColumn.initWidth(colData.width)
    	nColumn.setPreferredWidth(colData.width)
    } else{
      nColumn.initWidth(SpreadSheet.defaultColumnWidth)
    	nColumn.setPreferredWidth(SpreadSheet.defaultColumnWidth)
    }
  }
  
  
  def writeColumnWidths()= {
      //println("writeColWidths"+ getColumnCount)
      for(i<-0 until math.min(getColumnCount(),columnModel.getColumnCount())-1){
        val theCol=columnModel.getMyColumn(i)
        val width=columnModel.getColumn(i).getWidth()       
        if(theCol.widthChanged ) {
          //println("Write width col "+i+" w:"+theCol.getWidth())
          columnData.theMap.get(i) match{
          	case Some(colData)=>colData.writeWidth(width)
          	case None =>  createColumnData(i,width)         
          }
          theCol.widthChanged=false
        }
      }     
    }  
  
  def createColumnData(col:Int,width:Int):Unit={
    
    ClientQueryManager.createInstances(Array(controller.columnOwnerRef),Seq((SpreadSheet.spreadSheetColumnType,
                Array(new IntConstant(col),EMPTY_EX,new IntConstant(width),EMPTY_EX))))}

}