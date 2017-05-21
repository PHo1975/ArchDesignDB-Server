package client.spreadsheet

import definition.expression.{Constant, EMPTY_EX, IntConstant}

sealed trait SpreadSheetRange {
   def getSelectedCells(mod:SpreadSheetTableModel):Iterable[SpreadSheetCell]
   def isDefinedAt(col:Int,row:Int):Boolean
   def touchesRange(other:SpreadSheetRange):Boolean
   def fillsRange(other:SpreadSheetRange):Boolean // this range completely covers other range
   def toConstants:Array[Constant]
}

case object NO_SELECTION extends SpreadSheetRange {
  def getSelectedCells(mod: SpreadSheetTableModel): Seq[Nothing] = Seq.empty
  def isDefinedAt(col:Int,row:Int):Boolean=false
  def touchesRange(other:SpreadSheetRange):Boolean=false
  def fillsRange(other:SpreadSheetRange):Boolean=false
  def toConstants=Array(SpreadSheetRange.minusOne,SpreadSheetRange.minusOne,SpreadSheetRange.minusOne,EMPTY_EX)
}

case object All_SELECTION extends SpreadSheetRange {
  def getSelectedCells(mod: SpreadSheetTableModel): Iterable[SpreadSheetCell] = mod.colCellList.values.flatMap(_.cellMap.values)
  def isDefinedAt(col:Int,row:Int):Boolean=true
  def touchesRange(other:SpreadSheetRange):Boolean=true
  def fillsRange(other:SpreadSheetRange):Boolean=true
  def toConstants=Array(SpreadSheetRange.minusOne,SpreadSheetRange.minusOne,EMPTY_EX,EMPTY_EX)
}


case class RowsSelection(rows:Range) extends SpreadSheetRange {
  def getSelectedCells(mod: SpreadSheetTableModel): Iterable[SpreadSheetCell] = mod.colCellList.values.flatMap(_.cellMap.filter(rows contains _._1).values)
  def isDefinedAt(col:Int,row:Int):Boolean=rows.contains(row)
  
  def touchesRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> true
    case o:RowsSelection=>o.rows.contains(rows.start)||rows.contains(o.rows.start)
    case o:ColsSelection=>true
    case s:SingleCellSelection=>rows.contains(s.row)
    case o:RangeSelection=>o.rows.contains(rows.start)||rows.contains(o.rows.start)
  }
  def fillsRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> false
    case o:RowsSelection=>rows.start<=o.rows.start&& o.rows.end<=rows.end
    case o:ColsSelection=>false
    case s:SingleCellSelection=>rows.contains(s.row)
    case o:RangeSelection=>rows.start<=o.rows.start&& o.rows.end<=rows.end
  }
  def toConstants=Array(SpreadSheetRange.minusOne,IntConstant(rows.start),EMPTY_EX,IntConstant(rows.end-1))
}


case class ColsSelection(cols:Range) extends SpreadSheetRange {
  def getSelectedCells(mod: SpreadSheetTableModel): Iterable[SpreadSheetCell] = mod.colCellList.filter(cols contains _._1).values.flatMap(_.cellMap.values)
  def isDefinedAt(col:Int,row:Int):Boolean=cols.contains(col)
  
  def touchesRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> true
    case o:ColsSelection=>o.cols.contains(cols.start)||cols.contains(o.cols.start)
    case o:RowsSelection=>true
    case s:SingleCellSelection=>cols.contains(s.col)
    case o:RangeSelection=>o.cols.contains(cols.start)||cols.contains(o.cols.start)
  }
  def fillsRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> false
    case o:ColsSelection=>cols.start<=o.cols.start&& o.cols.end<=cols.end
    case o:RowsSelection=>false
    case s:SingleCellSelection=>cols.contains(s.col)
    case o:RangeSelection=>cols.start<=o.cols.start&& o.cols.end<=cols.end
  }
  def toConstants=Array(IntConstant(cols.start),SpreadSheetRange.minusOne,IntConstant(cols.end-1),EMPTY_EX)
}

case class SingleCellSelection(col:Int, row:Int) extends SpreadSheetRange {
  def getSelectedCells(mod: SpreadSheetTableModel): Seq[SpreadSheetCell] = {
    //println("col:"+mod.colCellList.get(col))
    
    val res=mod.colCellList.get(col).flatMap(_.cellMap.get(row))
    //println("get SelectedCells:"+res)
    res.toSeq
  }
  def isDefinedAt(ncol:Int,nrow:Int):Boolean= ncol==col&&nrow==row
  def touchesRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> true
    case o:ColsSelection=>o.cols.contains(col)
    case o:RowsSelection=>o.rows.contains(col)
    case s:SingleCellSelection=>s.col==col&&s.row==row
    case o:RangeSelection=>o.cols.contains(col)&&o.rows.contains(row)
  }
  def fillsRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> false
    case o:ColsSelection=>false
    case o:RowsSelection=>false
    case s:SingleCellSelection=>s.col==col&&s.row==row
    case o:RangeSelection=>col==o.cols.start&&col==o.cols.end&&row==o.rows.start&&row==o.rows.end
  }

  def toConstants: Array[Constant] = {
   val cc=IntConstant(col)
   val rc=IntConstant(row)
   Array(cc,rc,cc,rc) 
  }
  
}

case class RangeSelection(cols:Range, rows:Range) extends SpreadSheetRange {
  def this(col1:Int,row1:Int,col2:Int,row2:Int)= this(math.min(col1,col2) to math.max(col1,col2),
      math.min(row1,row2) to math.max(row1,row2))
  def this(v1:(Int,Int),v2:(Int,Int))=this(v1._1,v1._2,v2._1,v2._2)

  def getSelectedCells(mod: SpreadSheetTableModel): Iterable[SpreadSheetCell] = mod.colCellList.filter(cols contains _._1).values.flatMap(
    _.cellMap.filter(rows contains _._1).values)
  
  def isDefinedAt(col:Int,row:Int):Boolean=cols.contains(col)&&rows.contains(row)
  
  def touchesRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> true
    case o:ColsSelection=>o.cols.contains(cols.start)||cols.contains(o.cols.start)
    case o:RowsSelection=>o.rows.contains(rows.start)||rows.contains(o.rows.start)
    case s:SingleCellSelection=>cols.contains(s.col)&&rows.contains(s.row)
    case o:RangeSelection=>o.cols.contains(cols.start)||cols.contains(o.cols.start)&&o.cols.contains(cols.start)||cols.contains(o.cols.start)
  }
  def fillsRange(other:SpreadSheetRange):Boolean= other match {
    case NO_SELECTION=> false
    case All_SELECTION=> false
    case o:ColsSelection=>false
    case o:RowsSelection=>false
    case s:SingleCellSelection=>cols.contains(s.col)&&rows.contains(s.row)
    case o:RangeSelection=>cols.start<=o.cols.start&& o.cols.end<=cols.end && 
      rows.start<=o.rows.start&& o.rows.end<=rows.end
  }
  def toConstants=Array(IntConstant(cols.start),IntConstant(rows.start),IntConstant(cols.end),IntConstant(rows.end))
  
  
}


object SpreadSheetRange {
  val minusOne = IntConstant(-1)

  def apply(startX: Int, endX: Int, startY: Int, endY: Int): SpreadSheetRange = if (startX == -1) {
    if(startY== -1 ) All_SELECTION
    else RowsSelection(math.min(startY, endY) to math.max(startY, endY))
  }else {
    if (startY == -1) ColsSelection(math.min(startX, endX) to math.max(startX, endX))
    else if (startX == endX && startY == endY) SingleCellSelection(startX, startY)
    else RangeSelection(startX to endX, startY to endY)
  }

  def delta(oldRange: SpreadSheetRange, deltaX: Int, deltaY: Int): SpreadSheetRange = oldRange match {
    case ColsSelection(cols) => ColsSelection((cols.start + deltaX) to (cols.end + deltaX))
    case RowsSelection(rows) => RowsSelection((rows.start + deltaY) to (rows.end + deltaY))
    case RangeSelection(cols, rows) => RangeSelection((cols.start + deltaX) to (cols.end + deltaX), (rows.start + deltaY) to (rows.end + deltaY))
    case SingleCellSelection(col, row) => SingleCellSelection(col + deltaX, row + deltaY)
    case o => o
  }
}


sealed trait RangeIteratorResult

sealed trait CellIteratorResult

object NoChange extends RangeIteratorResult with CellIteratorResult

object Delete extends RangeIteratorResult with CellIteratorResult

case class ChangeRows(newRows:Range) extends RangeIteratorResult
case class ChangeCols(newCols:Range) extends RangeIteratorResult

case class ChangeRow(row:Int) extends CellIteratorResult
case class ChangeCol(col:Int) extends CellIteratorResult


