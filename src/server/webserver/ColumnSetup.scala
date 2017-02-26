package server.webserver

import definition.comm.UserSetting
import definition.typ.AllClasses

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



trait AbstractColumnSetups{
  def getColumnSetup(typ:Int):AbstractColumnSetup
}

class ColumnSetups(userSetting:UserSetting) extends AbstractColumnSetups {
  val typeList = mutable.HashMap[Int,ColumnSetup]()
  def getColumnSetup(typ:Int)= typeList.getOrElseUpdate(typ,new ColumnSetup(typ,userSetting))
}

object DummyColumnSetups extends AbstractColumnSetups{
  def getColumnSetup(typ:Int)=DummyColumnSetup
}

trait AbstractColumnSetup{
  def getColumn(col:Int):Int
  def size:Int
}

class ColumnSetup(val typ:Int,userSetting:UserSetting) extends AbstractColumnSetup{
  val columns=ArrayBuffer[Int]()
  def getColumn(col:Int):Int=columns(col)
  def size:Int=columns.size

  readColumns()
  def readColumns()={
    val theClass=AllClasses.get.getClassByID(typ)
    val numColumn:Int=theClass.fields.size
    val settings=userSetting.getListProperty[(Int, Int)]("TableColumns",typ.toString,Nil)
    if(settings.nonEmpty)
      for(cs <-settings;if cs._1 <= numColumn)
        if(theClass.fieldSetting(cs._1-1).visible) columns+=cs._1-1
    for(i <-0 until numColumn;if (!columns.contains(i )) && theClass.fieldSetting(i).visible)
      columns+=i
  }
}

object DummyColumnSetup extends AbstractColumnSetup{
  def getColumn(col:Int):Int=col
  def size=99
}

