package client.dialog

import definition.expression.VectorConstant

import scala.collection.mutable

class PointSelectModel {
  val selectList: mutable.HashSet[VectorConstant] =collection.mutable.HashSet[VectorConstant]()
  var _bracketMode:Boolean=false
  
  def bracketMode: Boolean =_bracketMode
  
  def deselect():Unit= {
    if(selectList.nonEmpty) selectList.clear()
    _bracketMode=false
   // System.out.println("Deselect")   
  } 
  
  def addPoint(point:VectorConstant):Unit=  selectList +=point
  
  def addPoints(points:Iterator[VectorConstant],clear:Boolean):Unit= {
    if(clear) selectList.clear()
    selectList++=points
    //println("add points "+selectList.mkString(", "))
  } 
  
  def turnOnBracketMode():Unit={
     _bracketMode=true
     //println("PS bracket on ")
  }     
  
  def containsPoint(point:VectorConstant):Boolean = selectList.contains(point)
  
  def removePoint(point:VectorConstant):Unit= selectList -=point
}