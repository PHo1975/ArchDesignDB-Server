package client.dialog

import definition.expression.VectorConstant

class PointSelectModel {
  val selectList=collection.mutable.HashSet[VectorConstant]()
  var _bracketMode:Boolean=false
  
  def bracketMode=_bracketMode
  
  def deselect()= {     
    if(selectList.nonEmpty) selectList.clear()
    _bracketMode=false
   // System.out.println("Deselect")   
  } 
  
  def addPoint(point:VectorConstant)=  selectList +=point 
  
  def addPoints(points:TraversableOnce[VectorConstant],clear:Boolean)= {    
    if(clear) selectList.clear()
    selectList++=points
    //println("add points "+selectList.mkString(", "))
  } 
  
  def turnOnBracketMode()={
     _bracketMode=true
     //println("PS bracket on ")
  }     
  
  def containsPoint(point:VectorConstant):Boolean = selectList.contains(point)
  
  def removePoint(point:VectorConstant)= selectList -=point
}