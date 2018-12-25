package client.comm
import definition.data.OwnerReference
import definition.expression.Expression

import scala.collection.mutable.ArrayBuffer

class CreateInstancesBuffer(val owners:Array[OwnerReference],bufferSize:Int,checkLinks:Boolean=false) {
  
  val buffer=new ArrayBuffer[(Int,Array[Expression])](bufferSize)
    
  def addInstance(typ:Int,fields:Array[Expression]): Unit = {
    buffer += ((typ,fields))
    if(buffer.size>=bufferSize) flush()   
  } 
  
  def flush(): Unit = {
    ClientQueryManager.createInstances(owners,buffer,checkLinks)
    buffer.clear()
  }

}