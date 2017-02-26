package client.comm
import scala.collection.mutable.ArrayBuffer
import definition.expression.Expression
import definition.data.OwnerReference

class CreateInstancesBuffer(owners:Array[OwnerReference],bufferSize:Int) {
  
  val buffer=new ArrayBuffer[(Int,Array[Expression])](bufferSize)
    
  def addInstance(typ:Int,fields:Array[Expression])= {
    buffer += ((typ,fields))
    if(buffer.size>=bufferSize) flush()   
  } 
  
  def flush(): Unit = {
    ClientQueryManager.createInstances(owners,buffer)
    buffer.clear()
  }

}