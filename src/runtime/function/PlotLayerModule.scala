package runtime.function

import definition.data.{InstanceData, OwnerReference}
import definition.expression.{Constant, DoubleConstant, EMPTY_EX, VectorConstant}
import definition.typ.{CommandQuestion, DataType, ModuleType, SystemSettings}
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule, StorageManager}
import transaction.handling.TransactionManager

class PlotLayerModule extends ActionModule {
  var layerTypeID:Int= -1
  
  lazy val actions=List(moveAction,alignHor,alignVert,setCutRect,resetCutRect)  
  
  lazy val scales: Map[Int, Double] = SystemSettings().enums("DrawingScales").enumValues.map { case (st, k) => (k, st.split(':').last.toDouble) }.toMap
  
  def setObjectType(typeID:Int): Unit =layerTypeID=typeID
  
  def getCustomQuestion(aname:String): Option[CommandQuestion] = Some(new CommandQuestion(ModuleType.Plot, aname))
  
  val moveAction=new ActionIterator("Verschieben",getCustomQuestion("Move"),doMove)
  
  val alignHor=new ActionIterator("Hor.ausrichten",None,doAlignHor)
  val alignVert=new ActionIterator("Vert.ausrichten",None,doAlignVert)
  val setCutRect=new ActionIterator("Zuschneiden",getCustomQuestion("Zuschneiden"),doSetCutRect)
  val resetCutRect=new ActionIterator("Rahmen entfernen",None,doResetCutRect)
  
  
  def doMove(u:AbstractUserSocket, owner:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]): Boolean =  {
		if(param.size==2) {
			val delta =
				if(param.head._2.getType==DataType.VectorTyp )
				{
					val startPoint=param.head._2.toVector
					val endPoint=param.last._2.toVector
					endPoint-startPoint
				}
				else if(param.head._2.getType==DataType.DoubleTyp )
					new VectorConstant (param.head._2.toDouble,param.last._2.toDouble,0)
 			  else throw new IllegalArgumentException(" move wrong parametertype ")
			  //System.out.println("move delta:"+delta)
				for(d <-data) {				  
					TransactionManager.tryWriteInstanceField(d.ref,7.toByte,new DoubleConstant(d.fieldValue(7).toDouble+delta.x))	
					TransactionManager.tryWriteInstanceField(d.ref,8.toByte,new DoubleConstant(d.fieldValue(8).toDouble+delta.y))
				}			  
		  true	
		}
		else false
	}
  
  def doAlignHor (u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]): Boolean =  if(data.size>1){
    var xValue=0d
    for(d<-data) 
      xValue+=d.fieldValue(7).toDouble    
    val newValue=new DoubleConstant(xValue/data.size.toDouble)
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,7.toByte,newValue)
    true
  } else false
  
  def doAlignVert (u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]): Boolean =  if(data.size>1){
    var yValue=0d
    for(d<-data) 
      yValue+=d.fieldValue(8).toDouble    
    val newValue=new DoubleConstant(yValue/data.size.toDouble)
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,8.toByte,newValue)
    true
  } else false
  
  def doSetCutRect (u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]): Boolean =  if(data.nonEmpty&& param.size==2){
    val p1=param.head._2.toVector
    val p2=param.last._2.toVector
    val firstLRef=data.head
    val scale= {
      val lrScale=firstLRef.fieldValue(1).toInt   	
      if(lrScale==0) {
        StorageManager.getInstanceData(firstLRef.fieldValue.head.toObjectReference).fieldValue(2).toInt
      } else lrScale 
    }
    val scaleValue=scales(scale)        
    for(d<-data){
      val sp=new VectorConstant(d.fieldValue(7).toDouble,d.fieldValue(8).toDouble,0)      
      val d1=(p1-sp)*scaleValue
      val d2=(p2-sp)*scaleValue      
      TransactionManager.tryWriteInstanceField(d.ref,3.toByte,DoubleConstant(Math.min(d1.x,d2.x)))
      TransactionManager.tryWriteInstanceField(d.ref,4.toByte,DoubleConstant(Math.min(d1.y,d2.y)))
      TransactionManager.tryWriteInstanceField(d.ref,5.toByte,DoubleConstant(Math.abs(d1.x-d2.x)))
      TransactionManager.tryWriteInstanceField(d.ref,6.toByte,DoubleConstant(Math.abs(d1.y-d2.y)))
    }     	
    true
  } else  false
  
  def doResetCutRect(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]): Boolean =  if(data.nonEmpty){
    for(d<-data;i<-0 to 3)      
      TransactionManager.tryWriteInstanceField(d.ref,(i+3).toByte,EMPTY_EX)
    true
  }else false
  

}