package runtime.function

import definition.data.{InstanceData, OwnerReference}
import definition.expression.{Constant, VectorConstant}
import definition.typ.{CommandQuestion, ModuleType}
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule, CreateActionImpl}
import transaction.handling.TransactionManager

class SymbolFillerModule extends ActionModule with GraphActionModule {
  
  override val createActions=List(createSymbolAction) 
  val actions: Seq[ActionIterator] =Seq(replaceAction)
  
  def createSymbolAction=new CreateActionImpl("SymbolFiller", Some(new CommandQuestion(ModuleType.Graph,
    "CreateSymbolFiller")), doCreateSymbol)
  
  def replaceAction=new ActionIterator("Symbol austauschen",Some(new CommandQuestion(ModuleType.Graph,
  "ChangeSymbol")),doReplace)
  
  
  
  def doCreateSymbol(u:AbstractUserSocket, parents:Iterable[InstanceData], param:Seq[(String,Constant)], newTyp:Int, formFields:Seq[(Int,Constant)]):Boolean= {
    //println("do Symbol parents "+parents.mkString(", ")+"\n params:"+param.mkString("; "))
    val layer=parents.head
    val symbolRef=param.head._2
    val angle=param(1)._2
    val scale=param(2)._2
    val start=param(3)._2
    val end=param(4)._2
    val num=param(5)._2
    val symbInst=TransactionManager.tryCreateInstance(TypeInfos.symbolFillType, Array(new OwnerReference(0,layer.ref)),notifyRefandColl = true)
    TransactionManager.tryWriteInstanceField(symbInst.ref, 1, symbolRef)    
    TransactionManager.tryWriteInstanceField(symbInst.ref, 2, angle)
    TransactionManager.tryWriteInstanceField(symbInst.ref, 3, scale)
    TransactionManager.tryWriteInstanceField(symbInst.ref, 5, start)
    TransactionManager.tryWriteInstanceField(symbInst.ref, 6, end)
    TransactionManager.tryWriteInstanceField(symbInst.ref, 8, num)
    true
  }
  
  def doReplace(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean =  {
    val symbolRef=param.head._2
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,1,symbolRef)
    true
  }
  
  def moveElement(elem:InstanceData,delta:VectorConstant): Unit = {
    TransactionManager.tryWriteInstanceField(elem.ref,5,elem.fieldValue(5).toVector+delta)
    TransactionManager.tryWriteInstanceField(elem.ref,6,elem.fieldValue(6).toVector+delta)
  }
  
  def copyElement(elem:InstanceData,delta:VectorConstant): InstanceData = {
    elem.setField(5,elem.fieldValue(5).toVector+delta).setField(6,elem.fieldValue(6).toVector+delta)
  } 
  
  def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit = {
    GraphElemModule.rotateAngleField(elem,2,angle)
    TransactionManager.tryWriteInstanceField(elem.ref,5,rotator(elem.fieldValue(5).toVector))
    TransactionManager.tryWriteInstanceField(elem.ref,6,rotator(elem.fieldValue(6).toVector))
  }
  def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData = {
    elem
  }
  
  override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]): Unit = {
    val p1=elem.fieldValue(5).toVector
    val p2=elem.fieldValue(6).toVector
    if (chPoints.contains(p1)) TransactionManager.tryWriteInstanceField(elem.ref,5,p1+delta)
    if (chPoints.contains(p2)) TransactionManager.tryWriteInstanceField(elem.ref,6,p2+delta)
  }
  
  
}