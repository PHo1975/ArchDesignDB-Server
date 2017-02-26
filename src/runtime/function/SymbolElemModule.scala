package runtime.function

import definition.data.{InstanceData, OwnerReference}
import definition.expression.{Constant, VectorConstant}
import definition.typ.{DataType, AnswerDefinition, DialogQuestion, CommandQuestion}
import server.comm.{AbstractUserSocket, JavaClientSocket}
import server.storage.{ActionIterator, ActionModule, CreateActionImpl, StorageManager}
import transaction.handling.{ActionList, TransactionManager}
import util.GraphUtils

class SymbolElemModule extends ActionModule with GraphActionModule {
  val horizontalText="Horizontal"
  override val createActions=List(createSymbolAction) 
  val actions=Seq(replaceAction,breakUpAction,alignHor,alignVert,distributeAction)
  
  def createSymbolAction=new CreateActionImpl("Symbol",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
  "CreateSymbol")),doCreateSymbol)
  
  def replaceAction=new ActionIterator("Symbol austauschen",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
  "ChangeSymbol")),doReplace)

  def alignHor=new ActionIterator("Vert.ausrichten",None,doAlignHor)
  def alignVert=new ActionIterator("Hor.ausrichten",None,doAlignVert)
  def distributeAction=new ActionIterator("Verteilen",Some(new DialogQuestion("Verteilen",Seq(new AnswerDefinition("Ausrichtung",DataType.EnumTyp,None,horizontalText+",Vertikal")))),
    doDistribute)
  def breakUpAction=new ActionIterator("Symbol aufbrechen",None,doBreakUp)
  
  def doCreateSymbol(u:AbstractUserSocket, parents:Seq[InstanceData], param:Seq[(String,Constant)], newTyp:Int, formFields:Seq[(Int,Constant)]):Boolean= {
    //println("do Symbol parents "+parents.mkString(", ")+"\n params:"+param.mkString("; "))
    val layer=parents.head
    val symbolRef=param.head._2
    val angle=param(1)._2
    val scale=param(2)._2
    val pos=param(3)._2
    val symbInst=TransactionManager.tryCreateInstance(TypeInfos.symbolElemType, Array(new OwnerReference(0,layer.ref)),true)
    TransactionManager.tryWriteInstanceField(symbInst.ref, 1, symbolRef)    
    TransactionManager.tryWriteInstanceField(symbInst.ref, 2, angle)
    TransactionManager.tryWriteInstanceField(symbInst.ref, 3, scale)    
    TransactionManager.tryWriteInstanceField(symbInst.ref, 5, pos)
    true
  }
  
  def doReplace(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
    val symbolRef=param.head._2
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,1,symbolRef)
    true
  }
  
  def moveElement(elem:InstanceData,delta:VectorConstant) = {
    TransactionManager.tryWriteInstanceField(elem.ref,5,elem.fieldValue(5).toVector+delta)
  }
  
  def copyElement(elem:InstanceData,delta:VectorConstant) = {
    elem.setField(5,elem.fieldValue(5).toVector+delta) 
  } 
  
  def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit = {
    GraphElemModule.rotateAngleField(elem,2,angle)
    TransactionManager.tryWriteInstanceField(elem.ref,5,rotator(elem.fieldValue(5).toVector))
  }
  def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData = {
    elem
  }
  
  override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = pointModField(5,elem,delta,chPoints)
  
  def doBreakUp(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
    for(d<-data;if d.ref.typ == theTypeID){
      val symbolRef=d.fieldValue(1).toObjectReference
      val angle=d.fieldValue(2).toDouble*Math.PI/180d
      val scale=d.fieldValue(3).toDouble
      val pos=d.fieldValue(5).toVector
      val rotator=GraphUtils.createRotator(pos, angle)
      val owners=Array(owner)
      for(props<-StorageManager.getInstanceProperties(symbolRef);grElemRef<-props.propertyFields(0).propertyList;
        grElem=ActionList.getInstanceData(grElemRef)){
        val createInst=TransactionManager.tryCreateInstance(grElemRef.typ,owners,false)
        var newInst=grElem.clone(createInst.ref,owners,Seq.empty)
        val module=TypeInfos.moduleMap(grElemRef.typ)
        newInst=module.copyElement(newInst,pos)          
        TransactionManager.tryWriteInstanceData(newInst)
        module.rotateElement(newInst,angle,rotator)
      }
      TransactionManager.tryDeleteInstance(d.ref, Some(owner),None)
    }
    
    true
  }

  def doAlignHor (u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  if(data.size>1){
    var xValue=0d
    for(d<-data)
      xValue+=d.fieldValue(5).toVector.x
    val newValue=xValue/data.size.toDouble
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,5.toByte,new VectorConstant(newValue,d.fieldValue(5).toVector.y,0))
    true
  } else false

  def doAlignVert (u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  if(data.size>1){
    var yValue=0d
    for(d<-data)
      yValue+=d.fieldValue(5).toVector.y
    val newValue=yValue/data.size.toDouble
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,5.toByte,new VectorConstant(d.fieldValue(5).toVector.x,newValue,0))
    true
  } else false

  def doDistribute (u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  if(data.size>2){
    //println("Verteilen "+param.mkString("|"))
    val horizontal= param.head._2.toString==horizontalText

    def getValue(d:InstanceData):Double={
      val vector=d.fieldValue(5).toVector
      if(horizontal) vector.x else vector.y
    }

    val sortedList=data.sortBy(getValue)
    val min:Double=getValue(sortedList.head)
    val max:Double=getValue(sortedList.last)
    val step=(max-min)/(data.size-1)
    var current=min+step
    for(i<-1 until data.size-1;d=sortedList(i)){
      val vector=d.fieldValue(5).toVector
      val nv=new VectorConstant(if(horizontal)current else vector.x,if(horizontal)vector.y else current,vector.z)
      TransactionManager.tryWriteInstanceField(d.ref,5,nv)
      current+=step
    }

    true
  } else false
  
  
}