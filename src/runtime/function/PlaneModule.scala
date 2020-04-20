package runtime.function

import definition.data.{InstanceData, OwnerReference}
import definition.expression.{Constant, StringConstant, VectorConstant}
import definition.typ.{AnswerDefinition, DataType, DialogQuestion}
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule}
import transaction.handling.TransactionManager

class PlaneModule extends ActionModule {
  var theTyp:Int=0
  val mQuestion: DialogQuestion =DialogQuestion("Ebene Verschieben", Seq(
    new AnswerDefinition("dx", DataType.DoubleTyp, None),
    new AnswerDefinition("dy", DataType.DoubleTyp, None),
    new AnswerDefinition("dz", DataType.DoubleTyp, None)))

  val moveAction=new ActionIterator("verschieben",Some(mQuestion),doMove,false,1000)

  val copyAction=new ActionIterator("kopieren",Some(mQuestion),doCopy,false,1000)

  val actions = List(moveAction,copyAction)

  override def setObjectType(typeID: Int): Unit = {theTyp=typeID}

  def doMove(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val amount=param.head._2.toDouble
    val deltaVector=
    param.head._1 match{
      case "dx"=> new VectorConstant(amount,0d,0d)
      case "dy"=> new VectorConstant(0d,amount,0d)
      case "dz"=> new VectorConstant(0d,0d,amount)
      case o=> throw new IllegalArgumentException("falscher Parameter "+o)
    }
    for(d<-data){
      val oldVector=d.fieldValue(1).toVector
      TransactionManager.tryWriteInstanceField(d.ref,1,oldVector+deltaVector)
    }
    true
  }

  def doCopy(u:AbstractUserSocket, parent:OwnerReference, data:Iterable[InstanceData], param:Iterable[(String,Constant)]):Boolean = {
    val amount=param.head._2.toDouble
    val deltaVector=
      param.head._1 match{
        case "dx"=> new VectorConstant(amount,0d,0d)
        case "dy"=> new VectorConstant(0d,amount,0d)
        case "dz"=> new VectorConstant(0d,0d,amount)
        case o=> throw new IllegalArgumentException("falscher Parameter "+o)
      }

    for(d<-data){
      val oldVector=d.fieldValue(1).toVector
      val name=d.fieldValue(0).toString.takeWhile(!_.isDigit)
      val newInst=TransactionManager.tryCreateInstance(theTyp,d.owners,true)
      TransactionManager.tryWriteInstanceField(newInst.ref,1,oldVector+deltaVector)
      TransactionManager.tryWriteInstanceField(newInst.ref,2,d.fieldData(2))
      if(!d.fieldData(3).isNullConstant)TransactionManager.tryWriteInstanceField(newInst.ref,3,d.fieldData(3))
      TransactionManager.tryWriteInstanceField(newInst.ref,0,StringConstant(name+" "+newInst.ref.instance))
    }
    true
  }
}
