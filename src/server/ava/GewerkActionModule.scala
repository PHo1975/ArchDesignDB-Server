package server.ava

import util.{Log, StrToDouble}
import scala.collection.Iterable
import server.storage.{StorageManager, ActionModule, ActionIterator}
import definition.expression._
import definition.data.InstanceData
import definition.data.{OwnerReference,Reference}
import server.comm.{AbstractUserSocket, JavaClientSocket}
import definition.typ._
import transaction.handling.TransactionManager

object GewerkActionModule {
  val gewerkOZField=1.toByte
  val posOZField=1.toByte
  val lvOZField=1.toByte
  
  def doNum(ozField:Byte) (u:AbstractUserSocket, owner:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]) =  {
    val step=param.head._2.toInt
    val firstValue=data.head.fieldValue(ozField).toInt
    var (startElem,startValue)= if(firstValue==0) (0,0) else (1,firstValue)     
    for (i <-startElem until data.size;elem=data(i)) {
      startValue+=step
      TransactionManager.tryWriteInstanceField(elem.ref,ozField,IntConstant(startValue))
    }
    true
  }
}

class GewerkActionModule extends ActionModule {  
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction)
  
  val numAction=new ActionIterator("Nummerieren",Some(new DialogQuestion("Neu Nummerieren",
		Seq(new AnswerDefinition("Schrittweite:",DataType.DoubleTyp,None)))),doNum(gewerkOZField)) 
  
  def setObjectType(otype:Int) = { }
}


class KalkListeActionModule extends ActionModule {
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction)

  val numAction=new ActionIterator("Nummerieren",Some(new DialogQuestion("Neu Nummerieren",
    Seq(new AnswerDefinition("Schrittweite:",DataType.DoubleTyp,None)))),doNum(1))

  def setObjectType(otype:Int) = { }
}



class PosActionModule extends ActionModule {
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction)

  val numAction=new ActionIterator("Nummerieren",Some(new DialogQuestion("Neu Nummerieren",
    Seq(new AnswerDefinition("Schrittweite:",DataType.DoubleTyp,None)))),doNum(posOZField))

  def setObjectType(otype:Int) = { }
}

class LVActionModule extends ActionModule {
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction,vergabeAction)

  val numAction=new ActionIterator("Nummerieren",Some(new DialogQuestion("Neu Nummerieren",
  Seq(new AnswerDefinition("Schrittweite:",DataType.DoubleTyp,None)))),doNum(lvOZField))

  def setObjectType(otype:Int) = { }

  val vergabeAction=new ActionIterator("Vergabe",Some(new PanelRemoteQuestion("client.model.ava.CostTransferPanel")),doVergabe)

  def doVergabe(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {
    val allcl=AllClasses.get
    val zusstellTyp=allcl.getClassByName("Kostenzusammenstellung").get.id
    val auftragTyp=allcl.getClassByName("Auftrag").get.id
    val reGewerkTyp=allcl.getClassByName("Re-Gewerk").get.id
    val rePosTyp=allcl.getClassByName("Re-Position").get.id
    val projectTyp=allcl.getClassByName("Projekt").get.id
    val psGewerkTyp=allcl.getClassByName("PSGewerkSumme").get.id
    val psPreisTyp=allcl.getClassByName("PSPreis").get.id

    println("GewerkTyp "+psGewerkTyp+" preisTyp "+psPreisTyp)
    val bieterCol=StorageManager.getInstanceData(param.head._2.toObjectReference.ref)
    val nachlass=param(1)._2.toString
    val skonto=param(2)._2.toDouble
    val datumString=param(3)._2.toString.trim()
    val datum=if(datumString.length==0) DateConstant()
     else StringParser.parse(datumString,DataType.DateTyp) match{
      case ex:Expression=>ex
      case p:ParserError=>throw new IllegalArgumentException("falsches Datum:"+datumString)
    }

    val lv=StorageManager.getInstanceData(bieterCol.owners(0).ownerRef)
    for(pr<-StorageManager.getInstanceProperties(bieterCol.ref);headerRef<-pr.propertyFields(1).propertyList.headOption){
      val header=StorageManager.getInstanceData(headerRef)
      val bieterName=header.fieldData.head.toString+header.fieldData(1).toString
      println("Vergabe von "+lv.fieldData(2).toString+" an "+bieterName+" nachlass:"+nachlass+" skonto:"+skonto)
      for(projectRef<-StorageManager.getNextParentOfType(lv.ref,projectTyp);
          kostenZusRef<-StorageManager.searchFoldersForType(projectRef,2,zusstellTyp)){
        println("Zus: "+StorageManager.getInstanceData(kostenZusRef).fieldValue.head.toString)
        val auftrag=TransactionManager.tryCreateInstance(auftragTyp,Array(new OwnerReference(1,kostenZusRef)),true)
        TransactionManager.tryWriteInstanceField(auftrag.ref,0,lv.fieldValue(1))
        TransactionManager.tryWriteInstanceField(auftrag.ref,1,lv.fieldValue(2))
        TransactionManager.tryWriteInstanceField(auftrag.ref,5,datum)
        TransactionManager.tryWriteInstanceField(auftrag.ref,4,new StringConstant(bieterName))
        TransactionManager.tryWriteInstanceField(auftrag.ref,14,new DoubleConstant(skonto))
        TransactionManager.tryWriteInstanceField(auftrag.ref,7,if(nachlass.contains("%")){
          nachlass.trim.replace("%","") match {
            case StrToDouble(d)=>BinaryOperation(FieldReference(None,None,6),BinOperator.getOp('*'),
              BinaryOperation(DoubleConstant(d),BinOperator.getOp('/'),DoubleConstant(100d)))
            case _ => EMPTY_EX
          }
        } else nachlass.trim match {
          case StrToDouble(d)=>DoubleConstant(d)
          case _ => EMPTY_EX
        })

        def loopGewerk(auftragParent:Array[OwnerReference],colParent:Reference):Unit={
          //println("loop gewerk:"+colParent)
          for(cpr<-StorageManager.getInstanceProperties(colParent);childRef<-cpr.propertyFields(0).propertyList;
              psInst=StorageManager.getInstanceData(childRef);
              lvInst=StorageManager.getInstanceData(psInst.owners(0).ownerRef)) {
            //println("child "+psInst.ref+" lv:"+lvInst.ref+" "+lvInst)
            childRef.typ match {
              case `psGewerkTyp` =>
                val reGewerk=TransactionManager.tryCreateInstance(reGewerkTyp,auftragParent,true)
                TransactionManager.tryWriteInstanceField(reGewerk.ref,2,lvInst.fieldValue(1))
                TransactionManager.tryWriteInstanceField(reGewerk.ref,3,lvInst.fieldValue(2))
                loopGewerk(Array(new OwnerReference(0,reGewerk.ref)),childRef)
              case `psPreisTyp`  =>
                val rePos=TransactionManager.tryCreateInstance(rePosTyp,auftragParent,true)
                if(lvInst.fieldValue(1)!=EMPTY_EX) TransactionManager.tryWriteInstanceField(rePos.ref,2,lvInst.fieldValue(1))
                if(lvInst.fieldValue(2)!=EMPTY_EX) TransactionManager.tryWriteInstanceField(rePos.ref,3,lvInst.fieldValue(2))
                if(lvInst.fieldValue(3)!=EMPTY_EX) TransactionManager.tryWriteInstanceField(rePos.ref,4,lvInst.fieldData(3).getValue)
                if(lvInst.fieldValue(4)!=EMPTY_EX) TransactionManager.tryWriteInstanceField(rePos.ref,6,lvInst.fieldValue(4))
                TransactionManager.tryWriteInstanceField(rePos.ref,7,lvInst.fieldValue(5))
                if (psInst.fieldData(1) != EMPTY_EX)
                  TransactionManager.tryWriteInstanceField(rePos.ref,8,psInst.fieldValue(1))
                else psInst.fieldData.head match {
                  case FunctionCall(_,"if",_)=> // no gp
                  case _=> TransactionManager.tryWriteInstanceField(rePos.ref,0,psInst.fieldValue.head)
                }
              case o=> Log.w("unknown ps type:"+o)
            }
          }
        }

        loopGewerk(Array(new OwnerReference(1,auftrag.ref)),bieterCol.ref)
      }
    }

    true
  }
}

