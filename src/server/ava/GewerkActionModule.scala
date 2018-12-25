package server.ava

import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression._
import definition.typ._
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule, StorageManager}
import transaction.handling.TransactionManager
import util.{Log, StrToDouble}

object GewerkActionModule {
  val gewerkOZField: Byte =1.toByte
  val posOZField: Byte =1.toByte
  val lvOZField: Byte =1.toByte
  val posTyp=133

  
  def doNum(ozField:Byte) (u:AbstractUserSocket, owner:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]): Boolean =  {
    val step=param.head._2.toInt
    val firstValue=data.head.fieldValue(ozField).toInt
    var (startElem,startValue)= if(firstValue==0) (0,0) else (1,firstValue)     
    for (i <-startElem until data.size;elem=data(i)) {
      startValue+=step
      TransactionManager.tryWriteInstanceField(elem.ref,ozField,IntConstant(startValue))
    }
    true
  }

  def doRemoveMeasure(posPropField:Int) (u:AbstractUserSocket, owner:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]): Boolean =  {
    val measureExpression=AllClasses.get.getClassByID(posTyp).fieldSetting(3).startValue
    val measureTerm=measureExpression.getTerm
    for (gewerk<-data) {
      for(prop<-StorageManager.getInstanceProperties(gewerk.ref);
          posRef<-prop.propertyFields(posPropField).propertyList;
          if posRef.typ==posTyp){
        for(posProp<-StorageManager.getInstanceProperties(posRef);measureRef<-posProp.propertyFields(1).propertyList)
          TransactionManager.tryDeleteInstance(measureRef,None,None)
        if(StorageManager.getInstanceData(posRef).fieldData(3).getTerm != measureTerm)
          TransactionManager.tryWriteInstanceField(posRef,3.toByte,measureExpression)
      }
    }
    true
  }
}

class GewerkActionModule extends ActionModule {  
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction,removeMeasureAction)
  
  val numAction=new ActionIterator("Nummerieren",Some(DialogQuestion("Neu Nummerieren",
    Seq(new AnswerDefinition("Schrittweite:", DataType.DoubleTyp, None)))),doNum(gewerkOZField))

  val removeMeasureAction=new ActionIterator("Massen entfernen",None,doRemoveMeasure(1))

  def setObjectType(otype:Int): Unit = { }
}


class KalkListeActionModule extends ActionModule {
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction)

  val numAction=new ActionIterator("Nummerieren",Some(DialogQuestion("Neu Nummerieren",
    Seq(new AnswerDefinition("Schrittweite:", DataType.DoubleTyp, None)))),doNum(1))

  def setObjectType(otype:Int): Unit = { }
}



class PosActionModule extends ActionModule {
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction)

  val numAction=new ActionIterator("Nummerieren",Some(DialogQuestion("Neu Nummerieren",
    Seq(new AnswerDefinition("Schrittweite:", DataType.DoubleTyp, None)))),doNum(posOZField))

  def setObjectType(otype:Int): Unit = { }
}

class LVActionModule extends ActionModule {
  import server.ava.GewerkActionModule._
  lazy val actions=List(numAction,vergabeAction,ubertragAction,removeMeasureAction)

  lazy val allcl: AllClasses[_ <: AbstractObjectClass] =AllClasses.get
  lazy val psGewerkTyp: Int =allcl.getClassByName("PSGewerkSumme").get.id
  lazy val psPreisTyp: Int =allcl.getClassByName("PSPreis").get.id


  val numAction=new ActionIterator("Nummerieren",Some(DialogQuestion("Neu Nummerieren",
    Seq(new AnswerDefinition("Schrittweite:", DataType.DoubleTyp, None)))),doNum(lvOZField))

  def setObjectType(otype:Int): Unit = { }

  val vergabeAction=new ActionIterator("Vergabe",Some(PanelRemoteQuestion("client.model.ava.CostTransferPanel")),doVergabe)
  val ubertragAction=new ActionIterator("Schätzung übertr.",Some(PanelRemoteQuestion("client.model.ava.CostTransferPanel")),doUbertrag)
  val removeMeasureAction=new ActionIterator("Massen entfernen",None,doRemoveMeasure(2))

  def doVergabe(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]): Boolean =  {
    val zusstellTyp=allcl.getClassByName("Kostenzusammenstellung").get.id
    val auftragTyp=allcl.getClassByName("Auftrag").get.id
    val reGewerkTyp=allcl.getClassByName("Re-Gewerk").get.id
    val rePosTyp=allcl.getClassByName("Re-Position").get.id
    val projectTyp=allcl.getClassByName("Projekt").get.id


    println("GewerkTyp "+psGewerkTyp+" preisTyp "+psPreisTyp)
    val bieterCol=StorageManager.getInstanceData(param.head._2.toObjectReference.ref)
    val aufschlagEPString=param(1)._2.toString
    val aufschlagEP= if(aufschlagEPString.contains("%")){
      aufschlagEPString.trim.replace("%","") match {
        case StrToDouble(d)=>d
        case _ => 0
      }
    } else 0
    val nachlass=param(2)._2.toString
    val skonto=param(3)._2.toDouble
    val datumString=param(4)._2.toString.trim()
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
        TransactionManager.tryWriteInstanceField(auftrag.ref,4,StringConstant(bieterName))
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
                if (psInst.fieldData(1) != EMPTY_EX) {
                  val ep=if(aufschlagEP!=0) DoubleConstant(psInst.fieldValue(1).toDouble*(100d+aufschlagEP)/100d) else psInst.fieldValue(1)
                  TransactionManager.tryWriteInstanceField(rePos.ref, 8, ep)
                }
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

  def doUbertrag(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]): Boolean =  {
    val bieterCol=StorageManager.getInstanceData(param.head._2.toObjectReference.ref)

    def loopGewerk(colParent:Reference):Unit={
      //println("loop gewerk:"+colParent)
      for(cpr<-StorageManager.getInstanceProperties(colParent);childRef<-cpr.propertyFields(0).propertyList;
          psInst=StorageManager.getInstanceData(childRef);
          lvInst=StorageManager.getInstanceData(psInst.owners(0).ownerRef)) {
        //println("child "+psInst.ref+" lv:"+lvInst.ref+" "+lvInst)
        childRef.typ match {
          case `psGewerkTyp` => loopGewerk(childRef)
          case `psPreisTyp`  =>
            if(psInst.fieldValue(1)!=EMPTY_EX) TransactionManager.tryWriteInstanceField(lvInst.ref,6,psInst.fieldValue(1))
          case o=> Log.w("unknown ps type:"+o)
        }
      }
    }

    loopGewerk(bieterCol.ref)
    true
  }
}

