package client.graphicsView.symbol

import client.comm.ClientQueryManager
import client.graphicsView._
import definition.data.{InstanceData, Referencable, Reference}
import definition.expression._
import definition.typ.DataType

import scala.collection.immutable.Map
import scala.collection.{immutable, mutable}


case class SymbolParam(ref:Reference,name:String,typ:DataType.Value,defaultValue:Constant){
  def this(data:InstanceData)=this(data.ref,data.fieldValue.head.toString,DataType(data.fieldValue(1).toInt),data.fieldValue(2))
}



class SymbolStamp (stampData:InstanceData) extends Referencable {
  var params: Map[Int, SymbolParam] =
    (ClientQueryManager.queryInstance(stampData.ref, 1).view.map(inst=>(inst.ref.instance ,new SymbolParam(inst) ))).toMap
    
  val templates: immutable.IndexedSeq[InstanceData] = ClientQueryManager.queryInstance(stampData.ref,0)
  val ref: Reference =stampData.ref
  val name: String =stampData.fieldValue.head.toString
  
  def getParamValue(param:SymbolParam,paramValues:Map[String,Constant]): Constant ={
    paramValues.getOrElse(param.name ,param.defaultValue)
  }    
  
  def generateElements(paramValues:Map[String,Constant],angle:Double): Seq[GraphElem] = {
    val radAngle=angle*Math.PI/180
    val cosa=math.cos(radAngle)
    val sina=math.sin(radAngle)
    def rotator(v:VectorConstant):VectorConstant=       
      new VectorConstant(v.x*cosa-v.y*sina,v.x*sina+v.y*cosa,0)
        
    def translateElements(ex:Expression):Constant= {
      ex.replaceExpression {
        case FieldReference(Some(rtyp), Some(rinst), field, value) =>
          if (rtyp == GraphElemConst.ParamClassID) getParamValue(params(rinst), paramValues) else value
        case _ => ex
      }
      ex.getValue
    }
    templates.flatMap(StampPool.generateElement(_,angle,translateElements,rotator))
  }
}


object StampPool {
  val maxSize=50
  
  val measureElemContainer: ElemContainer = new ElemContainer{
    def scaleRatio: Double =1d/100d
  }
  
  lazy val generatorMap:Map[Int,(InstanceData,Double,Expression=>Constant,VectorConstant=>VectorConstant)=>GraphElem]=Map(
      GraphElemConst.lineClassID->createLine _,
      GraphElemConst.arcClassID->createArc _,
      GraphElemConst.ellipseClassID->createEllipse _)
  
  val poolList: mutable.LinkedHashMap[Reference, Option[SymbolStamp]] = mutable.LinkedHashMap[Reference,Option[SymbolStamp]]()
  def getStamp(stampRef:Reference):Option[SymbolStamp] = {
     poolList.getOrElse(stampRef,{
      ClientQueryManager.queryInstance(stampRef,-1).headOption match {
        case Some(data)=>
          val newStamp=new SymbolStamp(data)
          if(poolList.size>maxSize)poolList.drop(1)
          poolList(stampRef)=Some(newStamp)
          Some(newStamp)
        case None => util.Log.e("Stamp not found "+stampRef);None
      }
    })
  }
  
  def createLine(data:InstanceData,angle:Double,translator: Expression=>Constant,rotator:VectorConstant=>VectorConstant): LineElement = {
    LineElement(data.ref, translator(data.fieldData.head).toInt, translator(data.fieldData(1)).toInt,
      translator(data.fieldData(2)).toInt, rotator(translator(data.fieldData(3)).toVector), rotator(translator(data.fieldData(4)).toVector))
  }
   
  def createArc(data:InstanceData,angle:Double,translator: Expression=>Constant,rotator:VectorConstant=>VectorConstant): ArcElement = {
    ArcElement(data.ref, translator(data.fieldData.head).toInt, translator(data.fieldData(1)).toInt,
      translator(data.fieldData(2)).toInt, rotator(translator(data.fieldData(3)).toVector), translator(data.fieldData(4)).toDouble,
      translator(data.fieldData(5)).toDouble + angle, translator(data.fieldData(6)).toDouble + angle)
  } 
  
   def createEllipse(data:InstanceData,angle:Double,translator: Expression=>Constant,rotator:VectorConstant=>VectorConstant): EllipseElement = {
    EllipseElement(data.ref, translator(data.fieldData.head).toInt, translator(data.fieldData(1)).toInt,
      translator(data.fieldData(2)).toInt, rotator(translator(data.fieldData(3)).toVector), translator(data.fieldData(4)).toDouble,
      translator(data.fieldData(5)).toDouble, translator(data.fieldData(6)).toDouble + angle, translator(data.fieldData(7)).toDouble,
      translator(data.fieldData(8)).toDouble)
  } 
  
  //private def getNone(data:InstanceData,translator:Expression=>Constant)= None
  
  def generateElement(data:InstanceData,angle:Double,translator: Expression=>Constant,rotator:VectorConstant=>VectorConstant)
    :Option[GraphElem]=
    generatorMap.get(data.ref.typ).map(_(data,angle,translator,rotator))
    //Some(generatorMap.getOrElse(data.ref.typ,{return None})(data,angle,translator,rotator))


  def parseParamValues(paramString:String):Map[String,Constant]=paramString.split('|').flatMap(_.split('#') match {
    case Array(a,b)=> StringParser.parse(b,DataType.StringTyp) match {
      case p:ParserError=> None
      case ex:Expression=>Some(a -> ex.getValue)
    } 
    case _=> None
  }).toMap
}