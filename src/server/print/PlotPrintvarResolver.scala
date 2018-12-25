package server.print

import definition.data.InstanceData
import definition.expression.{Constant, StringConstant}
import definition.typ.SystemSettings
import server.storage.StorageManager


class PlotPrintvarResolver extends CustomPrintvarResolver {
  lazy val scales: collection.Map[Int, String] =SystemSettings().enums("DrawingScales").enumValues.map(_.swap)
  val measureLayerType: Int =SystemSettings().systemTypes("MeasureLayer")
  val actionMap: Map[String, InstanceData => Constant] =Map[String, InstanceData =>Constant]("dim"->getDimensions,"layids"->getLayIDs)
  
  
  
  def resolve(varName: String, currData: InstanceData): Constant = {
   if(actionMap.contains(varName))  actionMap(varName)(currData)
   else throw new IllegalArgumentException ("Unbekannte Funktion "+varName+" in PlotPrintResolver")
 }
  
  protected def getFieldValue(currData:InstanceData,fieldNr:Byte):Set[Constant]= {
    StorageManager.getInstanceProperties(currData.ref) map (dprops=> {
      return (for(plRef <-dprops.propertyFields(1).propertyList;
      		layRef=StorageManager.getInstanceData(plRef).fieldValue.head.toObjectReference
                  if StorageManager.instanceExists(layRef.typ, layRef.instance);
      		layer=StorageManager.getInstanceData(layRef)
      		) yield layer.fieldValue(fieldNr+ (if(layRef.typ==measureLayerType)1 else 0))
      ).toSet
    })
    Set.empty
  }
  
  def emptyIterator: Iterator[Nothing] =Seq.empty.iterator
  
  def layerIterator(currData:InstanceData):Iterator[(InstanceData,InstanceData)]= {
    for(dprops <-StorageManager.getInstanceProperties(currData.ref))  {
      return for(plRef <-dprops.propertyFields(1).propertyList.iterator;
          lrData=StorageManager.getInstanceData(plRef);
      		layRef=lrData.fieldValue.head.toObjectReference
                 if StorageManager.instanceExists(layRef.typ, layRef.instance);
      		layer=StorageManager.getInstanceData(layRef)
      		) yield (lrData,layer)     
    }
    emptyIterator
  }
  
  
  def getDimensions(currData:InstanceData):Constant= {//new StringConstant(getFieldValue(currData,2).map(value=>scales(value.toInt)).mkString(", "))
    StringConstant((for((lrData,layerData)<-layerIterator(currData:InstanceData)) yield{
      scales(if(lrData.fieldValue(1).toInt>0) lrData.fieldValue(1).toInt else layerData.fieldValue(2).toInt)
    }).toSeq.distinct.mkString(", "))    
  }
  
  def getLayIDs(currData:InstanceData):Constant= StringConstant(getFieldValue(currData, 0).mkString(", "))
}

