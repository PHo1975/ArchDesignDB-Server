package client.graphicsView

import scala.collection.Seq
import definition.data.OwnerReference
import definition.data.Reference
import definition.data.InstanceData
import client.comm.ClientQueryManager
import definition.expression.IntConstant

class MeasureLayer(ncontroller:GraphViewController,nref:Reference,    
    nvisible:Boolean,nedible:Boolean) extends AbstractLayer(ncontroller,nref,nvisible,nedible) with SimpleLoader[GraphElem] { 
  
  var name:String=""
  var scale:Int=0

  override def setupSelfFromInstance(data:InstanceData)= {
    name=data.fieldValue(2).toString
    //println("Name:"+name)
    scale=data.fieldValue(3).toInt
  }
  
  var ownerRef:OwnerReference= new OwnerReference(0,nref)  
  def id="Mess"    
    
  def createElement(inst:InstanceData)={
    if(inst.ref.typ==GraphElemConst.AreaPolyClassID) {
      val hatch=inst.fieldValue(6).toInt
    Some(new AreaPolyElement(inst.ref,inst.fieldValue(1).toInt,inst.fieldValue(2).toInt,inst.fieldValue(3).toInt,
        inst.fieldValue(5).toInt,HatchHandler.getHatch(math.abs(hatch)),hatch<0,inst.fieldValue(4).toPolygon,
        inst.fieldValue(7).toVector,inst.fieldValue(8).toDouble,inst.fieldValue(9).toString))
    }
    else None
  }
  
  override def setLayerScale(newRelativeScaleID:Int)= {
    ClientQueryManager.writeInstanceField(ref,3.toByte,new IntConstant(newRelativeScaleID))
  }
 
}