package client.graphicsView

import client.comm.ClientQueryManager
import definition.data.{InstanceData, Named, OwnerReference, Reference}
import definition.expression.IntConstant

class MeasureLayer(ncontroller: GraphViewController, nref: Reference, npath: Array[String],
                   nvisible:Boolean, nedible:Boolean) extends AbstractLayer(ncontroller,nref,nvisible,nedible) with SimpleLoader[GraphElem] {
  
  var name:String=""
  var scale:Int=0

  override def path: Array[String] = npath

  override def setupSelfFromInstance(data: InstanceData): Unit = {
    name=data.fieldValue(2).toString
    //println("Name:"+name)
    scale=data.fieldValue(3).toInt
  }
  
  var ownerRef:OwnerReference= new OwnerReference(0,nref)  
  def id="Mess"

  def createElement(inst: InstanceData): Option[LinearElement with Named] = {
    if (inst.ref.typ == MeasureElemFactory.AreaPolyClassID || inst.ref.typ == MeasureElemFactory.wohnflaechenClassID) {
      val hatch=inst.fieldValue(6).toInt
    Some(new AreaPolyElement(inst.ref,inst.fieldValue(1).toInt,inst.fieldValue(2).toInt,inst.fieldValue(3).toInt,
        inst.fieldValue(5).toInt,HatchHandler.getHatch(math.abs(hatch)),hatch<0,inst.fieldValue(4).toPolygon,
        inst.fieldValue(7).toVector,inst.fieldValue(8).toDouble,inst.fieldValue(9).toString,inst.fieldValue(11),inst.fieldValue(0).toUnitNumber))
    }
    else if (inst.ref.typ == MeasureElemFactory.measureLineClassID) {
      val hatch = inst.fieldValue(8).toInt
      Some(new MeasureLineElement(inst.ref, inst.fieldValue(1).toInt, inst.fieldValue(2).toInt, inst.fieldValue(3).toInt,
        inst.fieldValue(4).toPolygon, inst.fieldValue(5).toDouble, inst.fieldValue(6).toDouble, inst.fieldValue(7).toDouble,
        HatchHandler.getHatch(math.abs(hatch)), inst.fieldValue(9).toDouble, hatch < 0, inst.fieldValue(10).toString,inst.fieldValue(12),
        inst.fieldValue(0).toUnitNumber))
    }
    else None
  }

  override def setLayerScale(newRelativeScaleID: Int): Unit = {
    ClientQueryManager.writeInstanceField(ref, 3.toByte, IntConstant(newRelativeScaleID))
  }
 
}