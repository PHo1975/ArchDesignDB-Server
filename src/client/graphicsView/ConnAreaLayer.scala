package client.graphicsView
import definition.data.{Reference,InstanceData,OwnerReference}
import definition.expression.{VectorConstant,Polygon,NULLVECTOR}
import java.awt.geom.{Rectangle2D,Area}
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import scala.swing.Swing




class ConnAreaLayer(ncontroller:GraphViewController,nref:Reference,val name:String,    
    nvisible:Boolean,nedible:Boolean) extends AbstractLayer(ncontroller,nref,nvisible,nedible) { 
  
  class BorderInfo(val bref:Reference,val roomName:String,val roomColor:Int)
  //class AreaInfo(val aref:Reference,parents:Seq[BorderInfo],polygon:Polygon)  
  var subsID:Int= -1
  var elemList:Seq[GraphElem]=Seq.empty
  var scale=1
  var firstLoad=true
  def id="conn"
  
  def load(listener:Option[()=>Unit]=None,alwaysNotify:Boolean):Unit= {    
    visible=true
    if(subsID>=0) ClientQueryManager.changeSubscription(subsID,ref,0) else
    subsID=ClientQueryManager.createSubscription(ref,0){
			(ntype:NotificationType.Value,data:IndexedSeq[InstanceData]) => 
			Swing.onEDT{				
				ntype match {
						case NotificationType.sendData|NotificationType.updateUndo  =>
							val borders=data.map(d=>loadBorder(d.ref))
							val elMap=collection.mutable.HashMap[Reference,PolyElement]()
							borders.foreach(b=>{
								ClientQueryManager.queryInstance(b.bref,0) foreach {inst=> if(elMap.contains(inst.ref))  {
									val oldEl=elMap(inst.ref)
									oldEl.name=oldEl.name+" + "+b.roomName
								} else {
									elMap(inst.ref)=new PolyElement(b.bref,b.roomColor,0,0,0,None,false,inst.fieldValue(3).toPolygon,NULLVECTOR,0,b.roomName)
								}
								}
							})
							elemList=elMap.values.toSeq
							calcBounds()
							controller.layerChanged(this, false)
							controller.graphElementsChanged(this,elemList,false)
							controller.refreshCanvas()
							if(ntype==NotificationType.sendData&& (alwaysNotify||firstLoad)) for(l<-listener) l()
							firstLoad=false
						case _ => 
				}
			}
    }
  }
  
  private def loadBorder(bref:Reference):BorderInfo= {   
    val myInst=ClientQueryManager.queryInstance(bref,-1)
    val roomRef=myInst.head.owners(0).ownerRef
    val roomInst=ClientQueryManager.queryInstance(roomRef,-1)
    new BorderInfo(bref,roomInst.head.fieldValue(1).toString,roomInst(0).fieldValue.head.toInt)
  }
  
  
  override def hide():Unit= {
    super.hide()
		ClientQueryManager.pauseSubscription(subsID)		
		elemList=IndexedSeq.empty		
  }  
  
  
  override def shutDown():Unit= {
    ClientQueryManager.removeSubscription(subsID)
		elemList=IndexedSeq.empty
		super.shutDown()
  } 
  
  
  var ownerRef:OwnerReference= new OwnerReference(2,nref)
  
  //TODO: set scale Ratio for ConAreaLayer
  def scaleRatio=50d 
}