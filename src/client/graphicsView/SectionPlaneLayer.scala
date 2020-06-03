package client.graphicsView

import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data.{Composition, InstanceData, OwnerReference, Reference}
import definition.expression.VectorConstant

import scala.collection.mutable
import scala.swing.Swing

trait SimpleLoader[A <: GraphElem]{
  self: AbstractLayer =>
  var elemList:Seq[A]=Seq.empty
  var subsID:Int= -1
  var firstLoad=true
  var alwaysNotifyVar=false
  var listenerVar:Option[()=>Unit]=None
  
  def createElement(inst:InstanceData):Option[A]

  def updateElements(zoomAll: Boolean): Unit = {
    calcBounds()
    //controller.layerChanged(this,zoomAll)

    controller.graphElementsChanged(this, elemList)
  }

  def updateElement(el: A, zoomAll: Boolean): Unit = {
    //calcBounds()
    //controller.layerChanged(this,zoomAll)
    controller.graphElementsChanged(this, List(el))
  }
  
  def load(listener:Option[()=>Unit]=None,alwaysNotify:Boolean): Unit = {    
    visible=true
    alwaysNotifyVar=alwaysNotify
    listenerVar=listener
    if(subsID>=0) ClientQueryManager.changeSubscription(subsID,ref,0) else
    subsID=ClientQueryManager.createSubscription(ref,0){
			(ntype:NotificationType.Value,data:IndexedSeq[InstanceData]) => 
			Swing.onEDT{				
				ntype match {
						case NotificationType.sendData|NotificationType.updateUndo  =>
              elemList=data flatMap createElement
              updateElements(ntype!=NotificationType.sendData)
              if(ntype==NotificationType.sendData&&(alwaysNotifyVar||firstLoad)){
                for(l<-listenerVar) l()
                firstLoad=false
              }
            case NotificationType.childAdded =>
              createElement(data.head) match {
                case Some(el)=>
                  elemList=elemList :+ el
                  if (visible) controller.graphElemAdded(this, el)
                case None =>
              }
            case NotificationType.fieldChanged =>
              val sref=data.head.ref
              val nElement = createElement(data.head)
              //println("Lay field changed "+nElement)
              elemList=elemList.map(el =>if(el.ref==sref){
                nElement match {
                  case Some(crel)=>crel
                  case None =>el
                }
              }  else el)
              if (visible) for (ne <- nElement)
                updateElement(ne, false)
            case NotificationType.instanceRemoved =>
              val searchRef=data.head.ref
              elemList.find(searchRef==_.ref) match {
                case Some(oldElem)=>
                  elemList =elemList.filter(searchRef!= _.ref)
                  controller.graphElemRemoved(this,oldElem)
                case _ =>
              }
            case _ =>
				}
			}
    }
  }
  override def hide(): Unit = {
    if(subsID>0) ClientQueryManager.pauseSubscription(subsID)
		elemList=IndexedSeq.empty		
		internalHide()
  }
 

  override def shutDown(): Unit = {
    if(subsID>0)ClientQueryManager.removeSubscription(subsID)
    subsID=0
		elemList=IndexedSeq.empty
		internalShutDown()
		
  }

  
}

class SectionPlaneLayer(ncontroller:GraphViewController,nref:Reference,val name:String,    
    nvisible:Boolean,nedible:Boolean) extends AbstractLayer(ncontroller,nref,nvisible,nedible) with SimpleLoader[SectionLineElement] {
  
  var scale=1 
  var ownerRef:OwnerReference= new OwnerReference(2,nref)  
  def id="Sect"

  val pointMap: mutable.HashMap[VectorConstant, List[LineConnection]] = collection.mutable.HashMap[VectorConstant, List[LineConnection]]()
  
  CompositionHandler.registerListener(compositionsUpdated)

  override def updateElements(zoomAll: Boolean): Unit = {
    connectLines()    
    super.updateElements(zoomAll)
    //controller.canvas.repaint
  }
  
  private def connectLines() = {    
    for(line <-elemList){
      line.p1RightConnection=None
      line.p2RightConnection=None
      line.p1LeftConnection=None
      line.p2LeftConnection=None      
      checkPoint(line,line.startPoint,true)
      checkPoint(line,line.endPoint,false)
    }
    
    println( pointMap.mkString)
    
    for (point <-pointMap.keysIterator;connList=pointMap(point);if connList.size > 1){ // iterate all connected points
      for (lineConn <-connList) {        
        val thisLine=lineConn.otherLine        
        val angles=connList.filterNot(_.otherLine.ref==thisLine.ref)map(l=>(l,SectionLineElement.getAngle(thisLine,l,lineConn.isP1)))
        val otherLines=angles.sortWith(_._2 <= _._2)        
        if(lineConn.isP1) thisLine.p1LeftConnection=Some(otherLines.head._1)
        else thisLine.p2LeftConnection=Some(otherLines.head._1)
        if(otherLines.size>1) {
          if(lineConn.isP1)thisLine.p1RightConnection=Some(otherLines.last._1)
          else thisLine.p2RightConnection=Some(otherLines.last._1)
        }
        
      }
    }
    pointMap.clear()
    for(el<-elemList) {
      el.adaptLineLengths()
      el.updateHatches()
    }
  }  
  
  
  private def checkPoint(line:SectionLineElement,point:VectorConstant,isP1:Boolean)= {
    if(pointMap.contains(point)) {
      val oldList=pointMap(point)
      pointMap(point)=new LineConnection(line,isP1 ):: oldList
    } else {
      val ix=pointMap.keysIterator.indexWhere(VectorConstant.similar(_,point))
      if(ix== -1){ // no similar point, create new entry
        pointMap(point)=List(new LineConnection(line,isP1))
      }
      else {
        val similarPoint=pointMap.keys.toSeq(ix)
        val oldList=pointMap(similarPoint)
        pointMap(similarPoint)=new LineConnection(line,isP1 ):: oldList
      }
    }
  }
  
  def createElement(data:InstanceData)= {
    Some(new SectionLineElement(data.ref,data.fieldValue.head.toVector,data.fieldValue(1).toVector,data.fieldValue(2).toVector,
        data.fieldValue(3).toInt,data.fieldValue(4).toDouble,data.owners(1).ownerRef))
  }
  
  def compositionsUpdated(newValue:Map[Int,Composition]) = if(subsID> -1){  
    //println("compos updated ")
    elemList=elemList.map { case s: SectionLineElement => s.createClone; case o => o}
    calcBounds()
    controller.layerChanged(this,true)														
    controller.graphElementsChanged(this,elemList,false)
    controller.canvas.repaint()
  }
  def scaleRatio=50d
}