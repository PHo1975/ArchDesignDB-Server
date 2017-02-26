/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import definition.data.{InstanceData,Reference,Referencable,OwnerReference}
import client.comm.ClientQueryManager
import definition.typ.AllClasses
import definition.comm.NotificationType
import java.awt.geom.Rectangle2D
import definition.expression.VectorConstant
import definition.data.EMPTY_REFERENCE
import scala.swing.Swing
import definition.expression.IntConstant


abstract class AbstractLayer(val controller:GraphViewController,override val ref:Reference,
    var visible:Boolean,var edible:Boolean) extends Referencable /*with ElemContainer*/ {  
  def name:String 
  def id:String
  def scale:Int
  val bounds=new Rectangle2D.Double(0,0,0,0)
  
  val selfSubsID =if(ref==EMPTY_REFERENCE) -1 else ClientQueryManager.createSubscription(ref,-1) {
    (ntype:NotificationType.Value,data:IndexedSeq[InstanceData]) => Swing.onEDT{
      ntype match {
        case NotificationType.sendData| NotificationType.fieldChanged| NotificationType.updateUndo=>
					if(data.nonEmpty)
          setupSelfFromInstance(data.head)
					controller.layerModel.layerChanged(this)
				case NotificationType.instanceRemoved=>
					shutDown()
					controller.layerModel.layerRemoved(this)
			}
    }
  }
  def setLayerScale(newRelativeScaleID:Int)= {
    ClientQueryManager.writeInstanceField(ref,2.toByte,new IntConstant(newRelativeScaleID))
  }
  override def toString="L "+name+" "+ref
  
  def load(listener:Option[()=>Unit],alwaysNotify:Boolean):Unit
  def hide()=internalHide()
  protected def internalHide()= {
		visible=false		
		controller.selectModel.deselectLayer(this)		
		controller.layerChanged(this,false)		
	}
  def lock() = {
		edible=false
		controller.selectModel.deselectLayer(this)		
		controller.layerChanged(this,false)
	}
  
  def shutDown():Unit =internalShutDown()
  protected def internalShutDown():Unit={
    controller.layerChanged(this,false)
    if(selfSubsID> -1) ClientQueryManager.removeSubscription(selfSubsID)
  }
  
  def setupSelfFromInstance(data:InstanceData)={}  
  
  def getBounds:Rectangle2D.Double=bounds
  
  def calcBounds()={
		bounds.x=Double.MaxValue
		bounds.y=Double.MaxValue
		bounds.width=Double.MinValue
		bounds.height=Double.MinValue
		for(elem<-elemList) 
			controller.checkElemBounds(elem,bounds)
		//println("Layer "+name+" minmaxValue: "+bounds)	
		bounds.width-=bounds.x
		bounds.height-=bounds.y
		//println("Layer "+name+" bounds: "+bounds)
		bounds
	}
  def ownerRef:OwnerReference
  def elemList:Iterable[GraphElem]
  
  def filterSelection(onlyEdible:Boolean,filterFunc:(GraphElem)=>Boolean):Iterable[GraphElem] = {
		if (onlyEdible && !edible)  Seq.empty
		else  elemList.filter(filterFunc)
	}
  
 	
	def checkElementPoints(checkFunc:(GraphElem)=>Seq[(Byte,VectorConstant)]):Iterable[(Byte,VectorConstant)]= {
		if (!visible) Seq.empty
		else {
			val buffer=elemList.flatMap(checkFunc)				
			buffer
		}		
	}
	
	def checkElementPointsWithLayer(checkFunc:(GraphElem,ElemContainer)=>Iterable[(Byte,VectorConstant)]):Iterable[(Byte,VectorConstant)]= {
		if (!visible) Seq.empty
		else {
			val buffer=elemList.flatMap(checkFunc(_,controller))				
			buffer
		}		
	}
	
	 override def equals(other: Any): Boolean =
		other match {
				case that: AbstractLayer =>
				that.ref.equals(ref)			
				case _ => false
		}
	 
	 override def hashCode() = ref.hashCode
	
	 def getElementByRef(ref:Reference)= elemList.find(_.ref==ref)
  
}



/**
 * 
 */
class Layer(ncontroller:GraphViewController,nref:Reference,
    nvisible:Boolean,nedible:Boolean)
  extends AbstractLayer(ncontroller,nref,nvisible,nedible) {
  
  var name:String=""
  var id:String=""
  var scale:Int=0
  //println("Load LAyer:"+nref)
    
  override def setupSelfFromInstance(data:InstanceData)={
    name= data.fieldValue(1).toString
	  id=data.fieldValue.head.toString
	  scale=data.fieldValue(2).toInt
  }
	var subsID:Int= -1
	var _elemList:Seq[GraphElem]=IndexedSeq.empty  
	var startTime:Long=_
	var firstLoad=true
	val ownerRef=new OwnerReference(0,ref) 
	
	var alwaysNotifyVar=false
	var listenerVar:Option[()=>Unit]=None
	
	def load(listener:Option[()=>Unit],alwaysNotify:Boolean):Unit = {
	  alwaysNotifyVar=alwaysNotify
	  listenerVar=listener
	  val h=HatchHandler // load hatches
		visible=true
				
		if(subsID>=0) ClientQueryManager.changeSubscription(subsID,ref,0)		
		else subsID=ClientQueryManager.createFactSubscription(ref,0,GraphElemFactory){
			(ntype:NotificationType.Value,data:IndexedSeq[GraphElem]) => 
			Swing.onEDT{				
				ntype match {
						case NotificationType.sendData|NotificationType.updateUndo =>
							_elemList=data
							//println("Layer "+name+" elems loaded")
							calcBounds()
							//println("Layer "+name+" elems calcBounds")
							if(visible)controller.graphElementsChanged(this,elemList,true)
							//println("load layer firstLoad:"+firstLoad+" listener:"+listenerVar+" alwaysNotify:"+alwaysNotifyVar)
							if(ntype==NotificationType.sendData&&listenerVar.isDefined && (alwaysNotifyVar || firstLoad) ) {
                listenerVar.get()
              } else {
                if(visible)controller.layerChanged(this,false	)		//undo
              }
							firstLoad=false

						case NotificationType.fieldChanged  =>
							val searchRef=data.head.ref
							_elemList=_elemList.map(el=>if(el.ref==searchRef)data.head else el)
							if(visible) controller.graphElementsChanged(this,data)
						case NotificationType.instanceRemoved =>
							val searchRef=data.head.ref
							_elemList.find(searchRef==_.ref) match {
								case Some(oldElem)=>
									_elemList =_elemList.filter(searchRef!= _.ref)
									if(visible)controller.graphElemRemoved(this,oldElem)
								case _ =>
							}
						case NotificationType.childAdded =>
							_elemList= _elemList :+ data.head
							//checkElemBounds(data(0))
							if(visible)controller.graphElemAdded(this,data.head)
						case NotificationType.parentNotExistend => shutDown()
				}
			}
		}
	}
	def elemList=if(controller.layerModel.viewFilter.isDefined)_elemList.view.filter(controller.layerModel.viewFilter.get.elementMatch)
	 else _elemList
	
		
	override def hide() = {
		super.hide()
		
		ClientQueryManager.pauseSubscription(subsID)		
		_elemList=IndexedSeq.empty				
	}
	
	override def shutDown() = {
	  if(subsID> -1){
	    ClientQueryManager.removeSubscription(subsID)
	    subsID= -1
	  }	  
		_elemList=IndexedSeq.empty
		super.shutDown()
	}
	
	def scaleRatio=scale.toDouble
}




object Layer {
  val allowedDisplayListClasses=List("Layer","Plane","Schnittebene","MeasureLayer")
	var allowedDisplayListTypes:Seq[Int]=Seq.empty	
	ClientQueryManager.registerSetupListener(()=>{
	  //println("AllowedDisplayListTypes "+allowedDisplayListClasses.map(AllClasses.get.getClassIDByName(_)))
		allowedDisplayListTypes=allowedDisplayListClasses.map(AllClasses.get.getClassIDByName)
	})
	
	def createLayer(controller:GraphViewController,nref:Reference,visible:Boolean,edible:Boolean):AbstractLayer = {
		if(nref.typ==allowedDisplayListTypes.head)
		  new Layer(controller,nref,visible,edible)
		else { 
		  val data=ClientQueryManager.queryInstance(nref,-1).head
		  if(nref.typ==allowedDisplayListTypes(1))
		  new ConnAreaLayer(controller,data.ref,data.fieldValue.head.toString,visible,edible)
		else if(nref.typ==allowedDisplayListTypes(2))
		  new SectionPlaneLayer(controller,data.ref,data.fieldValue.head.toString,visible,edible)
		else if(nref.typ==allowedDisplayListTypes(3))
		  new MeasureLayer(controller,data.ref,visible,edible)
		else throw new IllegalArgumentException("Create Layer not possible for typ "+data.ref)
		}
	}  
}