package client.plotdesign
import java.awt.geom.Rectangle2D

import client.comm.ClientQueryManager
import client.graphicsView.{AbstractLayerModel, ElemContainer, Formatable, HatchHandler}
import definition.comm.NotificationType
import definition.data.Reference
import definition.expression.VectorConstant

import scala.swing.Swing

class LayerRefList(controller:PlotDesignController) extends AbstractLayerModel {
  var listLock=new Object
   var list:Seq[LayerRef]=Seq.empty
   var subsID= -1
   val bounds=new Rectangle2D.Float
   val h=HatchHandler // load hatches
   
   
   def load(ref:Reference,doneListener:()=>Unit)= {
     //println("load layerRefList: "+ref)
     //var firstLoad=true
     var layersLoaded=0
     if(subsID== -1) {
       subsID=ClientQueryManager.createSubscription(ref,1.toByte){(ntype,data)=> listLock.synchronized {Swing.onEDT{
           ntype match {
             case NotificationType.sendData =>
               list=data.map(LayerRef(controller,_))
               if(list.isEmpty) {
                 controller.layersChanged()
                 calcBounds()
                 doneListener()
               }
               else list.head.loadLayer(loadNextLayer _)
             case NotificationType.updateUndo =>
               for (nd<-data)
                 list.find(_.ref==nd.ref) foreach (_.setData(nd))
             case NotificationType.fieldChanged =>
               val newInst=data.head
               list.find(_.ref==newInst.ref) foreach (_.setData(newInst))
               calcBounds()
             case NotificationType.childAdded =>
               val newInst=LayerRef(controller,data.head)
               list=newInst +: list
               newInst.loadLayer(()=>{})
               controller.layersChanged()
             case NotificationType.instanceRemoved=>
               val removeRef=data.head.ref
               val removeEl=list.find(_.ref==removeRef)
               list=list.filterNot(_.ref==removeRef)
               for(el<-removeEl) {
                 el.shutDown()
                 controller.layerRemoved(el)
                 calcBounds()
               }
             case NotificationType.parentNotExistend=>
               doneListener()
           }
       	 }
       }}
     }
     else doneListener()
     
     def loadNextLayer():Unit = listLock.synchronized{
       //println("load next Layer :"+layersLoaded)
       layersLoaded+=1
       if(layersLoaded<list.size)
         list(layersLoaded).loadLayer(loadNextLayer _)
       else {         
         doneListener()
       }
     } 
   }
   
   //def doNothing()={}
   
   def calcBounds()= listLock.synchronized{
     val pb=controller.pageModel.bounds
     var x1=pb.x
     var y1=pb.y
     var x2=pb.x+pb.width
     var y2=pb.y+pb.height
     for(lay<-list;lb=lay.screenBounds) {
       if(lb.x<x1) x1=lb.x
       if(lb.y<y1) y1=lb.y
       if((lb.x+lb.width)>x2) x2=lb.x+lb.width
       if((lb.y+lb.height)>y2) y2=lb.y+lb.height
     }
     bounds.setRect(x1,y1,x2-x1,y2-y1) 
     controller.calcAllBounds()     
   }   
   
   def shutDown() = listLock.synchronized { if(subsID!= -1) {
     ClientQueryManager.removeSubscription(subsID)
     subsID= -1
     list foreach (_.shutDown())
     list=Seq.empty
   }}
   
   def filterSelection(filterFunc:(LayerRef)=>Boolean):Seq[LayerRef] = list.filter(filterFunc)
   
   def checkElementPoints(checkFunc:(Formatable)=>Seq[(Byte,VectorConstant)]):Seq[(Byte,VectorConstant)] = {
     list.flatMap(checkFunc)
   }
   
   def checkElementPointsWithLayer(checkFunc:(Formatable,ElemContainer)=>Seq[(Byte,VectorConstant)]):Seq[(Byte,VectorConstant)]= {
     list.flatMap(e=>checkFunc(e,null))
   }
   
   def isEmpty:Boolean= list.isEmpty 
}