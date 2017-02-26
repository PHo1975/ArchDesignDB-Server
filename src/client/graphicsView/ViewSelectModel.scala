/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import client.dialog._
import scala.collection.mutable.ArrayBuffer
import definition.typ.SelectGroup
import definition.data.Referencable
import definition.expression.VectorConstant
import java.awt.geom.Rectangle2D

/**
 * 
 */

trait AbstractSelectModel[A] {
  def deselect(notify:Boolean):Unit
  
  val lastSelection=collection.mutable.HashMap[Int,Formatable]()

  def selectionList:Iterator[Formatable]
  def storeSelectionList():Unit = {	 
 //TODO: only update, when format fields were changed    
    lastSelection ++= selectionList.map(el => (el.ref.typ,el)).toMap
	}
  
  def addSelection(el:Iterable[A],toggle:Boolean):Unit
  def setSelection(el:Iterable[A]):Unit
  def elemRemoved(el:A):Unit
  def elementChanged(el:A):Unit  
  def selectionContainsOneOf(testEls:Iterable[A]):Boolean  
  def getPointsInRectangle(minX:Double,minY:Double,maxX:Double,maxY:Double):TraversableOnce[VectorConstant]
}




class ViewSelectModel(controller:GraphViewController) extends SelectSender with AbstractSelectModel[(AbstractLayer,Iterable[GraphElem])] {
	private val elMap=collection.mutable.HashMap[AbstractLayer,SelectGroup[GraphElem]]()	
	
	def list=elMap.valuesIterator
	
	def deselect(notify:Boolean)= {	  
		elMap.clear()
		controller.lastHittedElements=Nil
	  controller.clearNewElements()
		controller.stopModus()
		controller.canvas.repaint()
		if(notify) notifyListeners(true)
	}
	
	/** removes all elements from the given Layer from the selection
	 * 
	 * @param lay
	 */
	def deselectLayer(lay:AbstractLayer) = {
		elMap-= lay
		notifyListeners	()
	}
	
	/** adds the elements to the selection
	 *
	 * @param toggle should the elements be removed when they are already in the selection ?
	 */
	def addSelection(dataList:Iterable[(AbstractLayer,Iterable[GraphElem])],toggle:Boolean) ={
	  for(data<-dataList;if data._2.nonEmpty) {
	  	val elList=getElList(data._1) 
	  	if(toggle) {
	  		for(ne<-data._2) {
	  			val ix=elList.indexOf(ne)
	  			if(ix<0) elList +=ne
	  			else elList.remove(ix)
	  		}				
	  	}
	  	else for(ne<-data._2)
	  		if(!elList.contains(ne)) elList +=ne
	  }
		storeSelectionList()	
		notifyListeners()
	}
	
	 
	
	def setSelection(dataList:Iterable[(AbstractLayer,Iterable[GraphElem])]) = {
		//System.out.println("set selection "+dataList.mkString("| "))	  
	  for((alayer,list)<-dataList){
	    if(list.isEmpty)throw new IllegalArgumentException("Set selection empty content layer:"+alayer)
	  	val elList=getElList(alayer)
	  	elList.clear()
	  	elList ++=list
	  	storeSelectionList()	  	
	  }
		notifyListeners()
	}
	
	def getSelectionFromLayer(lay:AbstractLayer)= elMap.get(lay).map(_.children)
	
	private def getElList(lay:AbstractLayer)=
	  elMap.getOrElseUpdate(lay,new SelectGroup[GraphElem](lay.ownerRef,new ArrayBuffer[GraphElem])).children.asInstanceOf[ArrayBuffer[GraphElem]]	  
	
		
	def elemRemoved(el:(AbstractLayer,Iterable[GraphElem])) = {
		val elList=getElList(el._1)
		val elListSize=elList.size
		elList --= el._2		
		if(elList.size!=elListSize) {
			storeSelectionList()
			notifyListeners()	
		}
	}
	
	
	def elementChanged(el:(AbstractLayer,Iterable[GraphElem]))={
		val elList=getElList(el._1)
		val elListSize=elList.size
		for(changedEl<-el._2) {
		  val ix=elList.indexWhere(_.ref==changedEl.ref)
		  if(ix> -1) elList(ix)=changedEl	
		}	
		//println("El changed elList:"+elList.size+ "old size:"+elListSize)		
		storeSelectionList() 
	}
	
	
	def notifyListeners(cleared:Boolean=false)={	  
	  val alsoSelected=if(cleared) Nil else controller.lastHittedElements.flatMap(_._2)
	  val groups=if(cleared) Nil else elMap.values.filterNot(_.children.isEmpty)
	  selectListeners.foreach(_.selectionChanged(this,groups,alsoSelected))
	}
	
	def selectionList:Iterator[GraphElem]= for(gr <-elMap.valuesIterator; el <-gr.children;if el != null) yield el
	
	def numSelected=elMap.values.foldLeft(0)(_+ _.children.size)
	
	def selectionContainsOneOf(testEls:Iterable[(AbstractLayer,Iterable[GraphElem])]):Boolean=
    testEls.exists{case((lay,elems))=> elems.exists(selectionList.contains)}

	
	def selectionBounds()={
	  val bounds=new Rectangle2D.Double(Double.MaxValue,Double.MaxValue,Double.MinValue,Double.MinValue)
		for(elem<-selectionList) 
			controller.checkElemBounds(elem,bounds)
		//println("Layer "+name+" minmaxValue: "+bounds)	
		bounds.width-=bounds.x
		bounds.height-=bounds.y
		//println("Layer "+name+" bounds: "+bounds)
		bounds
	}
	
	def getPointsInRectangle(minX:Double,minY:Double,maxX:Double,maxY:Double):TraversableOnce[VectorConstant]={
	  for(lay<-elMap.valuesIterator;el<-lay.children;points=el.getEdiblePoints;p<-points
        if p.x >= minX && p.y >= minY && p.x <= maxX && p.y <= maxY) yield p
	}
	
	def getDragDropSelection:GraphElemTransferable = {
	  new GraphElemTransferable((for((layer,selGroup)<-elMap;if selGroup.children.nonEmpty)
	      yield new LayerTransferable(layer.ref,layer.name,selGroup.children)
	    ).toArray)	  
	}
	
	def hasSelection= elMap.nonEmpty && elMap.values.exists(_.children.nonEmpty)
	
	def filter(filterFunc:(GraphElem)=>Boolean):Unit= {
	  for((lay,selGroup)<-elMap){
	    val newList=selGroup.children.filter(filterFunc)
	    if(!newList.equals(selGroup.children)){
	      if(newList.isEmpty) elMap.-=(lay)
	      else elMap.update(lay, new SelectGroup(selGroup.parent,newList))
	    }	     
	  }
	  notifyListeners()
	}

}