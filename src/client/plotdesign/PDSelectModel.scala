package client.plotdesign

import client.dialog.SelectSender
import client.dialog.SelectListener
import definition.typ.SelectGroup
import definition.data.Reference
import definition.data.OwnerReference
import definition.data.EMPTY_REFERENCE
import client.graphicsView.AbstractSelectModel
import client.graphicsView.GraphElem
import client.graphicsView.Formatable

class PDSelectModel(controller:PlotDesignController) extends SelectSender with AbstractSelectModel[LayerRef] {
  
  var selGroup:SelectGroup[LayerRef]=new SelectGroup[LayerRef](new OwnerReference(0,EMPTY_REFERENCE),List.empty)
  var selGroupList=List(selGroup)
  //rivate val listeners=collection.mutable.HashSet[SelectListener]()  
  
  //var lastSelection:collection.Map[Int,Formatable]=Map.empty

  def setOwner(ref:Reference):Unit= {
    selGroup.parent=new OwnerReference(1.toByte,ref)
  }
  
  def deselect(notify: Boolean): Unit = {
    selGroup.children=Nil
    controller.stopModus()
		controller.canvas.repaint()
		if(notify) notifyListeners()
  }

  /*def registerSelectListener(listener:SelectListener)= {
		listeners+=listener
	}*/
  
  def notifyListeners()={
	  val alsoSelected=controller.lastHittedElements
	  selectListeners.foreach(_.selectionChanged(this,selGroupList,alsoSelected))
	}
  
  def addSelection(newElems:Iterable[LayerRef],toggle:Boolean) ={		
    //println("Add selection "+newElems.size+" "+toggle+" "+Thread.currentThread.getStackTrace().drop(2).take(10).mkString("\n"))
		if(toggle) {
			for(ne<-newElems) {
			  if(selGroup.children.exists(_==ne)) selGroup.children=selGroup.children.filterNot(_.ref==ne.ref)
				else  selGroup.children=ne :: selGroup.children.toList				 
			}				
		}
		else for(ne<-newElems)
			if(!selGroup.children.exists(_==ne)) selGroup.children=ne :: selGroup.children.toList
		storeSelectionList()	
		notifyListeners()
	}	
	 
	
	def setSelection(newElems:Iterable[LayerRef]) = {		
	  //println("set selection "+Thread.currentThread.getStackTrace().drop(2).take(10).mkString("\n"))
	  selGroup.children=newElems		
		storeSelectionList()
		notifyListeners()
	}
	
	def elemRemoved(elem:LayerRef) = {	
		selGroup.children=selGroup.children.filter(_.ref!=elem.ref)
		storeSelectionList()
		notifyListeners()
	}
	
	def elementChanged(newEl:LayerRef)=	if(selGroup.children.exists(_.ref==newEl.ref))
		  selGroup.children=selGroup.children.map(el=>if(el.ref==newEl.ref) newEl else el)		
	
	
	def selectionContainsOneOf(testEls:Iterable[LayerRef]):Boolean=
    testEls.exists(selectionList.contains)
	
	/*def elementsChanged(newElements:Seq[LayerRef])={		
		for(el<-newElements) {
		  val ix=selGroup.children.indexWhere(_.ref==el.ref)
		  if(ix> -1) selGroup.children=selGroup.children.updated(ix,el)	
		}	
		storeSelectionList() 
	}*/
	
	def selectionList=selGroup.children.toIterator
  
  def getPointsInRectangle(minX:Double,minY:Double,maxX:Double,maxY:Double)={
	  Seq.empty
	}

}