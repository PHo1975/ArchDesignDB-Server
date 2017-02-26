/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import client.dialog.form.FormBox

import scala.swing._
import client.dataviewer.sidePanel.SidePanelController
import client.dialog.AbstractFocusContainer
import client.dialog.EMPTY_GROUP
import client.dialog.SelectSender
import client.model.FormPanel
import client.model.PathControllable
import definition.data.EMPTY_OWNERREF
import definition.data.InstanceData
import definition.data.Referencable
import definition.data.Reference
import definition.typ.AbstractObjectClass
import definition.typ.AllClasses
import definition.typ.CustomInstanceEditor
import definition.typ.SelectGroup
import client.model.TableViewbox
//import server.test.SimpleProfiler

/** controls the DataViewer
 *  manages the general loading of data
 * 
 */


class DataViewController(val viewBox:TableViewbox)  extends PathControllable with SelectSender with AbstractFocusContainer with Referencable  {
	//SimpleProfiler.dprint =true
	private var loaded=false
	var ref:Reference= _
	var mainClass:AbstractObjectClass = _	
	var selGroup=new SelectGroup[InstanceData](EMPTY_OWNERREF,Seq.empty)
	var selGroupList=List(selGroup)		
	val propertyModels =scala.collection.mutable.ArrayBuffer[PropertyModel]()
		
	var activeSidePanelController:Option[SidePanelController]=None
	val formModel=new FormModel(this)
	//var openChildCallBack:(Reference)=> Unit = _
	
	var runningEditor:Option[CustomInstanceEditor[Component]]=None
	val maxDimension=new Dimension(Int.MaxValue,Int.MaxValue)
	val minDimension=new Dimension
	
	var lastFocusedTable:Option[Table]=None
		
	val tablePanel=new BoxPanel(Orientation.Vertical)  {		  
		override lazy val peer = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {       
			val l = new javax.swing.BoxLayout(this, Orientation.Vertical.id)
			setLayout(l)			
			def getPreferredScrollableViewportSize: Dimension=getPreferredSize		  
			override def getMaximumSize=maxDimension
			override def getMinimumSize=getPreferredSize
			def getScrollableTracksViewportHeight: Boolean =false
			def getScrollableTracksViewportWidth: Boolean=false
			def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
			def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10			
		}		
	}	
	
	val tableScroller=new ScrollPane()  {  			   			
  	viewportView=tablePanel
  	peer.setWheelScrollingEnabled(true) 
  	xLayoutAlignment=0d
  	minimumSize=new Dimension(100,120)  	
  	
  }	
	
	val formPanel= new FormPanel(this)	
	
	val splitBox=new BoxPanel(Orientation.Vertical){
	    xLayoutAlignment=0d
	    contents+=formPanel+=tableScroller	    
	    maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
	  }
	
	def closeSplitBox()= {	  
	  if(splitBox.contents.contains(formPanel))
	  	splitBox.contents.-=(formPanel)
	  splitBox.revalidate()
	}
	
	def maximizeSplitBox(): Unit = {
	  splitBox.contents.clear()
	  splitBox.contents+=formPanel
	  splitBox.revalidate()
	}
	
	def splitBoxToNormal(): Unit = {
	  if(splitBox.contents.size!=2) {
	    splitBox.contents.clear()
	    splitBox.contents+=formPanel+=tableScroller
	    splitBox.revalidate()
	    splitBox.repaint()
	  }
	}	
	
	
	/** is called by PathModel when another instance should be loaded
	 *  @param parentRef the new Instance to be loaded
	 *  @param selectRef reference of an instance that should be selected
	 */
	def openData(nparentRef:Reference,selectRef:Option[Reference],indent:Int,doneListener:Option[()=>Unit]) = {		
		
	  if(loaded) shutDown()
	  ref=nparentRef	  	  
	  mainClass=AllClasses.get.getClassByID(ref.typ)
	  
	  def numPropfieldsToLoad=if(DataViewController.hideProperties) mainClass.propFields.foldLeft(0)((a,b)=>if(!b.hidden)a+1 else a)
	    else mainClass.propFields.size	   
	  var loadedField=0	  
	  
	  mainClass.customInstanceEditor match {
	  	case Some(editorName) =>
				val ed=Class.forName(editorName).newInstance.asInstanceOf[CustomInstanceEditor[Component]]
				runningEditor=Some(ed)
				if(ed.fullSize)ed.load(nparentRef,()=>for(d<-doneListener)d()) else ed.load(nparentRef,formsLoaded _)
				formPanel.setCustomEditor(ed)
				tableScroller.visible= !ed.fullSize
			case None => // no editor, load FormBox
        val newForm=mainClass.formBox map(_.makeCopy)
				tableScroller.visible=true
				newForm match {
					case Some(f:FormBox)=>
						formPanel.setForms(Some(f),indent,nparentRef)
						formModel.setForms(mainClass,Some(f))
					case None=>
						formPanel.setForms(None,indent,nparentRef)
						formModel.setForms(mainClass,None)
					case _=> util.Log.w("unknown formbox "+newForm)
				}
				formModel.loadData(nparentRef,formsLoaded _)
		}

		def formsLoaded()={
		  loadedField=0
		  //println("Forms loaded "+nparentRef+" numFieldsToLoad:"+numPropfieldsToLoad)
			val hiddenFields=mainClass.getNum_FirstHiddenPropFields
			var propIx=0
			if(numPropfieldsToLoad==0){
			  //println("NumPropfieldsToLoad==0")
			  for(d<-doneListener)d()
			} 
			else for(i <- 0 until mainClass.propFields.size) {
				val propFieldInfo=mainClass.propFields(i)
				if(!(propFieldInfo.hidden && DataViewController.hideProperties)) {
					val mod= new PropertyModel(propIx,this)
					propIx+=1
					propertyModels append mod
					tablePanel.contents+=mod.panel	  	
					mod.load(propFieldInfo.allowedClass,i.toByte,propFieldInfo.name,selectRef,i==hiddenFields,mainClass.propFields.size==1,
					    propFieldInfo.single, propFieldLoaded _)
				}	  	
			}					  
		}	  
		
		def propFieldLoaded() = {		  
		  loadedField+=1
		  //println("PropFieldLoaded "+loadedField+" numFieldsToLoad:"+numPropfieldsToLoad)
		  if(loadedField>=numPropfieldsToLoad) {
		  	if(!selectRef.isDefined) notifySelectListeners(EMPTY_GROUP.list)
		  	//println("Loaded=true")
		  	loaded =true
				tableScroller.horizontalScrollBar.value=0
		  	for(d<- doneListener)d()  
		  }
		}	  
	}
	
	
	def propFieldExitsToUp(propIx:Int):Unit={
	  var ix=propIx
	  while(ix>0){
	    if(propertyModels(ix - 1).tableModMap.nonEmpty) {
	    	propertyModels(ix-1).enterFromBottom()
	    	return
      }
	    ix-=1
	  }
	}
	
	def propFieldExitsToDown(propIx:Int):Unit={
	  var ix=propIx
	  while(ix<propertyModels.size-1){
	    if(propertyModels(ix + 1).tableModMap.nonEmpty){
	      propertyModels(ix+1).enterFromTop()
	      return
	    }
	    ix+=1
	  } 
	  
	}
	
	/*def registerOpenChildCallBack(callBack: (Reference)=> Unit) = {
		openChildCallBack=callBack
	}	*/
	
	
	def updateHeight() = Swing.onEDT {	 
	  tablePanel.revalidate()
	  formPanel.revalidate()
	  splitBox.revalidate()
	}
	
	
	def shutDown():Unit = if(loaded){	  
		runningEditor match {
			case Some(editor)=> editor.shutDown()
			case None => formModel.shutDown()
		}		
		tablePanel.contents.clear()
		updateHeight()
		propertyModels foreach (_.shutDown())
		// save the models for later use		
		propertyModels.clear()
		loaded=false		
	}
	
	
	def selectionChanged(tabMod:TypeTableModel,proMod:PropertyModel,instList:Seq[InstanceData]):Unit = {
		//System.out.println("sel: propfield:"+proMod.propertyField+" typ:"+tabMod.typ +" \n"+instList.mkString)
		selGroup.parent =proMod.ownerRef
		selGroup.children =instList
		for(mod<-propertyModels)
			mod.deselect(if(proMod==mod) tabMod.typ else -1)
		selectListeners foreach(_.selectionChanged(this,selGroupList))			
	}
	
	def containerFocused(atable:Option[Table],currPropertyField:Int):Unit = {	 
	  atable match {
	    case a @ Some(table)=> lastFocusedTable=a
	    case _=> 
	  }
	  notifyContainerListeners(currPropertyField)		
	}
	
	def deselect(notify:Boolean) = {
		for(mod<-propertyModels)
			mod.deselect(-1)
		if(notify)
		  notifyContainerListeners(0)		
	}
	
	/** sends a message to the path controller that it should open a child instance
	 * 
	 * @param ref the reference of the child instance
	 */
	def openChild(ref:Reference) = viewBox.pathController.addPathElement(ref)
    
	
	// FocusContainer interface	
	def containerName=""	
	def containerRef=Some(this)
	def requestFocus() = {
	  lastFocusedTable match {
	    case Some(table)=> table.requestFocusInWindow()
	    case _=>
	  }
	}	
}

object DataViewController {
  val printCommand="Ausgabe"
	var hideProperties:Boolean=false	
	var pathIndent=" \u2002"
}
