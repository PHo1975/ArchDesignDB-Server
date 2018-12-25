/**
 * Author: Peter Started:16.09.2010
 */
package client.model

import java.awt.{Color, Dimension}

import client.dataviewer.{DataViewController, ViewConstants}
import definition.data._
import util.MyListView

import scala.swing._
import scala.swing.event._





/** Renderer for a pathView line
 * 
 */
class PathLineRenderer(model:Option[PathModel]=None) extends BoxPanel(Orientation.Horizontal ) {
  val firstLabel: Label = ViewConstants.label()
  val resultLabel: Label = ViewConstants.label()

		firstLabel.yLayoutAlignment=0d
		resultLabel.yLayoutAlignment=0d
		val glue: Component =Swing.Glue
		glue.yLayoutAlignment=0d
  maximumSize = new Dimension(Short.MaxValue, 21 * ViewConstants.fontScale / 100)
		contents+=firstLabel+=glue+=resultLabel		
		yLayoutAlignment=0
		xLayoutAlignment=0

  override def foreground_=(c: Color): Unit = {
			firstLabel.foreground= c
			resultLabel.foreground=c
		}
		
		def config( isSelected: Boolean, focused: Boolean, a: InstanceData, index: Int): Unit = {
			opaque=true			       
  	  firstLabel.text=getLabelText(a,index)
  	  firstLabel.horizontalAlignment=Alignment.Left
  	  resultLabel.text=if (a!=null)a.resultString else ""  	 
		} 
		
		def getLabelText(a:InstanceData,index:Int):String = {
		val prefix=if(index==0) "\u252c"
  		else if (model.isDefined){
  			if(index==model.get.dataList.size-1) "\u2514\u2500"
  			else "\u2514\u252c"	
  		}  
  		else "\u2514\u2500"
  	val indent=DataViewController.pathIndent * (if(index==0) 0 else index - 1)
  	indent+prefix+" "+(if(a!=null) a.toString() else "")
	}
	}


/** manages the connection of a PathModel and a ListView
 * 
 */
class PathController (val model:PathModel, val view:MyListView[InstanceData],val listener:PathControllable) {
	
	private val lock=new Object
	def lineHeight=24	
	private var oldIndex= -1	
	private val renderPrototype=new PathLineRenderer(Some(model))
	private var sizeChangeListeners= collection.mutable.HashSet[(Int)=>Unit]()
	protected var _withCustomEditor:Boolean=true
	
	view.focusable=false
	view.peer.setModel(model)
	view.peer.setDragEnabled(true)
	view.selection.intervalMode=MyListView.IntervalMode.Single	
	
	view.background=new Color(225,225,230)
	view.selectionForeground=new Color(0,0,40)
	view.selectionBackground=new Color(210,210,215)
	
	view.listenTo(view.selection)
	view.renderer=new MyListView.AbstractRenderer[InstanceData,PathLineRenderer](renderPrototype){
		def configure(list: MyListView[InstanceData], isSelected: Boolean, focused: Boolean, a: InstanceData, index: Int): Unit = {
			component.config(isSelected,focused,a,index)
		}
		
	}
	view.reactions += {
		case e:ListSelectionChanged[_] =>
			if (!e.live&& view.selection.indices.nonEmpty) selectionChanged(view.selection.indices.head)
	}

  def selectionChanged(newPos: Int): Unit = lock.synchronized {
		//println("selectionChanged: newPos:"+newPos+" oldIndex:"+oldIndex+ " model.getSize:"+model.getSize/*+" updating:"+updating*/)
		if (  (newPos!=oldIndex) && (newPos <= model.getSize) ) {
			
			// change Subscription only to the remaining elements
			val selectRef:Option[Reference] = // what element to select in the table
			if(newPos<oldIndex) {
				if(newPos >=model.getSize) None
				else {
					val ret=Some(model.getInstanceAt(newPos+1).ref) // select last pos below newPos
					model.jumpUp(newPos)
					ret
				}
			} else None			
			oldIndex=newPos
			// notify listener
			listener.openData(model.getInstanceAt(newPos).ref,selectRef,newPos,None,true)
		}		
		notifySizeListeners()
	}


  def loadPath(newPath: Seq[Reference], doneListener: () => Unit, selectRef: Option[Reference] = None): Unit = lock.synchronized {
	  var firstTime=true

    def selectLastLine(sendPath: Seq[Reference]): Unit = Swing.onEDT {
		  //println("Select last Line newpath size:"+ sendPath.size+" firstTime:"+firstTime+ "listener:" +doneListener)
			view.selectIndices(-1)
			oldIndex=model.getSize()
			notifySizeListeners()
      //doneListener()
      //if(selectRef.isEmpty)
      listener.openData(sendPath.last, selectRef, sendPath.size - 1, if (firstTime) Some(doneListener) else None,_withCustomEditor)
			_withCustomEditor=true
      //else if(firstTime) doneListener
      if (firstTime) firstTime = false
    }

    //if(pathEquals(model.dataList,newPath)) doneListener()
    //else
    model.loadPath(newPath, selectLastLine)
	} 

	
	
	/** adds a new element to the path
	 */
  def addPathElement(newElement: Reference,withCustomEditor:Boolean=true): Unit = lock.synchronized {
		oldIndex+=1
		_withCustomEditor=withCustomEditor
		model.addPathElement(newElement)
		notifySizeListeners()		 
	}


  def shutDown(): Unit = model.shutDown()


  def registerSizeChangeListener(func: (Int) => Unit): Unit = sizeChangeListeners += func
	
	
	private def notifySizeListeners(): Unit = {
		val size=oldIndex//model.getSize
		for(func <-sizeChangeListeners) func(size)
	}	
	
}