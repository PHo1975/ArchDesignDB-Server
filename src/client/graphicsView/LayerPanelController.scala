/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import client.dialog._
import scala.swing._
import scala.swing.event._
import definition.data._
import javax.swing.ImageIcon
import java.net.URL
import client.comm.ClientQueryManager
import definition.typ.SelectGroup
import javax.swing.TransferHandler
import client.dataviewer.InstanceSelection
import javax.swing.DropMode
import java.awt.datatransfer.Transferable
import javax.swing.JTable
import client.icons.IconManager
import client.dataviewer.OnMoveHandler
/** manages the Layer panel
 * 
 */

class MyMoveHandler(contr:LayerPanelController) extends  OnMoveHandler{  
    def moved(targetHandler:DragDropListener[_ <: Transferable],inst:InstanceSelection)={              
      targetHandler match {
        case l:LayerPanelController=>  contr.deleteLastSelected()
        case _ =>  
      }
    }
}      

class LayerPanelController(viewController:GraphViewController) extends DragDropListener[InstanceSelection] {
	
  val transferHandler=new CommonTransferHandler(this)
  var lastMousePos:Point = new Point
  val moveHandlerID=InstanceSelection.addOnMoveHandler(new MyMoveHandler(this))
  	
  val layerTable:Table=new Table() {		
  	var lastColumn:Int= -1
  	var lastRow:Int= -1 
    
    
		peer.setModel(viewController.layerModel)
		peer.setTransferHandler(transferHandler)
		peer.setDragEnabled(true)
		//peer.setDropMode(DropMode.ON)
		autoResizeMode=Table.AutoResizeMode.Off
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None
		rowHeight= LayerPanelController.lineHeight
		focusable=false
		val colMod=peer.getColumnModel()		
		colMod.getColumn(0).setPreferredWidth(45)
		colMod.getColumn(1).setPreferredWidth(40)
		colMod.getColumn(2).setPreferredWidth(40)
		colMod.getColumn(3).setPreferredWidth(45)
		colMod.getColumn(4).setPreferredWidth(260)
		colMod.getColumn(5).setPreferredWidth(50)
		
		listenTo(mouse.clicks)
		reactions+={  	  
			case e:MousePressed =>
				lastColumn= peer.columnAtPoint(e.point)
				lastRow=peer.rowAtPoint(e.point)
				lastMousePos=e.point
			case e:MouseReleased => if(e.clicks ==1) {
				if(lastColumn==peer.columnAtPoint(e.point)&& lastColumn> -1&&
						lastRow==peer.rowAtPoint(e.point)&& lastRow> -1)
					tableCellClicked(lastColumn,lastRow)
			} else if(e.clicks==2)
			  if(peer.columnAtPoint(e.point)==2) {
			     viewController.layerModel.setActiveLayerIx(lastRow,true)
			    
			  }			
		}
    
    
  	def boolIdentity(o:Boolean)= o	
  	
  	val eyeRend = new MyRenderer[Boolean] ( boolIdentity  ,LayerPanelController.eyeIcon,true)
  	val editRend = new MyRenderer[Boolean] ( boolIdentity  ,LayerPanelController.editIcon,true)
  	val newElemRend = new MyRenderer[Boolean] ( boolIdentity  ,LayerPanelController.newElemIcon,false)
  	override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
  		//FIND VALUE
  		val v = model.getValueAt(
  			peer.convertRowIndexToModel(row), 
  			peer.convertColumnIndexToModel(col))	
  			if (row >= viewController.layerModel .layerList .size) super.rendererComponent(sel,foc,row,col) 
  			else {  				
  				col match {
  					case 0 => eyeRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
  					case 1 => editRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
  					case 2 => newElemRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
  					case _ => super.rendererComponent(sel,foc,row,col)
  				}
  			}  	
  	}
  	
  	class MyRenderer[A](convert: A => Boolean,myIcon:ImageIcon,showIconUnselected:Boolean) extends Table.AbstractRenderer[A, Label](new Label) {	   
	    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: A, row: Int, column: Int): Unit = {
	      val checked = convert(a)	      
	      component.icon = if(showIconUnselected) myIcon else  {if (checked) myIcon else null } 
	      component.text = ""
	      component.background = if (checked) LayerPanelController.selectColor
	      else LayerPanelController.notSelectColor
	      //else component.background = table.background
	    }
	  }
	} // end Table
  
  def shutDown()=InstanceSelection.removeOnMoveHandler(moveHandlerID)
  
  def lastSelectedRow():Int = {
    val ix=(lastMousePos.getY()/LayerPanelController.lineHeight).toInt
    if(ix>=viewController.layerModel.layerList.size) viewController.layerModel.layerList.size-1
    else ix
  }
  
  def tableCellClicked(col:Int,row:Int)= {  	
  	if(row<viewController.layerModel.layerList.size)
  	col match {  		 
  		case 0 => toggleVisible(row)
  		case 1 => toggleEdible(row)
  		case 2 => toggleActive(row)
  		case 5 => removeLayer(row) 
  		case _ =>
  	}
  }  
  
  def updateProperty() = {  	
  	viewController.canvas.repaint()
  }
  	 
  
  
  def getSelectedLayer:Int = if ( layerTable.selection.rows.isEmpty) -1 else layerTable.selection.rows.head 
	
	def removeLayer(ix:Int) = {		 
		 if(ix> -1) {
			 viewController.layerModel.layerList(ix).shutDown()
			 viewController.layerModel.removeLayer(ix)			 
		 }
		 viewController.selectModel.deselect(true)
	}
  
  def toggleVisible(ix:Int) = {		 
		if(ix> -1)  viewController.layerModel.toggleVisibility(ix)	
		updateProperty()
		
	}
  
  def toggleEdible(ix:Int) = { 
		if(ix> -1)  viewController.layerModel.toggleEdible(ix)	
		updateProperty()
	}
  
  def toggleActive(ix:Int) = {		 
		if(ix> -1)  viewController.layerModel.toggleActive(ix)
		updateProperty()
	}
  
 // ********************** Drag Drop
  
  def sourceActions=TransferHandler.COPY_OR_MOVE
  def DDfactory():InstanceSelection = {
    val selectedIx=lastSelectedRow()
    if(selectedIx<0) null
    else {
      val selLayer = viewController.layerModel.layerList(selectedIx)
      val ret = new InstanceSelection(Array.empty, 0, List(selLayer.ref), Array(selLayer.name), 0, Array.empty)
      ret.onMoveHandlerID = moveHandlerID
      ret
    }
  }
  
  def deleteLastSelected()={
    val selectedIx=lastSelectedRow()
    if(selectedIx>=0) removeLayer(selectedIx)
  }
  
  def canImport(action:Int,data:Transferable,pos:TransferHandler.DropLocation):Boolean = {
    data match {
      case insData:InstanceSelection=> insData.selection.exists(a=>Layer.allowedDisplayListTypes.contains(a.typ)) &&
      		!  insData.selection.exists(viewController.layerModel.containsRef)
      case graphTr:GraphElemTransferable =>
				val row=pos.asInstanceOf[JTable.DropLocation].getRow
				if(row>=viewController.layerModel.layerList.size) false
        else {
          val targetLayer = viewController.layerModel.layerList(row)
          if (graphTr.layerList.exists(_.layerRef == targetLayer.ref)) false
          else true
        }
			case _=> false
    }
      
  } 
    
  def importData(action:Int, data:Transferable,pos:TransferHandler.DropLocation):Boolean = {
    var success=false
    if(canImport(action,data,pos)) {	
      data match {
        case instData:InstanceSelection=> success= viewController.importLayers(instData)
        case graphTr:GraphElemTransferable =>
					val row=pos.asInstanceOf[JTable.DropLocation].getRow
					val targetLayer=viewController.layerModel.layerList(row)
					viewController.importDDToLayer(graphTr,action,targetLayer)
					success=true
			}
		}
    success
  }

  
  /*override def exportDone(data: InstanceSelection,action:Int):Unit = {
    println("export done "+myMoveHandler.moved+" "+myMoveHandler)
    if(myMoveHandler.moved){
      deleteLastSelected()
      println("move it, baby")
    }
  }*/
  
  lazy val flavors=Array(InstanceSelection.flavor,InstanceSelection.graphElemFlavor) 
     
}

object LayerPanelController {
	val lineHeight=20
	val eyeIcon=IconManager.createImageIcon("eye.gif")
  val editIcon=IconManager.createImageIcon("editsmall.gif")
  val newElemIcon=IconManager.createImageIcon("newElem.gif")
  val selectColor=new Color(70,160,230)
	val notSelectColor=new Color(250,250,250)
  
  /*def  createImageIcon(path:String):ImageIcon = {
		val imgURL:URL   = this.getClass.getResource(path)
		if (imgURL != null) {			
			return new ImageIcon(imgURL);
		} else {
			System.err.println("Couldn't find file: " + path);
			return null;
		}
	}*/
}
