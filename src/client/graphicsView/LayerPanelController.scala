/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import java.awt.Color
import java.awt.datatransfer.Transferable

import client.dataviewer.{InstanceSelection, OnMoveHandler, ViewConstants}
import client.dialog._
import client.icons.IconManager
import javax.swing.table.TableColumnModel
import javax.swing.{ImageIcon, JTable, TransferHandler}

import scala.swing._
import scala.swing.event._
/** manages the Layer panel
 * 
 */

class MyMoveHandler(contr:LayerPanelController) extends  OnMoveHandler{
  def moved(targetHandler: DragDropListener[_ <: Transferable], inst: InstanceSelection): Unit = {
      targetHandler match {
        case _: LayerPanelController => contr.deleteLastSelected()
        case _ =>  
      }
    }
}      

class LayerPanelController(viewController:GraphViewController) extends DragDropListener[InstanceSelection] {
	
  val transferHandler=new CommonTransferHandler(this)
  var lastMousePos:Point = new Point
  val moveHandlerID: Int = InstanceSelection.addOnMoveHandler(new MyMoveHandler(this))
  var lastRow: Int = -1
  var lastColumn:Int= -1
  	
  val layerTable:Table=new Table() {
		peer.setModel(viewController.layerModel)
		peer.setTransferHandler(transferHandler)
		peer.setDragEnabled(true)
		autoResizeMode=Table.AutoResizeMode.Off
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None
		rowHeight= LayerPanelController.lineHeight
		focusable=false
    val colMod: TableColumnModel = peer.getColumnModel
    colMod.getColumn(0).setPreferredWidth(45 * ViewConstants.fontScale / 100)
    colMod.getColumn(1).setPreferredWidth(40 * ViewConstants.fontScale / 100)
    colMod.getColumn(2).setPreferredWidth(40 * ViewConstants.fontScale / 100)
    colMod.getColumn(3).setPreferredWidth(45 * ViewConstants.fontScale / 100)
    colMod.getColumn(4).setPreferredWidth(260 * ViewConstants.fontScale / 100)
    colMod.getColumn(5).setPreferredWidth(50 * ViewConstants.fontScale / 100)
		
		listenTo(mouse.clicks,mouse.moves)
		reactions+={  	  
			case e:MousePressed =>
				lastColumn= peer.columnAtPoint(e.point)
				lastRow=peer.rowAtPoint(e.point)
				lastMousePos=e.point
        if((lastColumn<3||lastColumn==5)&&lastColumn> -1 && lastRow> -1)tableCellClicked(lastColumn,lastRow)
        if(lastColumn==3) viewController.layerModel.setActiveLayerIx(lastRow,exclusive = true)
			case e:MouseReleased =>
        val col=peer.columnAtPoint(e.point)
        System.out.println("released lc:"+lastColumn+" lr"+lastRow+" cc:"+peer.columnAtPoint(e.point)+" cr:"+peer.rowAtPoint(e.point)+" c:"+e.clicks)
        if(e.clicks ==1) {

				  /*if(lastColumn==peer.columnAtPoint(e.point)&& lastColumn> -1&&
						lastRow==peer.rowAtPoint(e.point)&& lastRow> -1)
					tableCellClicked(lastColumn,lastRow)*/
        } else if(e.clicks==2)
          if (col == 2)
             viewController.layerModel.setActiveLayerIx(lastRow,exclusive = true)
      /*case e:MouseDragged=>
        val control = (e.modifiers & Key.Modifier.Control) > 0
        if(peer.columnAtPoint(e.point)>2)
          transferHandler.exportAsDrag(peer, e.peer, if(control)TransferHandler.COPY else TransferHandler.MOVE)*/
		}

    val eyeRend = new MyRenderer(LayerPanelController.eyeIcon, true)
    val editRend = new MyRenderer(LayerPanelController.editIcon, true)
    val newElemRend = new MyRenderer(LayerPanelController.newElemIcon, false)
    val pathRend = new MyPathRenderer()

    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int): Component = {
  		//FIND VALUE
      val v = model.getValueAt(peer.convertRowIndexToModel(row),
        peer.convertColumnIndexToModel(col))
      if (row >= viewController.layerModel.layerList.size) super.rendererComponent(sel, foc, row, col)
      else {
        col match {
          case 0 => eyeRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
          case 1 => editRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
          case 2 => newElemRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
          case 4 => pathRend.componentFor(this, sel, foc, v.asInstanceOf[(String, Array[String])], row, col)
          case _ => super.rendererComponent(sel, foc, row, col)
        }
      }
  	}


    class MyRenderer(myIcon: ImageIcon, showIconUnselected: Boolean) extends Table.AbstractRenderer[Boolean, Label](ViewConstants.label()) {
      def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, checked: Boolean, row: Int, column: Int): Unit = {
	      component.icon = if(showIconUnselected) myIcon else  {if (checked) myIcon else null } 
	      component.text = ""
	      component.background = if (checked) LayerPanelController.selectColor
        else LayerPanelController.notSelectColor
	    }
	  }

    class MyPathRenderer extends Table.AbstractRenderer[(String, Array[String]), Label](ViewConstants.label()) {
      def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, value: (String, Array[String]), row: Int, column: Int): Unit = {
        component.text = value._1
        component.tooltip = "<html>" + value._2.mkString("<br>\\  ") + "</html>"
        component.background = if (row % 2 == 0) Color.white else LayerPanelController.notSelectColor
        component.horizontalAlignment = Alignment.Left
      }
    }
	} // end Table

  def shutDown(): Unit = InstanceSelection.removeOnMoveHandler(moveHandlerID)
  
  def lastSelectedRow():Int = {
    val ix=(lastMousePos.getY/LayerPanelController.lineHeight).toInt
    if(ix>=viewController.layerModel.layerList.size) viewController.layerModel.layerList.size-1
    else ix
  }

  def tableCellClicked(col: Int, row: Int): Unit = {
  	if(row<viewController.layerModel.layerList.size)
  	col match {  		 
  		case 0 => toggleVisible(row)
  		case 1 => toggleEdible(row)
  		case 2 => toggleActive(row)
      case 3 => viewController.layerModel.setActiveLayerIx(lastRow, exclusive = true)
  		case 5 => removeLayer(row) 
  		case _ =>
  	}
  }

  def updateProperty(): Unit = viewController.canvas.repaint()
  
  def getSelectedLayer:Int = if ( layerTable.selection.rows.isEmpty) -1 else layerTable.selection.rows.head

  def removeLayer(ix: Int): Unit = {
		 if(ix> -1) {
			 viewController.layerModel.layerList(ix).shutDown()
			 viewController.layerModel.removeLayer(ix)			 
		 }
		 viewController.selectModel.deselect(true)
	}

  def toggleVisible(ix: Int): Unit = if (ix > -1) {
    viewController.layerModel.toggleVisibility(ix)
		updateProperty()
		
	}

  def toggleEdible(ix: Int): Unit = if (ix > -1) {
    viewController.layerModel.toggleEdible(ix)
		updateProperty()
	}

  def toggleActive(ix: Int): Unit = if (ix > -1) {
    viewController.layerModel.toggleActive(ix)
    updateProperty()
	}
  
 // ********************** Drag Drop
  
  def sourceActions: Int =TransferHandler.COPY_OR_MOVE
  def DDfactory():InstanceSelection = {
    val selectedIx=lastSelectedRow()
    if(selectedIx<0||lastColumn<4) null
    else {
      val selLayer = viewController.layerModel.layerList(selectedIx)
      val ret = new InstanceSelection(Array.empty, 0, List(selLayer.ref), Array(selLayer.name), 0, Array.empty, Array.empty)
      ret.onMoveHandlerID = moveHandlerID
      ret
    }
  }

  def deleteLastSelected(): Unit = {
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
  lazy val flavors=Array(InstanceSelection.flavor,InstanceSelection.graphElemFlavor) 
     
}

object LayerPanelController {
  lazy val lineHeight: Int = 20 * ViewConstants.fontScale / 100
  val eyeIcon: ImageIcon = IconManager.createImageIcon("eye.gif")
  val editIcon: ImageIcon = IconManager.createImageIcon("editsmall.gif")
  val newElemIcon: ImageIcon = IconManager.createImageIcon("newElem.gif")
  val selectColor=new Color(70,160,230)
	val notSelectColor=new Color(250,250,250)
}
