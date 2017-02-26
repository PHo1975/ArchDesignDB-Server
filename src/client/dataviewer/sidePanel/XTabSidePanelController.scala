/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel

import scala.swing._
import definition.data.Reference
import definition.typ._
import scala.swing.event.ButtonClicked
import java.awt.{Color,Dimension}
import java.awt.event.{MouseAdapter,MouseEvent}
import client.dataviewer.TypeTableModel
import sun.swing.table.DefaultTableCellHeaderRenderer
import javax.swing.BorderFactory
import definition.expression.Expression
import client.dataviewer.MultilineEditor
import javax.swing.table.TableCellEditor
import client.dataviewer.ViewConstants
import client.icons.IconManager
import client.dialog.FocusContainer
import client.dialog.SelectSender
import javax.swing.event.ListSelectionListener
import client.dialog.AbstractFocusContainer
import javax.swing.event.ListSelectionEvent
import definition.data.EMPTY_REFERENCE
import definition.data.EMPTY_OWNERREF
import definition.data.OwnerReference
import client.dialog.NewButtonsList
import client.dialog.SelectEventDispatcher

/** Controller for XTab SidePanel
 * 
 */
class XTabSidePanelController extends SidePanelController with SelectSender with AbstractFocusContainer {
	
	val tmodel=new XTabSidePanelModel(this)
	var ydataModel:TypeTableModel= _
	val closeBut=new Button ("")
	var notifySelectChange=true
	var container:ControllerContainer=_
	val selectGroup=new SelectGroup[Reference](EMPTY_OWNERREF,Nil)
	val groupList=List(selectGroup)
	
	for(ic<-IconManager.getIcon("xtab","close3")) closeBut.icon=ic 
	
	
	
	class LineComponent(align:Double) extends Component{
      border=BorderFactory.createLineBorder(Color.GRAY)
      yLayoutAlignment=align
      minimumSize=new Dimension(2,1)
      preferredSize=minimumSize
    }
	
	var dropTransferHandler=new XTabHeaderTransferHandler(XTabSidePanelController.this)	
		
	val rightDropComp=new Panel {
		maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
		preferredSize=new Dimension(closeBut.preferredSize.width/*+15*/,10)
		yLayoutAlignment=0d		
		border=BorderFactory.createEtchedBorder()
		peer.setTransferHandler(dropTransferHandler)
	}
	
	val table=new Table{
		autoResizeMode=Table.AutoResizeMode.Off
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None  
		peer.setAutoCreateColumnsFromModel(false)
		model=tmodel
		showGrid=true
		gridColor=Color.gray
		//println("UI:" +peer .getTableHeader.getUI)
		peer.setColumnModel(tmodel.colModel .colModel )
		
		override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int):Component = {
			//FIND VALUE
			tmodel.structure match {
				case Some(s)=>
					val modCol=peer.convertColumnIndexToModel(col)
					val rendCol=s.numDataCellFields-(modCol % s.numDataCellFields )
					val v=model.getValueAt(row,modCol)
					v match {
						case ve:Expression => if(s.isNoteColumn(rendCol-1)) s.ntcr.componentFor(this,sel,foc,ve,row,rendCol)
						 else s.itcr.componentFor(this,sel,foc,ve,row,rendCol)
						case vt:(_, _) => s.etcr.componentFor(this,sel,foc,v.asInstanceOf[(String, Int)],row,rendCol)
						case _=> super.rendererComponent(sel,foc,row,col)
					}
				case None =>super.rendererComponent(sel,foc,row,col)
			}
									 		 
		}
		
		override def editor(row: Int, column: Int):TableCellEditor={
		  tmodel.structure match {
		    case Some(s)=>
					val rendCol=s.numDataCellFields-(column % s.numDataCellFields )
					if (s.isNoteColumn(rendCol-1)) noteEditor else  instEditor
				case None=>instEditor
		  }
		}
		
		peer.getColumnModel().getSelectionModel().addListSelectionListener(new ListSelectionListener(){
		  def valueChanged(e:ListSelectionEvent ):Unit = if(!e.getValueIsAdjusting()) {		    
		  	notifySelectionChanged(peer.getColumnModel().getSelectedColumns()) 
		  }
		})
	}
	
	val noteEditor:MultilineEditor=new MultilineEditor(table.peer,None,Some(new Dimension(220,80)))	{
		def setEditorValue(value:Object) = value.toString
	}
	
	
	val instEditor:MultilineEditor=new MultilineEditor(table.peer)	{
		def setEditorValue(value:Object) = value match {
		  case expr:Expression=> if(expr.getType==DataType.StringTyp) expr.toString
				else expr.getTerm			
		  case null=>"" 
		  case _=> value.toString
		}
	}
	
	lazy val XTabRowType:Int=SystemSettings().systemTypes("XTabRow")
	registerSelectListener(SelectEventDispatcher)
  registerContainerListener(NewButtonsList)
	
	def getDefaultHeaderRenderer=table.peer.getTableHeader.getDefaultRenderer
	
	def classFits(tableClass:AbstractObjectClass):Boolean = {
		tableClass.inheritsFrom(XTabRowType)
	}
	
	private def setYDataModel(ymod:TypeTableModel) = {
		ydataModel=ymod
	  table.rowHeight=ViewConstants.defaultRowHeight
	  table.font=ViewConstants.tableFont
	  table.peer.getTableHeader().setFont(ViewConstants.smallFont)
	}
	
  def parentsFits(dataModel:TypeTableModel,parentRef:Reference):Boolean = 
  {
  	val result= AllClasses.get.getClassByID(parentRef.typ).inheritsFrom(XTabRowType)
  	if(result) setYDataModel(dataModel)  	
  	result
  }
	

  def panelName:String="XT"
  
  lazy val panelIcon=IconManager.getIcon("xtab","open3")

  def openPanel(parentRef: Reference, tableClass:AbstractObjectClass,cont:ControllerContainer): Unit = { 
  	//println("openPanel:"+parentRef)
  	container=cont
  	selectGroup.parent=new OwnerReference(0,parentRef)
  	tmodel.initData(parentRef)  	
  }
  
  def closePanel():Unit = {
  	tmodel.shutDown()
  }

  

  lazy val headerComp= new BoxPanel(Orientation.Horizontal ) {  	
  	closeBut.yLayoutAlignment=1d
  	peer.setTransferHandler(dropTransferHandler)
  	val header=table.peer.getTableHeader
  	val headerWrapper= new XTabHeaderPanel(header)
  	headerWrapper.peer.setMaximumSize(new Dimension(tmodel.colModel.columnWidth,headerWrapper.preferredSize.height))
  	contents+=new LineComponent(1d)
  	contents+=headerWrapper  	
  	contents+=closeBut
  	listenTo(closeBut)
  	reactions += {
  		case b: ButtonClicked =>
				container.closeSideBar()
		}
  	table.peer.getTableHeader.addMouseListener(new MouseAdapter(){
		override def mouseClicked(ev:MouseEvent) = {
			var x=ev.getPoint.x
			val colMod=tmodel.colModel.colModel
			val col=colMod.getColumnIndexAtX(x)
			var offset=0
			for(i <-0 to col) offset+=colMod.getColumn(i).getWidth
			val pos=offset-x
			if(pos<23 && pos>0){
				//println("hit "+col)
				val modCol=table.peer.convertColumnIndexToModel(col)
				tmodel.deleteColumn(modCol)
			}			
		}
	})
  }

  lazy val mainComp= new BoxPanel(Orientation.Horizontal ) {
    contents+=new LineComponent(0d)
  	contents+=new XTabMainPanel(table)  	
  	contents+=rightDropComp
  }
  
  def notifyRowsChanged():Unit = {
  	tmodel.notifyRowsChanged()
  	mainComp.peer.invalidate()
  }
  
  // *************FocusContainer Interface
  
  def containerName:String="Preisspiegel"
  
  def containerRef=tmodel.getParentRef
  
  def requestFocus():Unit=table.requestFocus()
  
  //************* SelectSender Interface
  
  def deselect(notify:Boolean):Unit={
    notifySelectChange=false
    if(table.peer .isEditing) instEditor.stopCellEditing()
    table.peer.clearSelection()
		notifySelectChange=true
		table.repaint()
		if(notify) notifySelectListeners(Nil)
  } 
  
  def notifySelectionChanged(cols:Array[Int]):Unit= if(notifySelectChange){
    if(cols.length==1) for(st<-tmodel.structure){
      val col=cols(0)
      //println("Col sel:"+col+" modulo:"+(col / st.numDataCellFields)+" size:"+tmodel.colModel.columnList.size)
      val colIX=col / st.numDataCellFields
      if(colIX<tmodel.colModel.columnList.size){
	      selectGroup.children=List(tmodel.colModel.columnList(col / st.numDataCellFields).colData.ref)
	      notifySelectListeners(groupList)
	      return
      }
    }   
    notifySelectListeners(Nil)  
  }

}
