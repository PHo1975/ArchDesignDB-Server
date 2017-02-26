package client.spreadsheet
import java.awt.{Color, Dimension, Font, Point, Toolkit}
import java.awt.event.{AdjustmentEvent, AdjustmentListener}
import java.awt.font.FontRenderContext
import java.util.Date
import javax.swing.{BorderFactory, DropMode, JLabel, JScrollPane, JTable, ScrollPaneConstants, SwingConstants}
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.table.TableCellRenderer

import client.comm.{ClientQueryManager, SingleObjectDataModel, SubscriptionFactory}
import client.dataviewer.{LabelRenderer, MultilineEditor, ViewConstants}
import client.dialog.{AbstractFocusContainer, ActionPanel, NewButtonsList, SelectEventDispatcher, SelectSender}
import definition.data.{EMPTY_OWNERREF, EMPTY_REFERENCE, InstanceData, OwnerReference, Referencable, Reference}
import definition.expression._
import definition.typ.{CustomInstanceEditor, DataType, SelectGroup, SystemSettings}
import util.StringUtils

import scala.swing._
import scala.swing.event.{ButtonClicked, FocusGained}

class SpreadSheetSelectGroup(nparent:OwnerReference,nchildren:Iterable[SpreadSheetCell],val controller:SpreadSheetController) extends SelectGroup[Referencable](nparent,nchildren) {
  var commonFormat:SpreadSheetFormat=UndefinedFormat
  var range:SpreadSheetRange=NO_SELECTION
  override def toString="SSSG owner:"+parent+" children:"+children.size+" "+ children.map(i => i.toString + ":" + i.getClass.toString).mkString("|")
}


class SpreadSheetController extends BorderPanel with CustomInstanceEditor[Component] with SelectSender with AbstractFocusContainer {
  val emptyRange=Range(-1,-1)
  var myParser=new SpreadSheetParser
  var spreadSheetRef:Reference=_
  var someSpreadSheetRef:Option[Reference]=None
  var cellOwnerRef:OwnerReference= _
  var columnOwnerRef:OwnerReference= _
  var formatOwnerRef:OwnerReference= _
  var collFuncOwnerRef:OwnerReference = _
  var collFuncOwnerRefArray:Array[OwnerReference]= _
  var cellOwnerRefArray:Array[OwnerReference]= _  
  var formatOwnerRefArray:Array[OwnerReference]= _
  val tableModel=new SpreadSheetTableModel(this)
  val firstColumnTableModel=new SSFirstColumnTableModel(this)
  val formatList=new SpreadSheetFormatList(this)
  override val fullSize=true
  
  var selection:SpreadSheetRange=NO_SELECTION
  var notifySel=true
  var selGroup:SpreadSheetSelectGroup=_
  var selGroupList:List[SelectGroup[Referencable]]= _
  
  var colSelGroup:SpreadSheetSelectGroup=_
  var colSelGroupList:List[SelectGroup[Referencable]]= _  
  var loadDoneListener: ()=>Unit= _  
  
  val headerModel=new SingleObjectDataModel(
      (data)=> {
		    val name=data.fieldValue(1).toString
		    val date=data.fieldValue(2).toDate
		    val result=data.fieldValue.head
		    updateHeader(name,date,result)
		    (name,date,result)
		  }, ()=>{   })
  
  class MyHeaderPanel extends BoxPanel(scala.swing.Orientation.Horizontal){
    val nameLab=new Label("Tabelle:")
    val nameEdit=new TextField
    val resultBut=new Button("Ergebnis auswählen")
    val resultLab=new Label("   ")
    xLayoutAlignment=0d
    resultLab.opaque=true
    resultLab.background=Color.LIGHT_GRAY.brighter()
    nameEdit.maximumSize=new Dimension(300,50)
    nameEdit.preferredSize=new Dimension(200,25)
    contents+=nameLab+=nameEdit+=Swing.HStrut(30)+=resultBut+=Swing.HStrut(10)+=resultLab
    listenTo(resultBut)
    reactions+= {
      case ButtonClicked(`resultBut`)=> selection match{
        case SingleCellSelection(col,row)=>
          ClientQueryManager.executeAction(EMPTY_OWNERREF,List(spreadSheetRef),"setResultCell",
          		Seq(("col",IntConstant(col)),("row",IntConstant(row))))
        case _=>
      }
    }
  }
  
  val headerPanel=new MyHeaderPanel
  
  val transferHandler=new SpreadSheetTransferHandler(this)
  
  class MyFirstColTable extends Table() {
    var rowSelectRange:Range=emptyRange
    autoResizeMode=Table.AutoResizeMode.Off		
		rowHeight=SpreadSheet.defaultRowHeight
		model=firstColumnTableModel
		showGrid=true
		gridColor=Color.LIGHT_GRAY
		this.peer.getTableHeader().setReorderingAllowed(false)
		this.peer.setAutoCreateColumnsFromModel(false)
		this.peer.setColumnModel(firstColumnTableModel.columnModel)
		this.peer.setRowSelectionAllowed(false)
		this.peer.setColumnSelectionAllowed(false)
		for (l<-peer.getMouseMotionListeners()) peer.removeMouseMotionListener(l)
		var dragStartRow:Int= -1
		val rowMouseListener:java.awt.event.MouseAdapter=new java.awt.event.MouseAdapter() {
		   
		   override def mousePressed(e:java.awt.event.MouseEvent)= if(e.getButton()==java.awt.event.MouseEvent.BUTTON1&&
	       peer.columnAtPoint(e.getPoint)==0){		     
	       e.consume()
	       dragStartRow=peer.rowAtPoint(e.getPoint)		       
	       rowSelectRange=Range(dragStartRow,dragStartRow+1)
	       selectRow(dragStartRow)
	       peer.repaint()		       		     
		   } else if(e.getButton()==java.awt.event.MouseEvent.BUTTON3&&(peer.columnAtPoint(e.getPoint)==0)) {
		     e.consume()
		     SpreadSheetController.this.selection match {
		       case r:RowsSelection=> ActionPanel.showRightMenu(e.getPoint,MyFirstColTable.this) 
		       case _ =>
		     }
		     
		   }
		   override def mouseDragged(e:java.awt.event.MouseEvent)= if(dragStartRow>=0){
        val row=peer.rowAtPoint(e.getPoint())
        e.consume()        
        if(row>=0) {          
        	val rangeChanged=if(row<dragStartRow&& row !=rowSelectRange.start) { rowSelectRange=Range(row,dragStartRow+1);true}
        	else if(row >= dragStartRow && row!=rowSelectRange.end-1&& row < peer.getRowCount ) {rowSelectRange=Range(dragStartRow,row+1);true} else false        	
        	if(rangeChanged) {
        		selectRows(rowSelectRange)
        		peer.repaint()        		
        	}        	
        }
		   }
       override def mouseReleased(e:java.awt.event.MouseEvent)= if(dragStartRow > -1 ) dragStartRow= -1         
       override def mouseExited(e:java.awt.event.MouseEvent)= mouseReleased(e)  
		}
		
    peer.addMouseListener(rowMouseListener)
    peer.addMouseMotionListener(rowMouseListener) 
		val firstButRend=new LabelRenderer()   
    firstButRend.font=SpreadSheet.firstColFont
		val ftcr = new Table.AbstractRenderer[String, LabelRenderer](firstButRend) {
			def configure(t: Table, sel: Boolean, foc: Boolean, o: String, row: Int, col: Int) = {
			  component.config(sel,foc,o,row)
			  component.peer.setBackground(if(rowSelectRange.contains(row))t.selectionBackground else ViewConstants.buttonBackgroundColor)			  
			}			  
		}
    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {			
			val modCol=peer.convertColumnIndexToModel(col)
			val v=model.getValueAt(row,modCol)			
		  ftcr.componentFor(this,sel,foc,if(v==null) "" else v.toString, row, col)			
		}
     peer.getTableHeader().setDefaultRenderer(new JLabel() with TableCellRenderer{
      setBorder(BorderFactory.createRaisedBevelBorder())
  	  override def revalidate()= {}
      override val getPreferredSize=new Dimension(40,30)     
      def getTableCellRendererComponent(table:JTable, value:Object , isSelected:Boolean,
      hasFocus:Boolean, rowIndex:Int, colIndex:Int):java.awt.Component= this
      
    })    
  }
  
  val firstColTable=new MyFirstColTable
  
  class MyTable extends Table(){   
    
    object MySelectionListener extends ListSelectionListener{
      var selfSelected=false      
      def valueChanged(e:ListSelectionEvent ) = if(!e.getValueIsAdjusting()&& !selfSelected) updateRangeSelection()            
    }
    
    autoResizeMode=Table.AutoResizeMode.Off		
		rowHeight=SpreadSheet.defaultRowHeight
		font=SpreadSheet.tableFont
		model=tableModel
		showGrid=true
		gridColor=new Color(230,230,230)
		peer.setTransferHandler(transferHandler)
		peer.setDragEnabled(true)
		peer.setDropMode(DropMode.ON)	
		
		
		this.peer.getTableHeader().setReorderingAllowed(false)
		this.peer.setAutoCreateColumnsFromModel(false)
		this.peer.setColumnModel(tableModel.columnModel)
		selection.elementMode=Table.ElementMode.Cell
		this.peer.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_INTERVAL_SELECTION)		
		peer.setColumnSelectionAllowed(true)
		var headerSelectRange:Range=emptyRange				
		val header=peer.getTableHeader		
		val internMouseListeners=peer.getMouseMotionListeners()
		
		def disableInternMouseListeners()= for (l<-internMouseListeners) peer.removeMouseMotionListener(l)		
		def enableInternMouseListeners()= for(l<-internMouseListeners) peer.addMouseMotionListener(l)
		
		val headerMouseListener:java.awt.event.MouseAdapter=new java.awt.event.MouseAdapter() {
		  var dragStartWidth= 0
      var dragStartCol:Int= -1 
      var dragStartPos:Option[Point]=None
      var hasDragged=false
      
			override def mousePressed(e:java.awt.event.MouseEvent)= if(e.getButton()==java.awt.event.MouseEvent.BUTTON1){
				 dragStartCol=header.columnAtPoint(e.getPoint())				 
				 dragStartPos=Some(e.getPoint)
				 hasDragged=false				 
				 if(dragStartCol<peer.getColumnCount -1) {				   
				   headerSelectRange=Range(dragStartCol,dragStartCol+1)
				   selectColumn(dragStartCol)				   
				   header.repaint()  
				 }			 
			} else if(e.getButton()==java.awt.event.MouseEvent.BUTTON3) {
			  if(!table.hasFocus) {
			    table.requestFocus()
			  }
			  if(!headerSelectRange.isEmpty) ActionPanel.showRightMenu(e.getPoint(),table)
			}
      override def mouseReleased(e:java.awt.event.MouseEvent)={
        val col=header.columnAtPoint(e.getPoint())       
        if(col==peer.getColumnCount-1&& !hasDragged) {          
            tableModel.writeColumnWidths()
            tableModel.createColumnData(peer.getColumnCount-1,SpreadSheet.defaultColumnWidth)
            headerSelectRange=emptyRange                         
        } else if(hasDragged) {
          if(tableModel.columnModel.hasWidthChanged) tableModel.writeColumnWidths()
        }       
        dragStartCol= -1
        
      }
      override def mouseDragged(e:java.awt.event.MouseEvent):Unit= {
        dragStartPos match {
          case Some(oldPoint)=>
            val dx=e.getPoint.x-oldPoint.x
            val dy=e.getPoint.y-oldPoint.y
            if((dx*dx+dy*dy)<9) return
            else if(!hasDragged) hasDragged=true
          case _=> return
        }
        val col=header.columnAtPoint(e.getPoint()) 
        if(col>=0&& col<peer.getColumnCount -1) {
        	val rangeChanged=if(col<=dragStartCol&& col !=headerSelectRange.start) { headerSelectRange=Range(col,dragStartCol+1);true}
        	else if(col >= dragStartCol && col!=headerSelectRange.end-1&& col < peer.getColumnCount-1 ) {headerSelectRange=Range(dragStartCol,col+1);true} else false
        	if(rangeChanged) {
        		selectColumns(headerSelectRange)
        		header.repaint()
        	}
        }
      }
		}
		
		header.addMouseListener(headerMouseListener)
    header.addMouseMotionListener(headerMouseListener)
    peer.getSelectionModel().addListSelectionListener(MySelectionListener)   
    peer.getColumnModel().getSelectionModel().addListSelectionListener(MySelectionListener)
    
    peer.addMouseListener(new java.awt.event.MouseAdapter() {
      override def mousePressed(e:java.awt.event.MouseEvent)= if(e.getButton()==java.awt.event.MouseEvent.BUTTON3) {
			  if(!table.hasFocus) {
			    table.requestFocus()
			  }
			  ActionPanel.showRightMenu(e.getPoint(),table)
			}
    })
       
    val cellRenderer=new SpreadSheetRenderer(SpreadSheetController.this)
		val ctcr = new Table.AbstractRenderer[Expression, SpreadSheetRenderer](cellRenderer) {
			def configure(t: Table, sel: Boolean, foc: Boolean, o: Expression, row: Int, col: Int) =     
				component.config(t,sel,foc,o,row,col)
		}
    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {			
			val modCol=peer.convertColumnIndexToModel(col)
			val v=model.getValueAt(row,modCol)			
			 v match {
				case ve:Expression => ctcr.componentFor(this,sel,foc,ve,row,modCol)	
				case null=> ctcr.componentFor(this,sel,foc,null,row,modCol)
				case _=> super.rendererComponent(sel,foc,row,col)
			}
		}
    
    header.setDefaultRenderer(new JLabel() with TableCellRenderer{
      setBorder(BorderFactory.createRaisedBevelBorder())  	
  	  override def revalidate()= {}
      override val getPreferredSize=new Dimension(40,30)
      setOpaque(true)
      
      def getTableCellRendererComponent(table:JTable, value:Object , isSelected:Boolean,
      hasFocus:Boolean, rowIndex:Int, colIndex:Int):java.awt.Component= {
        setVerticalAlignment(SwingConstants.CENTER)
        setHorizontalAlignment(SwingConstants.CENTER)
        setText(if(value==null) "" else value.toString)
        if(headerSelectRange.contains(colIndex)) {
          setBackground(table.getSelectionBackground())
          setForeground(table.getSelectionForeground())
        } else {          
          setBackground(table.getBackground())
          setForeground(table.getForeground())
        }
        this
      }
    })    
    
    val instEditor:MultilineEditor=new MultilineEditor(this.peer)	{ 
      textArea.setFont(SpreadSheet.tableFont)
    	def setEditorValue(value:Object) =  value match {
          case expr:Expression => if(expr.getType==DataType.StringTyp) expr.toString
    				else replaceFieldReferences(expr,false).getTerm//.replace("_SpreadSheet","")    				
          case null => ""
          case o=> o.toString
        }    		
    	
    }
    
    override def editor(row: Int, column: Int)= instEditor			
		
    listenTo(this,mouse.clicks)
    reactions+= {
      case e:FocusGained =>  notifyContainerListeners(4)       
    }    
  }
  
  val table=new MyTable
   
  val headerScroller=new ScrollPane(){
     override lazy val peer: JScrollPane = new JScrollPane with SuperMixin {
      override def getPreferredSize=new Dimension(firstColTable.preferredSize.width+3,firstColTable.preferredSize.height)      
    }
    peer.setWheelScrollingEnabled(false)
    viewportView=firstColTable
    peer.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    peer.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
    
  }
  
  val tableScroller=new ScrollPane(){ 
    viewportView=table
    //peer.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    peer.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
    peer.getVerticalScrollBar.addAdjustmentListener(new AdjustmentListener() {
      def adjustmentValueChanged(e:AdjustmentEvent )= {
        //println("Adjustment type:"+e.getAdjustmentType()+" value: "+e.getValue()+" isadj:"+e.getValueIsAdjusting())
        headerScroller.peer.getVerticalScrollBar.setValue(e.getValue)
      } 
    })
    }
  
  
  //preferredSize=new java.awt.Dimension(50,Int.MaxValue)
  add(headerPanel,BorderPanel.Position.North)
  add(new BorderPanel() {
    add(headerScroller,BorderPanel.Position.West)
    add(tableScroller,BorderPanel.Position.Center)
  },BorderPanel.Position.Center)
  registerSelectListener(SelectEventDispatcher)
  registerContainerListener(NewButtonsList)
  
	def replaceFieldReferences(ex:Expression,allFieldRefs:Boolean):Expression = ex.replaceExpression {
    case f: FieldReference =>
      val ref = new Reference(f.remType.get, f.remInst.get)
      tableModel.findCellByRef(ref) match {
        case Some(cell) => new SSVariable(SpreadSheetUtil.columnIdToLetter(cell.col) + (cell.row + 1).toString, cell.exp.getValue)
        case None => tableModel.collFuncList.theMap.get(ref) match {
          case Some(collFuncData) => collFuncData.proxy
          case None => if (allFieldRefs) f.cachedValue else f
        }
      }
    case o => o
  }
  
  def getComponent =  this 

  def load(ref: Reference, listener: ()=>Unit): Unit = {
    loadDoneListener=listener
    spreadSheetRef=ref    
    someSpreadSheetRef=Some(ref)
    cellOwnerRef=new OwnerReference(4.toByte,spreadSheetRef)
    formatOwnerRef=new OwnerReference(3.toByte,spreadSheetRef)
    selGroup=new SpreadSheetSelectGroup(cellOwnerRef,Seq.empty,this)
    selGroupList=List(selGroup)
    columnOwnerRef=new OwnerReference(1.toByte,spreadSheetRef)
    collFuncOwnerRef=new OwnerReference(5.toByte,spreadSheetRef)
    collFuncOwnerRefArray=Array(collFuncOwnerRef)
    colSelGroup=new SpreadSheetSelectGroup(columnOwnerRef,Seq.empty,this)
    colSelGroupList=List(colSelGroup)
    cellOwnerRefArray=Array(cellOwnerRef)
    formatOwnerRefArray=Array(formatOwnerRef)
    headerModel.load(ref)
    tableModel.collFuncList.load(spreadSheetRef,5)
    revalidate()
  }
  
  def loadFormats() = formatList.load(spreadSheetRef)

  def shutDown(): Unit = {    
    deselect(false)
    headerModel.shutDown()
    tableModel.shutDown()
    formatList.shutDown()
    loadDoneListener=null
  }
  
  
  def refresh()= {
    table.repaint()
  }
  
  protected def updateHeader(nname:String,ndate:DateConstant,nresult:Constant)= {
    headerPanel.nameEdit.text=nname
    headerPanel.resultLab.text=nresult.toString
  }

  def editorName: String = "SpreadSheet"
    
  def updateRangeSelection():Unit=  if(firstColTable.dragStartRow == -1) {      	
    	if(table.headerSelectRange.nonEmpty) {
    		table.headerSelectRange=emptyRange
    		table.header.repaint()
    	}
    	if(firstColTable.rowSelectRange.nonEmpty) {
    		firstColTable.rowSelectRange=emptyRange
    		table.repaint()
    		firstColTable.repaint()
    	}
    	val rows=table.peer.getSelectedRows()
    	var cols=table.peer.getColumnModel().getSelectedColumns()    	
    	if(rows.nonEmpty&&cols.nonEmpty&&cols(0)>=0 ){
    		val newSel=SpreadSheetRange.apply(cols(0),cols(cols.size-1),rows(0),rows(rows.size-1))    		
    		if(newSel!=selection) {
    			selection=newSel    		
    			notifySelectionChanged()
    		}
    	} 
    	else if(selection!=NO_SELECTION) {
    			selection=NO_SELECTION
    			notifySelectionChanged()
    		}
    	
    }

  
  protected def notifyColSelectionChanged():Unit= {
    firstColTable.rowSelectRange=emptyRange
    firstColTable.repaint()
    if(notifySel&&table.headerSelectRange.size>0){
      colSelGroup.children=tableModel.getColumns(table.headerSelectRange)
      colSelGroup.commonFormat=formatList.getCommonFormatForRange(selection)
      colSelGroup.range=selection     
      notifySelectListeners(colSelGroupList) 
    }
  }
  
  
  
  protected def selectColumn(col:Int):Unit={
    firstColTable.rowSelectRange=emptyRange    
    table.MySelectionListener.selfSelected= true
    table.peer.setColumnSelectionInterval(col,col)
    table.peer.setRowSelectionInterval(0,tableModel.getRowCount()-1)
    table.MySelectionListener.selfSelected= false
    selection=new ColsSelection(table.headerSelectRange)
    //println("Col selection "+selection)
    notifyColSelectionChanged()
  }
  
  protected def selectColumns(cols:Range):Unit= if(cols.start >=0){        
    table.MySelectionListener.selfSelected= true
    table.peer.setColumnSelectionInterval(cols.start,cols.end-1)
    table.MySelectionListener.selfSelected= false
    selection=new ColsSelection(table.headerSelectRange)
    //println("Cols selection "+selection)
    notifyColSelectionChanged()
  }
  
  protected def selectRow(row:Int):Unit= {   
    table.headerSelectRange=emptyRange
    selection=new RowsSelection(firstColTable.rowSelectRange)
    notifySelectionChanged()    
    table.MySelectionListener.selfSelected= true      
    table.peer.setColumnSelectionInterval(0,tableModel.getColumnCount()-1)      
    table.peer.setRowSelectionInterval(row,row)
    table.MySelectionListener.selfSelected= false
  }  
  
  protected def selectRows(rows:Range):Unit= {      
    selection=new RowsSelection(rows)
    notifySelectionChanged()
    if(rows!=firstColTable.rowSelectRange) util.Log.w("Not same !!!")
    table.MySelectionListener.selfSelected= true
    table.peer.setRowSelectionInterval(rows.start,rows.end-1)     
    table.MySelectionListener.selfSelected= false
        
  }
  
    
  def notifySelectionChanged():Unit={    
    if(notifySel){ 
     //println("new selection:"+selection)
     selGroup.children=selection.getSelectedCells(tableModel)
     if(selGroup.children.isEmpty)selGroup.children=SpreadSheet.NoCellsList
     selGroup.commonFormat=formatList.getCommonFormatForRange(selection)
     selGroup.range=selection     
     notifySelectListeners(selGroupList)    
    } 
  }
  
  
  
  def deselect(notify:Boolean):Unit= {  
    //println(" Spreadsheet "+spreadSheetRef+" deselect :"+notify)
    notifySel= notify
    table.peer.getSelectionModel().setSelectionInterval(-1,-1)
    table.peer.getColumnModel().getSelectionModel().setSelectionInterval(-1,-1)
    notifySel= true    
    table.headerSelectRange=emptyRange
    firstColTable.rowSelectRange=emptyRange
    selection=NO_SELECTION
    if(table.peer .isEditing) table.instEditor.stopCellEditing()
			table.MySelectionListener.selfSelected= true
			table.peer.clearSelection()
			table.MySelectionListener.selfSelected= false
    table.repaint()
    table.header.repaint()
    firstColTable.repaint()
    if(notify) notifySelectListeners(Nil)
  }
	
	
	// Interface FocusContainer 
	
	
  
  def containerName:String = "Tabellenkalkulation"  
  
  def containerRef=someSpreadSheetRef 
 
  
  def addRows()= {    
    selection match {
      case selRows:RowsSelection=>
        //println("Add rows newSelection "+selRows.rows)
        ClientQueryManager.executeAction(EMPTY_OWNERREF,List(spreadSheetRef),"addRows",
        Seq(("startRow",IntConstant(selRows.rows.start)),("endRow",IntConstant(selRows.rows.end-1))))
      case _=>
    }
    deselect(true)
    Swing.onEDT{tableModel.myFireTableStructureChanged()}
  }
  
  def removeRows() = {
    selection match {
      case selRows:RowsSelection=>
        ClientQueryManager.executeAction(EMPTY_OWNERREF,List(spreadSheetRef),"removeRows",
       Seq(("startRow",IntConstant(selRows.rows.start)),("endRow",IntConstant(selRows.rows.end-1))))
      case _=>
    }
    deselect(true)
    Swing.onEDT{tableModel.myFireTableStructureChanged()}
  }
  
  def addCols()= {
    selection match {
      case selCols:ColsSelection=>
        tableModel.writeColumnWidths()
        ClientQueryManager.executeAction(EMPTY_OWNERREF,List(spreadSheetRef),"addColumns",
        Seq(("startCol",IntConstant(selCols.cols.start)),("endCol",IntConstant(selCols.cols.end-1))))
      case _=>
    }
    deselect(true)
    Swing.onEDT{tableModel.myFireTableStructureChanged()}
  }
  
  def removeCols() = {
    selection match {
      case selCols:ColsSelection=>
        tableModel.writeColumnWidths()
        ClientQueryManager.executeAction(EMPTY_OWNERREF,List(spreadSheetRef),"removeColumns",
        Seq(("startCol",IntConstant(selCols.cols.start)),("endCol",IntConstant(selCols.cols.end-1))))
      case _=>
    }   
    deselect(true)
    Swing.onEDT{tableModel.myFireTableStructureChanged()}    
  }
  
  def copyToClipBoard() = {
    Toolkit.getDefaultToolkit().getSystemClipboard().setContents(transferHandler.createTransferable(table.peer),null)
  }
}



class SpreadSheetVariableResolver extends VariableResolver {
  
  def parseString(st:String)= try {
    Integer.parseInt(st)
  } catch {
    case e:Expression => -1
  }
  
  def moduleName:String="SpreadSheet"
    
  def variableExists(varName:String):Boolean= if (varName.length>1){
    //println("VariableExists: "+varName)
    val ch=varName.charAt(0)
    if(ch>64&&ch<91) {
      val ch2=varName.charAt(1)
      if(ch2>64&&ch<91) {
        val z=parseString(varName.substring(2,varName.length))
        z>=0 
      }else {
        val z=parseString(varName.substring(1,varName.length))
        z>=0
      }
    } else false
  } else false
  
  def getVariableValue(varName:String):Constant= {
    new IntConstant(101)
  }
  def listVariables(forData:InstanceData):Iterable[String]=Seq.empty
  
  
}


object SpreadSheet {
  FunctionManager.get.registerVariableResolver(new SpreadSheetVariableResolver)
  val focusColor=new Color(0, 155, 250)
  lazy val spreadSheetType:Int = SystemSettings().systemTypes("SpreadSheet")
  lazy val spreadSheetColumnType:Int = SystemSettings().systemTypes("SpreadSheetColumn")
  lazy val spreadSheetFormatSetType:Int = SystemSettings().systemTypes("SpreadSheetFormatSet")
  lazy val spreadSheetDoubleCellType:Int = SystemSettings().systemTypes("SpreadSheetDoubleCell")
  lazy val spreadSheetStringCellType:Int = SystemSettings().systemTypes("SpreadSheetStringCell")  
  lazy val spreadSheetCollFuncType:Int = SystemSettings().systemTypes("SpreadSheetCollFuncData")
  lazy val spreadSheetCellType:Int = SystemSettings().systemTypes("SpreadSheetCell")
  
  lazy val NoCellsSelected = new  SpreadSheetCell(-1,-1,new Reference(spreadSheetDoubleCellType,-1),EMPTY_EX,DataType.undefined)
  lazy val NoCellsList=Seq(NoCellsSelected)
  
  val spreadSheetModulName="SpreadSheet"
  val rowOverhead=15
  val firstColumnWidth=40
  val lastColumnWidth=30
  val tableFont=new Font("Arial",0,14)
  val defaultRowHeight=23
  val defaultColumnWidth=100
  val firstColFont=new Font("Arial",0,12)
  
  lazy val defaultFormat=new SpreadSheetFormatRange(EMPTY_REFERENCE,All_SELECTION,new SpreadSheetFormat(HorAlign.LEFT,VertAlign.CENTER,
      CellFormat.Number,Some("%,1.2f"),Some(tableFont.getName), Some(tableFont.getSize().toFloat),
    Some(false),Some(false),Some(false),None,None,Some(false),Some(true),
    Some(false), None, None, None, None/*, None, None*/))
  
  lazy val factory = new SubscriptionFactory[SpreadSheetCell](){
    def emptyFunc(ref:Reference)=new SpreadSheetCell(-1,-1,ref,EMPTY_EX,DataType.undefined)
    registerClass(spreadSheetDoubleCellType,SpreadSheetCell.apply)
    registerClass(spreadSheetStringCellType,SpreadSheetCell.apply)
  }
 
  def frc=new FontRenderContext(null,true,true)
  
}

object SpreadSheetUtil {
   def lettersToCoords(letters:String)={
	  val up=letters.toUpperCase()
	  val c1=up.charAt(0)-65
	  val c2=up.charAt(1)
	   if(c2>64&&c2<91)  (c1 * 26 + c2 - 65,  StringUtils.stringToInt(letters.substring(2,letters.length))-1)
	   else (c1,StringUtils.stringToInt(letters.substring(1,letters.length))-1)
	}
   
   
  
  private def col27ToLetter(id:Int)=(65+id).toChar.toString  
  
  def columnIdToLetter(id:Int):String= if(id<0) "-" else  
    if(id<27) col27ToLetter(id) else 
      col27ToLetter(id/26)+col27ToLetter(id % 26)      
  
  def charToLetter(c:Char):Int= if(c>64&&c<91)c-65 else -1    
      
  def letterToColumnID(letter:String):Int= if(letter.length==1){
    charToLetter(letter.charAt(0))
  } else if(letter.length==2) {
    (charToLetter(letter.charAt(0))+1)*26+charToLetter(letter.charAt(1))
  }
  else -1
}
