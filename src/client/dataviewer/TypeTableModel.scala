/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import java.awt.{Color, Dimension, Font}
import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.{BorderFactory, DropMode, JComponent, JTextArea, KeyStroke}
import javax.swing.event.{ChangeEvent, ListSelectionEvent, ListSelectionListener, TableColumnModelEvent, TableColumnModelListener}
import javax.swing.table.AbstractTableModel

import client.comm.{ClientObjectClass, ClientQueryManager, HasError, KeyStrokeManager}
import client.dataviewer.sidePanel.{ControllerContainer, SPControllerList, SidePanelController}
import client.dialog.{DialogManager, ActionPanel, EditorFactory, Toast}
import client.ui.ClientApp
import definition.data.{InstanceData, Reference}
import definition.expression.{Expression, IntConstant, ParserError, ParserResult, StringConstant, StringParser}
import definition.typ.{AllClasses, DataType, EnumData}

import scala.Option.option2Iterable
import scala.swing.{Alignment, BoxPanel, Button, Component, Label, Orientation, Panel, SequentialContainer, Swing, Table}
import scala.swing.event.{ButtonClicked, FocusGained, KeyPressed, MousePressed, MouseReleased, TableRowsSelected}



class MySelectionModel extends javax.swing.DefaultListSelectionModel {
  override def setAnchorSelectionIndex(index:Int)= super.setAnchorSelectionIndex(if(index==0)1 else index)   
  override def setSelectionInterval(anchor:Int,lead:Int)= {     
    super.setSelectionInterval(if(anchor==0)1 else anchor,if(lead==0) 1 else lead)   
  }
}


/** table model for a table showing instances of a certain type
 * 
 */
class TypeTableModel(val tableIx:Int,val typ:Int,val propMod:PropertyModel,singleField:Boolean,showClassLabel:Boolean) extends AbstractTableModel with ControllerContainer {
  import client.dataviewer.ViewConstants._
	val objClass=AllClasses.get.getClassByID(typ).asInstanceOf[ClientObjectClass]
	var dataList:Seq[InstanceData]= Seq.empty	
	var selfSelectChanged=false
	var selfAdded=false
	val selectedInstances=new SelectList(dataList)
	var dragColumn:Int= -1
	var dragColumnNewPos:Int=0
	var wishSelection:Seq[Int]=Seq.empty // what items should be selected after a refresh (for move and copy)
	val transferHandler=new TableTransferHandler(this)
	val listLock=new Object
	val classLabel=new Label("   "+AllClasses.get.getClassByID(typ).getDescriptionOrName)
	var clickedRow= -1
	var clickedCol= -1	
	var editActionNotDone:java.awt.event.ActionEvent=null
	var oldEnterAction:javax.swing.Action=null
	val sideBarPanel=new BoxPanel(Orientation.Vertical)
  var currentSideBarController:Option[SidePanelController]=None
	
	lazy val emptyHeaderPanel=new BoxPanel(Orientation.Horizontal ) {
		reactions += {
			case ButtonClicked(e) => openSideBarController(e.name)
		}
	}
	
	lazy val sideControllerList=SPControllerList.generateList(objClass)  
	
	val sm=new MySelectionModel
	val table:Table=new Table(){			
			  autoResizeMode=Table.AutoResizeMode.Off		
				selection.elementMode=Table.ElementMode.Row 
				peer.setAutoCreateColumnsFromModel(false)
				peer.setTransferHandler(transferHandler)
				peer.setDragEnabled(true)
				peer.setDropMode(DropMode.ON)		
				rowHeight=defaultRowHeight
				font=tableFont
				xLayoutAlignment=0d
				this.peer.getTableHeader().setFont(ViewConstants.smallFont)
				
				reactions += {
					case TableRowsSelected(tble,range,live) =>
						if (!live&& ! selfSelectChanged) listLock.synchronized  {
							selectedInstances.setFilter(peer.getSelectedRows)
							//println("selectedInstances "+selectedInstances)
							propMod.mainController.selectionChanged(TypeTableModel.this,propMod,selectedInstances)
						}
					case e: MousePressed => if(e.peer.getButton== java.awt.event.MouseEvent.BUTTON1){
						clickedCol=peer.columnAtPoint(e.point)
						clickedRow=peer.rowAtPoint(e.point)
						table.repaint()
					} else if(e.peer.getButton==java.awt.event.MouseEvent.BUTTON3) {
					  if(!table.hasFocus) {
					    table.requestFocus()
					    table.selection.rows.clear()
					    table.selection.rows+=peer.rowAtPoint(e.point)
					    table.selection.columns.clear()
					    table.selection.columns+=peer.columnAtPoint(e.point)			    
					  } 			    
					  ActionPanel.showRightMenu(e.point,table)
					}
					case e: MouseReleased => if(dataList!=null && peer.columnAtPoint(e.point)== 0 && clickedCol==0 &&
							peer.rowAtPoint(e.point)==clickedRow && clickedRow>=0 && clickedRow < dataList.size&& !e.triggersPopup &&
							e.peer.getButton== java.awt.event.MouseEvent.BUTTON1)
						    openChild(clickedRow)
						clickedRow= -1
						clickedCol= -1
						table.repaint()
					case e:FocusGained =>propMod.focusGained(Some(table))
					case e: KeyPressed =>
						e.peer.getKeyCode match {
              case KeyEvent.VK_DOWN  => if(table.selection.rows.nonEmpty){
                if((e.peer.getModifiersEx() & InputEvent.CTRL_DOWN_MASK)>0) {
                  e.consume()
                  Swing.onEDT{openChild(table.selection.rows.head)}
                } else if(table.selection.rows.head==getRowCount-1) {
                  deselect()
                  propMod.tableExitsToDown(tableIx)
                }
              }
              case KeyEvent.VK_UP =>if((e.peer.getModifiersEx() & InputEvent.CTRL_DOWN_MASK)>0 ) propMod.mainController.viewBox.goUp()
                 else if(table.selection.rows.isEmpty||table.selection.rows.head==0){
                   deselect()
                   propMod.tableExitsToUp(tableIx)
                 }

              case _=> if(e.peer.getKeyChar== KeyEvent.CHAR_UNDEFINED ) // if there are no mappings for that key, ignore it
                if(table.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
                    get(KeyStroke.getKeyStroke(e.peer.getKeyCode,0,false))==null){
                  e.peer.consume()
                }
            }
				}
				model=TypeTableModel.this
				peer.setColumnModel(TableHeaderMap.getColumnModel(typ))		
				showGrid=false
				this.peer.setShowHorizontalLines(true)
				peer.setRowSelectionAllowed(true)
				peer.setColumnSelectionAllowed(true)					
				listenTo(selection,mouse.clicks,this,keys)		
				peer.setIntercellSpacing(new Dimension(0, 0))
				gridColor=Color.lightGray
					
				peer.getColumnModel().setSelectionModel(sm)
				sm.addListSelectionListener(new ListSelectionListener{
				  def valueChanged(e:ListSelectionEvent )= if(!e.getValueIsAdjusting()&& 
				      table.selection.columns.nonEmpty&& table.selection.rows.nonEmpty && table.hasFocus)
					  scrollToSelection()
				  
				})
				peer.getColumnModel().addColumnModelListener(new TableColumnModelListener(){ 
					def columnAdded(e:TableColumnModelEvent ): Unit = {}
					def columnMarginChanged(e:ChangeEvent ): Unit = {}
					def columnMoved(e:TableColumnModelEvent): Unit = {
						if (dragColumn == -1) 
							dragColumn = e.getFromIndex()
            dragColumnNewPos = e.getToIndex()
          }
					def columnRemoved(e:TableColumnModelEvent ): Unit = {}
					def columnSelectionChanged(e:javax.swing.event.ListSelectionEvent ): Unit = {}
				})

    val custRendererMap=(objClass.fields.indices filter(objClass.fieldSetting(_).editor.length>0) flatMap (
				    ix=>  EditorFactory.getInplaceEditor(objClass.fieldSetting(ix).editor) match {
				      case Some(editor)=> Some(ix -> editor.createRenderer)
				      case None => None
				    })).toMap
				
				peer.getTableHeader().addMouseListener(new java.awt.event.MouseAdapter() { 
					override def mouseReleased(e:java.awt.event.MouseEvent): Unit = {
						if (dragColumn != -1 && (dragColumn == 0 || dragColumnNewPos == 0)) 
							peer.moveColumn(dragColumnNewPos, dragColumn)
            dragColumn = -1;  dragColumnNewPos = -1
          }
				})

    // setup renderers
				val firstButRend=new FirstColumnRenderer(TypeTableModel.this)
				val ftcr = new Table.AbstractRenderer[String, FirstColumnRenderer](firstButRend) {
					def configure(t: Table, sel: Boolean, foc: Boolean, o: String, row: Int, col: Int) =     
						component.config(sel,foc,o,row)  
				}
				
				val instRenderer=new InstanceRenderer(AllClasses.get.getClassByID(typ))
				val itcr = new Table.AbstractRenderer[Expression, InstanceRenderer](instRenderer) {
					def configure(t: Table, sel: Boolean, foc: Boolean, o: Expression, row: Int, col: Int) =     
						component.config(t,sel,foc,o,row,col)
				}
				val etcr = new Table.AbstractRenderer[(String, Int), EnumRenderer](new EnumRenderer) {
					def configure(t: Table, sel: Boolean, foc: Boolean, o:(String, Int), row: Int, col: Int) = {
						component.prepare(t,sel,o,row)
				  }
				}				
				
				override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
					//FIND VALUE
					val modCol=peer.convertColumnIndexToModel(col)
					val v=model.getValueAt(row,modCol)
					//val v = model.getValueAt(	peer.convertRowIndexToModel(row),	modCol)
					if(col==0) {
						if(!singleField && row>=getRowCount-1) itcr.componentFor(this,false,false,null,row,col)
						else ftcr.componentFor(this,sel,foc,if(v==null) null else v.toString, row, col)
					}			  
					else v match {
						case ve:Expression => if(custRendererMap.contains(modCol-1)) custRendererMap(modCol-1).componentFor(this,sel,foc,ve,row,modCol) 
						else itcr.componentFor(this,sel,foc,ve,row,modCol)
						case vt:(_, _) => etcr.componentFor(this,sel,foc,v.asInstanceOf[(String, Int)],row,modCol)
						case null => itcr.componentFor(this,sel,foc,null,row,modCol)
						case _=> super.rendererComponent(sel,foc,row,col)
					}
				}
				
				super.rendererComponent(false,false,0,0).font=tableFont
				
				override def editor(row: Int, column: Int)= {
					if(column==0)null
					else {
						val edit=peer.getColumnModel.getColumn(column).getCellEditor
						if(edit!=null)edit
						else instEditor
					}
				}
		
	} // table
	
		
	KeyStrokeManager.replaceKeyAction(table.peer,KeyEvent.VK_ENTER,(oldAction)=>{
		oldEnterAction=oldAction
		new javax.swing.AbstractAction() {
			def actionPerformed(e:java.awt.event.ActionEvent)= {
				//System.out.println("Enter Abgefangen "+table.peer .isEditing)
				if(!table.peer.isEditing) {
					oldAction.actionPerformed(e)
					editActionNotDone=null
				}
				else {
					
					instEditor.stopCellEditing()
					editActionNotDone=e
				}			
			}
		}
	})
	
	KeyStrokeManager.replaceKeyAction(table.peer,KeyEvent.VK_ESCAPE,(oldAction)=> {
		new javax.swing.AbstractAction() {
			def actionPerformed(e:java.awt.event.ActionEvent)= {
				//System.out.println("Escape :"+table.peer .isEditing)
				if(table.peer.isEditing)
					oldAction.actionPerformed(e)				
			}
		}
	})
	
	val inputValidator=new MyInputValidator() {
	  var editorComponent:JComponent=null
	  def validate(text:String,col:Int):Option[Int] = {
	    val mcol=table.peer.convertColumnIndexToModel(col)
	    if(text!=null&& text.length>0&& col >0 && objClass.fields(mcol-1).typ!=DataType.StringTyp) {
	      StringParser.parse(text) match {
	        case ex:Expression => None
	        case err:ParserError =>
						util.Log.e("validation error:"+err)
						ClientQueryManager.printErrorMessage(err.message)
						if(editorComponent!=null){
              new Toast(err.message,editorComponent,ClientApp.top).visible=true
            } else util.Log.e("editorComponent == null")
						Some(err.offset)
				}
	      
	    }
	    else None
	  }
    def setEditorComponent(comp:JComponent)= editorComponent=comp
	}
	
	val instEditor=new MultilineEditor(table.peer,Some(inputValidator))	{
		def setEditorValue(value:Object) = value match {
				case expr: Expression =>
					if (expr.getType == DataType.StringTyp) expr.toString
					else expr.getTerm
				case _ => if (value == null) "" else value.toString
			}

	}	
	
	inputValidator.setEditorComponent(instEditor.component)
	
	
	
	val leftPanel= new Panel with SequentialContainer.Wrapper {
		yLayoutAlignment=0d
   	val headerWrapper= new Component {
			override lazy val peer=table.peer.getTableHeader
			xLayoutAlignment=0d
		}		
		xLayoutAlignment=0d
		if(!singleField&&showClassLabel) {
			classLabel.horizontalAlignment=Alignment.Left
			classLabel.xLayoutAlignment=0d
			classLabel.font=tableTypeFont
			//classLabel.border=BorderFactory.createLineBorder(classLabel.background,2)
			contents+=classLabel
		}			
		contents+=headerWrapper
		contents+=table//viewportWrapper			
		
		override lazy val peer = {
			val p = new javax.swing.JPanel with SuperMixin {
				override def getPreferredSize= new Dimension(table.peer.getPreferredSize.width,table.peer.getPreferredSize.height+
				    (if(singleField) 0 else classLabel.preferredSize.height)+headerWrapper.preferredSize.height)
				override def getMaximumSize=getPreferredSize
			}
			val l = new javax.swing.BoxLayout(p, Orientation.Vertical.id)
			p.setLayout(l)    
			p
		}		
	}	
	
	val scroller= new BoxPanel(Orientation.Horizontal){		
		sideBarPanel.yLayoutAlignment=0d
		sideBarPanel.maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
		sideBarPanel.xLayoutAlignment=0d	  
		contents+=leftPanel+=sideBarPanel
	  minimumSize=new Dimension(0,0)		
	}
	
	
	
	def isEmpty=dataList.isEmpty
	
	def openChild(row:Int)=listLock.synchronized { propMod.mainController.openChild(dataList(row).ref)}	
	
	def scrollToSelection():Unit= if(table.selection.rows.nonEmpty&& table.selection.columns.nonEmpty)
    table.peer.scrollRectToVisible(table.peer.getCellRect( table.selection.rows.head,/*table.selection.columns.head*/1 , true))
					      
  
	def enterFromTop()=Swing.onEDT{	  
	  table.selection.columns+=1
	  table.peer.setRowSelectionInterval(0,0)	  
	  table.requestFocus()	  
	}
	
	def enterFromBottom()=Swing.onEDT{
	  table.selection.columns+=1
	  table.peer.setRowSelectionInterval(getRowCount-1, getRowCount-1)
	  table.requestFocus()	  
	}
	
		
	/** Checks what sidebar controllers can be active for this kind of data
	 *
	 */
	def updateControllers():Unit = listLock.synchronized {
		  currentSideBarController match {
		  	case Some(contr) =>	contr.notifyRowsChanged()
		  	case None =>
					shutDown()
					propMod.mainController.activeSidePanelController match {
            case Some(oContr)=>
							//println("oldContr:"+oContr+" "+sideControllerList.size)
							for (c<-sideControllerList)
                //println("C:"+c+ " "+propMod.mainController.ref+" " +c.parentsFits(this,propMod.mainController.ref))
                if (c.parentsFits(this,propMod.mainController.ref)&&c.getClass==oContr.getClass)
                    Swing.onEDT{openSideBarController(c.panelName)}
						case None => Swing.onEDT{showEmptyHeaderPanel()}
          }
			}
	}
	
	private def showEmptyHeaderPanel() = {
			sideBarPanel.contents.clear()
			emptyHeaderPanel.contents.clear()
			//println("showEmptyPanel :"+sideControllerList.size)
			for (c<-sideControllerList;if c.parentsFits(this, propMod.mainController.ref)) {
				val but=new Button("")
				but.name=c.panelName
				but.peer.putClientProperty("JComponent.sizeVariant", "mini")
        but.peer.updateUI()
				for(ic<-c.panelIcon) but.icon=ic
				emptyHeaderPanel.listenTo(but)
				emptyHeaderPanel.contents+=but
			}
			emptyHeaderPanel.contents+=Swing.HGlue
			sideBarPanel.contents+=emptyHeaderPanel
	}
	
	/** is called by the emptyHeaderPanel when a Button is clicked
	 * opens the custom panels in the sidebar
	 * 
	 * @param name name of the Controller choosen
	 */
	private def openSideBarController(name:String):Unit = {
	  //println("open sidebar:"+name)
		currentSideBarController=None
		for(contr <-sideControllerList.find(_.panelName==name)) {
			currentSideBarController=Some(contr)			
			sideBarPanel.contents.clear()
			val header=contr.headerComp
			sideBarPanel.contents+=header			
			sideBarPanel.contents+=contr.mainComp
			//println("cl:"+classLabel.preferredSize.height+" th:"+table.peer.getTableHeader.size.height)
			header.preferredSize=new Dimension(1,classLabel.preferredSize.height+table.peer.getTableHeader.getSize().height)
			header.maximumSize=new Dimension(Short.MaxValue,classLabel.preferredSize.height+table.peer.getTableHeader.getSize().height)	
			header.font=ViewConstants.smallFont
			contr.openPanel(propMod.mainController.ref, objClass,this)
			propMod.mainController .activeSidePanelController =Some(contr)
			//propMod.mainController .currentSidePanelControllerWasUsed =true
		}
		sideBarPanel.peer.invalidate()
		scroller.peer.revalidate()
		scroller.peer.repaint()
	}
	
	/** shuts down eventually open sidebar controllers 
	 * 
	 */
	def shutDown() = {
		removeSideBar()
	}
	
	private def removeSideBar() = {
		for(c <- currentSideBarController)
			c.closePanel()
		currentSideBarController=None
		sideBarPanel.contents.clear()
	}
	
	/** is called from the sideBarController when it's close button was clicked
	 * 
	 */
	def closeSideBar() = {
		removeSideBar()
		sideBarPanel.contents+=emptyHeaderPanel
		propMod.mainController.activeSidePanelController=None
		scroller.repaint()
	}		
	
	def getParentRef=propMod.mainController.ref
	def getPropField= propMod.ownerRef.ownerField 
	    
  /** load the current values in the table model
   * 
   * @param data the new data
   * @param selectInstance what instance should be selected (when moving up the path)
   * @param onlyRefresh // true= this is only a refresh call after a move, false: it is first time called for loading
   * @param isFirstTable is the first Table in the TableViewBox
   */
	def setDataList(data:Seq[InstanceData],selectInstance:Option[Reference],onlyRefresh:Boolean,isFirstTable:Boolean) =  {
		listLock.synchronized {
			Swing.onEDT{
				clickedRow= -1
				clickedCol= -1
				//System.out.println("tableMod set Data "+data.mkString)
				dataList=data
				selfSelectChanged=false
				selfAdded=false
				selectedInstances.buf=data
				selectedInstances.setFilter(Array())
				if(wishSelection.isEmpty)
					wishSelection=table.selection.rows.iterator.toSeq
				fireTableDataChanged()
				selectInstance match {
					case Some(ref) =>if(ref.typ == typ){
						val ix= dataList.indexWhere(_.ref==ref)
						if(ix>=0)Swing.onEDT{
							table.selection.rows+=ix
							table.selection.columns+=1
							table.requestFocus()
							scrollToSelection()
							table.repaint()
							if(!onlyRefresh){
								selectedInstances.setFilter(Array(ix))
								propMod.mainController.selectionChanged(TypeTableModel.this,propMod,selectedInstances)
							}
						}
					}
					case None => if(onlyRefresh) {
						wishSelection.foreach(a => if(a<data.size) table.selection.rows+= a)
					} else if(isFirstTable)Swing.onEDT{
						table.selection.columns+=1
						table.selection.rows+=0
						table.requestFocusInWindow()
						scrollToSelection()
					}
				}
				wishSelection=Seq.empty

			}
			updateControllers()
		}
	}	
	
	def calcSize() = {}

	def changeInstance(newInst:InstanceData):Unit = listLock.synchronized {
		//System.out.println("tablemod change inst: "+newInst.ref)
		val pos = dataList.indexWhere(_.ref==newInst.ref)
		//System.out.println("change "+newInst.ref+ " size:"+dataList.size+ " pos:"+pos+ " list:"+dataList.mkString+ "  "+ Thread.currentThread)
		if(pos<0) util.Log.e("prop "+(if(propMod!=null&& propMod.ownerRef!=null)getPropField else "")+" Table typ: "+typ+" Change Instance "+newInst.ref+" not found ! " + dataList.size+" "+Thread.currentThread)
		else  { 
			//System.out.println("change selfadded:"+selfAdded+" EditActionNotDone:"+editActionNotDone)
			dataList=dataList.updated(pos,newInst)
			selectedInstances.buf=dataList
			/*propMod.runSw*/fireTableRowsUpdated(pos,pos)
			if(editActionNotDone!=null)	{
				oldEnterAction.actionPerformed(editActionNotDone)
				editActionNotDone=null
			}
				
		}	
	}

	def addInstance(newInst:InstanceData) = listLock.synchronized {
		//System.out.println("tablemod add inst: "+newInst.ref)
		if(dataList==null) {
			dataList=IndexedSeq(newInst)
			//setupColumns()
		}
		else {
			//if(pos<0 || pos >=dataList.size)
				dataList=dataList :+ newInst
				//else  dataList=(dataList.take(pos):+newInst)++ dataList.drop(pos)
		}
		selectedInstances.buf=dataList
		//System.out.println("added "+dataList.size+" "+Thread.currentThread+ " "+table.selection.rows)
		val newSize=dataList.size
		
		fireTableRowsInserted(newSize,newSize)
		for(c <-currentSideBarController) c.notifyRowsChanged()
		calcSize()
		//System.out.println(" add selfadded:"+selfAdded+" EditActionNotDone:"+editActionNotDone)
		if(selfAdded){ 
			propMod.mainController.selectionChanged(TypeTableModel.this,propMod,Array(newInst))
			selfAdded=false
			if(editActionNotDone!=null)	{
				oldEnterAction.actionPerformed(editActionNotDone)
				editActionNotDone=null
			}
		}
		
			//table.peer.setRowSelectionInterval(newSize-1,newSize-1)}
	}

	def removeInstance(ref:Reference) = listLock.synchronized{	
		//System.out.println("tablemod remove inst: "+ref)
		val pos=dataList.indexWhere(_.ref==ref)
		if(pos<0) util.Log.e("Remove Instance "+ref+" not found !")
		else {
			dataList=dataList filterNot(_.ref ==ref)
			selectedInstances.buf=dataList
			calcSize()
			/*propMod.runSw*/fireTableRowsDeleted(pos,pos)
			for(c <-currentSideBarController) c.notifyRowsChanged()
		}
	}


	def getRowCount= listLock.synchronized{
		 //System.out.println("get size "+(dataList.size+1)+ " " +Thread.currentThread)
		if(dataList!=null) if(singleField)1 else dataList.size+1
		else 0
	}

	def getColumnCount= listLock.synchronized{
		objClass.fields.size+1
	}

	def getValueAt(row:Int,col:Int) = listLock.synchronized{
		if(dataList!=null&& row<dataList.size) {
			val el=dataList(row)
			if(col==0) { 
				var retStr=if (el.hasChildren) "+" else " "
				if(el.secondUseOwners.size>0) 
					retStr=retStr+" ·"
				retStr
			} // childInfo in column 0
			else {	
			  objClass.enumFields.get(col-1) match {
			    case Some(eData)=>eData.getElem(el.fieldValue(col-1).toInt)
			    case None=> if(col-1<el.fieldData.size) el.fieldData(col-1)else {
            util.Log.e("Wrong columnNr:"+(col-1)+" allowed: "+el.fieldData.size+" "+el.ref);null
			    }
			  }							
			}
		}
		else null
	}
	
	def getRowReference(row:Int):Option[Reference] = listLock.synchronized {
		if(dataList!=null&& row<dataList.size) Some(dataList(row).ref)
		else None
	}
	
	/** gets the index of the ydata with the given reference (for fire update in xtab model)
	 * 
	 */
	def getReferenceRow(sref:Reference):Option[Int]= listLock.synchronized {
	  if(dataList!=null) {
	    val r=dataList.indexWhere(_.ref ==sref)
	    if(r== -1) None
	    else Some(r)
	  }
	  else None
	}

	override def setValueAt(aValue: Object, rowIndex: Int, columnIndex: Int): Unit =		
		if(dataList!=null&& columnIndex >0)  listLock.synchronized {  
			
			val expr= if(objClass.enumFields.exists(_._1==columnIndex-1)) 
				IntConstant(if(aValue==null) 0 else aValue.asInstanceOf[EnumData].id) // enumeration 
				else parseValue(columnIndex,aValue) match {
				  case ex:Expression =>ex
				  case err:ParserError =>
						ClientQueryManager.printErrorMessage(err.message)
						util.Log.e("nach fehler "+err)
						new Toast(err.message,table.peer.getEditorComponent().asInstanceOf[JComponent],ClientApp.top).visible=true
						return
			}
			//println("Expr:"+expr)
			val ref= if(rowIndex==dataList.size) { // create new
				selfAdded=true
				val id=ClientQueryManager.createInstance(typ,Array(propMod.getOwnerRef))				
				Reference(typ,id)											
			}
			else dataList(rowIndex).ref				
			ClientQueryManager.writeInstanceField(ref,(columnIndex-1).toByte,expr, {
        case HasError(err: Exception) =>
					Swing.onEDT({
            //println("col:"+columnIndex)
            table.peer.editCellAt(rowIndex, table.peer.convertColumnIndexToView(columnIndex))
            Swing.onEDT({
              if (aValue != null)
                table.peer.getEditorComponent() match {
                  case m: JTextArea => m.setText(aValue.toString); new Toast(err.getMessage(), table.peer.getEditorComponent().asInstanceOf[JComponent], ClientApp.top).visible = true
                  case o => util.Log.e("Other Editor:" + o + " " + o.getClass)
                }
            })
          })
				case _ =>
      }) 			
		}  	


	override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = { 
		columnIndex>0 
	}

	override def getColumnName(col:Int) = listLock.synchronized {
		if(col==0) "ch" else objClass.fields(col-1).name
	}
	
	override def getColumnClass(col:Int):java.lang.Class[_] =  {
		//println("get Column class for class "+objClass.name+" "+objClass.enumFields.mkString)
		if(col==0) classOf[String] else {
			if(objClass.enumFields!=null && objClass.enumFields.exists(_._1==col-1)) classOf[(String,Int)]
		  else classOf[definition.expression.Expression]
		}
	}


	def parseValue(columnIndex:Int,value:Object):ParserResult = {
			if(value==null)Expression.generateNullConstant(objClass.fields(columnIndex-1).typ) else
				if (objClass.fields(columnIndex-1).typ==DataType.StringTyp)	{
						StringParser.parse( value.toString) match {
						  case ex:Expression => ex
						  case err:ParserError => new StringConstant(value.toString)
						} 
					}					
				else StringParser.parse( value.toString) // throw exception when fail
	}


	def deselect() = listLock.synchronized{
		//System.out.println("deselect " +typ+ " rows:"+table.selection.rows)
		if(dataList!=null && table.selection.rows.nonEmpty) {
			if(table.peer .isEditing) instEditor.stopCellEditing()
			selfSelectChanged=true
			table.peer.clearSelection()
			selfSelectChanged=false
			selectedInstances.setFilter(Array())
		}
	}
	
	

	


	class SelectList[A](var buf: Seq[A],private var filterSet:Array[Int]=Array()) 
	extends collection.immutable.IndexedSeq[A] {
		def length = if(buf==null) 0 else filterSet.size
		def apply(idx: Int) ={ //System.out.println ("buf size:"+buf.size+ " "+" idx:"+idx+" filterSet:"+filterSet.mkString+" "+Thread.currentThread)
			val bufIx=filterSet(idx)
			if(bufIx<buf.size)
			buf.apply(filterSet(idx))
			else null.asInstanceOf[A]
		}
		def setFilter(newFilter:Array[Int]) = if(buf!=null && newFilter.contains(buf.size)) {
			filterSet=newFilter.take(newFilter.length-1)
		} else filterSet=newFilter

		override def toString:String ="buffer:"+(if(buf == null)"null" else buf.mkString(",")+"\nfilter:"+filterSet.mkString(","))
	 }

  class FirstColumnRenderer(mod:TypeTableModel) extends LabelRenderer {
  	val raisedBorder=BorderFactory.createRaisedBevelBorder()
    val loweredBorder=BorderFactory.createLoweredBevelBorder()

    override def config( isSelected: Boolean, focused: Boolean, a: String, row: Int): Unit = {
			if(a==null) text="" else text=a
  	  super.background= if(a==null)DialogManager.leftPanelColor else if(clickedCol==0&&clickedRow==row)Color.lightGray else buttonBackgroundColor
  	  border=if(clickedCol==0&&clickedRow==row)loweredBorder else raisedBorder
		}
	}


}

class EnumRenderer extends Label {
  	override def revalidate()= {}
  	def prepare(t:Table,isSelected:Boolean,o: (String, Int),row:Int): Unit = {
  		horizontalAlignment=Alignment.Left
  		text = "· "+o._1 //or whatever
  		background=if(isSelected)  t.selectionBackground 
  		else  /*if (row % 2 == 0)InstanceRenderer.alternateColor
  		else*/ Color.white
  	}
  }


class LabelRenderer extends Label {
  	//border=BorderFactory.createRaisedBevelBorder();
  	
  	override def revalidate()= {}
  	super.background=ViewConstants.buttonBackgroundColor
  	super.foreground=Color.black
  	override def background_=(c: Color) = {				
		}
  	override def foreground_=(c: Color) = {				
		}
  	
		def config( isSelected: Boolean, focused: Boolean, a: String, row: Int)= text=a  
		
	}


object ViewConstants {
  val nearWhite=new Color(254,254,254)
  val defaultRowHeight=25
  val tableFont=new Font("Arial",0,14)
  val smallFont=new Font("Arial",0,12)
  val tinyFont=new Font("Arial",0,10)
	val tableTypeFont=new Font("Arial",Font.ITALIC,11)
  val labelFont=new Font("Arial",0,14)
  val questionFont=new Font("Arial",0,15)
  val errorFont=new Font("Arial",1,14)
  
	val buttonBackgroundColor= /*UIManager.getLookAndFeel() match {
    case nimbus:com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel => UIManager.getColor("Panel.background") 
    case other =>*/new Color(214,217,223)
  //}
	  
//	
}