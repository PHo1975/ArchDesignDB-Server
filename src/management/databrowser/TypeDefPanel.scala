/**
 * Author: Peter Started:28.11.2010
 */
package management.databrowser

import java.awt.event.MouseWheelListener
import java.awt.{Color, Dimension}
import javax.swing.border._
import javax.swing.table._
import javax.swing._

import client.dataviewer.{FieldColumnModel, MultilineEditor, ViewConstants}
import client.dialog.{ComboBoxEditor, ReactiveTextField}
import definition.typ._
import server.config.FSPaths
import server.storage._
import transaction.handling.{SessionManager, TransactionManager}

import scala.collection.JavaConverters._
import scala.swing.event._
import scala.swing.{Color, _}
import scala.util.control.NonFatal


class FormatLine(labelWidth:Int,labelText:String,getter:()=> String,setter: (String)=>Unit) extends 
		BoxPanel(Orientation.Horizontal)
	{
	  xLayoutAlignment=0
		val label: Label = ViewConstants.label(labelText + ":")
		label.font = ViewConstants.labelFont
		label.preferredSize=new Dimension(labelWidth,0)
		val edit=new TextField(getter())
		contents+=label+=edit
		def update(): Unit = edit.text=getter()
		listenTo(edit)
		reactions+= {
			case e:EditDone => setter(edit.text)
		}	
		maximumSize=new Dimension(Short.MaxValue,30)
	}

/**
 * 
 */
object TypeDefPanel extends BorderPanel {
  
	var isCreating:Boolean=false
	var theClass:ServerObjectClass =EmptyServerClass		
	val nameEdit=new ReactiveTextField((s)=> theClass.name =s)
	val descriptionEdit=new ReactiveTextField((s)=> theClass.description =s)
	val commentEdit=new ReactiveTextField((s)=> theClass.comment =s)
	val idEdit=new ReactiveTextField((s)=>theClass.id=s.toInt)
	val classesListview = new ListView[(Int,String)]
	classesListview.selection.intervalMode=ListView.IntervalMode.Single
	val superClassesListview = new ListView[(Int,String)]
	superClassesListview.selection.intervalMode=ListView.IntervalMode.Single
	val addSuperClassBut=new Button("add >")
	val removeSuperClassBut= new Button("< remove ")
	val disableComps=List(idEdit,addSuperClassBut,removeSuperClassBut)	
	val saveBut=new Button("Save changes")
	val formEditBut=new Button("Form-Designer")
	val autoCreateChildBut=new Button("Auto create children...")
	val addFieldBut=new Button("add Field ...")
	val addPropFieldBut=new Button("Insert PropField ...")
	val inheritedFieldMod=new FieldDefTableModel(false)
	val ownFieldMod=new FieldDefTableModel(true)
	val typeVect=new java.util.Vector[DTWrap](DataType.wrappedValues.asJava)
  val typeComboBox=new JComboBox(typeVect)
	val typeEditor=new ComboBoxEditor(typeComboBox)
	val enumVect=new java.util.Vector[EnumDefinition](SystemSettings().enums.values.asJavaCollection)
	//println("enums :"+enumVect.mkString(" + "))
	val enumCombo=new JComboBox(enumVect)
	val enumEditor=new ComboBoxEditor(enumCombo)
	lazy val formEditDialog=new FormDesignerDialog(management.databrowser.MainWindow.top)
	lazy val autoCreateDialog=new AutoCreateDialog(management.databrowser.MainWindow.top)
	lazy val addFieldDialog=new AddFieldDialog(management.databrowser.MainWindow.top) 
	
	val fieldColMod=new FieldColumnModel{
	    createColumn(0,"ix",25)
    	createColumn(1,"Name",110)
    	createColumn(2,"Typ",70)
    	createColumn(3,"EnumID",110)
    	createColumn(4,"lock",35)    	
    	createColumn(5,"term",35) 
    	createColumn(6,"form",35)
    	createColumn(7,"visi",35)
    	createColumn(8,"editor",90)
    	createColumn(9,"startValue",100)
    	createColumn(10,"Format",80)
    	getColumn(2).setCellEditor(typeEditor)
    	getColumn(3).setCellEditor(enumEditor)
    }
	val inheritedFieldTable=new FieldTable(inheritedFieldMod,fieldColMod)	
	val ownFieldTable=new FieldTable(ownFieldMod,fieldColMod)

	val inheritedPropMod=new PropFieldTableModel(false)
	val ownPropMod= new PropFieldTableModel(true)
	val propFieldColMod=new FieldColumnModel{
    	createColumn(0,"Name",80)
    	createColumn(1,"single",30)
    	createColumn(2,"Allowed Class",130)
    	createColumn(3,"hidden",30)    	
    	createColumn(4,"volat",30)    	
	}
	val inheritedPropTable=new FieldTable(inheritedPropMod,propFieldColMod)
	inheritedPropTable.background=Color.lightGray
	val ownPropTable=new FieldTable(ownPropMod,propFieldColMod)
	val classNameRenderer=new ClassNameRenderer
	inheritedPropTable.peer.setDefaultRenderer(classOf[java.lang.Integer], classNameRenderer)
	ownPropTable.peer.setDefaultRenderer(classOf[java.lang.Integer], classNameRenderer)
	val childDefMod=new ChildDefTableModel	
	val childDefColMod=new FieldColumnModel{
    	createColumn(0,"EditorName",100)
    	createColumn(1,"ChildClass",100)
    	createColumn(2,"ActionName",110)    	
    }
	val childDefTable=new FieldTable(childDefMod,childDefColMod)
	childDefTable.peer.setDefaultRenderer(classOf[java.lang.Integer], classNameRenderer)
	val childDefColor: Color =childDefTable.background
	val disableModels=List(inheritedFieldMod,ownFieldMod,inheritedPropMod,ownPropMod,childDefMod)
	
	val classEditor=new ClassNameEditor(new JComboBox)
	ownPropTable.peer.setDefaultEditor(classOf[java.lang.Integer], classEditor)
	childDefTable.peer.setDefaultEditor(classOf[java.lang.Integer], classEditor)
	
	
	
	val formatStringLines=Array(
		new FormatLine(100,"Short Format",()=>theClass.shortFormat.toString,(s)=>theClass.shortFormat =InstFormat.fromString(s)),
		new FormatLine(100,"Long Format",()=>theClass.longFormat.toString,(s)=>theClass.longFormat =InstFormat.fromString(s)),
		new FormatLine(100,"Result Format",()=>theClass.resultFormat.toString,(s)=>theClass.resultFormat =InstFormat.fromString(s))	)
	val moduleLine=new FormatLine(100,"ActionModule",()=>theClass.moduleName,(s)=>theClass.moduleName=s)
	val instanceEditorLine=new FormatLine(100,"InstanceEditor",
		()=>theClass.customInstanceEditor match {case Some(n)=>n;case _=> ""},(s)=>theClass.customInstanceEditor=if(s.length==0)None else Some(s))
	val importDescriptorLine=new FormatLine(100,"ImportDescriptor",
		()=>theClass.importDescriptor match {case Some(n)=>n;case _=> ""},(s)=>theClass.importDescriptor=if(s.length==0) None else Some(s))
	
	
	val basicsPan = new BoxPanel(Orientation.Vertical) {
	  contents +=new BoxPanel(Orientation.Horizontal ) {						
			idEdit.inputVerifier=checkID
			contents += ViewConstants.label("id:") += idEdit += Swing.HStrut(10) += ViewConstants.label("Name:") += nameEdit += Swing.HStrut(10) +=
				ViewConstants.label("Description:") += descriptionEdit
			maximumSize=new Dimension(Short.MaxValue,50)
		} += new BoxPanel(Orientation.Horizontal) {
			contents += ViewConstants.label("Comment:") += commentEdit
		}
	} 
	
	val superClassesPan = new BoxPanel(Orientation.Horizontal) {	
		val overTopBorder: TitledBorder = BorderFactory.createTitledBorder("Superclasses")
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP)
		border=overTopBorder
		val leftScroller=new ScrollPane {
			viewportView=classesListview
		}
		val centerPan= new BoxPanel(Orientation.Vertical){
			contents+=addSuperClassBut+=removeSuperClassBut+=Swing.VGlue
		}
		val rightScroller=new ScrollPane {
			viewportView=superClassesListview
		}		
		contents+=leftScroller+=centerPan+=rightScroller
		listenTo(addSuperClassBut,removeSuperClassBut)
		reactions += {
			case ButtonClicked(`addSuperClassBut`)=> if(classesListview.selection.indices.size==1) {
				 val selIx=classesListview.selection.indices.head
				 val newType=MainWindow.shortClassList (selIx)._1
				 if(!theClass.superClasses .contains(newType)){
					 theClass.superClasses=theClass.superClasses:+newType
					 updateInheritedFields()
				 }
			}
			case ButtonClicked(`removeSuperClassBut`)=>if(superClassesListview.selection.indices.size==1)  {
				val selIx=superClassesListview.selection.indices.head
				val remType=theClass.superClasses(selIx)
				theClass.superClasses=theClass.superClasses.filterNot(_ ==remType)
				updateInheritedFields()
			}
		}
	}
	
	val formatStringPan = new BoxPanel(Orientation.Vertical) {
		val overTopBorder: TitledBorder = BorderFactory.createTitledBorder("Format Strings")
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP)
		border=overTopBorder
		contents ++=formatStringLines		
	}
	
	
	
	private def makeHeaderComp(table:Table):Component= new Component  {
			override lazy val peer: JTableHeader =table.peer.getTableHeader
		} 	
		
	
	val fieldPan = new BorderPanel {
		val overTopBorder: TitledBorder = BorderFactory.createTitledBorder("Fields")
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP)
		border=overTopBorder
		val headerComp: Component = makeHeaderComp(ownFieldTable)
		add(new UnWheelingScroller(){
			viewportView= new BoxPanel(Orientation.Vertical) {				
				contents+=headerComp+=inheritedFieldTable+=ownFieldTable				
			}			
		},BorderPanel.Position.Center)		
		inheritedFieldTable.background=Color.lightGray
		preferredSize=new Dimension(800,180)
	}
	
	
	val propFieldPan = new BoxPanel(Orientation.Horizontal) {
		val overTopBorder: TitledBorder = BorderFactory.createTitledBorder("PropertyFields")
		overTopBorder.setTitlePosition(TitledBorder.ABOVE_TOP)
		border=overTopBorder
		contents+=new UnWheelingScroller(){			
			val header: Component = makeHeaderComp(inheritedPropTable)
			viewportView= new BoxPanel(Orientation.Vertical) {
				contents+=header+=inheritedPropTable+=ownPropTable								
			}			
			preferredSize=new Dimension(320,100)
		}
		contents+=Swing.HStrut(10)+=new UnWheelingScroller(){			
			viewportView= childDefTable			
			preferredSize=new Dimension(310,100)
		}
		listenTo(inheritedPropTable.selection)
		listenTo(ownPropTable.selection)
		reactions+= {
					case TableRowsSelected(table,range,live)=> if (!live){						
						var row=table.peer.getSelectedRow
						if(row> -1){							
							val mod=table.model.asInstanceOf[PropFieldTableModel]
							//System.out.println("select row:"+row+" size:"+mod.propFieldList .size)
							//row=row-inheritedPropMod.getRowCount
							if(row<mod.propFieldList.size){
								val propField=mod.getPropField(row)
								val ed=childDefTable.peer.getCellEditor()
								if (ed != null)  ed.stopCellEditing()
								childDefMod.setValues(propField.createChildDefs.asInstanceOf[Seq[ServerCCD]],mod,row)
							}
							else childDefMod.setValues(Seq.empty,null,0)
							if (mod==ownPropMod){
								inheritedPropTable.peer.getSelectionModel.clearSelection()
								childDefTable.background=childDefColor
							}
							else {
								ownPropTable.peer.getSelectionModel.clearSelection()
								childDefTable.background=Color.lightGray
							}
						}
					}
				}
	}
	
	val actionPan=new BoxPanel(Orientation.Horizontal) {
		border=BorderFactory.createEmptyBorder(10,5,10,5)
		val checkBut=new Button("Check out")
		checkBut.tooltip="Updates ClassInfo and prints out XML"
		contents+=saveBut+=checkBut+=formEditBut+=autoCreateChildBut+=addFieldBut+=addPropFieldBut+=Swing.HGlue
		listenTo(saveBut,checkBut,formEditBut,autoCreateChildBut,addFieldBut,addPropFieldBut)
		reactions+= {
			case ButtonClicked(`saveBut`) => safeClass()
			case ButtonClicked(`checkBut`) =>
				updateClassInfo()
				System.out.println(theClass.saveToXML().mkString("\n"))
			case ButtonClicked(`formEditBut`)=> showFormDesigner()
			case ButtonClicked(`autoCreateChildBut`)=> showAutoCreateDialog()
			case ButtonClicked(`addFieldBut`)=> showAddFieldDialog()
			case ButtonClicked(`addPropFieldBut`) => showAddPropFieldDialog()
		}
	}
	
	def updateClassInfo(): Unit ={
	  disableModels.foreach(_.update(theClass))
	  if(inheritedFieldMod.isDirty||ownFieldMod.isDirty)
	    theClass.ownFieldSettings=inheritedFieldMod.getFieldSettings++ownFieldMod.getFieldSettings
	}
	
	def safeClass(): Unit = {
		updateClassInfo()
		//if(isCreating) {
		SessionManager.scl .classList=SessionManager.scl.classList+(theClass.id -> theClass)
		MainWindow.generateDataList()
		//}			
		scala.xml.XML.save(FSPaths.configDir+"types.xml",SessionManager.scl.saveToXML(),"UTF-8",true,null)
	}
	
	def addField(fieldName:String,fieldType:DataType.Value): Unit = {
	  updateClassInfo()
	  theClass.ownFields=theClass.ownFields:+ new FieldDefinition(fieldName,fieldType)
	  SessionManager.scl .classList=SessionManager.scl.classList+(theClass.id -> theClass)
		scala.xml.XML.save(FSPaths.configDir + "types.xml", SessionManager.scl.saveToXML(), "UTF-8", true, null)
	  println("ClassInfo stored")
	  StorageManager.addField(theClass,theClass.fields.size)
	  println("Field added to all existing Instances, starting Reorg")
	  TransactionManager.doReorgDB((ix,name)=>System.out.println("Reorg "+name))
	  System.exit(0)
	}
	
	def showFormDesigner(): Unit = {
		formEditDialog.setLocationRelativeTo(formatStringPan)
		formEditDialog.title="Form-Designer for "+theClass.name
		formEditDialog.showDialog(theClass,theClass.formBox )		
	}
	
	def showAutoCreateDialog(): Unit = {
		autoCreateDialog.setLocationRelativeTo(formatStringPan)
		autoCreateDialog.title="Define auto create entries for "+theClass.name
		autoCreateDialog.showDialog(theClass)
	}
	
	def showAddFieldDialog(): Unit = {
	  addFieldDialog.setLocationRelativeTo(formatStringPan)
		addFieldDialog.title="Add field to "+theClass.name
		addFieldDialog.showDialog(theClass, typeVect.asScala)
	}
	
	def showAddPropFieldDialog(): Unit = {
	  if(JOptionPane.showConfirmDialog(peer, "Do you really want to add an Property Field Array Element in the first position?")== JOptionPane.OK_OPTION) {
	    StorageManager.addPropertyField(theClass)
	    println("Starting reorg, then quit ...")
	    TransactionManager.doReorgDB((ix,name)=>System.out.println("Reorg "+name))
	    System.exit(0)
	  }
	}
	
	val generalPan = new BoxPanel(Orientation.Vertical) {
		val belowTopBorder: TitledBorder = BorderFactory.createTitledBorder("General Info")
		belowTopBorder.setTitlePosition(TitledBorder.BELOW_TOP)
		border=belowTopBorder
		contents+=basicsPan+=superClassesPan+=Swing.VStrut(10)+=moduleLine+=instanceEditorLine+=importDescriptorLine
	}
	
	def checkID (comp:Component):Boolean = {
		var n:Int= -1
		try { n=comp.asInstanceOf[TextField].text.toInt } catch {
			case NonFatal(e) => util.Log.e(e); return false
			case other:Throwable =>println(other);System.exit(0)
		}
		if (isCreating)! MainWindow.usedIDs.contains(n)
		else true
	} 
	
	val mainScroller=new ScrollPane{
		viewportView= new BoxPanel(Orientation.Vertical){
			override lazy val peer = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {       
				val l = new javax.swing.BoxLayout(this, Orientation.Vertical.id)
				setLayout(l)
				def getPreferredScrollableViewportSize: Dimension=getPreferredSize 
				def getScrollableTracksViewportHeight: Boolean =false
				def getScrollableTracksViewportWidth: Boolean=false
				def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
				def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10  
			}
			contents+=generalPan+=formatStringPan+=fieldPan+=propFieldPan
		}
	}
  
  def setClass(newClass:ServerObjectClass,create:Boolean): Unit = {
  	isCreating=create
  	disableComps.foreach(_.enabled=isCreating)
  	disableModels.foreach(_.isCreating=create)
		theClass=newClass
		nameEdit.text=theClass.name
		idEdit.text =theClass.id.toString
		descriptionEdit.text=theClass.description
		commentEdit.text=theClass.comment
		classesListview.listData=MainWindow.shortClassList		
		formatStringLines.foreach( _.update())
		moduleLine.update()
		instanceEditorLine.update()
		importDescriptorLine.update()
		setInheritedFields()
	} 
  
  def setInheritedFields(): Unit = {
  	superClassesListview.listData=theClass.superClasses.map(id => (id,AllClasses.get.getClassByID(id).name))
    val numOwnFields=theClass.getNumOwnFields
		val fieldSettingsList=theClass.getFieldSettingsList
		val inheritedFields=theClass.fields.view.dropRight(numOwnFields)
		inheritedFieldMod.setValues(inheritedFields,
			fieldSettingsList.dropRight(numOwnFields),0)
		ownFieldMod.setValues(theClass.fields.view.takeRight(numOwnFields),
			fieldSettingsList.takeRight(numOwnFields),inheritedFields.size)
		
		val numOwnPropFields=theClass.getNumOwnPropFields
		inheritedPropMod.setValues(theClass.propFields.view.dropRight(numOwnPropFields))
		ownPropMod.setValues(theClass.propFields.view.takeRight(numOwnPropFields))
		childDefMod.setValues(Seq.empty,null,0)	
  }
  
  def updateInheritedFields(): Unit = {
  	theClass=theClass.makeClone
  	SessionManager.scl.addClass(theClass)
    setInheritedFields()
  }
  
  
  class UnWheelingScroller extends ScrollPane {
		val wheelListeners: Array[MouseWheelListener] =peer.getMouseWheelListeners
			for(l <-wheelListeners) peer.removeMouseWheelListener(l)
			peer.setWheelScrollingEnabled(false)
	} 

  this.add(mainScroller,BorderPanel.Position.Center)
  this.add(actionPan,BorderPanel.Position.South)
  
  class FieldTable(mod:TableModel,cm:TableColumnModel=null) extends Table {
		val stringEditor=new MultilineEditor(peer)	{
			def setEditorValue(value:Object): String = if(value==null) "" else value.toString
		}
		model=mod
		peer.setRowHeight(19)
		peer.setDefaultEditor(classOf[String],stringEditor)		
		peer.setDefaultEditor(classOf[DTWrap],typeEditor)	
		peer.setDefaultEditor(classOf[EnumDefinition],enumEditor)
		autoResizeMode=Table.AutoResizeMode.Off
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None    
    if(cm!=null) {
    	peer.setAutoCreateColumnsFromModel(false)
    	peer.setColumnModel(cm)
    }
    showGrid=true
		gridColor=Color.gray    
	}
  
}  

class ClassNameRenderer extends JLabel with TableCellRenderer {
  	override def invalidate(): Unit = {}
  	override def revalidate(): Unit = {}
  	def getTableCellRendererComponent(table:JTable, a:Object, isSelected: Boolean, focused: Boolean,  row: Int,col:Int):java.awt.Component = {
			val theText = if ((a == null) || (!a.isInstanceOf[java.lang.Integer])) ""
  		else {
  			val aValue=a.asInstanceOf[java.lang.Integer].intValue
			  if(aValue<0) "None" 
			  else if(aValue==0)	"Any"
				else AllClasses.get.getClassByID(aValue).name
  		}
  	  setText(theText)
  	  setToolTipText(theText)
  		this
		}
	}  

class ClassNameEditor(box:JComboBox[String]) extends DefaultCellEditor(box) with ClassListListener {
  	var classList:Seq[Int]=Seq.empty
  	var classNameList: Array[String] =Array[String]()
  	def classListChanged(list:Seq[(Int,String)]): Unit = {
  	   classList=0 +: list.map(_._1)
  	   classNameList="Any" +: list.map(_._2).toArray
  	   box.setModel(new DefaultComboBoxModel(classNameList))
  	 } 	   	  	
  	MainWindow.registerClassListListener(this)
  	
  	override def getTableCellEditorComponent(table: JTable,value: Object,isSelected:Boolean,row:Int,column:Int ): JComboBox[_] = {
  		val editor:JComboBox[_] =super.getTableCellEditorComponent( table, value, isSelected, row, column ).asInstanceOf[JComboBox[_]]
			editor.setSelectedIndex( classList.indexOf(value.asInstanceOf[java.lang.Integer].intValue) )
			editor
  	}
  	override def getCellEditorValue():java.lang.Object =
  	{
  		val ix =getComponent().asInstanceOf[JComboBox[String]].getSelectedIndex()
			//System.out.println("getValue "+ix)
  		if(ix<0) new java.lang.Integer(-1)
  		else classList(ix).asInstanceOf[AnyRef]
  	}
  }


