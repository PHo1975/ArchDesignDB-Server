/**
 * Author: Peter Started:14.04.2011
 */
package management.databrowser

import client.dialog.ComboBoxEditor
import client.dialog.form._
import definition.typ.AbstractObjectClass
import definition.typ.form.AbstractFormElement
import server.storage.ServerObjectClass

import java.awt.Color
import javax.swing.table.TableCellEditor
import javax.swing.{BorderFactory, JComboBox, JPopupMenu}
import scala.jdk.CollectionConverters._
import scala.swing._
import scala.swing.event.{ButtonClicked, MousePressed}


/**
 * 
 */
class FormDesignerDialog(w:Window) extends Dialog(w) with DesignerListener {
	var theClass:AbstractObjectClass= _
	var isCanceled:Boolean=false
	var currentValue:Option[FormBox]=None
	var selectedElement:Option[FormElement]=None
	val selectColor: Color =Color.blue.brighter.brighter
	
  preferredSize=new Dimension(750,500)
	val previewPanel=new BorderPanel{
		background=Color.gray
		def addComp(comp:Component,v:Constraints): Unit =super.add(comp,v)
	}
	val saveButton=new Button("Save")
	val cancelButton=new Button("Cancel")
	val deleteButton=new Button("Delete Comp")
	
	lazy val readFormContext: FormCreateContext =ServerObjectClass.readFormContext

	
	val insertButton=new Button("Insert Comp"){
		val menu=new PopupMenu(this)
		listenTo(menu.buttons: _*)
		reactions += {
			case b:ButtonClicked =>
				for(se:FormElement<-selectedElement;cu<-currentValue) {
          val ret=cu.findFormBoxFor(se)
          if(ret._2> -1&& b.source .text!="Insert Comp"){
            val newElem=createFromMenu(b.source .text)
            addElementToFormBox(newElem,ret._1,ret._2)
          }
        }
		}
		
		
	}
	val typeLabel=new Label(" ")
	
	val propTableMod=new FormElementTableModel(readFormContext)
	
	var fieldComboEditor:ComboBoxEditor=_
	
	val propTable=new Table{
		model=propTableMod
		selection.elementMode=Table.ElementMode.Row 
		propTableMod.listener =Some(FormDesignerDialog.this)
		
		override def editor(row: Int, column: Int): TableCellEditor = {
			if(column==1&&propTableMod.isFieldField(row)&&fieldComboEditor!=null)	fieldComboEditor
      else super.editor(row, column)
		}
	}
	
	val propPanel=new BorderPanel{
		add(typeLabel,BorderPanel.Position.North)
		add(new ScrollPane {
			viewportView=propTable
		},BorderPanel.Position.Center)
		preferredSize= new Dimension(200,60)
	}
	
	val mainPanel: BorderPanel =new BorderPanel(){
		add(previewPanel,BorderPanel.Position.Center )
		add( propPanel,BorderPanel.Position.West )
		add(new BoxPanel(Orientation.Horizontal){
			background=Color.white
			border=BorderFactory.createEmptyBorder(8,8,8,8)
			contents+=saveButton+=cancelButton+=Swing.VStrut(30)+=insertButton+=deleteButton
		},BorderPanel.Position.South)
		
		listenTo(saveButton,cancelButton,deleteButton,insertButton)
		reactions += {
			case ButtonClicked(`cancelButton`)=>
				isCanceled=true
				unregisterComponents()
				close()
			case ButtonClicked(`saveButton`)=>
				isCanceled=false
				TypeDefPanel.theClass=TypeDefPanel.theClass.setFormBox(currentValue)
				//println(TypeDefPanel.theClass.formBox.map(_.toXML))
				unregisterComponents()
				FormDesignerDialog.this.visible=false
			case ButtonClicked(`deleteButton`)=>
				deleteElement()
			case ButtonClicked(`insertButton`)=>
				if(selectedElement.isDefined)
				  insertButton.menu.show()
		}
	}	
	
	reactions+={		
		case m:MousePressed =>
			m.source match {
        case f:FormElement =>
        selectComponent(f)
        case _ =>
      }
	}
	
	contents=mainPanel
	
	def selectComponent(f:FormElement): Unit = {
		for(s <-selectedElement)
				s.deselect()
		f.select(selectColor)
		selectedElement=Some(f)
		propTableMod.setFormElement(selectedElement)
		typeLabel.text=f.getClass.getSimpleName
		//println("Focused "+f)
	}
	
	
	def unregisterComponents(): Unit = {
		typeLabel.text=""
		for(s <-selectedElement)
				s.deselect()
		for(c<-currentValue) 
			c match {
				case fb:FormBox => fb.foreach(e =>deafTo(e.asInstanceOf[Component]))
				case other => println("Wrong root type "+other)
			}	
	}
	
	
	def showDialog(aClass:AbstractObjectClass,noldValue:Option[FormBox]): Unit = {
		theClass=aClass
		currentValue=noldValue
		fieldComboEditor=new ComboBoxEditor(new JComboBox(new java.util.Vector[(String,Int)](
			(for(i <-theClass.fields.indices; field=theClass.fields(i) )
					yield(field.name,i)).asJava
				 )))
		
		for(c<-currentValue) {
			c match {
				case fb:FormBox => fb.foreach{case fe:FormElement=>initComponent(fe)}
				case other => println("Wrong root type "+other)
			}			
			//println(c.toXML)
		}
		propTableMod.setFormElement(None)
		selectedElement=None
		visible=true
		updatePreview()
	}
	
	def updatePreview(): Unit = {
		if(currentValue.isDefined) previewPanel.addComp (currentValue.get,BorderPanel.Position.Center) 
		else {
			previewPanel.addComp (emptyFormPanel,BorderPanel.Position.Center)
		}
		mainPanel.revalidate()
		mainPanel.repaint()
	}
	
	def initComponent(el:AbstractFormElement): Unit = {
		val asComponent=el.asInstanceOf[Component]
		asComponent.opaque=true
		asComponent.focusable=true
		listenTo(asComponent.mouse.clicks)
		el match {
			case fb:FormBox => if(fb.specialComp.isEmpty) fb.setSpecialComp(new AddButton(fb.orient,fb))
			case te:AbstractFormTextField=>
				te.editComponent.editable=false
				te.editComponent.text=theClass.fields(te.fieldNr).name
			case _ =>
		}
	}

	
	def createFormBox(orient:Orientation.Value):FormBox = {		
		val ret=new FormBox(0,-1,0,-1,orient,Seq.empty)
		//but.forBox=ret
		ret.border=BorderFactory.createLineBorder(Color.gray)		
		ret.background=Color.green
		ret
	}
	
	def addElementToFormBox(newElem:FormElement,formBox:FormBox,pos:Int):Unit = {
		initComponent(newElem)			
		val nbox=formBox.addElement(newElem,pos)				
		changeElement(formBox,nbox)		
		selectComponent(newElem)
	}
	
	def deleteElement():Unit= for (s<-selectedElement){
		deafTo(s.asInstanceOf[Component])		
		currentValue match {
			case Some(rbox:FormBox) =>
				if(rbox==s)currentValue=None
				else currentValue=Some(rbox.deleteElement(s)._1)
			case a => println("wrong root type "+a)
		}		
		updatePreview()
	}
	
	def changeElement(oldEl:FormElement,newEl:FormElement):Unit = {
		currentValue match {
			case Some(rbox:FormBox) => currentValue=Some(rbox.updateElement(oldEl, newEl)._1)
			case a => println("wrong root type "+a)
		}				
		//println("Curr:" + currentValue)
		updatePreview()
	}
	
	class AddButton(orient:Orientation.Value,var _formBox:FormBox) extends Button(if(orient==Orientation.Horizontal)">" else "\\/")
	with SpecialComponent {
		val menu=new PopupMenu(this)
		listenTo(menu.buttons :+ this: _*)

		reactions+= {
			case b: ButtonClicked =>
				if (b.source == this) menu.show()
				else {
					val elName = b.source.asInstanceOf[MenuItem].peer.getText
					if (elName == "Select Panel") selectComponent(_formBox)
					else {
						val newElement = createFromMenu(elName)
						addElementToFormBox(newElement, _formBox, -1)
					}
				}
		}

		override def toString(): String = "AddButton "+orient
	}
	
	private def createFromMenu(elName:String) = 
	 elName match {						
							case "Horizontal Box" => createFormBox(Orientation.Horizontal)
							case "Vertical Box" => createFormBox(Orientation.Vertical)
							case otherName=>FormElement.getElementByName(otherName,readFormContext ) 
						}		
	
	class PopupMenu(where:Component) extends Component
	{
		override lazy val peer : JPopupMenu = new JPopupMenu
		val buttons: List[MenuItem] =(FormElement.validFormElements.filter(_ != "FormBox")++List("Horizontal Box","Vertical Box","Select Panel")).map(new MenuItem(_))
		
		def add(item:MenuItem) : Unit = { peer.add(item.peer) }
		def show(): Unit = peer.show(where.peer, 3, 3)
		buttons.foreach(add)
		
		//FormElement.validFormElements.map(a=> add(new MenuItem(a)))
	}
	
	//class MyMenuItem(val title:String) extends Button(title)

	
	object emptyFormPanel extends BoxPanel(Orientation.Horizontal ){
		val horBut=new Button("Horizontal Box")
		val vertBut=new Button("Vertical Box")
		contents+=horBut+=Swing.HStrut(10)+=vertBut+=Swing.HGlue
		listenTo(horBut,vertBut)
		reactions+= {
			case ButtonClicked(`horBut`)=>
				val box=createFormBox(Orientation.Horizontal)
				initComponent(box)
				currentValue=Some(box)
				updatePreview()
			case ButtonClicked(`vertBut`)=>
				val box=createFormBox(Orientation.Vertical)
				initComponent(box)
				currentValue=Some(box)
				updatePreview()
		}
	}
}