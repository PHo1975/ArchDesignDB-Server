/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import java.awt.Color
import java.awt.event.InputEvent

import client.comm._
import client.dialog.{CreateActionList, CreateActionMenuButton, CreateMenuButton}
import definition.comm._
import definition.data._
import definition.typ._
import javax.swing.border._
import javax.swing.{BorderFactory, JComponent, JPopupMenu}

import scala.collection.mutable
import scala.swing._
import scala.swing.event._

class TitlePopupMenu(title:String) extends Component {		
	override lazy val peer : JPopupMenu = new JPopupMenu(title)	
	def add(item:MenuItem,separator:Boolean) : Unit = { peer.add(item.peer); if (separator) peer.addSeparator() }

	def show(comp: JComponent): Unit = peer.show(comp, 3, 3)

	def show(comp: JComponent, x: Int, y: Int): Unit = peer.show(comp, x, y)

	def show(): Unit = peer.setVisible(true)
	
	focusable=false

	def addButtons(cList: Seq[Component]): Unit = {
    //peer.setLayout(new GridLayout(cList.size,1))
    //peer.setPopupSize(new Dimension(200,cList.size*33))
	  cList.indices.foreach(i=>{
      val but=cList(i)
      val sep= i!=cList.size-1
      but  match {
    		case e:CreateMenuButton=> add(e,sep)
    		case e:CreateActionMenuButton=> add(e,sep)
    		case e:MenuItem=> add(e,sep)
    		case _ =>
  	  }
    })
	}
}


/** manages all data changes of a property field of a instance
 * the PropertyModels are specialized on a certain allowed class
 * 
 * @param mainController the main controller managing the whole dataView *
 */
class PropertyModel(val propIx:Int,val mainController:DataViewController) {	
	var ownerRef:OwnerReference = _
	var loaded=false
	var subscriptionID: Int = -1
	protected var allowedClass:Int= _	
	var listLock=new Object
	var isFirstPropField:Boolean=false
	var tableModMap: mutable.LinkedHashMap[Int, TypeTableModel] = scala.collection.mutable.LinkedHashMap[Int, TypeTableModel]()
	lazy val vGlue=new ClickComp(this)
	val titleLabel: Label = ViewConstants.label("Prop")
	titleLabel.font=ViewConstants.smallFont
	titleLabel.foreground=Color.DARK_GRAY
	val tableArea=new BoxPanel (scala.swing.Orientation.Vertical ) {
		contents += vGlue
	}
	
	val panel=new BorderPanel {
		add(titleLabel,BorderPanel.Position.North)
		add(tableArea,BorderPanel.Position.Center)
		border= BorderFactory.createEmptyBorder(2,2,2,2)			
	}
	var selectRef:Option[Reference]=None
	
	/** loads data into the propertyModel
	 * 
	 * @param nallowedClass what classes are allowed to be in this model
	 * @param fieldToLoad what property field to load
	 * @param fieldName name of the property field
	 * @param selRef reference of an instance that should optionally be selected
	 * @Param nfirstPropField is this the first property field
	 * @param onlyPropField is this the only property field
	 * @param singleField is this a single property field
	 */
	def load(nallowedClass:Int,fieldToLoad:Byte,fieldName:String,selRef:Option[Reference],nfirstPropField:Boolean,
	    onlyPropField:Boolean,singleField:Boolean,doneListener:()=>Unit):Unit =	  
		listLock.synchronized{
			if(loaded) shutDown()
			vGlue.update(mainController.mainClass.asInstanceOf[ClientObjectClass],fieldToLoad)   
			//println("Field +"+fieldToLoad+" "+mainController.mainClass.propFields(fieldToLoad).createChildDefs.mkString(","))
			selectRef=selRef
			allowedClass=nallowedClass
			//if((ownerRef!=null) && (ownerRef.ownerField==fieldToLoad) && (ownerRef.ownerRef==mainController.ref)) println("propfield reloaded "+ownerRef)
			ownerRef=new OwnerReference(fieldToLoad,mainController.ref)		
			isFirstPropField=nfirstPropField
			titleLabel.text=" "+fieldName+(if(allowedClass==0) "" else  " erlaubt:"+ AllClasses.get.getClassByID(allowedClass).name)
			titleLabel.visible= !onlyPropField
			titleLabel.horizontalAlignment=Alignment.Left
			
			if(subscriptionID<0)
				subscriptionID=ClientQueryManager.createSubscription(mainController.ref,fieldToLoad)((notType,data)=>
						listLock.synchronized {
							Swing.onEDT {
								notType match {
									case NotificationType.sendData | NotificationType.updateUndo =>
										if (data.isEmpty) {
											if (isFirstPropField) vGlue.requestFocusInWindow()
											vGlue.revalidate()
											vGlue.visible = true
										}
										else {
											val hasSubClasses = AllClasses.get.hasSubClasses(allowedClass)
											vGlue.checkVisibility(singleField)
											val grouped: Map[Int, Seq[InstanceData]] = data.groupBy(_.ref.typ)
											var firstTable: Table = null
											var ix = 0
											for ((i, data) <- grouped.iterator) {
												val mod = tableModMap.getOrElseUpdate(i,
													createTableModel(ix, i, singleField, allowedClass == 0 || hasSubClasses))
												ix += 1
												mod.setDataList(data, selectRef, notType == NotificationType.updateUndo, nfirstPropField && firstTable == null)
												if (firstTable == null) firstTable = mod.table
											}
										}
										if (notType == NotificationType.sendData) doneListener()
									case NotificationType.childAdded =>
										val typ = data.head.ref.typ
										val mod = tableModMap.getOrElseUpdate(typ,
											createTableModel(tableModMap.size, typ, singleField, allowedClass == 0 || AllClasses.get.hasSubClasses(allowedClass), true)
										)
										mod.addInstance(data.head)
										mainController.updateHeight()
										vGlue.checkVisibility(singleField)
										tableArea.revalidate()
									case NotificationType.fieldChanged =>
										val typ = data.head.ref.typ
										if (tableModMap.contains(typ))
											tableModMap(typ).changeInstance(data.head)

									case NotificationType.instanceRemoved =>
										val typ = data.head.ref.typ
										if (tableModMap.contains(typ))
											tableModMap(typ).removeInstance(data.head.ref)

									case NotificationType.parentNotExistend => doneListener()
								}
								selectRef = None
							}
						}
				)			
			loaded=true
		}


	def getPropFieldDefinition: PropertyFieldDefinition = mainController.mainClass.propFields(ownerRef.ownerField)
	
	def isEmpty:Boolean= tableModMap.valuesIterator.forall(_.isEmpty)


	def createTableModel(ix: Int, typ: Int, singleField: Boolean, showClassLabel: Boolean, focusTable: Boolean = false): TypeTableModel = {
		val newMod= new TypeTableModel(ix,typ,this,singleField,showClassLabel)	
		if(tableArea.contents.isEmpty)
			tableArea.contents +=newMod.scroller
			else {				
				tableArea.contents(tableArea.contents.size-1)=Swing.VStrut(5)
				tableArea.contents+=	newMod.scroller
			}
		tableArea.contents+=vGlue	
		mainController.updateHeight()
		tableArea.revalidate()
		if (focusTable) newMod.enterFromTop()
		newMod
	}

	def enterFromTop(): Unit = if (tableModMap.nonEmpty) tableModMap.values.head.enterFromTop()

	def enterFromBottom(): Unit = if (tableModMap.nonEmpty) tableModMap.values.last.enterFromBottom()

	def getOwnerRef: OwnerReference = listLock.synchronized(new OwnerReference(ownerRef.ownerField, mainController.ref))

	def focusGained(table: Option[Table]): Unit = mainController.containerFocused(table, ownerRef.ownerField)

	def shutDown(): Unit = listLock.synchronized {
		ClientQueryManager.removeSubscription(subscriptionID)
		subscriptionID= -1
		tableArea.contents.clear()
		tableArea.contents+=vGlue
		tableModMap.valuesIterator.foreach(_.shutDown())
		tableModMap.clear()
		loaded=false
		vGlue.shutDown()
	}

	def deselect(selectedType: Int): Unit = listLock.synchronized {
		for(m <-tableModMap.valuesIterator;if m.typ != selectedType) m.deselect()
	}

	def tableExitsToUp(tableIx: Int): Unit = if (tableIx > 0) tableModMap.values.toSeq(tableIx - 1).enterFromBottom()
	  else mainController.propFieldExitsToUp(propIx)

	def tableExitsToDown(tableIx: Int): Unit = if (tableIx < tableModMap.size - 1) tableModMap.values.toSeq(tableIx + 1).enterFromTop()
	  else mainController.propFieldExitsToDown(propIx)	
	

	
	class ClickComp(propMod:PropertyModel) extends BoxPanel(Orientation.Horizontal) {
		val popup=new TitlePopupMenu("Erzeugen:")
		val standBorder: Border = BorderFactory.createEmptyBorder
		//createLineBorder(Color.lightGray)
		val selectBorder: Border = BorderFactory.createLineBorder(Color.blue, 2)
		focusable=true
		val theClass: PropertyModel = propMod
		peer.setTransferHandler(new PropAreaTransferHandler(propMod))		
		val addBut=new Button("Tabelle erzeugen...")
		addBut.peer.putClientProperty("JComponent.sizeVariant", "small")
		addBut.font = ViewConstants.tableFont
    addBut.peer.updateUI()
    addBut.focusable=false
		maximumSize=new Dimension(Short.MaxValue,30)			
		contents+=addBut
		protected var buttonList: Seq[CreateMenuButton] = Seq.empty

    listenTo(mouse.clicks,this,addBut,keys)
		reactions+= {			
			case e:MouseReleased =>
				//System.out.println("mouseclick "+peer.isFocusOwner+" "+size)
				requestFocus()
				focusGained(None)
			case _: FocusGained => border = selectBorder; repaint()
			case _: FocusLost => border = standBorder; repaint()
			case ButtonClicked(`addBut`) =>
				CreateActionList.listenToButtons(buttonList)
				requestFocusInWindow()
				if (buttonList.size == 1) {
          focusGained(None)
          buttonList.head.strokeHit()
        } else {
          showCreatePopup()
        }
			case e:KeyPressed => e.key match {
			  case Key.Up if (e.peer.getModifiersEx()&InputEvent.CTRL_DOWN_MASK)>0 => propMod.mainController.viewBox.goUp()
			  case _=>
			}
		}

		private def showCreatePopup(): Unit = {
	    requestFocus()
			focusGained(None)
			Swing.onEDT{
	      if(buttonList.size>1) {
					popup.peer.removeAll()
					//println("ButtonsList:"+NewButtonsList.buttonList.map(_.text).mkString(","))
					popup.addButtons(buttonList)
					popup.show(addBut.peer)
	      }
		  }				
		}


		def update(mainClass: ClientObjectClass, fieldToLoad: Int): Unit = {
		  visible= mainClass.propFields(fieldToLoad).createChildDefs.nonEmpty
		  if(visible){
        CreateActionList.deafToButtons(buttonList)
		    buttonList=mainClass.simpleCreateMenuItems(fieldToLoad).filter(_.ccd.editorName==propMod.mainController.containerName)
		    addBut.text=if (buttonList.size==1) buttonList.head.commandName+"-Tabelle erzeugen" else "Tabelle erzeugen..."
		    
		  }
		}

		def shutDown(): Unit = {
			CreateActionList.deafToButtons(buttonList)
			buttonList = Seq.empty
		}

		def checkVisibility(singleField: Boolean): Unit = visible = !singleField && (buttonList.size > 1)
	}
}

