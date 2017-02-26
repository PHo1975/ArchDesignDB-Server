/**
 * Author: Peter Started:25.11.2010
 */
package client.dataviewer

import javax.swing._
import javax.swing.table._
import javax.swing.text._
import javax.swing.event._

import definition.expression._
import definition.typ.DataType
import java.awt.{BorderLayout, Color, Dimension, Insets, Point}
import java.awt.event._
import java.util.EventObject

import client.comm.KeyStrokeManager

import scala.swing.Swing
/**
 * 
 */

trait MyInputValidator{
  /**
   * @param col:Column that is edited
   * @return if wrong: Offset of wrong character, if correct:None
   */
  def validate(text:String,col:Int):Option[Int]  
  def setEditorComponent(comp:JComponent): Unit
}

abstract class MultilineEditor(theTable:JTable,validator:Option[MyInputValidator]=None,definedSize:Option[Dimension]=None) extends 
	AbstractCellEditor with TableCellEditor {
	val editColor=new Color(255,255,230)
	val document=new PlainDocument
	val maxEditorHeight=300
	var editingColumn= -1
	val minWidth=150
	var ownHide=false

	val textArea=new JTextArea(document)

	//val usedActionKeys=Array(KeyEvent.VK_ENTER,KeyEvent.VK_ESCAPE,KeyEvent.VK_F2,KeyEvent.VK_TAB)
	val usedActionKeyStrokes=Array(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_F2,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_TAB,0),
		KeyStroke.getKeyStroke(KeyEvent.VK_TAB,InputEvent.SHIFT_DOWN_MASK))

	val component:JTextArea = new JTextArea(document) {
    setBackground(editColor)
		override protected def processKeyBinding( ks: KeyStroke,e: KeyEvent, condition:Int , pressed:Boolean):Boolean ={
			//println("Edit process Key Bindings:"+ks+" condition:"+condition)
			if (textArea.hasFocus()) super.processKeyBinding(ks, e, condition, pressed)
			else {
			  if(e.getKeyCode==KeyEvent.VK_DELETE) e.consume()
				else Swing.onEDT{
						if(e.getKeyChar!=KeyEvent.CHAR_UNDEFINED) {
							if(e.getKeyCode==KeyEvent.VK_TAB/*||e.getKeyCode==KeyEvent.VK_ENTER*/) e.consume()
							else textArea.setText(e.getKeyChar.toString)
						}
						else super.processKeyBinding(ks, e, condition, pressed)
          textArea.requestFocus()
        }
				true
			}
		}
    addAncestorListener(new AncestorListener(){
    	def ancestorAdded(e:AncestorEvent ) = {	}
    	def ancestorMoved(e:AncestorEvent ) = {
    		val newPoint:Point=e.getComponent.getLocation
    		//System.out.println("Moved oldPos:"+oldPos+" newPos:"+newPoint)
    	  setPopupLocation(newPoint)
    	}
    	def ancestorRemoved(e:AncestorEvent) = {
    		if(popupContent.isVisible) stopCellEditing()
    	}
    })
    addFocusListener(new FocusListener(){
    	 def focusGained(e:FocusEvent)= textArea.requestFocus()
    	 def focusLost(e:FocusEvent)= {}
    })

	}

	val popupContent=new PopupContent
	var rootPane:JRootPane=null
	var layeredPane:JLayeredPane=null

	textArea.addKeyListener(new KeyAdapter () {
		override def keyPressed(e:KeyEvent) = {
			e.getKeyCode match {
				case KeyEvent.VK_ENTER =>
					if(e.isControlDown) {
						textArea.insert("\n",textArea.getCaretPosition())
            e.consume()
					}
					else {
						val action=textArea.getActionMap.get("selectNextRowCell")
						//System.out.println("action :"+action)
						SwingUtilities.notifyAction(action,KeyStroke.getKeyStroke(e.getKeyCode,e.getModifiers),e,theTable, e.getModifiers)
						e.consume()
					}
				case _ =>
			}
		}
	})

	textArea.addFocusListener(new FocusAdapter() {
		 override def focusLost(e:FocusEvent)= if(!ownHide&& e.getOppositeComponent!=component){
			 stopCellEditing()
		 }
	})



	document.addDocumentListener(new DocumentListener {
		def insertUpdate(e:DocumentEvent)= update()
    def removeUpdate(e:DocumentEvent )= update()
    def changedUpdate(e:DocumentEvent ) = update()
    def update()= Swing.onEDT { updateEditorHeight()}
	})

	usedActionKeyStrokes.foreach(registerAction)


	override def shouldSelectCell( e:EventObject  )=true

	override def cancelCellEditing()= {
		ownHide=true
		super.cancelCellEditing()
		hidePopup()
	}

	override def stopCellEditing():Boolean = {
		//System.out.println("stop ")
	  for(v<-validator;ix<- v.validate(textArea.getText, editingColumn)) {
	    textArea.setCaretPosition(ix)
	    //val tix=if(ix>=textArea.getText.length-1) textArea.getText.length-2 else ix
	    textArea.setSelectionStart(ix)
	    textArea.setSelectionEnd(ix+1)
	    return false
	  }
		val ret=super.stopCellEditing
		hidePopup()
		ret
	}

	def hidePopup()= {
	  KeyStrokeManager.enableBindings()
		ownHide=true

    Swing.onEDT{
			popupContent.setVisible(false)
			val root=SwingUtilities.getRoot(theTable)
			if(root!=null) root.repaint()
			theTable.requestFocus()
		}
	}

	def setPopupLocation(pos:Point) = if(layeredPane!=null) {
		val newPos=SwingUtilities.convertPoint(theTable,pos,layeredPane)
		popupContent.setLocation(newPos)
	}

	def getTableCellEditorComponent(table:JTable,value: Object, isSelected:Boolean,rowIndex: Int, vColIndex:Int) ={
	  editingColumn=vColIndex
	  KeyStrokeManager.disableBindings()
	  val bounds=table.getCellRect(rowIndex,vColIndex,true)

	  popupContent.setSize(definedSize match {
	    case Some(ds)=>ds
	    case None => if(bounds.width<minWidth) new Dimension(minWidth,bounds.height) else bounds.getSize
	  }) // calculate the text height
	  val oldPos=bounds.getLocation
	  component.setText(setEditorValue(value)) // changes the size also
	  if(layeredPane==null) {
	  	rootPane=table.getRootPane
	  	layeredPane=rootPane.getLayeredPane
	  	layeredPane.add(popupContent, JLayeredPane.POPUP_LAYER )
	  }
	  setPopupLocation(oldPos)
	  popupContent.setVisible(true)
	  popupContent.revalidate()
	  textArea.requestFocus()
		ownHide=false
	  component
	}

	def setEditorValue(value:Object):String

	def updateEditorHeight()= {
		val size:Dimension=popupContent.getSize
		var wishHeight=math.max(textArea.getPreferredSize.height+4,size.height)
		//print("Updateheight size:"+size+" WishHeight "+wishHeight)
	  if(wishHeight>maxEditorHeight) wishHeight=maxEditorHeight
	  if(wishHeight!=size.height) {
	  	size.height=wishHeight
	    popupContent.setSize(definedSize match {
	    case Some(ds)=>ds
	    case None => size
	  })
	    textArea.revalidate()
	  }
	}

	def getCellEditorValue():Object ={
		component.getText()
	}

	override def removeCellEditorListener(l:CellEditorListener) = {
		if(popupContent.isVisible) hidePopup()
		super.removeCellEditorListener(l)
	}

	override def isCellEditable(evt:EventObject ):Boolean =	evt match {
      case event: MouseEvent =>
        event.getClickCount() >= 2
      case _ => true
    }

	private def registerAction(keyStroke:KeyStroke) = {		
		val actionName=theTable.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
			get(keyStroke)
		if(actionName!=null) {
		  textArea.getInputMap(JComponent.WHEN_FOCUSED).put(keyStroke, actionName)
		  val action=theTable.getActionMap().get(actionName)
		  //System.out.println("instEdit install action :"+actionName +" => "+action)
		  if(action!=null) textArea.getActionMap().put(actionName,new ActionWrapper(action))
		}	
	}
	
	class ActionWrapper(oldAction:Action) extends AbstractAction {	  
		def actionPerformed(e:ActionEvent) = {
			//System.out.println("wrapper actionPerformed "+e+" oldaction:"+oldAction)
			val newEvent=new ActionEvent(theTable,e.getID,e.getActionCommand,e.getWhen,e.getModifiers)
			oldAction.actionPerformed(newEvent)
		}
			
	}
	
	
	class PopupContent extends JScrollPane{
		override def isOptimizedDrawingEnabled=false
		setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)			
		setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.gray),BorderFactory.createLineBorder(getBackground)))
		textArea.setBorder(BorderFactory.createEmptyBorder(0,3,0,3))
		textArea.setLineWrap(true)
		textArea.setWrapStyleWord(true)
		setViewportView(textArea)
		textArea.setBackground(editColor)
	}
}
