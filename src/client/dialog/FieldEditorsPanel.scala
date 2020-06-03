/**
 * Author: Peter Started:10.10.2010
 */
package client.dialog

import java.awt.Dimension

import client.ui.ViewConstants
import definition.data.Referencable
import definition.typ.{AllClasses, SelectGroup}
import javax.swing.BorderFactory
import util.Log

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.BoxPanel
import scala.util.control.NonFatal


/** panel that manages Field Editors for selected Instances
 * 
 */
class FieldEditorsPanel extends BoxPanel(scala.swing.Orientation.Vertical) with SelectListener {
  opaque = false
  lazy val maxSize = new Dimension(ViewConstants.sidePanelWidth, Short.MaxValue)

	protected var groupList:Iterable[SelectGroup[_<:Referencable]]= _
	protected var commonTyp:Int = -1
  protected var currentEditors: ArrayBuffer[FieldEditor] = collection.mutable.ArrayBuffer[FieldEditor]()
	border=BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(8,0,6,0),
	    BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(8,0,6,0),"Felder:"))
	xLayoutAlignment=0.5d

  def selectionChanged[T <: Referencable](sender: SelectSender, groups: Iterable[SelectGroup[T]], alsoSelected: Iterable[T] = Nil): Unit = {
		groupList=groups.asInstanceOf[Iterable[SelectGroup[_<:Referencable]]]
		val newCommonTyp=AllClasses.get.getCommonClassForGroups(groupList)
		visible= contents.nonEmpty
		//System.out.println("new common Typ:"+newCommonTyp)
		//println("FieldEditorsPanel sel changed Sender:"+sender+" groups:"+groups.mkString("| "))
		if(newCommonTyp!=commonTyp) { // type was changed
			contents.clear()
			currentEditors.clear()
			commonTyp=newCommonTyp
			
			if(newCommonTyp> 0) {
				//System.out.println("fieldEditorPanel sel changed "+groups+" "+sender+" commonTyp:"+newCommonTyp)
				val editorNames=AllClasses.get.getClassByID(newCommonTyp).fieldEditors				
				for(aName <-editorNames) try {
					EditorFactory.getEditor(aName) match {
					  case Some(editor)=>
							contents+=editor.getPanel
							currentEditors+=editor
						case _=>
					}
					//System.out.println("editor:"+editor)
										
				}	catch {
				  case e:Throwable => util.Log.e("Error when trying to load editor :"+aName+"\n"+e.toString+"\n",e)
				}				
			}
      if (contents.nonEmpty) maximumSize = new Dimension(ViewConstants.sidePanelWidth, preferredSize.height + 60)
      else maximumSize = maxSize
			visible= contents.nonEmpty
			revalidate()
			repaint()
		}
		
	  notifyEditors()
	}
	
	def notifyEditors():Unit = //println("Notify Editors "+groupList.map(gr=> "gr:"+gr.parent+" ch"+gr.children.mkString(",")))
		for(editor <-currentEditors)  editor.setData(groupList)
}




object EditorFactory {
  val editorCache: mutable.HashMap[String, FieldEditor] = collection.mutable.HashMap[String, FieldEditor]()
  val inplaceEditorCache: mutable.HashMap[String, InplaceFieldEditor] = collection.mutable.HashMap[String, InplaceFieldEditor]()
	
	private def updateFieldEditor(name:String,newEditor:FieldEditor): Unit = {
		newEditor.getPanel.xLayoutAlignment=0.5d
		editorCache(name)=newEditor		
	}
	
	def getEditor(name:String):Option[FieldEditor] = {	  
		if(editorCache.contains(name)) Some(editorCache(name))
		else if(inplaceEditorCache.contains(name)) None
		else {		  
			Class.forName(name).getConstructor().newInstance().asInstanceOf[AbstractFieldEditor] match {
			  case newEditor:FieldEditor =>
					updateFieldEditor(name,newEditor)
					Some(newEditor)
				case inplace:InplaceFieldEditor =>
					inplaceEditorCache(name)=inplace
					None
				case o=> throw new IllegalArgumentException("Wrong editor Type "+ o)
			}			
		}
	}
	
	def getInplaceEditor(name:String):Option[InplaceFieldEditor] = {	  
	  if(editorCache.contains(name)) None		
		else {
			try
			Class.forName(name).getConstructor().newInstance().asInstanceOf[AbstractFieldEditor] match {
			  case newEditor:FieldEditor =>
					updateFieldEditor(name,newEditor)
					None
				case inplace:InplaceFieldEditor =>
					inplaceEditorCache.getOrElseUpdate(name, inplace)
					Some(inplace)
				case o=> Log.e("Wrong editor Type "+ o);None
			}
			catch {
				case NonFatal(e)=> Log.e("start FieldEditor",e);None
			}
		}
	}
}