/**
 * Author: Peter Started:06.11.2010
 */
package client.layout

import definition.comm.PropertyGroup

import scala.collection.mutable._
import scala.swing.Component

/** abstract definition of a content component that fits in a viewbox
 * 
 */
trait ViewboxContent extends Component {
	//self : Component =>
	//type CompType=ViewboxContent with Component
	
	/** openy a new empty box
	 * 
	 */
	def open(readyListener:()=>Unit,sourceBoxSelectedItems:AnyRef): Unit
	
	def close(): Unit
	
	def storeSettings(pgroup:PropertyGroup): Unit
	
	/** restores the content on restart
	 * @param pgroup the Propertygroup where the content can get the settings from
	 * @param readyListener listener that should be called after restoring is done
	 */
	def restoreSettings(pgroup:PropertyGroup,readyListener: ()=>Unit): Unit
	
	def setViewbox(box:Viewbox): Unit
	def typeID:String

	def selectedItems:AnyRef
}


case class ViewboxContentType(id:Int, name:String, buttonText:String,tooltipText:String, factory:()=>ViewboxContent)

object ViewboxContentTypeList {
  val list: ArrayBuffer[ViewboxContentType] = ArrayBuffer[ViewboxContentType]()

  def addType(newType: ViewboxContentType): Unit = {
		list +=newType
	}

  def getType(typeName: String): Option[ViewboxContentType] =
	  list.find(_.name==typeName)


  def size: Int = list.size
}