/**
 * Author: Peter Started:31.10.2010
 */
package client.graphicsView
import definition.data.Reference

import scala.collection.mutable.ArrayBuffer
/**
 * 
 */
class NewElemLayer(ncontroller: GraphViewController) extends Layer(ncontroller, new Reference(0, 0), Array.empty[String], true, true) {

  val myList: ArrayBuffer[GraphElem] = collection.mutable.ArrayBuffer[GraphElem]()
  _elemList=myList

  def addTempElement(elem: GraphElem): Unit = {
  	_elemList= myList += elem
  	controller.graphElemAdded(this,elem)  	
  }

  override def shutDown(): Unit = {
  	myList.clear()
  }
  
}