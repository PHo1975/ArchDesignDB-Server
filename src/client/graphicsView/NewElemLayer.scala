/**
 * Author: Peter Started:31.10.2010
 */
package client.graphicsView
import definition.data.Reference

/**
 * 
 */
class NewElemLayer(ncontroller: GraphViewController) extends Layer(ncontroller, new Reference(0, 0), Array.empty[String], true, true) {

  def addTempElement(elem: GraphElem): Unit = {
  	_elemList += elem
  	controller.graphElemAdded(this,elem)  	
  }

  def clear():Unit={
    _elemList.clear()
  }

  override def shutDown(): Unit =if(_elemList.nonEmpty){
    _elemList.clear()
    controller.layerChanged(this,updateZoom = false)
  }
  
}