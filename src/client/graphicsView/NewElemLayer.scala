/**
 * Author: Peter Started:31.10.2010
 */
package client.graphicsView
import definition.data.Reference
/**
 * 
 */
class NewElemLayer(ncontroller:GraphViewController) extends Layer	(ncontroller,new Reference(0,0),true,true){  
  
  val myList=collection.mutable.ArrayBuffer[GraphElem]()
  _elemList=myList
  
  def addTempElement(elem:GraphElem) ={  	
  	_elemList= myList += elem
  	controller.graphElemAdded(this,elem)  	
  }  
  
  override def shutDown()= {
  	myList.clear()
  }
  
}