/**
 * Author: Peter Started:21.09.2010
 */
package server.storage

import definition.typ.AllClasses

import scala.xml.{Elem, Node}

/** Server-side class list to be set as AllClasses instance
 * 
 */
class ServerClassList (node: scala.xml.Node) extends AllClasses [ServerObjectClass] {
	var classList:Map[Int,ServerObjectClass]=fromXML(node)
  def addClass(cl:ServerObjectClass): Unit = classList += (cl.id -> cl)
	
	def fromXML(node: scala.xml.Node):Map[Int,ServerObjectClass]=  {
  	if(node==null) Map[Int,ServerObjectClass]()
  	else {
  	  val sNode= node //\"ClassList"
  	  (for (ac<- sNode \ "OClass";oc= ServerObjectClass.fromXML(ac))	yield oc.id -> oc).toMap}
  }
	
	def toXML: Elem =  {
  	<ClassList> {for (c<-classList.valuesIterator) yield c.toXML  }  </ClassList>
  }
	
	def saveToXML(): Node =  {
		scala.xml.Utility.trim(
  	<ClassList> {for (c<-classList.valuesIterator) yield c.saveToXML()  }  </ClassList>
		)
  }
	
	
	
}