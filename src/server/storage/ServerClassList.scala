/**
 * Author: Peter Started:21.09.2010
 */
package server.storage

import definition.typ.{AllClasses, BlockClass}

import scala.xml.{Elem, Node}

/** Server-side class list to be set as AllClasses instance
 * 
 */
class ServerClassList (node: scala.xml.Node) extends AllClasses [ServerObjectClass] {
	var classList:Map[Int,ServerObjectClass]=classListfromXML(node)
	var blockClassList:Map[Int,BlockClass]=blockClassListfromXML(node)
  def addClass(cl:ServerObjectClass): Unit = classList += (cl.id -> cl)
	def addBlockClass(bl:BlockClass):Unit= blockClassList+=(bl.id->bl )
	
	def classListfromXML(node: scala.xml.Node):Map[Int,ServerObjectClass]=
  	if(node==null) Map[Int,ServerObjectClass]()
  	else {
  	  val sNode= node \"ClassList"
  	  (for (ac<- sNode \ "OClass";oc= ServerObjectClass.fromXML(ac))	yield oc.id -> oc).toMap}

	def blockClassListfromXML(node: scala.xml.Node):Map[Int,BlockClass]=
		if(node==null) Map[Int,BlockClass]()
		else {
			val sNode= node \"BlockClassList"
			(for (ac<- sNode \ "BClass";oc= BlockClass.fromXML(ac))	yield oc.id -> oc).toMap}


	def toXML: Elem =  {
		<all>
  	<ClassList> {for (c<-classList.valuesIterator) yield c.toXML  }  </ClassList>
		<BlockClassList>{for(b<-blockClassList.valuesIterator) yield b.toXML} </BlockClassList>
		</all>
  }
	
	def saveToXML(): Node =  {
		scala.xml.Utility.trim(
			<all>
				<ClassList> {for (c<-classList.valuesIterator) yield c.toXML  }  </ClassList>
				<BlockClassList>{for(b<-blockClassList.valuesIterator) yield b.toXML} </BlockClassList>
			</all>
		)
  }
	
	
	
}