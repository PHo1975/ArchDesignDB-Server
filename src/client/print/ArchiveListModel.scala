/**
 * Author: Peter Started:25.04.2011
 */
package client.print

import client.comm.ClientQueryManager
import definition.data.Reference
import javax.swing.AbstractListModel

/**
 * 
 */
class ArchiveListModel extends AbstractListModel[ArchivePageable] {
  
	var archiveList:Seq[ArchivePageable]= Seq.empty
	
	
	def load(outDefRef:Reference): Unit = {
	  val data=ClientQueryManager.queryInstance(outDefRef,1)
	  //println("Load Archive "+data.mkString("|"))
		archiveList=data.map(new ArchivePageable(_))
		//println("la "+outDefRef+" list:"+archiveList.mkString("\n"))
		fireContentsChanged(this,0,archiveList.size-1)
	}
	
	def clear(): Unit ={
		archiveList=Seq.empty
		fireIntervalRemoved(this,0,0)
	}
	
	
	
	
	
	
  def getSize: Int = { archiveList.size }

  def getElementAt(index: Int): ArchivePageable = { archiveList(index) }

}