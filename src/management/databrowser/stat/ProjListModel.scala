package management.databrowser.stat

import javax.swing.AbstractListModel
import scala.collection.mutable.ArrayBuffer
import server.storage.StorageManager
import server.print.PrintEngine

class ProjListModel extends AbstractListModel[String] {
  val projectList=ArrayBuffer[(Int,String)]()
  
  def getSize()=projectList.size
  
  def getElementAt(pos:Int):String=projectList(pos)._2
  
  def load()={
    projectList.clear 
    StorageManager.ixHandler(PrintEngine.projectType).foreachInstance ( ref =>{
      val data=StorageManager.getInstanceData(ref)
      projectList+= ((ref.instance,data.fieldValue.head.toString))
    })
    fireContentsChanged(this, 0, projectList.size)
  }
}