package management.databrowser.stat

import javax.swing.AbstractListModel
import server.print.PrintEngine
import server.storage.StorageManager

import scala.collection.mutable.ArrayBuffer

class ProjListModel extends AbstractListModel[String] {
  var projectList: ArrayBuffer[(Int, String)] =ArrayBuffer[(Int,String)]()
  
  def getSize: Int =projectList.size
  
  def getElementAt(pos:Int):String=projectList(pos)._2
  
  def load(): Unit ={
    projectList.clear 
    StorageManager.ixHandler(PrintEngine.projectType).foreachInstance ( ref =>{
      val data=StorageManager.getInstanceData(ref)
      projectList+= ((ref.instance,data.fieldValue.head.toString))
    })
    projectList=projectList.sortWith((a,b)=>a._2<b._2)
    fireContentsChanged(this, 0, projectList.size)
  }
}