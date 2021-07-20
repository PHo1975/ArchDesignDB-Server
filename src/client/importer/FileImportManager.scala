package client.importer
import definition.data.{InstanceData, OwnerReference}
import definition.typ.AllClasses

import java.awt.{Point, Toolkit}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.io.File
import javax.swing.SwingWorker
import scala.collection.Map
import scala.collection.mutable._
import scala.swing.Window

class DescriptorHolder(val className:String,id:Int) {
  def createDescriptor: FileImportDescriptor = {
    val d=Class.forName(className).getConstructor().newInstance().asInstanceOf[FileImportDescriptor]
    d.baseID=id
    d
  }
  lazy val descriptor: FileImportDescriptor =   createDescriptor
}

class ProcData(val fileNr:Int,val file:File) 

object FileImportManager {
  lazy val descriptorMap: Map[Int, DescriptorHolder] =AllClasses.get.classList.filter(_._2.importDescriptor.isDefined).map(
      (i)=>(i._1,new DescriptorHolder(i._2.importDescriptor.get,i._1)))

  /** checks, if the given files can be imported
   * @param fileNames list of fileNames
   * @param targetType object type of the target table
   * @param droppedTarget instance of the target object, if known
   * 
   */
  def canImport(fileNames:Seq[File], targetType:Int, droppedTarget:Option[InstanceData], ownerRef:OwnerReference):Boolean = {
    if(descriptorMap.contains(targetType)) {
      //println("canimport "+targetType)
      val descriptor=descriptorMap(targetType).descriptor
      descriptor.canImport(fileNames,droppedTarget,ownerRef)
    }
    else false
  }
  
  def correctScreenPos(wPos:Point,width:Int,height:Int):Point= {
    val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
    new Point(if(wPos.x+width> screenSize.width -20) screenSize.width-width-20 else wPos.x,
        if(wPos.y+height>(screenSize.height-60)) screenSize.height-height-60 else wPos.y)
  }
  
  def showImportDialog(window:Window, wpos:Point, files:Seq[File], targetType:Int, dropTarget:Option[InstanceData], ownerRef:OwnerReference):Boolean= {
    if(descriptorMap.contains(targetType)) {
      val descriptor=descriptorMap(targetType).descriptor
      val settings= descriptor.showImportDialog(window,wpos,files,dropTarget)
      if(settings.isEmpty) false
      else {
        var processDialog: ProcessDialog = null
        val worker: SwingWorker[Boolean, ProcData] = new SwingWorker[Boolean, ProcData] {
          override def doInBackground(): Boolean = {
            val accFiles = files.filter(descriptor.acceptFile)
            for (ix <- accFiles.indices; currFile = accFiles(ix)) {
              if (isCancelled) {
                //println("Cancelled")
                return false
              }
              publish(new ProcData(ix, currFile))
              val baseObject = descriptor.getBaseObject(ownerRef, dropTarget, currFile)
              descriptor.importFile(currFile, baseObject, settings, (i) => {
                setProgress(i)
                //if(isCancelled) println("Callback cancelled "+i)
                isCancelled
              })
            }
            //println("Loop end ")
            true
          }

          override def process(data: java.util.List[ProcData]): Unit = {
            //println("Process: "+data.mkString(" | "))
            processDialog.setCurrFile(data.get(data.size-1))
          }

          override def done(): Unit = {
            //println("Done ")
            processDialog.close()
          }
        }
        def cancelWorker():Unit = {
          worker.cancel(false)
        }

        processDialog = new ProcessDialog(window, cancelWorker _)
        processDialog.showDialog(descriptor.name, wpos, files.size)

        worker.addPropertyChangeListener(new PropertyChangeListener() {
          def propertyChange(e: PropertyChangeEvent): Unit = {
            if ("progress" == e.getPropertyName())
              processDialog.setCurrentFileState(e.getNewValue().asInstanceOf[java.lang.Integer])
          }
        })

        worker.execute()
        //rintln("Executed")
        true
      }
    }
    else false
  }
  
}