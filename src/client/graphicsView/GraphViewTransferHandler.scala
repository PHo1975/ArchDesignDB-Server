package client.graphicsView


import java.awt.datatransfer.{DataFlavor, Transferable, UnsupportedFlavorException}
import java.io._
import java.util.Date

import client.dataviewer.InstanceSelection
import client.dialog.DragDropListener
import client.spreadsheet.SpreadSheetTransferable
import definition.data.Reference
import javax.swing.TransferHandler
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

class GraphViewDDListener(controller:GraphViewController) extends DragDropListener[GraphElemTransferable] {  
  def sourceActions:Int={
    //println("get source ")
    TransferHandler.COPY_OR_MOVE
  }
  def DDfactory():GraphElemTransferable={
    controller.selectModel.selectionList
    controller.layerModel.getActiveLayer match {
      case Some(al)=> controller.selectModel.getSelectionFromLayer(al) match {
        case Some(sel)=>controller.selectModel.getDragDropSelection
        case None=> null
      }
      case None => null
    }
    
  }
  
  def canImport(action:Int,data:Transferable,pos:TransferHandler.DropLocation):Boolean={
    data match {
      case insData:InstanceSelection if insData.selection.exists(a => Layer.allowedDisplayListTypes.contains(a.typ)) &&
        !insData.selection.exists(controller.layerModel.containsRef) => true
      case _=> controller.layerModel.getActiveLayer match {
        case Some(activeLayer)=> data match {
          case d:GraphElemTransferable=>
            ! d.layerList.exists(_.layerRef == activeLayer.ref)
          case spread:SpreadSheetTransferable=> spread.numCols==4
          case o=> false
        }
        case _ => false
      }
    }

        
  }
  
  def importData(action:Int, data:Transferable,pos:TransferHandler.DropLocation):Boolean= {
    data match {
      case instData: InstanceSelection => controller.importLayers(instData)
      case _ => controller.layerModel.getActiveLayer match {
        case Some(activeLayer) => data match {
          case gdata: GraphElemTransferable =>
            if (!gdata.layerList.exists(_.layerRef == activeLayer.ref)) {
              controller.importDDToLayer(gdata, action, activeLayer)
              true
            } else false
          case spread: SpreadSheetTransferable => if (spread.numCols == 4) {
            controller.importSpreadSheetData(spread)
            true
          } else false
          case _ => false
        }
        case _ => false
      }
    }
  }
  
  lazy val flavors=Array(InstanceSelection.flavor,InstanceSelection.graphElemFlavor,SpreadSheetTransferable.flavor)
  
}



@SerialVersionUID(42752002L) class LayerTransferable extends Serializable{
  var layerRef:Reference=_
  var graphElems:Array[Reference]=Array[Reference]()
  var name:String=_
  var scale:Double= _
  @transient private  var nativeElems:Iterable[GraphElem]=_
  def this(nlayRef:Reference,nname:String,nelems:Iterable[GraphElem],nscale:Double)= {
    this()
    layerRef=nlayRef;graphElems=nelems.map(_.ref).toArray 
    nativeElems=nelems
    name=nname
    scale=nscale
  }

  def createDXFData(handleHolder:HandleHolder,buffer:ArrayBuffer[String]):Unit= {
    for(l<-nativeElems) {
      val outputString=l.getDXFString(handleHolder,name)
      if(outputString.length>0) {
        buffer.append(outputString)
      }
    }
  }

  
  
}

@SerialVersionUID(42752001L) 
class GraphElemTransferable extends Transferable with Serializable{
  var layerList:Array[LayerTransferable]=Array[LayerTransferable]()
  var fileList:java.util.ArrayList[File]=null
  def this(nLayerList:Array[LayerTransferable])= {
    this()
    layerList=nLayerList    
  }
  
  def getTransferDataFlavors: Array[DataFlavor] = InstanceSelection.graphElemFlavorArray
  
  def isDataFlavorSupported(flavor:DataFlavor):Boolean = flavor match {
    case InstanceSelection.graphElemFlavor=>true
    case DataFlavor.javaFileListFlavor=>true
    case _=>false
  }
  
  def getTransferData(flavor:DataFlavor ) = flavor match {
    case InstanceSelection.graphElemFlavor=> this
    case DataFlavor.javaFileListFlavor=> if(fileList==null) createFilesList else fileList
    case _=>throw new UnsupportedFlavorException(flavor)
	}
  
  def createFilesList:java.util.ArrayList[File]={
    fileList=new java.util.ArrayList[File]()
    val tempDir=new File(System.getProperty("java.io.tmpdir"))
    fileList.add(createFile(tempDir))
    fileList
  }

  def createFile(dir:File): File =  try {
    val wishFileName=dir.getAbsolutePath+File.separatorChar+ layerList.head.name.replace(" ","_")+"_"+util.JavaUtils.shortDateFormat.format(new Date())+".dxf"
    val wishFile=new File(wishFileName)
    println("WishFile "+wishFile)
    if(wishFile.exists)wishFile.delete()
    val outBuffer=new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(wishFile),"windows-1252")))

    appendStream("header1.dat",outBuffer)

    val handleHolder=new HandleHolder
    val buffer=new ArrayBuffer[String]()
    for(l<-layerList) {
      l.createDXFData(handleHolder,buffer)
    }
    println("lines out "+buffer.size )
    val lineStyles=handleHolder.createLineStyleTable(layerList.head.scale)
    if(lineStyles.length>0)
      outBuffer.println(lineStyles)
    appendStream("header2.dat",outBuffer)
    println("header 2")
    for(line ← buffer) outBuffer.println(line)
    appendStream("footer.dat",outBuffer)
    println("ready out")
    outBuffer.close()
    wishFile
  } catch {
    case NonFatal(er) ⇒ Log.e("create DXF File ",er);null
  }


  def appendStream(fileName:String,writer:PrintWriter): Unit = {
    val reader=new BufferedReader(new InputStreamReader(this.getClass.getResourceAsStream(fileName)))
    while (reader.ready())
      writer.println(reader.readLine)
    reader.close()
  }

  
}