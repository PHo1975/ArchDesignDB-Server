package client.graphicsView


import client.dialog.DragDropListener
import client.dataviewer.InstanceSelection
import java.awt.Point
import javax.swing.TransferHandler
import java.awt.datatransfer.Transferable
import java.awt.datatransfer.DataFlavor
import client.spreadsheet.SpreadSheetTransferable
import definition.data.Reference
import java.awt.datatransfer.UnsupportedFlavorException
import java.io._
import client.print.PrintQuestionHandler
import java.util.Date

import definition.expression.DateConstant

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
  var layerRef:Reference=null
  var graphElems:Array[Reference]=Array[Reference]()
  var name:String=null
  @transient private  var nativeElems:Iterable[GraphElem]=null
  def this(nlayRef:Reference,nname:String,nelems:Iterable[GraphElem])= {
    this()
    layerRef=nlayRef;graphElems=nelems.map(_.ref).toArray 
    nativeElems=nelems
    name=nname
  }
  
  def createFile(dir:File): File = {
    val wishFileName=dir.getAbsolutePath()+File.separatorChar+ name.replace(" ","_")+"_"+util.JavaUtils.shortDateFormat.format(new Date())+".dxf"
    val wishFile=new File(wishFileName)
    if(wishFile.exists)wishFile.delete()    
    val outBuffer=new PrintWriter(new BufferedWriter(new FileWriter(wishFile)))

    appendStream("header.dat",outBuffer)
    var handleNr=45
    val layerName="0"
    for(l<-nativeElems) {
      val outputString=l.getDXFString(handleNr.toHexString,layerName)
      if(outputString.length>0) {
        outBuffer.println(outputString)
        handleNr+=1
      }      
    }
    appendStream("footer.dat",outBuffer)
    outBuffer.close()
    wishFile
  }
  

  def appendStream(fileName:String,writer:PrintWriter): Unit = {
    val reader=new BufferedReader(new InputStreamReader(this.getClass.getResourceAsStream(fileName)))
    while (reader.ready())
      writer.println(reader.readLine)
    reader.close()
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
  
  def getTransferDataFlavors()= InstanceSelection.graphElemFlavorArray
  
  def isDataFlavorSupported(flavor:DataFlavor):Boolean = flavor match {
    case InstanceSelection.graphElemFlavor=>true
    case DataFlavor.javaFileListFlavor=>true
    case _=>false
  }
  
  def getTransferData(flavor:DataFlavor ) = flavor match {
    case InstanceSelection.graphElemFlavor=> this
    case DataFlavor.javaFileListFlavor=> if(fileList==null) createFilesList else fileList
    case _=>throw new UnsupportedFlavorException(flavor);
	}
  
  def createFilesList:java.util.ArrayList[File]={
    fileList=new java.util.ArrayList[File]()
    val tempDir=new File(System.getProperty("java.io.tmpdir"))
    for(lay<-layerList) {
      fileList.add(lay.createFile(tempDir))
    }
    
    fileList
  }
  
}