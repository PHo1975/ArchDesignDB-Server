package client.dialog

import java.awt.datatransfer.{DataFlavor, Transferable}

import client.dataviewer.InstanceSelection
import javax.swing.{JComponent, TransferHandler}


trait DragDropListener[A <: Transferable]{
  def sourceActions:Int
  def DDfactory():A
  def canImport(action:Int,data:Transferable,pos:TransferHandler.DropLocation):Boolean
  def importData(action:Int, data:Transferable,pos:TransferHandler.DropLocation):Boolean
  def exportDone(data: A,action:Int):Unit = {}
  def flavors:Array[DataFlavor]
}




class CommonTransferHandler[A <: Transferable](listener:DragDropListener[A]) extends TransferHandler {  
 
  
  override def exportDone(source:JComponent,data: Transferable,action:Int):Unit = {
    //println("Export done data:"+data+" action:"+action+" source:"+source)
    var run=true
    if(data!=null) for(fl<-listener.flavors;if run && data.isDataFlavorSupported(fl)) {
       listener.exportDone(data.getTransferData(fl).asInstanceOf[A],action)
       run=false
    }
      
  }
  
  override def getSourceActions(c:JComponent):Int =
      listener.sourceActions

  
  override def createTransferable(c:JComponent ):Transferable =
    listener.DDfactory() 

  
  override def canImport(support: TransferHandler.TransferSupport):Boolean = {
    for(fl<-listener.flavors)
    	if( support.isDataFlavorSupported(fl)) {
  			 val data = support.getTransferable.getTransferData(fl).asInstanceOf[Transferable]  			 
  			 return listener.canImport(support.getDropAction,data,support.getDropLocation) 			 
    	} 
    false
  }
  
  override def importData(info:TransferHandler.TransferSupport):Boolean  =
    //println("import "+info.getTransferable+" isdrop:"+info.isDrop()+" move:"+(info.getDropAction==TransferHandler.MOVE))
  	if (!info.isDrop)	false
    else {
      val transferable = info.getTransferable
      for (fl <- listener.flavors)
        if (transferable.isDataFlavorSupported(fl)) {
          val data = transferable.getTransferData(fl).asInstanceOf[Transferable]
          val ret = listener.importData(info.getDropAction, data, info.getDropLocation)
          if (info.getDropAction == TransferHandler.MOVE) data match {
            case instData: InstanceSelection => if (instData.onMoveHandlerID != 0)
              InstanceSelection.onMoveHandlerMap(instData.onMoveHandlerID).moved(listener, instData)
            case _ =>
          }
          return ret
        }
      false
    }

}