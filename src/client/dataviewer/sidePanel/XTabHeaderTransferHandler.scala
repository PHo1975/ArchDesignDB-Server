/**
 * Author: Peter Started:20.03.2011
 */
package client.dataviewer.sidePanel

import client.dataviewer.InstanceSelection
import definition.data._
import javax.swing._

import scala.util.control.NonFatal

/**
 * 
 */
class XTabHeaderTransferHandler(controller:XTabSidePanelController) extends TransferHandler {

	override def getSourceActions(c:JComponent):Int = {  	    
  	   TransferHandler.NONE 
    }
	
	override def canImport(support: TransferHandler.TransferSupport):Boolean = {
  	val loc=support.getDropLocation
  	if( support.isDataFlavorSupported(InstanceSelection.flavor)) {
  			 val data = support.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]  			 
  			 if(data.selection.length ==0) false
  			 else true
  	} else false
  }
	
	
	override def importData(info:TransferHandler.TransferSupport):Boolean  =
    if (!info.isDrop()) false
    else {
      var data: InstanceSelection = null
      try {
        data = info.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
        val fromOwner = new OwnerReference(data.propField.toByte, data.parentRefs.head)
        for (dat <- data.selection)
          controller.tmodel.addColumn(dat, fromOwner)
        true

      } catch {
        case NonFatal(e) => System.out.println(e); false
        case other:Throwable =>println(other);System.exit(0);false
      }
    }

}