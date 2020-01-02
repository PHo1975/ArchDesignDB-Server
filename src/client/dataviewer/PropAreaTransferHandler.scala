/**
 * Author: Peter Started:14.11.2010
 */
package client.dataviewer

import client.comm.ClientQueryManager
import definition.data._
import definition.typ.AllClasses
import javax.swing._

import scala.util.control.NonFatal
/**
 * 
 */
class PropAreaTransferHandler(propMod:PropertyModel) extends TransferHandler{
	def propField=propMod.getPropFieldDefinition
	
	override def getSourceActions(c:JComponent):Int = {  	    
  	   TransferHandler.NONE 
    }
	
  override def canImport(support: TransferHandler.TransferSupport):Boolean = {
  	//val loc=support.getDropLocation
  	if( support.isDataFlavorSupported(InstanceSelection.flavor)) {
  			 val data = support.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]  			 
  			 if(data.selection.length ==0 ||
  			 (propField.single&&( !propMod.isEmpty|| data.selection.length !=1)) ||
  			 (propField.allowedClass>0 && !AllClasses.get.getClassByID(data.selection.head.typ).inheritsFrom(  propField.allowedClass)))  false
         else support.getDropAction match {
  				 case TransferHandler.LINK => true
  				 case TransferHandler.COPY | TransferHandler.MOVE => true
  				 case _ => false
  			 }
  	} else false
  }
  
  override def importData(info:TransferHandler.TransferSupport):Boolean  =
    if (!info.isDrop) false
    else {
      val action = info.getDropAction
      var data: InstanceSelection = null
      try {
        data = info.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
      } catch {
        case NonFatal(e) => System.out.println(e); return false
        case other:Throwable =>println(other);System.exit(0)
      }
      action match {
        case TransferHandler.COPY =>
          //System.out.println("Copied")
          ClientQueryManager.copyInstances(data.selection,
            new OwnerReference(data.propField.toByte, data.parentRefs.head),
            propMod.ownerRef, -1)
        case TransferHandler.MOVE =>
          //System.out.println("Moved")
          ClientQueryManager.moveInstances(data.selection,
            new OwnerReference(data.propField.toByte, data.parentRefs.head),
            propMod.ownerRef, -1)
        case TransferHandler.LINK =>
          //System.out.println("Linked")
          ClientQueryManager.secondUseInstances(data.selection,
            new OwnerReference(data.propField.toByte, data.parentRefs.head),
            propMod.ownerRef, -1)
        case _ =>
      }
      true
    }

}