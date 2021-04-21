/**
 * Author: Peter Started:13.11.2010
 */
package client.dataviewer

import client.comm.ClientQueryManager
import client.dataviewer.export.{ExportModule, GAEB83Module, RechXTabExportModule}
import client.dialog.DragDropListener
import client.graphicsView.GraphElemTransferable
import client.importer.{FileImportManager, OldDBConvertSettings}
import client.ui.ClientApp
import definition.data._
import definition.expression.FieldReference
import definition.typ.AllClasses
import util.ExportContainer

import java.awt.datatransfer._
import java.io.{File, Serializable}
import javax.swing._
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**  Created by Peter Holzer
 * 
 */
class TableTransferHandler(tableMod:TypeTableModel) extends TransferHandler {	
  
   override def getSourceActions(c:JComponent):Int =  TransferHandler.COPY_OR_MOVE +TransferHandler.LINK
  	   
   override def createTransferable(c:JComponent ):InstanceSelection =      
  	  new InstanceSelection(Array(tableMod.getParentRef),tableMod.getPropField,tableMod.selectedInstances,
  		 tableMod.selectedInstances .map (_.toString()).toArray,tableMod.table.peer.getSelectedColumn,
        tableMod.table.peer.getSelectedRows, tableMod.propMod.mainController.viewBox.pathController.model.dataList.map(_.toString).toArray)
   

   
   override def canImport(support: TransferHandler.TransferSupport):Boolean = {
  	 //System.out.println("tth can Import loc: "+support.getDropLocation+ " da:" + support.getDropAction+" sda: "+support.getSourceDropActions+" "+support.isDrop)
     support.getDropLocation match {
       case tabLoc: JTable.DropLocation =>
         if (support.isDataFlavorSupported(InstanceSelection.flavor)) {
           val data = try {
             support.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
           }
           catch {
             case e: java.awt.dnd.InvalidDnDOperationException => return e.getMessage == "No drop current"
             case NonFatal(e) => return false
             case other:Throwable =>println(other);System.exit(0);null
           }
           if (data.selection.length == 0 ||
            (tableMod.propMod.getPropFieldDefinition.single && (!tableMod.propMod.isEmpty || data.selection.length != 1)) ||
            data.dragRows.isEmpty) false
           else support.getDropAction match {
             case TransferHandler.LINK =>
               if (tabLoc.getRow >= tableMod.dataList.size) {
                 (data.selection.head.typ == tableMod.typ) && !(data.parentRefs.head equals tableMod.getParentRef)
               } else {
                 if (data.selection.length != 1) false
                 else {
                   val thisInst = tableMod.dataList(tabLoc.getRow)
                   if (data.selection.head equals thisInst.ref) data.dragColumn != tabLoc.getColumn
                   else true
                 }
               }
             case TransferHandler.MOVE | TransferHandler.COPY =>
               if (data.selection.head.typ != tableMod.typ) {
                 TypeConverter.convertRules.contains((data.selection.head.typ,tableMod.typ))
               }
               else if ((data.parentRefs.head equals tableMod.getParentRef) &&
                 data.propField == tableMod.propMod.ownerRef.ownerField &&
                 data.dragRows.contains(tabLoc.getRow)) {
                 //System.out.println("same same")
                 false // same parent
               }
               else true
             case _ => false
           }
           //System.out.println(" loc:"+tabLoc.getRow+ " data.parent:"+data.parentRefs .first + " "+tableMod.getParentRef)
         } else // no Instanceselection flavor:
         if (support.getDataFlavors.exists(_.isFlavorJavaFileListType())) {
           val flavor = support.getDataFlavors.filter(_.isFlavorJavaFileListType()).head
           var fileList: mutable.Seq[File] = null
           try {
             fileList = support.getTransferable.getTransferData(flavor).asInstanceOf[java.util.List[File]].asScala
           }
           catch {
             case e: java.awt.dnd.InvalidDnDOperationException => return e.getMessage == "No drop current"
             case NonFatal(e) => return false
             case other:Throwable =>println(other);System.exit(0)
           }
           val droppedObj = if (tabLoc.getRow >= tableMod.dataList.size) None else Some(tableMod.dataList(tabLoc.getRow))
           FileImportManager.canImport(fileList, tableMod.typ, droppedObj, tableMod.propMod.ownerRef)
         } else
          if (support.isDataFlavorSupported(InstanceSelection.oldDBFlavor)) {
           var data: Array[ExportContainer] = null
           try {
             data = support.getTransferable.getTransferData(InstanceSelection.oldDBFlavor).asInstanceOf[Array[ExportContainer]]
           }
           catch {
             case e: java.awt.dnd.InvalidDnDOperationException => return e.getMessage == "No drop current"
             case NonFatal(e) => println(e); return false
             case other:Throwable =>println(other);System.exit(0);null
           }
           if (data == null) false
           else {
             val parentClass = AllClasses.get.getClassByID(tableMod.getParentRef.typ)
             data.exists(d => OldDBConvertSettings.canImport(d.classID, parentClass, tableMod.getPropField, tableMod.isEmpty))
           }
         } else false
       case _ => false
     }
   }
   
   
   
  override def importData(info:TransferHandler.TransferSupport):Boolean  =
    if (!info.isDrop) false else {
      val tabLoc=info.getDropLocation.asInstanceOf[JTable.DropLocation]
      val action= info.getDropAction
      val row=tabLoc.getRow
      var data:InstanceSelection=null

      if(info.isDataFlavorSupported(InstanceSelection.flavor)){
        try {
          data= info.getTransferable.getTransferData(InstanceSelection.flavor).asInstanceOf[InstanceSelection]
        } catch {
          case NonFatal(e) => util.Log.e("importData action:"+action+" row:"+row,e);return false
          case other:Throwable =>println(other);System.exit(0);null
        }
        if( (tableMod.propMod.getPropFieldDefinition.single && ( !tableMod.propMod.isEmpty || data.selection.length !=1 ) ) ||
        data.dragRows.isEmpty ||
          ( (action==TransferHandler.COPY || action==TransferHandler.MOVE ) &&
            ( ((data.selection.head.typ != tableMod.typ)&&
              ! TypeConverter.convertRules.contains((data.selection.head.typ,tableMod.typ))) ||
           (data.parentRefs.head equals tableMod.getParentRef) &&
            data.propField==tableMod.propMod.ownerRef.ownerField&&
            data.dragRows.contains(tabLoc.getRow) )  // same parent
            )
        ) false
        else action match {
          case TransferHandler.COPY =>
            if(TypeConverter.convertRules.contains((data.selection.head.typ,tableMod.typ))) {
              val rule=TypeConverter.convertRules((data.selection.head.typ,tableMod.typ))
              val toOwner=tableMod.propMod.ownerRef
              ClientQueryManager.convertInstances(data.selection.toSeq,toOwner,tableMod.typ,rule.fieldList)
              true
            } else {
              tableMod.wishSelection = row until row + data.selection.length
              ClientQueryManager.copyInstances(data.selection, data.ownerRef, tableMod.propMod.ownerRef, row)
              true
            }
          case TransferHandler.MOVE =>
            //System.out.println("Moved")
            if(!TypeConverter.convertRules.contains((data.selection.head.typ,tableMod.typ))){
            val fromOwner=data.ownerRef
            val toOwner=tableMod.propMod.ownerRef
            val wishRow=if(fromOwner==toOwner && row>=tableMod.dataList.size) tableMod.dataList.size-1 else row
            tableMod.wishSelection= wishRow until (wishRow + data.selection.length)
            ClientQueryManager.moveInstances(data.selection,fromOwner,toOwner,row)
            true
            } else false
          case TransferHandler.LINK =>
            if( tabLoc.getRow>=tableMod.dataList.size) {
              if((data.selection.head.typ==tableMod.typ)&& !(data.parentRefs.head equals tableMod.getParentRef)){
                ClientQueryManager.secondUseInstances(data.selection, data.ownerRef,tableMod.propMod.ownerRef,-1)
                true
              }else false
            }
            else if(data.selection.length !=1) false
            else {
              val thatRef = data.selection.head //.toReference
              val thisInst = tableMod.dataList(tabLoc.getRow)
              val remType = if (thisInst.ref.typ == thatRef.typ) None else Some(thatRef.typ)
              val remInst = if (remType.isEmpty && thisInst.ref.instance == thatRef.instance) None else Some(thatRef.instance)
              if (remType.isEmpty && remInst.isEmpty && data.dragColumn == tabLoc.getColumn) {
                util.Log.e(" cant link to same field " + thatRef + " " + tabLoc.getColumn)
                false
              } else {
                ClientQueryManager.writeInstanceField(thisInst.ref, colToMod(tabLoc.getColumn),
                  new FieldReference(remType, remInst, colToMod(data.dragColumn)))
                true
              }
            }
          case TransferHandler.NONE => false
        }
      }
      else if(info.getDataFlavors.exists(_.isFlavorJavaFileListType())) {
           val flavor=info.getDataFlavors.filter(_.isFlavorJavaFileListType()).head
           val fileList=info.getTransferable.getTransferData(flavor).asInstanceOf[java.util.List[File]]
           val droppedObj=tableMod.dataList.lift(tabLoc.getRow)
           FileImportManager.showImportDialog(ClientApp.top,tableMod.table.peer.getLocationOnScreen,fileList.asScala,tableMod.typ,droppedObj,tableMod.propMod.ownerRef)
         }
      else if (info.isDataFlavorSupported(InstanceSelection.oldDBFlavor)){
           val data= info.getTransferable.getTransferData(InstanceSelection.oldDBFlavor).asInstanceOf[Array[ExportContainer]]
           if(data==null) false
           else {
             //println("children "+data.size+" "+data.mkString("\n\n"))
             ClientQueryManager.runInPool {
               val parentRef = tableMod.propMod.ownerRef
               // new OwnerReference(tableMod.getPropField,tableMod.getParentRef)
               val parentClass = AllClasses.get.getClassByID(tableMod.getParentRef.typ)
               for (d <- data)
                 OldDBConvertSettings.importData(d, parentRef, parentClass)
             }
             true
           }
         }
      else false
    }

  def colToMod(col:Int):Byte =	(tableMod.table.peer.convertColumnIndexToModel(col)-1).toByte

}



@SerialVersionUID(4276L) 
class InstanceSelection extends Transferable with Serializable {
	//private val serialVersionUID = 4275L
	
	def this(parents:Array[Referencable], npropField:Int, selChildren:Seq[Referencable], texts:Array[String],
           ncolumn: Int, nrow: Array[Int], npathFromRoot: Array[String]) = {
		this()
		parentRefs=parents.map(_.ref/*.serialized*/)
		selection=selChildren.map(_.ref/*.serialized*/).toArray
		textArray=texts
		dragColumn=ncolumn
		propField=npropField
		dragRows=nrow
    pathFromRoot = npathFromRoot
		//System.out.println(" create Sel :"+dragRows.mkString(","))
	}
	
	var dragColumn:Int=0
	var dragRows:Array[Int]=Array()
	var parentRefs:Array[Reference]=Array()
	var propField:Int=0
	var selection:Array[Reference]=Array()
	var textArray:Array[String]=Array()
  var onMoveHandlerID:Int=0
  var fileList:java.util.ArrayList[File]=_
  var pathFromRoot: Array[String] = Array()
  
	lazy val ownerRef=new OwnerReference(propField.toByte,parentRefs.head)
	
	def getTransferData(flavor:DataFlavor ):AnyRef = {
		//System.out.println("get Transfer Data "+flavor)
		if(flavor.equals(InstanceSelection.flavor)) this
		else if (flavor.equals(InstanceSelection.flavors(1))) 
	     toString	 
	 else if(flavor.equals(DataFlavor.javaFileListFlavor)){
      if(selection!=null && selection.length==1&& InstanceSelection.exportModules.contains(selection(0).ref.typ)) {
        if(fileList==null)
          fileList=InstanceSelection.exportModules(selection(0).ref.typ).export(this)
        fileList
      }
      else throw new UnsupportedFlavorException(flavor)
    } else throw new UnsupportedFlavorException(flavor)
  }
	
	
	def getTransferDataFlavors: Array[DataFlavor] = {
    if(selection!=null && selection.length==1&& InstanceSelection.exportModules.contains(selection(0).ref.typ))InstanceSelection.flavorsWithFiles
		else InstanceSelection.flavors
	}	
	
	def isDataFlavorSupported(flavor:DataFlavor):Boolean =if(flavor==DataFlavor.javaFileListFlavor){
    if(selection!=null && selection.length==1) {
      InstanceSelection.exportModules.contains(selection(0).ref.typ)
    } else false
  } else InstanceSelection.flavors.contains(flavor)
	
	override def toString:String ="InstSel parents:"+parentRefs.mkString+"selection:"+selection.mkString+"\ntextArray:"+(  textArray mkString "\n")+
	 " dragColumn:"+dragColumn+" dragrows:"+dragRows.mkString(",")+" propField:"+propField
}

trait OnMoveHandler{
  def moved(targetHandler:DragDropListener[_ <: Transferable],sel:InstanceSelection):Unit
}


object InstanceSelection {
	val flavor= new DataFlavor(classOf[InstanceSelection],"DatabaseObject")	
	val oldDBFlavor= new DataFlavor(classOf[Array[ExportContainer]],"OLD_DB")
	val flavors= Array(flavor,DataFlavor.stringFlavor)
  val flavorsWithFiles= Array(flavor,DataFlavor.stringFlavor,DataFlavor.javaFileListFlavor)
	val graphElemFlavor=new DataFlavor(classOf[GraphElemTransferable],"GraphElem")	
	val graphElemFlavorArray=Array(graphElemFlavor,DataFlavor.javaFileListFlavor)
  
  var lastOnMoveHandlerID=0
  val onMoveHandlerMap: mutable.HashMap[Int, OnMoveHandler] =collection.mutable.HashMap[Int,OnMoveHandler]()
  def addOnMoveHandler(handler:OnMoveHandler): Int ={
    lastOnMoveHandlerID+=1
    onMoveHandlerMap(lastOnMoveHandlerID)=handler
    lastOnMoveHandlerID
  }
  def removeOnMoveHandler(id:Int):Unit =onMoveHandlerMap.remove(id)

  val exportModules: Map[Int, ExportModule] =Map[Int,ExportModule]((136,GAEB83Module),(163,RechXTabExportModule))
}