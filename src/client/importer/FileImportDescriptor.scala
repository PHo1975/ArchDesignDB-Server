package client.importer
import java.awt.Point
import java.io.File

import client.comm.ClientQueryManager
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.StringConstant

import scala.swing.Window

trait FileImportDescriptor {
  
  def name:String
  var baseID:Int= -1
  
  def dbNameField:Byte
  /**
   * @return the list of supported file types to import
   */
	def allowedFileTypes:Seq[(String,String)]
	
	protected def getFileExtension(fileName:String)= {
	  val pix=fileName.lastIndexOf('.')
	  if(pix== -1) "" else fileName.substring(pix+1,fileName.length)
	}
	
	def acceptFile(file:File):Boolean= {
	  if(file==null){util.Log.e("Acceptfile ==null");return false}
	  val ext= getFileExtension(file.getName()) 
	  allowedFileTypes.exists(_._2.equalsIgnoreCase(ext))
	}
	
	protected def hasRightFileTypes(files:Seq[File]):Boolean= files.exists(acceptFile)
	
	/** checks if the given files can be inserted. File type checks are done by ImportManager
	 * 
	 */
	def canImport(files:Seq[File],droppedTarget:Option[InstanceData],ownerRef:OwnerReference):Boolean
	
	/**
	 *  shows an import Dialog at the given place
	 *  @param wpos position of the dialog window
	 *  @param dropTarget on what object are the files dropped
	 *  @return a list of setting data, to start the import, or Nil to stop it 
	 */
	def showImportDialog(window:Window, wpos:Point,files:Seq[File],dropTarget:Option[InstanceData]):Seq[AnyRef]
	
	
	/** imports a single file to the database
	 * @param file the dropped datafile
	 * @param settings the settings data from the import dialog
	 * @param progressListener a function to be called after every import step, and to give the current success of the file import in percent
	 * @return success of the file import 
	 */
	def importFile(file:File,baseObject:Reference,settings:Seq[AnyRef],progressListener:(Int)=>Boolean):Boolean
	
	
	/** gets or creates the base object where all objects should be imported 
	 *
	 */
	def getBaseObject(ownerRef:OwnerReference,dropTarget:Option[InstanceData],file:File)= 
	  dropTarget match {
	    case Some(d)=> d.ref  // add to drop object
	    case None => // create new object
	      val ref=new Reference(baseID,ClientQueryManager.createInstance(baseID,Array(ownerRef)))
				val fn=file.getName
				val dotIx=fn.lastIndexOf('.')
				val na=if(dotIx< 0) fn else fn.substring(0,dotIx)
				ClientQueryManager.writeInstanceField(ref,dbNameField,new StringConstant(na))
				ref
		}
	
}