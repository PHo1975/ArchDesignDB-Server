package client.importer

import scala.collection.Seq
import java.awt.Point
import definition.data.OwnerReference
import java.io.File
import scala.swing.Window
import definition.data.InstanceData
import definition.data.Reference

class GAEBImporter extends FileImportDescriptor {
  def name:String="GAEB-Import"  
  
  def dbNameField:Byte=2.toByte
  
  def allowedFileTypes:Seq[(String,String)]=Seq(("Blanko-LV GAEB 81","D81"),("Blanko-LV GAEB 83","D83"),("Angebotsabgabe GAEB 84","D84"),
      ("Auftrag GAEB 86","D86"))
	
	
	
	
	
	/** checks if the given files can be inserted. File type checks are done by ImportManager
	 * 
	 */
	def canImport(files:Seq[File],droppedTarget:Option[InstanceData],ownerRef:OwnerReference):Boolean= {
    //println("Can import "+files.mkString(",")+" target:"+droppedTarget)
    true
  }
	
	/**
	 *  shows an import Dialog at the given place
	 *  @param wpos position of the dialog window
	 *  @param dropTarget on what object are the files dropped
	 *  @return a list of setting data, to start the import, or Nil to stop it 
	 */
	def showImportDialog(window:Window, wpos:Point,files:Seq[File],dropTarget:Option[InstanceData]):Seq[AnyRef]= {
	  Nil
	}
	
	
	/** imports a single file to the database
	 * @param file the dropped datafile
	 * @parentRef a Reference to the parent object where the data should be added
	 * @dropTarget on what object are the files dropped
	 * @settings the settings data from the import dialog
	 * @progressListener a function to be called after every import step, and to give the current success of the file import in percent
	 * @return success of the file import 
	 */
	def importFile(file:File,baseObject:Reference,settings:Seq[AnyRef],progressListener:(Int)=>Boolean):Boolean = {
	  false
	}
}