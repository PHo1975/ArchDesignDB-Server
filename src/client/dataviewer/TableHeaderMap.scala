/**
 * Author: Peter Started:05.11.2010
 */
package client.dataviewer

import client.comm.{UserSettings, _}
import client.dialog.{ComboBoxEditor, EditorFactory}
import client.ui.ViewConstants
import definition.typ._

import javax.swing.JComboBox
import javax.swing.table._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.util.control.NonFatal
/** stores the header configuration for all Tables
 * 
 */
object TableHeaderMap {
  val theMap: mutable.HashMap[Int, MyColumnModel] =collection.mutable.HashMap[Int,MyColumnModel]()
  
  ClientQueryManager.registerStoreSettingsListener(() => {
  	for(entry <-theMap)
  		UserSettings.setListProperty("TableColumns",entry._1.toString,
  		  entry._2.createTupleList)
  })
  
  
  
  def getColumnModel(typ:Int): MyColumnModel =  theMap.getOrElseUpdate(typ, setupEmptyColumnModel(typ))
  
  def setupEmptyColumnModel(typ:Int): MyColumnModel = {
  	val newModel=new MyColumnModel
  	val theClass=AllClasses.get.getClassByID(typ).asInstanceOf[ClientObjectClass]
  	val numColumn:Int=theClass.fields.size
  	
  	//val enumEditors:Seq[ComboBoxEditor]=for(ef<-theClass.enumFields) yield new ComboBoxEditor(new JComboBox(ef._2 .javaVect))
  	
  	val firstColumn=new TableColumn(0)
  	firstColumn.setHeaderValue(" ")
  	firstColumn.setMaxWidth(25*ViewConstants.fontScale/100)
		firstColumn.setPreferredWidth(23*ViewConstants.fontScale/100)
		firstColumn.setResizable(false)
		
		newModel.addColumn(firstColumn)
		// is there a stored TableSetup for this type ?
		val settings=UserSettings.getListProperty[(Int, Int)]("TableColumns",typ.toString,Nil)
		var usedColumns:List[Int]=Nil
		if(settings.nonEmpty) {
			//System.out.println("settings for Typ:"+typ+" "+settings+" ")
			for(cs <-settings;if cs._1 <= numColumn){
			  val fs=theClass.fieldSetting(cs._1-1)
				if(fs.visible) {
					val newCol=new TableColumn(cs._1)
					try {
					  
						newCol.setHeaderValue(theClass.fields(cs._1-1).name)					
						newCol.setWidth(cs._2)
						newCol.setPreferredWidth(cs._2)
						theClass.enumFields get(cs._1-1) match {
						  case Some(enumData)=> newCol.setCellEditor(new ComboBoxEditor(new JComboBox(enumData.mapValues.toArray)))
						  case None =>
						}					
						val ed=fs.editor
						if(ed.length>0) {
						  EditorFactory.getInplaceEditor(ed) match {
						    case Some(inplace) =>
									newCol.setCellEditor(inplace.getEditor)
									//newCol.setCellRenderer(inplace.getRenderer)
								case _ => //println("cant find inplaceEditor :"+ed+" in class "+theClass.name +" field "+cs._1 )
						  } 
						}				
					} catch {case NonFatal(e) => util.Log.e("setupEmptyColumnModel typ:"+typ,e)
					case other:Throwable =>println(other);System.exit(0);null}
					usedColumns=cs._1::usedColumns
					newModel.addColumn(newCol)
				}
			  //restColumns +=1
			}			  
		} 
		for(i <-0 until numColumn;if (!usedColumns.contains(i + 1)) && theClass.fieldSetting(i).visible) {
			val newCol=new TableColumn(i+1)
			newCol.setHeaderValue(theClass.fields(i).name)
			newModel.addColumn(newCol)
		}  	
		newModel	
  }
}

class MyColumnModel extends DefaultTableColumnModel {
	def createTupleList: IndexedSeq[(Int, Int)] ={
		for(i<-1 until tableColumns.size;col=tableColumns.get(i))
		  yield (col.getModelIndex,col.getWidth)
	}
}