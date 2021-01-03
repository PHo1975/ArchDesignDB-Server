/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import definition.comm.GeneratorType
import definition.data._
import definition.expression._
import definition.typ.{AllClasses, SystemSettings}
import server.comm.JavaClientSocket
import server.storage.{ActionNameMap, StorageManager}
import transaction.handling.TransactionManager

import java.io.DataOutput
import java.util.Date
import scala.collection.mutable
import scala.util.matching.Regex

/**
 * 
 */
object PrintEngine {
	val context=new PrintContext
	val lock=new Object
	val dataEater=new DataEater
	val FieldMatcher: Regex ="""field([A-Za-z0-9%äöüÄÖÜß_]+)""".r
	val FieldFormattedMatcher: Regex ="""field_F([A-Za-z0-9%äöüÄÖÜß_]+)""".r
	val ParentFieldMatcher: Regex ="""pfield([A-Za-z0-9%äöüÄÖÜß_]+)""".r
	val FormParamMatcher: Regex ="""formParam([A-Za-z0-9%äöüÄÖÜß_]+)""".r
	val FieldTermMatcher: Regex ="""fieldTerm([A-Za-z0-9%äöüÄÖÜß_]+)""".r
	val CustomMatcher: Regex ="""custom([A-Za-z0-9%äöüÄÖÜß]+)_(\w+)""".r
	val orphanTreshold=15
	val widowTreshold=15
	val printDateKeyName="Date"

		//var printableType:Int = -1
	
	val generatorMap: mutable.Map[String, CustomGenerator] =collection.mutable.Map[String,CustomGenerator]()
	val customIteratorMap: mutable.Map[String, Null] =collection.mutable.Map[String,Null]()
	
	lazy val printFormType: Int =SystemSettings().systemTypes("PrintForm")
	lazy val archiveType: Int =SystemSettings().systemTypes("PrintArchive")
  lazy val iteratorType: Int =SystemSettings().systemTypes("PrintIterator")
  lazy val singlePageGeneratorType: Int =SystemSettings().systemTypes("SinglePageGenerator")
  lazy val sectionIteratorType: Int =SystemSettings().systemTypes("SectionIterator")
	lazy val parentIteratorType: Int =SystemSettings().systemTypes("ParentIterator")
	lazy val printableType: Int =SystemSettings().systemTypes("Printable")
	lazy val projectType: Int =SystemSettings().systemTypes("Project")
	lazy val projectAdressesType: Int =SystemSettings().systemTypes("ProjectAdresses")
	lazy val customIteratorType: Int =AllClasses.get.getClassIDByName("CustomIterator")

	private val printableClassesMap = collection.mutable.HashMap[Int, Boolean]()
  
	
	def getGenerator(name:String): CustomGenerator = generatorMap.getOrElseUpdate(name,Class.forName(name.trim).getConstructor().newInstance().asInstanceOf[CustomGenerator])

	def generatePages(u:JavaClientSocket, dataParent:InstanceData, oDef:OutputDefinition, pageWidth:Float, pageHeight:Float, form:FormDescription): Unit =
		lock.synchronized {						
			context.initPrintSession()
			val formRef=new Reference(printFormType,oDef.formInst)			
			context.updateProjectInfo(dataParent)
			val paramsMap=oDef.paramValues.view.map(el=>(el.paramName,el.result)).toMap
			context.setFormParams(paramsMap)
			context.setPrintDate(paramsMap.get(PrintEngine.printDateKeyName) match {
			  case Some(date) if !date.isNullConstant => date
			  case _ =>StringConstant(util.JavaUtils.shortDateFormat.format(new Date))
			})
			val iteratorList=StorageManager.getInstanceProperties(formRef) match {
				case Some(pData) => AbstractGenerator.loadIteratorsFromProperty(form,  pData.propertyFields (0).propertyList)
				case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef+" "+oDef)
			}
			dataEater.initPrinting(if(oDef.portrait)pageWidth else pageHeight,if(oDef.portrait)pageHeight else pageWidth,form,context,oDef.paramValues)
      var first:Boolean=true
			for(i<-iteratorList){
        i.iterate(dataParent,dataEater,context,first)
        if(first) first=false
      }
			//val pagesData=dataEater.getPagesData		
			//System.out.println("Done print iterating, sending to "+u.userEntry.name)
			u.sendGeneratedData(write(dataParent.toString,oDef.odInst))		
		}	

	def storePages(u:JavaClientSocket, dataParent:InstanceData, oref:Reference, oDef:OutputDefinition, pageWidth:Float, pageHeight:Float, form:FormDescription): Option[Exception] =
		lock.synchronized {
			context.initPrintSession()	
			//if(printFormType== -1) printFormType=SystemSettings().systemTypes("PrintForm")
			val formRef=new Reference(printFormType,oDef.formInst)
			context.setPrintDate( StringConstant(oDef.paramValues.find(el => el.paramName == "Date") match {
				case Some(tuple) => tuple.result.toString
				case None => util.JavaUtils.shortDateFormat.format(new Date)
			}))
			context.updateProjectInfo(dataParent)
			val iteratorList=StorageManager.getInstanceProperties(formRef) match {
				case Some(pData) => AbstractGenerator.loadIteratorsFromProperty(form,  pData.propertyFields (0).propertyList)
				case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef+" "+oDef)
			}

			dataEater.initPrinting(if(oDef.portrait)pageWidth else pageHeight,if(oDef.portrait)pageHeight else pageWidth,form,context,oDef.paramValues)
      var first:Boolean=true
      for(i<-iteratorList){
        i.iterate(dataParent,dataEater,context,first)
        if(first) first=false
      }
			dataEater.getPagesData

			TransactionManager.doTransaction(u.userID, ActionNameMap.getActionID("Ausdruck archivieren"),oref, false, archiveType,{
				var archiveInst=TransactionManager.tryCreateInstance(archiveType, Array(new OwnerReference(1,oref)), notifyRefandColl = true, -1)
				archiveInst=archiveInst.setField(0,StringConstant(<fl>
					{form.fonts.toXML}
				</fl>.toString))
				archiveInst=archiveInst.setField(2,IntConstant(if (oDef.portrait) pageWidth.toInt else pageHeight.toInt))
				archiveInst=archiveInst.setField(3,IntConstant(if (oDef.portrait) pageHeight.toInt else pageWidth.toInt))
				archiveInst=archiveInst.setField(4,BoolConstant(form.isLandscape))
				archiveInst=archiveInst.setField(5,IntConstant(form.left))
				archiveInst=archiveInst.setField(6,IntConstant(form.top))
				archiveInst=archiveInst.setField(7,IntConstant(form.right))
				archiveInst=archiveInst.setField(8,IntConstant(form.bottom))
				archiveInst=archiveInst.setField(9,BlobConstant.fillData(dataEater.write ))
				archiveInst=archiveInst.setField(10,DateConstant())
				TransactionManager.tryWriteInstanceData(archiveInst)
			})

		}

	def write(title:String,odefInst:Int)(out:DataOutput): Unit = {
		out.writeInt(GeneratorType.printGenerator .id)
		out.writeUTF(title)
		out.writeInt(odefInst)
		dataEater.write(out)
		//System.out.println("write printdata done")
	}


	// in printable classes the first property field should be ignored
	// this map caches the info what classes are printable
	def isClassPrintable(classType:Int):Boolean= {
		if(printableClassesMap.contains(classType)) printableClassesMap(classType)
		else {
			val isPrintable=AllClasses.get.getClassByID(classType).inheritsFrom(printableType)
			printableClassesMap(classType)=isPrintable
			isPrintable
		}
	}
}

trait CustomGenerator {
  def fillPlotPage(dataParent:InstanceData,dataEater:DataEater,generator:SinglePageGenerator):Unit
  def fillInIterator(dataParent:InstanceData,dataEater:DataEater,context:PrintContext):Seq[PrintElement]={
    Seq.empty
  }
}








