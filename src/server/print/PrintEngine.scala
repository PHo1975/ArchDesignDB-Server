/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import java.io.DataOutput
import java.util.Date
import definition.comm.GeneratorType
import definition.data.FormDescription
import definition.data.InstanceData
import definition.data.OutputDefinition
import definition.data.OwnerReference
import definition.data.Reference
import definition.expression.BlobConstant
import definition.expression.BoolConstant
import definition.expression.DateConstant
import definition.expression.IntConstant
import definition.expression.StringConstant
import definition.typ.AllClasses
import definition.typ.SystemSettings
import server.comm.JavaClientSocket
import server.storage.ActionNameMap
import server.storage.StorageManager
import transaction.handling.TransactionManager
import definition.data.PrintElement

/**
 * 
 */
object PrintEngine {
	val context=new PrintContext
	val lock=new Object
	val dataEater=new DataEater
	val FieldMatcher="""field([A-Za-z0-9äöüÄÖÜß_]+)""".r
	val FieldFormattedMatcher="""field_F([A-Za-z0-9äöüÄÖÜß_]+)""".r
	val ParentFieldMatcher="""pfield([A-Za-z0-9äöüÄÖÜß_]+)""".r
	val FormParamMatcher="""formParam([A-Za-z0-9äöüÄÖÜß_]+)""".r
	val FieldTermMatcher="""fieldTerm([A-Za-z0-9äöüÄÖÜß_]+)""".r
	val CustomMatcher="""custom([A-Za-z0-9äöüÄÖÜß]+)_(\w+)""".r
	val orphanTreshold=15
	val widowTreshold=15
	val printDateKeyName="Date"
	

	//var printableType:Int = -1	
	
	val generatorMap=collection.mutable.Map[String,CustomGenerator]()
	val customIteratorMap=collection.mutable.Map[String,Null]()
	
	lazy val printFormType=SystemSettings().systemTypes("PrintForm")		
	lazy val archiveType=SystemSettings().systemTypes("PrintArchive")
  lazy val iteratorType=SystemSettings().systemTypes("PrintIterator")
  lazy val singlePageGeneratorType=SystemSettings().systemTypes("SinglePageGenerator")
  lazy val sectionIteratorType=SystemSettings().systemTypes("SectionIterator")
	lazy val parentIteratorType=SystemSettings().systemTypes("ParentIterator")
	lazy val printableType=SystemSettings().systemTypes("Printable")	
	lazy val projectType=SystemSettings().systemTypes("Project")
	lazy val projectAdressesType=SystemSettings().systemTypes("ProjectAdresses")
	lazy val customIteratorType=AllClasses.get.getClassIDByName("CustomIterator")

	private var printableClassesMap=collection.mutable.HashMap[Int,Boolean]()
  
	
	def getGenerator(name:String)= generatorMap.getOrElseUpdate(name,Class.forName(name.trim).newInstance().asInstanceOf[CustomGenerator])

	def generatePages(u:JavaClientSocket, dataParent:InstanceData, oDef:OutputDefinition, pageWidth:Int, pageHeight:Int, form:FormDescription) =
		lock.synchronized {						
			context.initPrintSession()
			val formRef=new Reference(printFormType,oDef.formInst)			
			context.updateProjectInfo(dataParent)
			val paramsMap=oDef.paramValues.toMap
			context.setFormParams(paramsMap)
			context.setPrintDate(paramsMap.get(PrintEngine.printDateKeyName) match {
			  case Some(date) if !date.isNullConstant => date
			  case _ =>new StringConstant(util.JavaUtils.shortDateFormat.format(new Date))
			})
			val iteratorList=StorageManager.getInstanceProperties(formRef) match {
				case Some(pData) => AbstractGenerator.loadIteratorsFromProperty(form,  pData.propertyFields (0).propertyList)
				case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef+" "+oDef)
			}
			dataEater.initPrinting(if(oDef.portrait)pageWidth else pageHeight,if(oDef.portrait)pageHeight else pageWidth,form,context,oDef.paramValues)			
			iteratorList.foreach(_.iterate(dataParent,dataEater,context)	)			
			//val pagesData=dataEater.getPagesData		
			//System.out.println("Done print iterating, sending to "+u.userEntry.name)
			u.sendGeneratedData(write(dataParent.toString,oDef.odInst))		
		}	

	def storePages(u:JavaClientSocket, dataParent:InstanceData, oref:Reference, oDef:OutputDefinition, pageWidth:Int, pageHeight:Int, form:FormDescription) =
		lock.synchronized {
			context.initPrintSession()	
			//if(printFormType== -1) printFormType=SystemSettings().systemTypes("PrintForm")
			val formRef=new Reference(printFormType,oDef.formInst)
			context.setPrintDate( new StringConstant(oDef.paramValues.find(el=>el._1=="Date") match {			  
			  case Some(tuple)=>tuple._2.toString
			  case None =>util.JavaUtils.shortDateFormat.format(new Date)
			}))
			context.updateProjectInfo(dataParent)
			val iteratorList=StorageManager.getInstanceProperties(formRef) match {
				case Some(pData) => AbstractGenerator.loadIteratorsFromProperty(form,  pData.propertyFields (0).propertyList)
				case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef+" "+oDef)
			}

			dataEater.initPrinting(if(oDef.portrait)pageWidth else pageHeight,if(oDef.portrait)pageHeight else pageWidth,form,context,oDef.paramValues)		
			iteratorList.foreach(_.iterate(dataParent,dataEater,context)	)
			val pagesData=dataEater.getPagesData	

			TransactionManager.doTransaction(u.userEntry.id, ActionNameMap.getActionID("Ausdruck archivieren"),oref, false, archiveType,{
				var archiveInst=TransactionManager.tryCreateInstance(archiveType, Array(new OwnerReference(1,oref)), true, -1, true, true)
				archiveInst=archiveInst.setField(0,new StringConstant(<fl>
					{form.fonts.toXML}
				</fl>.toString))
				archiveInst=archiveInst.setField(2,new IntConstant(if(oDef.portrait)pageWidth else pageHeight))
				archiveInst=archiveInst.setField(3,new IntConstant(if(oDef.portrait)pageHeight else pageWidth))
				archiveInst=archiveInst.setField(4,new BoolConstant(form.isLandscape))
				archiveInst=archiveInst.setField(5,new IntConstant(form.left))
				archiveInst=archiveInst.setField(6,new IntConstant(form.top))
				archiveInst=archiveInst.setField(7,new IntConstant(form.right))
				archiveInst=archiveInst.setField(8,new IntConstant(form.bottom))
				archiveInst=archiveInst.setField(9,BlobConstant.fillData(dataEater.write ))
				archiveInst=archiveInst.setField(10,DateConstant())
				TransactionManager.tryWriteInstanceData(archiveInst)
			})

		}

	def write(title:String,odefInst:Int)(out:DataOutput)= {
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








