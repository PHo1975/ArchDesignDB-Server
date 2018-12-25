/**
 * Author: Peter Started:19.12.2010
 */
package server.print
import definition.data.{FormDescription, InstanceData, ParameterDescription, Reference}
import definition.expression.TRUE
import definition.typ.SystemSettings
import server.storage.StorageManager

/**
 * 
 */
object PrintFormsHandler {
	val folderType=SystemSettings().systemTypes("Folder")
	val formType=SystemSettings().systemTypes("PrintForm")
	val plotType=SystemSettings().systemTypes("PlotDesign")
	def subFolders=SystemSettings().getCustomSettings("PrintForms")	
	
	
	protected def loadFormDescription(formRef:Reference)= {
	  val paramChildList=StorageManager.getInstanceProperties(formRef) match {
	  	case Some(data)=>for(c <-data.propertyFields (2).propertyList
													 if c.ref.typ == ParameterDescription.parameterType) yield ParameterDescription(StorageManager.getInstanceData(c))
	  	case None => Seq.empty
		}
	  val iteratorList=StorageManager.getInstanceProperties(formRef) match {
				case Some(pData) => pData.propertyFields (0).propertyList .map(a=> AbstractGenerator(null,StorageManager.getInstanceData(a),false,false))
				case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef)
			}
	  
	  val itersWithOption=iteratorList.flatMap(someIter=> someIter.map(a=>a.optionName)).flatten.
	    map(optionName=> ParameterDescription(optionName,5,"Element "+optionName+" drucken ?",TRUE)).toSet
	  //println("Iters with Option "+itersWithOption.mkString(", ")) 
		val fontChildList:Seq[InstanceData]=StorageManager.getInstanceProperties(formRef) match {
			case Some(data)=>for(c <-data.propertyFields (3).propertyList) yield StorageManager.getInstanceData(c)
			case None => Seq.empty
	  }
	  FormDescription(StorageManager.getInstanceData(formRef),paramChildList++itersWithOption,fontChildList)
	}
		
	def loadFormsForType(dataType:Int):Seq[FormDescription]= {
	  subFolders.find(_.fieldValue.head.toInt==dataType) match {
	    case Some(subFolder)=> StorageManager.getInstanceProperties(subFolder.ref) match {
	    	case Some(propData)=> for(formRef<-propData.propertyFields(1).propertyList;if formRef.typ == formType)
							yield loadFormDescription(formRef)
	    	case None => Seq.empty
	    }
	    case None => Seq.empty
	  }
		
	}	
	
	def getForm(formInst:Int)=loadFormDescription(new Reference(formType,formInst))
}

