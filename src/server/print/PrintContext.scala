/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import definition.expression.{Constant,EMPTY_EX}
import definition.typ.{AbstractObjectClass,AllClasses}
import definition.data.InstanceData
import definition.expression.IntConstant
import definition.expression.DateConstant
import definition.data.PlaceHolderElement
import definition.expression.StringConstant
import server.storage.StorageManager
import definition.typ.SystemSettings
import definition.typ.DataType
import scala.util.control.NonFatal


case class PlaceHolderValue(var holderList:List[PlaceHolderElement],var value:Option[String]=None)


trait CustomPrintvarResolver {
  def resolve(varName:String,currData:InstanceData):Constant
}

/**
 * 
 */
trait Context {
	def getVarValue(varName:String):Constant
	def createPlaceHolder(name:String,el:PlaceHolderElement):Option[String]
	def setPlaceHolderValue(name:String,newValue:String): Unit
}

class PrintContext extends Context{
	var currentClass:AbstractObjectClass= _
	var currentInstance:InstanceData = _
	var parentInstance:Option[InstanceData] = None
	var sectionInstance:Option[InstanceData]=None	
	val placeHolders=collection.mutable.Map[String,PlaceHolderValue]()	
	val customResolverMap=collection.mutable.HashMap[String,CustomPrintvarResolver]()	
	val printVariables=collection.mutable.HashMap[String,Constant]()
	var formParams:Map[String,Constant]=Map.empty
	var ignoreItersWithOption:Seq[String]=Seq.empty
	
	def initPrintSession()= placeHolders.clear
	
	
	def setCurrInstance(data:InstanceData)= {
		if(currentClass==null || currentClass.id!=data.ref .typ)
			currentClass=AllClasses.get.getClassByID(data.ref .typ)
		currentInstance=data
	}
	
	def setPageNr(value:Int )=printVariables("pageNr")=new IntConstant(value)	
	
	private def clearProjectVars()= printVariables.keySet.foreach(varName=>
	  if(varName.length>7&&varName.substring(0,7).equals("project")) printVariables.remove(varName) )
	
	def setPrintDate(value:Constant) =printVariables("date")=value
	
	def setFormParams(params:Map[String,Constant])= {	  
	  formParams=params	  
	  ignoreItersWithOption=params.filter( tuple=>tuple._2.getType==DataType.BoolTyp&& !tuple._2.toBoolean).keys.toSeq
	}
	
	def updateProjectInfo(parentInst:InstanceData)= {
	  clearProjectVars()
	  StorageManager.getNextParentOfType(parentInst.ref,PrintEngine.projectType) match {
	    case Some(projectRef)=>
				StorageManager.getInstanceProperties(projectRef) match {
          case Some(pprops)=> if(pprops.propertyFields(0).propertyList.nonEmpty) {
            val adressGroupRef=pprops.propertyFields(0).propertyList.head
            StorageManager.getInstanceProperties(adressGroupRef) match {
              case Some(gprops)=>
								gprops.propertyFields(0).propertyList.headOption foreach (prAdressRef =>{
                  val prData=StorageManager.getInstanceData(prAdressRef)
                  printVariables("projectDescription")=new StringConstant(prData.fieldValue.head.toString+" "+prData.fieldValue(1).toString)
                  printVariables("projectStreet")=new StringConstant(prData.fieldValue(2).toString)
                  printVariables("projectZip")=new StringConstant(prData.fieldValue(3).toString)
                  printVariables("projectCity")=new StringConstant(prData.fieldValue(4).toString)
                })
								gprops.propertyFields(1).propertyList.headOption foreach (prAdressRef =>{
                  val prData=StorageManager.getInstanceData(prAdressRef)
                  printVariables("projectClientName")=new StringConstant(prData.fieldValue.head.toString+" "+prData.fieldValue(1).toString)
                  printVariables("projectClientStreet")=new StringConstant(prData.fieldValue(2).toString)
                  printVariables("projectClientZip")=new StringConstant(prData.fieldValue(3).toString)
                  printVariables("projectClientCity")=new StringConstant(prData.fieldValue(4).toString)
                })
							case _ =>
            }
          }
          case _ =>
        }
			case _ =>
	  }
	  SystemSettings().getCustomSettings("officeAddress").headOption foreach (prData =>{
	     printVariables("officeName")=new StringConstant(prData.fieldValue.head.toString+" "+prData.fieldValue(1).toString)
	     printVariables("officeStreet")=new StringConstant(prData.fieldValue(2).toString)
	     printVariables("officeZip")=new StringConstant(prData.fieldValue(3).toString)
	     printVariables("officeCity")=new StringConstant(prData.fieldValue(4).toString)
	     printVariables("officeTel")=new StringConstant(prData.fieldValue(5).toString)
	     printVariables("officeFax")=new StringConstant(prData.fieldValue(6).toString)
	     printVariables("officeEmail")=new StringConstant(prData.fieldValue(7).toString)
	  })
	  
	}
	
  def getVarValue(varName:String):Constant = if(printVariables.contains(varName)) printVariables(varName)
  	else varName match {
	    case PrintEngine.FieldTermMatcher(fieldName)=> currentClass.fields.indices .find(i=>
	      fieldName.equalsIgnoreCase(currentClass.fields(i).name)) match { 
	        case Some(i)=>StringConstant(currentInstance.fieldData(i).getTerm)
	        case None=> StringConstant("unknown fieldTermName "+fieldName+" curr:"+currentClass.name)
	      }	    
	    case PrintEngine.FieldFormattedMatcher(fieldName)=>currentClass.fields.indices .find(i=>
	      fieldName.equalsIgnoreCase(currentClass.fields(i).name)) match {
	      case Some(i)=>
					currentClass.fieldSetting(i).formString match {
            case ""=> return StringConstant(currentInstance.fieldData(i).getValue.toString)
            case formst=> return StringConstant(formst.format(currentInstance.fieldData(i).getValue.toDouble))
          }
					return currentInstance.fieldData(i).getValue
				case None=> StringConstant("unknown fieldName "+fieldName+" curr:"+currentClass.name)
	    }
	    case PrintEngine.ParentFieldMatcher(fieldName)=>try {
				parentInstance match {
					  case Some(pi)=>
						val parentClass=AllClasses.get.getClassByID(pi.ref.typ)
						parentClass.fields.indices.find(i=>fieldName.equalsIgnoreCase(parentClass.fields(i).name)) match {
							  case Some(i)=> pi.fieldData(i).getValue
								case None=> StringConstant("no parentfield "+fieldName)
						}
						case None => StringConstant("no parent, f:"+fieldName)
				}
	      /*val owner=currentInstance.owners(0).ownerRef
	      val parentClass=AllClasses.get.getClassByID(owner.typ)
	      parentClass.fields.indices.find(i=> fieldName.equalsIgnoreCase(parentClass.fields(i).name)) match {
	        case Some(i)=> StorageManager.getInstanceData(owner).fieldData(i).getValue
	        case None =>StringConstant("unknown Owner fieldName "+fieldName+" class:"+parentClass.name)
	      }*/
	    } catch {case NonFatal(e)=>util.Log.e(e); StringConstant(e.getMessage());case other:Throwable =>println(other);System.exit(0);null}
	    
	    case PrintEngine.FieldMatcher(fieldName)=> currentClass.fields.indices .find(i=>
	      fieldName.equalsIgnoreCase(currentClass.fields(i).name)) match {
	      case Some(i)=>currentInstance.fieldData(i).getValue
	      case None=> StringConstant("unknown fieldName "+fieldName+" curr:"+currentClass.name)
	    }	    
	    case PrintEngine.FormParamMatcher(paramName)=>  
	      formParams.getOrElse(paramName,new StringConstant("Unknown Form Param '"+paramName+"'"))
	    case PrintEngine.CustomMatcher(customVarName,classString)=>
				val className=classString.replace('_','.')
				//System.out.println("CustomResover "+className+" varName:"+customVarName)
				return customResolverMap.getOrElseUpdate(className,
            Class.forName(className).newInstance.asInstanceOf[CustomPrintvarResolver]).
              resolve(customVarName, currentInstance)
			case _ => EMPTY_EX
	  }  	    		
  
  
  def createPlaceHolder(name:String,el:PlaceHolderElement):Option[String] = {
    //println("create PlaceHolder "+name+" el:"+el)
		if(placeHolders.contains(name)){
			val vel=placeHolders(name)			
			vel.holderList=el :: vel.holderList
			if(vel.value .isDefined)el.value=vel.value.get
			vel.value
		}
		else {
			placeHolders(name)=new PlaceHolderValue( List(el))
			None
		}
  }
  
	def setPlaceHolderValue(name:String,newValue:String)= {
		//System.out.println("set placeHolder "+name+" '"+newValue+"'")
		if(placeHolders.contains(name)){
			val vel=placeHolders(name)
			vel.holderList.foreach(_.value =newValue)
			vel.value =Some(newValue)
		}
		else{ 
		  placeHolders(name)=new PlaceHolderValue(Nil,Some(newValue))
		  util.Log.e("set placeholder, unknown name:"+name+" ->" +newValue)
		} 
	}
}