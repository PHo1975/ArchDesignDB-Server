/**
 * Author: Peter Started:18.12.2010
 */
package server.config
import java.io.DataOutput

import definition.data._
import definition.expression.StringConstant
import definition.typ.{EnumDefinition, NOENUM, SystemSettings}
import server.storage.{ActionNameMap, StorageManager}
import transaction.handling.{ActionList, TransactionManager}
import util.Log

import scala.collection.{Map, mutable}
import scala.util.control.NonFatal
/**
 * 
 */
class ServerSystemSettings(settingsRef:Reference)extends SystemSettings  {
	val _systemTypes: mutable.Map[String, Int] = collection.mutable.Map[String, Int]()
	
	var userFolders:Map[String,Reference]= if(StorageManager.instanceExists(FSPaths.userRootRef.typ,FSPaths.userRootRef.instance)) StorageManager.getInstPropList(FSPaths.userRootRef, 1).map(
      ref =>  (StorageManager.getInstanceData(ref).fieldValue.head.toString, ref)).toMap else {
		Log.e("User root not found "+FSPaths.userRootRef)
		Map.empty
	}

	var enums: mutable.LinkedHashMap[String, EnumDefinition] = collection.mutable.LinkedHashMap[String, EnumDefinition]("Undefined" -> NOENUM)
	var enumByID: Map[Int, EnumDefinition] = collection.Map[Int, EnumDefinition]()
	val topProperties:InstanceProperties= try { 
	  //System.out.println("Init topProperties settingsRef:"+settingsRef)
		StorageManager.getInstanceProperties(settingsRef) match {		  
		  case Some(propData)=>
				for(pField<-propData.propertyFields(1).propertyList) {
					val dat=StorageManager.getInstanceData(pField)
					//System.out.println("Set systemType "+dat.fieldValue(0).toString+" "+dat.fieldValue(1).toInt)
					_systemTypes(dat.fieldValue.head.toString )= dat.fieldValue(1).toInt
				}
				//println("Types:("+systemTypes.mkString(";")+")")
				for(pField<-propData.propertyFields(0).propertyList;
						enum=EnumDefinition(StorageManager.getInstanceData(pField),ActionList))
					enums+= (enum.name ->enum)
				enumByID=genIDMap
				//println("Enums:"+enums.mkString("\n"))
				propData
			case None => throw new IllegalArgumentException("No Systemsettings found in "+settingsRef)
		}	
	} catch {
		case NonFatal(e)=> util.Log.e(e);null
		case other: Throwable => util.Log.e(other); null
	}


	lazy val customFolderList: IndexedSeq[InstanceData] = if (topProperties == null) IndexedSeq.empty else for (pField <- topProperties.propertyFields(2).propertyList)
		yield StorageManager.getInstanceData(pField)
	
	
	def systemTypes(key:String):Int =  _systemTypes.getOrElse(key, {
	   Log.e("cant find SystemType "+key+"\n"+Thread.currentThread.getStackTrace().take(10).mkString("\n  "))
	   0
	 })
	
	def getCustomSettings(folderName:String):IndexedSeq[InstanceData]= {
		//println("CustomFoldersList:"+customFolderList)
		for(folder <-customFolderList;if folder.fieldValue(0).toString.equalsIgnoreCase(folderName))
			StorageManager.getInstanceProperties(folder.ref) match {
			case Some(data) => return data.propertyFields(1).propertyList.map{StorageManager.getInstanceData}
			case None => return IndexedSeq.empty
		}
		Log.e("Folder not found :"+folderName)
		IndexedSeq.empty
	}
	
	lazy val clientSettingsMap:Map[String,String]=(for(data <-getCustomSettings("ClientSettings")) 
		yield data.fieldValue(0).toString -> data.fieldValue(1).toString).toMap

	override lazy val holidays: IndexedSeq[HolidayDefinition] = getCustomSettings("Holidays") map (new HolidayDefinition(_))
		
	def getClientSetting(settingName:String):String=
		if(clientSettingsMap.contains(settingName)) clientSettingsMap(settingName)
		else ""


	def write(out: DataOutput): Unit = {
		println("write settings")
		out.writeInt(_systemTypes.size)
		for(t<-_systemTypes) {
			out.writeUTF(t._1)
			out.writeInt(t._2)
		}
		out.writeInt(enums.size)
		for(e<-enums) {
			out.writeUTF(e._1)
			e._2.write(out)
		}
		out.writeInt(clientSettingsMap.size)
		for(c<-clientSettingsMap){
			out.writeUTF(c._1)
			out.writeUTF(c._2)
		}
		out.writeInt(customFolderList.size)
		for(f<-customFolderList){
		  out.writeUTF(f.fieldValue.head.toString)
		  f.ref.write(out)
		}
		out.writeInt(holidays.size)
		for(h<-holidays) h.write(out)
	}
	
	
  def getUserRoot(userName:String):Reference=  userFolders.getOrElse(userName,{
    val userType=_systemTypes.get("UserStore") match {
      case Some(ut)=> ut
      case None=> Log.e("\nSystem Type 'UserStorage' is not defined, assume type 5 \n");5
    }
    val folderType=_systemTypes.get("Folder") match {
      case Some(ut)=> ut
      case None=> Log.e("\nSystem Type 'Folder' is not defined, assume type 110 \n");110
    }
    var inst:InstanceData= null
    TransactionManager.doTransaction(-1, ActionNameMap.getActionID("createUserFolder"), FSPaths.userRootRef, true, userType,{
			inst = TransactionManager.tryCreateInstance(userType, Array(new OwnerReference(1, FSPaths.userRootRef)), false).setField(0, StringConstant(userName))
	    TransactionManager.tryWriteInstanceData(inst)
	    if(FSPaths.libraryRootRef.instance>0) {
	      val libraryInst=ActionList.getInstanceData(FSPaths.libraryRootRef)
	      TransactionManager.trySecondUseInstances(List(FSPaths.libraryRootRef), libraryInst.owners.head, new OwnerReference(2,inst.ref), -1,false)  
	    }
	    
	    userFolders=userFolders + ((userName,inst.ref))
	    getClientSetting("UserSystemFolders") match {
	      case ""=>
	      case fn=>
					val ownerRef=Array(new OwnerReference(3,inst.ref))
					for(folderName<-fn.split(',')) {
						val finst = TransactionManager.tryCreateInstance(folderType, ownerRef, false).setField(0, StringConstant(folderName))
            TransactionManager.tryWriteInstanceData(finst)
          }
			}
    } )
    inst.ref    
  })    
  
}

