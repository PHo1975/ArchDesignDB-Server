/**
 * Author: Peter Started:19.12.2010
 */
package server.print

import definition.data._
import definition.expression.{Constant, IntConstant, StringConstant}
import definition.typ._
import server.comm.{AbstractUserSocket, JavaClientSocket}
import server.storage.{ActionIterator, ActionModule, ActionNameMap, StorageManager}
import transaction.handling.{ActionList, SessionManager, TransactionManager}

/**
 * 
 */
class PrintActionModule extends ActionModule {
	var outDefType:Int= _ 
	var paramValueType:Int = _	
	
	SessionManager.registerSetupListener(()=>{
		outDefType= SystemSettings().systemTypes("OutputDef") 
		paramValueType= SystemSettings().systemTypes("ParamValue")		
	})	
	
	val outputAction=new ActionIterator("Ausgabe",None,doOutputWindow)	
	val actions=List(outputAction)	
	 
	
	def doOutputWindow(u:AbstractUserSocket, parent:OwnerReference, data:Seq[InstanceData], param:Seq[(String,Constant)]):Boolean =
		if(data.isEmpty) false
		else {
		val outDefs= getOutDefs(data.head.ref)		
		
		val formsList=PrintFormsHandler.loadFormsForType(data.head.ref.typ)
		if(formsList.isEmpty)  throw new IllegalArgumentException("Kein Druckformular für Typ "+data.head.ref.typ)
		//println("Printforms :"+formsList.mkString("\n"))
		
		
		val question=new XMLQuestion("client.print.PrintQuestionHandler",
			<forms> {formsList.map(_.toXML)} </forms>
		  <outDefs> {outDefs}</outDefs>
			)
		u.askEnquiry(question,selectOutputDefFunc)
  	
  	
  	def selectOutputDefFunc(u:JavaClientSocket, returnData:Seq[(String,Constant)]):Unit= {
			//System.out.println("select:"+returnData.mkString(","))
			if(returnData.isEmpty) return
			var printForm:FormDescription = null
			returnData.head._1 match {
				case "NewOutDef" => // new definition
					val formNumber=returnData.head._2.toInt
					printForm=formsList(formNumber)
					val printer=returnData(1)._2
					val pageSettings=returnData(2)._2
					val portrait=returnData(3)._2
					val pageWidth=returnData(4)._2.toInt
					val pageHeight=returnData(5)._2.toInt
					//println("new Outdef "+printForm.name)
					TransactionManager.doTransaction(u.userID, ActionNameMap.getActionID("AusgabeDef erzeugen"),data.head.ref, false, outDefType,{
					  var outDefInst=TransactionManager.tryCreateInstance(outDefType, Array(new OwnerReference(0,data.head.ref)), true, -1, true, true)
					  outDefInst=outDefInst.setField(0,IntConstant(printForm.inst))
					  outDefInst=outDefInst.setField(1,printer)
					  outDefInst=outDefInst.setField(2,pageSettings)
					  outDefInst=outDefInst.setField(3,portrait)
					  TransactionManager.tryWriteInstanceData(outDefInst)
					  val paramList=returnData.drop(6)
					  //println("plist:"+paramList+" outdefInst="+outDefInst)
					  val paramOwnerRef=Array(new OwnerReference(0,outDefInst.ref))
					  val childList= for(p <-paramList) yield {
					  	//println("add param"+p+" "+p._2.getType)
					  	var pInst=TransactionManager.tryCreateInstance(paramValueType, paramOwnerRef,true)
					  	//println("pinst:"+pInst)
					  	pInst=pInst.setField(0,StringConstant(p._1))
					  	pInst=pInst.setField(1,p._2)
					  	TransactionManager.tryWriteInstanceData(pInst)
					  	pInst
					  }
					  PrintEngine.generatePages(u,data.head,OutputDefinition(outDefInst,childList),pageWidth,pageHeight,printForm)
					})
				case "DeleteOutDef" => // delete definition
					val outDefInst=returnData.head._2.toInt
					val outDefRef=new Reference(outDefType,outDefInst)
					TransactionManager.doTransaction(u.userID, ActionNameMap.getActionID("AusgabeDef löschen"),outDefRef, false, -1,{
						TransactionManager.tryDeleteInstance(outDefRef,Some(new OwnerReference(0,data.head.ref)),None)
					})
				case "ChoseOutDef" => // delete definition
					val outDefInstID=returnData.head._2.toInt
					val pageWidth=returnData(1)._2.toInt
					val pageHeight=returnData(2)._2.toInt
					val outDefRef=new Reference(outDefType,outDefInstID)
					val outDefInst=ActionList.getInstanceData(outDefRef)
					val paramList:Seq[InstanceData]=ActionList.getInstanceProperties(outDefRef) match {
					  	case Some(spdata)=> spdata.propertyFields(0).propertyList.map(StorageManager.getInstanceData)
					  	case None => Seq.empty
					  }
					val outDef=OutputDefinition(outDefInst,paramList)
					val printForm= PrintFormsHandler.getForm(outDef.formInst )
					PrintEngine.generatePages(u,data.head,outDef,pageWidth,pageHeight,printForm)

				case "ChangeOutDef" => // new definition
					val odInst=returnData.head._2.toInt
					val formNumber=returnData(1)._2.toInt
					printForm=formsList(formNumber)
					val printer=returnData(2)._2
					val pageSettings=returnData(3)._2
					val portrait=returnData(4)._2
					val pageWidth=returnData(5)._2.toInt
					val pageHeight=returnData(6)._2.toInt
					val odRef=new Reference(outDefType,odInst)
					//println("change outdef "+printForm.name)
					TransactionManager.doTransaction(u.userID, ActionNameMap.getActionID("AusgabeDef ändern"),data.head.ref, false, -1,{
					  var outDefInst=ActionList.getInstanceData(odRef)
					  outDefInst=outDefInst.setField(0,IntConstant(printForm.inst))
					  outDefInst=outDefInst.setField(1,printer)
					  outDefInst=outDefInst.setField(2,pageSettings)
					  outDefInst=outDefInst.setField(3,portrait)
					  TransactionManager.tryWriteInstanceData(outDefInst)
					  val paramList=returnData.drop(7)
					  //println("change plist:"+paramList+" outdefInst="+outDefInst)
					  // load existing param objects
					  val oldParamList:Seq[InstanceData]=StorageManager.getInstanceProperties(odRef) match {
					  	case Some(spdata)=> spdata.propertyFields(0).propertyList.map(StorageManager.getInstanceData)
					  	case None => Seq.empty
					  }
					  val childList = for(p <-paramList) yield {
					  	//println("write param"+p+" "+p._2.getType)
					  	var pInst= oldParamList.find(_.fieldValue.head.toString==p._1) match {
					  		case Some(instDat)=>instDat
					  		case None =>TransactionManager.tryCreateInstance(paramValueType, Array(new OwnerReference(0,outDefInst.ref)),true)
					  	}
					  	pInst=pInst.setField(0,StringConstant(p._1))
					  	pInst=pInst.setField(1,p._2)
					  	//println("pinstfield :"+pInst.fieldValue(1).getType)
					  	TransactionManager.tryWriteInstanceData(pInst)
					  	pInst
					  }
					  PrintEngine.generatePages(u,data.head,OutputDefinition(outDefInst,childList),pageWidth,pageHeight,printForm)
					})
				case "StorePrintData" =>
					val odInst=returnData.head._2.toInt
					val formNumber=returnData(1)._2.toInt
					printForm=formsList(formNumber)
					val printer=returnData(2)._2
					val pageSettings=returnData(3)._2
					val portrait=returnData(4)._2
					val pageWidth=returnData(5)._2.toInt
					val pageHeight=returnData(6)._2.toInt
					val odRef=new Reference(outDefType,odInst)
					var outDefInst=ActionList.getInstanceData(odRef)
					val paramList=returnData.drop(7)
					val outDef=new OutputDefinition(odInst,printForm.inst,printer.toString,pageSettings.toString,portrait.toBoolean,paramList)
					//println("Store "+printForm.name)
					PrintEngine.storePages(u,data.head,odRef,outDef,pageWidth,pageHeight,printForm)
			}
		}//selectOutputDefFunc	
		//println("Print action done ")
		true
  }// doOutputWindow
	
	def getOutDefs(ref:Reference)= StorageManager.getInstanceProperties(ref) match {			
			case Some(pdata)=> for(p <-pdata.propertyFields(0).propertyList;if p.typ == outDefType)
				yield  {
				  val paramList:Seq[InstanceData]=StorageManager.getInstanceProperties(p) match {
				  	case Some(spdata)=> spdata.propertyFields(0).propertyList.map(StorageManager.getInstanceData)
				  	case None => Seq.empty
				  }
					OutputDefinition( StorageManager.getInstanceData(p),paramList ).toXML
				}			
			case None => Seq.empty
		} 
  
  def setObjectType(typeID:Int)={}  

}