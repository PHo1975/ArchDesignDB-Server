/**
 * Author: Peter Started:21.04.2011
 */
package client.dataviewer
import client.comm.ClientQueryManager
import client.dialog.DialogManager
import client.dialog.form.FormBox
import definition.comm.NotificationType
import definition.data.{InstanceData, Reference}
import definition.expression._
import definition.typ.form.{DataChangeListener, FormDataField}
import definition.typ.{AbstractObjectClass, AllClasses, DataType, SelectGroup}

import scala.swing.Swing
import scala.util.control.NonFatal
/**
 * 
 */
class FormModel(controller:DataViewController) extends DataChangeListener {
	var theClass:AbstractObjectClass = _
	var forms:Option[FormBox ]=None
	var dataRef:Reference = _
	var dataValue:InstanceData = _	
	var bestID:Int= -1
	
	val lock = new Object
	
	def setForms(nclass:AbstractObjectClass,nform:Option[FormBox])= lock.synchronized{
		shutDown()
		theClass=nclass		
		forms=nform
		for (f<-forms) f.setListener(Some(this))			
	}
	
	def loadData(nref:Reference,doneListener:()=>Unit) = {
		shutDown()
		var firstTime=true
		//SimpleProfiler.measure("FormModel loadData")
		if(forms.isDefined) {
			if(nref.typ != theClass.id) throw new IllegalArgumentException("FormModel Wrong dataType "+nref+ " expected:"+theClass.name)
			dataRef=nref
			if(bestID> -1) shutDown()
			//theClass.formatFields
			bestID=ClientQueryManager.createSubscription(nref, -1) ((n:NotificationType.Value,data:IndexedSeq[InstanceData]) => 
			 lock.synchronized {Swing.onEDT{			   
				n match {
					case NotificationType.fieldChanged | NotificationType.sendData|NotificationType.updateUndo  =>
						dataValue=data.head
						setDataValue(data.head,theClass)
						if(firstTime){
              firstTime=false
              doneListener()
            }
					case NotificationType.instanceRemoved => shutDown()
				}
			}} )		
		} else doneListener()
	}
	
	def setDataValue(newValue:InstanceData,nclass:AbstractObjectClass) = lock.synchronized{ 
		for(f<-forms)
			f.setDataValue(newValue,nclass)			
		
	}
	
	def shutDown(): Unit = if(bestID>0)lock.synchronized{
		for (f<-forms;el<-f) el match{
			case de:FormDataField=>de.wantShutDown();de.shutDown()
			case _ =>
		}
		ClientQueryManager.removeSubscription(bestID)
		bestID= -1	
	}
	
	// interface DataChangelistener ************************************
	
	def fieldChanged(field:Byte,newValue:Expression): Unit = lock.synchronized{
		//println("field changed :"+dataRef+" field:"+field+" value:"+newValue.getTerm)
		if(bestID==0) throw new IllegalArgumentException("FormModel fieldchanged after shutDown: "+dataRef+" field:"+field+" value: "+newValue)
		ClientQueryManager.writeInstanceField(dataRef, field, newValue)
	}
	
	def parseValue(fieldNr:Byte,text:String):ParserResult = lock.synchronized{		
		if(text.length==0)Expression.generateNullConstant(theClass.fields(fieldNr).typ) else
			if (theClass.fields(fieldNr).typ==DataType.StringTyp)
				try {
					StringParser.parse( text) match {
					  case e:ParserError=> new StringConstant(text)
					  case ex=>ex
					}
				} 
		catch {
			case NonFatal(e) => new StringConstant(text)
			case other:Throwable =>println(other);System.exit(0);null
		}
		else StringParser.parse( text) // throw exception when fail
	}
	
	def flipMaximizeWindow(max:Boolean): Unit ={
	  if(max)controller.maximizeSplitBox()
	  else controller.splitBoxToNormal()
	}
	def print(): Unit =if(dataRef!=null&&dataValue!=null){
	  val dataClass=AllClasses.get.getClassByID(dataRef.typ)
	  dataClass.actions.find{case(string,atrait)=>string==DataViewController.printCommand} match {
	    case Some((str,tra))=>DialogManager.startActionDialog(tra,Seq(SelectGroup(dataValue.owners.head,Seq(dataRef))))
	    case _=>
	  }
	}
		
}