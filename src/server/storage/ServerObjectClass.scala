/**
 * Author: Peter Started:21.09.2010
 */
package server.storage

import client.dialog.form.{FormBox, FormCreateContext}
import definition.data._
import definition.expression.{EMPTY_EX, Expression}
import definition.typ._
import javax.swing.JComponent
import server.config.AutoCreateInfo
import util.StrToInt
import util.XMLUtils.{optText, readOptString}

import scala.collection.immutable.IndexedSeq
import scala.swing.{Button, ToggleButton}
import scala.xml.{Elem, Text}


case class ServerCCD(editorName:String,childClassID:Int,actionName:String="*") extends AbstractCCD {
  
  lazy val action:Option[ActionTrait]=if(actionName=="*")None else AllClasses.get.asInstanceOf[ServerClassList].
     getClassByID(childClassID).actionModule.createActions.find(el=>if(el==null) {
    util.Log.e("createActions==null in action "+actionName +"childClass:"+childClassID+" editor:"+editorName);false
  } else el.name==actionName)
  
  def setEditorName(newValue:String) = new ServerCCD(newValue,childClassID,actionName)
	def setChildClass(newValue:Int) = new ServerCCD(editorName,newValue,actionName)
	def setAction(newValue:String) = new ServerCCD(editorName,childClassID,newValue)	
  
  override def toXMLFile: Elem = {
		<CC cID={childClassID.toString}  e={optText(editorName)} actionName={optText(actionName)} />
   }
}


class ServerObjectClass (var name:String,var id:Int,var description:String="",var comment:String="",var ownFields:Seq[AbstractFieldDefinition]=Seq.empty,
	  var ownFieldSettings:Seq[FieldSetting]=Seq.empty,
	  var ownPropFields:Seq[PropertyFieldDefinition]=Seq.empty,var ownBlockPropFields:Seq[BlockPropertyFieldDefinition]=Seq.empty, var superClasses:Array[Int]=Array.empty,
	 var moduleName:String="",val actionModule:ActionModule=EmptyModule,var shortFormat:InstFormat=NOFORMAT,var longFormat:InstFormat=NOFORMAT,
	 var resultFormat:InstFormat=NOFORMAT,	 val formBox:Option[FormBox]=None,var customInstanceEditor:Option[String]=None,
	 val ownAutoCreateInfos:Seq[AutoCreateInfo]=Seq.empty,var importDescriptor:Option[String]=None)
	 extends AbstractObjectClass {

	def ownActions: Iterable[ActionTrait] = actionModule.actions
	
	def getCreateAction(aName:String):ActionTrait={
	  //println("class "+name+" getCreateAction "+aName)
	  actionModule.createActions.find(_.name==aName)	match {
	   case Some(cract)=>cract
	   case _=> throw new IllegalArgumentException("Try to get unknown CreateAction "+aName+" in class "+name)
	 }
	}
	
	def toXML: Elem =	{
		<OClass name={name} id={id.toString} desc={optText(description)} comm={optText(comment)} superC={superClasses.mkString(",")} edit={
			customInstanceEditor map(Text(_))}  shortForm={optText(shortFormat.toString)}
			longForm={optText(longFormat.toString)} resForm={optText(resultFormat.toString)} imDesc={importDescriptor map(Text(_)) } >
		<Fields> 		{			ownFields.map(_.toXML)	}</Fields>
    <FSS>{ownFieldSettings.map(_.toXML)}</FSS>
		<PropFields> 		{			ownPropFields.map(_.toXML(false))	}</PropFields>
		<BP>{ownBlockPropFields.map(_.toXML)}</BP>
		<Actions> {ownActions.filterNot(_.hiddenFromPanel).map(_.toXML)} </Actions>    
    <Forms>{formBox match {case Some(b)=> b.toXML ;case _ => xml.Null} }</Forms>
		</OClass>
	}	
	
	def saveToXML(): Elem = {
		<OClass name={name} id={id.toString} desc={optText(description)} comm={optText(comment)} superC={superClasses.mkString(",")} edit={
			customInstanceEditor map(Text(_))}     shortForm={optText(shortFormat.toString)}
			longForm={optText(longFormat.toString)} resForm={optText(resultFormat.toString)} moduleName={optText(moduleName)} imDesc={importDescriptor map(Text(_))}>
		<Fields> 		{			ownFields.map(_.toXML)	}</Fields>
    <FSS>{ownFieldSettings.map(_.toXML)}</FSS>
		<PropFields> 		{			ownPropFields.map(_.toXML(true))	}</PropFields>
			<BP>{ownBlockPropFields.map(_.toXML)}</BP>
    <Forms>{formBox match {case Some(b)=> b.toXML ;case _ => xml.Null}}</Forms>
    <AC> {ownAutoCreateInfos.map(_.toXML)}</AC>
    
		</OClass>
	}
	
	def makeClone: ServerObjectClass = {
		val theClone=new ServerObjectClass(name,id,description,comment,ownFields,ownFieldSettings,ownPropFields,ownBlockPropFields,
		superClasses,moduleName,actionModule,shortFormat,longFormat,resultFormat,formBox,customInstanceEditor,ownAutoCreateInfos,importDescriptor)
		theClone.resolveSuperFields()
		theClone
	}
	
	def setFormBox(newValue:Option[FormBox]): ServerObjectClass = {
		val theClone=new ServerObjectClass(name,id,description,comment,ownFields,ownFieldSettings,ownPropFields,ownBlockPropFields,
		superClasses,moduleName,actionModule,shortFormat,longFormat,resultFormat,newValue,customInstanceEditor,ownAutoCreateInfos,importDescriptor)
		theClone.resolveSuperFields()
		theClone		
	}
	
	def setAutoCreateInfo(aci:Seq[AutoCreateInfo]): ServerObjectClass = {
		val theClone=new ServerObjectClass(name,id,description,comment,ownFields,ownFieldSettings,ownPropFields,ownBlockPropFields,
		superClasses,moduleName,actionModule,shortFormat,longFormat,resultFormat,formBox,customInstanceEditor,aci,importDescriptor)
		theClone.resolveSuperFields()
		theClone
	}

	
	/** creates an emty Instance of this class
   * 
   * @param ref reference to the new instance
   */
  def createInstance(ref: Reference,owner:Array[OwnerReference],withStartValues:Boolean):InstanceData =  {
  	val fieldExpressions:IndexedSeq[Expression]=
  		for(i <-fields.indices) yield 
  			if(withStartValues) {
  				val sv=fieldSetting(i).startValue
  				if (sv.isNullConstant) Expression.generateNullConstant(fields(i).typ )
  				else fieldSetting(i).startValue.generate  		
  			}
  			else Expression.generateNullConstant(fields(i).typ )
		//TransactionManager.logStep("Generated")
  	new InstanceData(ref,fieldExpressions,owner,Array.empty,false)
  }
  
  /** creates an empty InstanceProperty of this class
   * 
   * @param ref the instance of that Property list
   * @return the new Property object
   */
  def createInstanceProperty(ref:Reference):InstanceProperties =  {
  	val pArray: Array[PropertyFieldData] =((for(i <- propFields.indices)
  		yield new PropertyFieldData(propFields(i).single,IndexedSeq.empty))++
			(for(i<-blockPropFields.indices)yield new PropertyFieldData(false,IndexedSeq.empty))
			).toArray
  	new InstanceProperties(ref,pArray)
  }
  
  def getEmpty: EMPTY_EX.type =EMPTY_EX
  
  def getNumOwnFields: Int =ownFields.size

	def getNumBlockPropFields:Int = ownBlockPropFields.size
  
  def getNumOwnPropFields: Int =ownPropFields.size

}



object EmptyServerClass  extends ServerObjectClass ("",0)


object ServerObjectClass {	
	// creates an ObjectClass object from XML
  val readFormContext: FormCreateContext =new FormCreateContext {
	  def getIconableButton(commandName:String,groupName:String,ntooltipText:String)=new Button(commandName)
	  def getIconableToggleButton(commandName:String,groupName:String,ntooltipText:String)=new ToggleButton(commandName)
	  def showError(text:String,component:JComponent):Unit= {
	    println("Error:"+text)
	    Thread.dumpStack()
	  }
  }
  
  def createCCDFromXML(node: scala.xml.Node): ServerCCD = {
    val editor=readOptString(node , "@e")
	  val childClass=(node \ "@cID").text match {
			case StrToInt(i)=> i
			case _=> 0
		}
	  val actionName=readOptString(node , "@actionName")
	  ServerCCD(editor, childClass, actionName)
  }
  
	def fromXML(node: scala.xml.Node):ServerObjectClass = 	{		
		val name=(node \"@name").text
		val id=(node \"@id").text.toInt
		val moduleName=readOptString(node ,"@moduleName")
		val superClasses=AllClasses.stringToIntList((node \"@superC").text)
		val instEditorName=readOptString(node ,"@edit")
		val module=if(moduleName=="") EmptyModule else {
			val m=ActionModule.load(moduleName)
			m.setObjectType(id)
			m			
		}
		val fieldsNode=node \ "Fields"
		val fieldData=for(afield <- fieldsNode \ "FD") yield FieldDefinition.fromXML(afield)
		val settingsNode=node \ "FSS"
		val settingsData=for(afield <- settingsNode \ "FS") yield FieldSetting.fromXML(afield)
		val propNode=node \ "PropFields"
		val propData=for(bfield <- propNode \ "PropertyFD")
		  yield PropertyFieldDefinition.fromXML(bfield,createCCDFromXML)
		val blockPropNode= node \ "BP"
		val blockPropData= for(bpfield <-blockPropNode\ "BlockProperty")
			yield BlockPropertyFieldDefinition.fromXML(bpfield)
		val acNode=node \"AC"
		val acData=for(afield <- acNode \ "AutoCreate") yield AutoCreateInfo.fromXML(afield)
		val formNode=node \"Forms"
		val formBoxData=readFormBox(formNode)		
		val importDescriptor=readOptString(node ,"@imDesc")
		new ServerObjectClass(name,id ,  readOptString(node ,"@desc"),readOptString(node ,"@comm"),
			fieldData,settingsData,propData,blockPropData,superClasses,
		  moduleName,module,InstFormat.read(node \"@shortForm"),InstFormat.read(node \"@longForm"),
		  InstFormat.read(node \"@resForm"),formBoxData,
		  if(instEditorName.length==0)None else Some(instEditorName),
		  acData,if(importDescriptor.length==0)None else Some(importDescriptor) )
	}
	
	def readFormBox(node:scala.xml.NodeSeq): Option[FormBox] = {
		val elList=for(abox <- node \ "FormBox") yield FormBox(abox,readFormContext)
		elList.headOption
	}
}