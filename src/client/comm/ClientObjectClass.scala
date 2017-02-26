/**
 * Author: Peter Started:26.06.2010
 */
package client.comm

import javax.swing.JComponent

import client.dialog.form.{FormBox, FormCreateContext}
import client.dialog.{ActionStrokeButton, CreateActionMenuButton, CreateMenuButton, IconableButton, IconableToggleButton, Toast}
import client.ui.ClientApp
import definition.typ._
import util.XMLUtils.readOptString

import scala.util.control.NonFatal

class ClientClasses (node:scala.xml.Node) extends AllClasses [ClientObjectClass] {

  val classList:Map[Int,ClientObjectClass]=fromXML(node)
	
	def fromXML(node: scala.xml.Node):Map[Int,ClientObjectClass] =
    (for (ac<- node \\ "OClass";oc= ClientObjectClass.fromXML(ac))	yield oc.id -> oc).toMap

}


/**
 *  description of a Class
 */
class ClientObjectClass (val name:String,val id:Int,val description:String,val comment:String,protected val ownFields:Seq[AbstractFieldDefinition],
	protected val ownFieldSettings:Seq[FieldSetting], protected val ownPropFields:Seq[PropertyFieldDefinition],protected val theActions:Seq[ActionDescription],
	 protected val superClasses:Seq[Int], val shortFormat:InstFormat,val longFormat:InstFormat,val resultFormat:InstFormat,val formBox:Option[FormBox],
	 val customInstanceEditor:Option[String],val importDescriptor:Option[String]=None)
	 extends AbstractObjectClass {
   def ownActions=theActions  
   var enumFields:Map[Int,EnumDefinition]= Map.empty // position of enum fields
   var actionButtons:Seq[ActionStrokeButton]=Seq.empty
   
   lazy val createMenuItems:Map[Int,Seq[CreateMenuButton]]=( for(i<-0 until propFields.size;pf=propFields(i)) yield {     
     val buttons=pf.createChildDefs .filter(! _.action.isDefined).map(new CreateMenuButton(description,i.toByte,_))
     i -> buttons
   }).toMap
   lazy val createActionMenuItems:Map[Int,Seq[CreateActionMenuButton]]=( for(i<-0 until propFields.size;pf=propFields(i)) yield {
     val buttons=pf.createChildDefs .filter(_.action.isDefined).map(new CreateActionMenuButton(description,i.toByte,_))
     i -> buttons
   }).toMap 
   
   
   override def resolveSuperFields()= if(!hasResolved) {
  	 super.resolveSuperFields()
  	 try {  	   
  	   enumFields=fields.view.zipWithIndex.collect(
  	       {case (enumField:EnumFieldDefinition,ix)=>(ix,SystemSettings().enumByID(enumField.enumID)) }).toMap   
  		
  		 actionButtons=(for(sc<-superClasses;superClass= AllClasses.get.getClassByID(sc).asInstanceOf[ClientObjectClass] ) 
  		   yield superClass.actionButtons.filterNot(sca=> theActions.exists(_.name==sca.commandName))).flatten ++ 
  		     theActions.map(new ActionStrokeButton(description,_))  		 
  	 } catch {
			 case NonFatal(e) => util.Log.e("resolveSuperFields",e)
		   case other:Throwable =>println(other);System.exit(0)}
   }   
   
}

object ClientObjectClass {
	val readFormContext=new FormCreateContext {
	  def getIconableButton(commandName:String,groupName:String,ntooltipText:String)=new IconableButton(commandName,groupName,ntooltipText)
	  def getIconableToggleButton(commandName:String,groupName:String,ntooltipText:String)=new IconableToggleButton(commandName,groupName,ntooltipText)
	  def showError(text:String,component:JComponent)= {
	    ClientQueryManager.printErrorMessage(text)
	    new Toast(text,component,ClientApp.top).visible=true
	  }
}
	
	// creates an ObjectClass object from XML
	def fromXML(node: scala.xml.Node) = 	{
		val name=(node \"@name").text
		val id=(node \"@id").text.toInt
		val actionsNode=node \"Actions"
		val superClasses=AllClasses.stringToIntList ((node \"@superC").text)
		val instEditorName=readOptString(node ,"@edit")
		var shortForm:InstFormat=null
		
		val fieldNode=node \"Fields"
		val settingsNode=node \"FSS"
		val propNode=node \"PropFields"
		val formNode=node \"Forms"
		val importDescriptor=readOptString(node ,"@imDesc")

		new ClientObjectClass(name,id ,  readOptString(node ,"@desc"),readOptString(node ,"@comm"),
			for(afield <- fieldNode \ "FD") yield FieldDefinition.fromXML(afield),
			for(afield <- settingsNode \ "FS") yield FieldSetting.fromXML(afield),
			for(bfield <- propNode \ "PropertyFD") yield PropertyFieldDefinition.fromXML(bfield,CreateChildDefinition.fromXML),
		  for(efield <- actionsNode \\ "Action")yield ActionDescription.fromXML(efield),
		  superClasses,{
		  	shortForm=InstFormat.read(node \"@shortForm")
		  	shortForm
		  }
		  ,{
		  	val lf=InstFormat.read(node \"@longForm")
		  	if(lf==NOFORMAT) shortForm else lf
		  },InstFormat.read(node \"@resForm"),
		  readFormBox(formNode),
		  if(instEditorName.length==0)None else Some(instEditorName),
		  if(importDescriptor.length==0) None else Some(importDescriptor)
	  )
	}
	
	def readFormBox(node:scala.xml.NodeSeq): Option[FormBox] = {
		val elList=for(abox <- node \ "FormBox") yield FormBox(abox,readFormContext)
		if(elList.isEmpty) None
		else Some(elList.head)
	}
	
}
