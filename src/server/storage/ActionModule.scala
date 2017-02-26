/**
 * Author: Peter Started:22.09.2010
 */
package server.storage

import definition.typ.ActionTrait
import definition.data.{InstanceData,Reference}
import definition.expression.Expression

import scala.util.control.NonFatal
import util.Log

/** superclass of all user-defined modules defining actions for classes
 * 
 */
trait ActionModule {	
	def actions:Iterable[ActionTrait]
	def createActions:Iterable[ActionTrait]=Seq.empty
	def setObjectType(typeID:Int):Unit
	def notifyChildAdded=false	
	def onChildAdded(self:InstanceData,propField:Byte,newChild:InstanceData)={}
	def notifyFieldChanged=false
	def onFieldChanged(self:InstanceData,fieldNr:Byte,newValue:Expression)= {}
	def notifyChildRemoved=false
	def onChildRemoved(self:InstanceData,propField:Byte,delRef:Reference) = {}
}

object ActionModule {
	def load(moduleName:String):ActionModule = try{
		Class.forName(moduleName).newInstance.asInstanceOf[ActionModule]
	} catch {case NonFatal(e)=> Log.e(e);println(e); EmptyModule
	case other:Throwable =>println(other);System.exit(0);null};
}

object EmptyModule extends ActionModule {
  def actions:Iterable[ActionTrait]=Seq.empty
  def setObjectType(typeID:Int)={}
}