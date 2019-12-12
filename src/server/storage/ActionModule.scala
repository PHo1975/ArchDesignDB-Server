/**
 * Author: Peter Started:22.09.2010
 */
package server.storage

import definition.data.{InstanceData, Reference}
import definition.expression.Expression
import definition.typ.ActionTrait
import util.Log

import scala.util.control.NonFatal

/** superclass of all user-defined modules defining actions for classes
 * 
 */
trait ActionModule {	
	def actions:Iterable[ActionTrait]
	def createActions:Iterable[ActionTrait]=Seq.empty
	def setObjectType(typeID:Int):Unit
	def notifyChildAdded=false

	def onChildAdded(self: InstanceData, propField: Byte, newChild: InstanceData): Unit = {}
	def notifyFieldChanged=false

	def onFieldChanged(self: InstanceData, fieldNr: Byte, newValue: Expression): Unit = {}
	def notifyChildRemoved=false

	def onChildRemoved(self: InstanceData, propField: Byte, delRef: Reference): Unit = {}
}

object ActionModule {
	def load(moduleName:String):ActionModule = try{
		Class.forName(moduleName).getConstructor().newInstance().asInstanceOf[ActionModule]
	} catch {case NonFatal(e)=> Log.e("Error loading ActionModule '"+moduleName+"'",e);println(e); EmptyModule
	case other: Throwable => println(other); System.exit(0); null
	}
}

object EmptyModule extends ActionModule {
  def actions:Iterable[ActionTrait]=Seq.empty

	def setObjectType(typeID: Int): Unit = {}
}