package client.comm
import java.awt.event.{InputEvent, KeyEvent}
import java.beans.PropertyChangeListener
import java.io.{DataInput, DataOutput}

import javax.swing.{JComponent, KeyStroke}

import scala.swing.Component

trait KeyStrokeReceiver extends javax.swing.Action {
  var _enabled=true
  def groupName:String
  def commandName:String
	def strokeHit():Unit

	def addPropertyChangeListener(listener:PropertyChangeListener ): Unit = {}
	def getValue(key:String):Object=null          
	def isEnabled: Boolean =_enabled
	def putValue(key:String,value:Object): Unit ={}
	def removePropertyChangeListener(listener:PropertyChangeListener): Unit = {}
	def setEnabled(b:Boolean):Unit = _enabled=b
	def actionPerformed(e:java.awt.event.ActionEvent):Unit = strokeHit()
	def setStroke(stroke:KeyStroke): Unit
}


case class CommandGroup(name:String)  {
  val commandMap: collection.mutable.Map[String, KeyStroke] =collection.mutable.HashMap[String,KeyStroke]()
  val receiverMap: collection.mutable.Map[String, KeyStrokeReceiver] =collection.mutable.HashMap[String,KeyStrokeReceiver]()
  
  def loadStrokes(in:DataInput): IndexedSeq[Unit] = {
    for(i<-0 until in.readInt) 
      yield setStroke(in.readUTF,KeyStroke.getKeyStroke(in.readInt,in.readInt))
  }  
  
  def write(out:DataOutput): Unit ={
    out.writeUTF(name)
    out.writeInt(commandMap.size)
    for((k,v)<-commandMap) {
      out.writeUTF(k)
      out.writeInt(v.getKeyCode)
      out.writeInt(v.getModifiers)
    } 
  }  
  
  def setStroke(commName:String, stroke:KeyStroke): Unit = commandMap(commName)=stroke
  def removeCommand(commName:String): Option[KeyStroke] = commandMap.remove(commName)
  def registerReceiver(commName:String,receiver:KeyStrokeReceiver): Unit =receiverMap(commName)=receiver
  def unregisterReceiver(commName:String): Option[KeyStrokeReceiver] = receiverMap.remove(commName)
}



object KeyStrokeManager { 
  var bindigsEnabledListener:Option[Boolean =>Unit]=None
  var topComponent:Option[Component]=None    
  var groupMap: collection.mutable.Map[String, CommandGroup] =collection.mutable.HashMap[String,CommandGroup]()
  
  def enableBindings(): Unit =setBindingsEnabledState(true)
  def disableBindings(): Unit =setBindingsEnabledState(false)
  
  private def setBindingsEnabledState(state:Boolean): Unit = topComponent match {
    case Some(top)=> val am= top.peer.getActionMap.allKeys()
    		if(am!=null)
    		am.foreach( _.asInstanceOf[javax.swing.Action]. setEnabled(state))
        for(listener<-bindigsEnabledListener) listener(state)
    case _=>
  }
  
  def load(in:DataInput): Unit = {
    val numGroups=in.readInt
    for(gr<-0 until numGroups) {
      val groupName=in.readUTF
      getGroup(groupName).loadStrokes(in)      
    }    
  }
  
  def write(out:DataOutput): Unit = {
    out.writeInt(groupMap.size)
    for(v<-groupMap.valuesIterator)
      v.write(out)    
  }
  
  def registerReceiver(receiver:KeyStrokeReceiver): Unit = {
    if(topComponent.isEmpty)println("top not defined "+receiver)
    //println("Register :"+receiver.groupName+" "+receiver.commandName)
    val group= getGroup(receiver.groupName)
    group.registerReceiver(receiver.commandName,receiver) // for the Stroke Dialog to show all receivers   
    if(group.commandMap.contains(receiver.commandName)) {
      val stroke=group.commandMap(receiver.commandName)      
      for(comp<-topComponent) {
          comp.peer.getInputMap(javax.swing.JComponent.WHEN_IN_FOCUSED_WINDOW).put(stroke,receiver)
          comp.peer.getActionMap.put(receiver,receiver)
          receiver.setStroke(stroke)
      }
    } //else println("group "+group+"does not contain commandname:"+receiver.commandName)
    
  }
  
  def unregisterReceiver(receiver:KeyStrokeReceiver): Unit = {
    //println("Unregister :"+receiver.groupName+" "+receiver.commandName)
    getGroup(receiver.groupName).unregisterReceiver(receiver.commandName)
		for (comp<-topComponent) comp.peer.getActionMap.remove(receiver)
  }
  
  def addStroke(groupName:String,command:String,stroke:KeyStroke): Unit = {
    val group=getGroup(groupName)
    group.setStroke(command,stroke)   
  }
  
  def getGroup(groupName:String): CommandGroup = if(groupName==null) throw new IllegalArgumentException("Null-Gruppe") else
    groupMap.getOrElseUpdate(groupName,new CommandGroup(groupName))
  
  def keyStrokeToString(stroke:KeyStroke): String = {
    val modifierText=InputEvent.getModifiersExText(stroke.getModifiers)
    val keyText= KeyEvent.getKeyText(stroke.getKeyCode)
    if(stroke==null) ""
    else if(modifierText.length>0) modifierText+" + "+ keyText else keyText
  } 
  
  
  /** replaces an keystroke->Action setting in a Component
   * @param peer the component
   * @param keyCode what keyCode to change

   */ 
  private def setKeyAction(peer:JComponent,keyCode:Int,newAction:javax.swing.Action): Unit = {
		val aName=peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
			get(KeyStroke.getKeyStroke(keyCode,0,false))			
		peer.getActionMap.put(aName,newAction)
	}
  
  
  /** replaces an keystroke->Action setting in a Component
   * @param peer the component
   * @param keyCode what keyCode to change
   * @param func a function that gets the old Action data and returns the new Action 
   */ 
  def replaceKeyAction(peer:JComponent,keyCode:Int,func: (javax.swing.Action )=>javax.swing.Action): Unit = {
		val aName=peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
			get(KeyStroke.getKeyStroke(keyCode,0,false))
		val oldAction=	peer.getActionMap.get(aName)
		//System.out.println("tablemod replace name:"+aName+" oldAction:"+oldAction)
		val newAction= func(oldAction)
		peer.getActionMap.put(aName,newAction)
	}
}