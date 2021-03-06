/**
 * Author: Peter Started:01.11.2010
 */
package server.storage

import definition.comm.ClientCommands
import server.config.FSPaths

import java.io._
import scala.collection.mutable

/** stores the names of Actions
 * the ids for the build-in Actions were taken from the ClientCommands enum
 * all custom actions start by id 100
 */
object ActionNameMap {
  val theMap: mutable.Map[String, Short] =collection.mutable.HashMap[String,Short ]()
  var maxID:Short=100
  val fileName=new File(FSPaths.dataDir+"actionMap.dat")
  
  def getActionID(name:String): Short = 
  	if(theMap contains name ) theMap (name)
  	else {  		 
  		maxID =  (maxID+1).toShort
  		theMap(name)=maxID
  		write()
  		maxID
  	}
  
  def getActionName(id:Short): String = theMap find(_._2 ==id) match {
  	case Some(a) => a._1
  	case None => ClientCommands(id) match {
  		case ClientCommands.createInstance => "Neu erstellt"
  		case ClientCommands.createInstances=> "Neue erstellt"  
  		case ClientCommands.deleteInstance => "Gelöscht"
  		case ClientCommands.copyInstances => "Kopiert"
  		case ClientCommands.moveInstances => "Verschoben"
  		case ClientCommands.writeField => "Feld geschrieben in"
  		case ClientCommands.writeInstance => "Objekt geschrieben"
  		case ClientCommands.writeMultiFields => "Felder geschrieben in"
  		case ClientCommands.secondUseInstances=> "Objekt verknüft"
  		case ClientCommands.executeAction=>"Action ausgeführt"
  	  case ClientCommands.executeCreateAction => "Objekt erzeugt"  
			case ClientCommands.createBlock=> "Block erstellt"
			case ClientCommands.changeBlock=> "Block geändert"
			case ClientCommands.deleteBlock=> "Block gelöscht"
			case ClientCommands.convertInstances=> "Instanzen konvertiert"	
  		case _ =>"Unbekannt "+id
  	}
  }
  
  def read(): Unit = if (fileName.exists) {
  	maxID=100
  	theMap.clear()
  	val is=new DataInputStream(new BufferedInputStream(new FileInputStream(fileName)))
  	val numElems=is.readInt
  	for(i <-0 until numElems) {
  		val id=is.readShort  		
  		theMap(is.readUTF)=id
  		if (id>maxID) maxID=id
  	}
  	is.close()
  }
  
  def write(): Unit = {
  	val os=new DataOutputStream(new BufferedOutputStream(new FileOutputStream(fileName)))  	
  	os writeInt theMap.size
  	for(el <-theMap ) {
  		os writeShort el._2
  		os writeUTF el._1
  	}  	
  	os.close()
  }
  
  override def toString: String = "ActionNameMap "+theMap.size+"\n"+ theMap.mkString("\n")
}