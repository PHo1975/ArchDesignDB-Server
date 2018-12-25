/**
 * Author: Peter Started:07.03.2011
 */
package client.dataviewer.sidePanel

/** Manages the List of SidePanelControllers
 * 
 */

import definition.typ.{AbstractObjectClass, SystemSettings}

import scala.util.control.NonFatal

object SPControllerList {  
	private lazy val classList:Seq[Class[_]] = try {
		val settingsString=SystemSettings().getClientSetting("SidePanelControllers").trim
		if (settingsString.length==0) Seq.empty 
		else settingsString.split(',').map(a=>{			
			Class.forName(a.trim())} )
	} catch {
		case NonFatal(e) => util.Log.e("Error when loading SidePanelController: "+e.toString);Seq.empty
		case other:Throwable =>println(other);System.exit(0);null
	}  
  
  private lazy val contrList=classList.map(_.newInstance.asInstanceOf[SidePanelController])  
  
  def generateList(tableClass:AbstractObjectClass):Seq[SidePanelController]=
  	contrList.filter(_.classFits(tableClass)).map(_.getClass.newInstance)
   
}