/**
 * Author: Peter Started:26.12.2010
 */
package client.print

import java.lang.reflect.Constructor

import definition.data.OutputDefinition
import javax.print.attribute._
import javax.print.attribute.standard._
/**
 * 
 */
object MediaMap {
	val mmUnit: Int =Size2DSyntax.MM
	
  private val mediaHash=collection.mutable.HashMap[MediaSizeName,MediaSizeWrapper]()
  def getMediaSize(msn:MediaSizeName): MediaSizeWrapper = {
		if(msn==null) println("msn == null")
		val ret=if(mediaHash.contains(msn))mediaHash(msn)
  	else {
  		val newWrapper=MediaSizeWrapper(msn)
  		mediaHash(msn)=newWrapper
  		newWrapper
  	}
		//println("Map "+msn+" ->"+ret+" "+ret.mn .getValue)
		ret
  }
}

case class MediaSizeWrapper(mn:MediaSizeName){
	lazy val mediaSize: MediaSize =MediaSize.getMediaSizeForName(mn)
	lazy val name: String =cutString(mn.toString.replace("iso-"," ").replace("jis-"," ").trim.toUpperCase,15)
	lazy val width: Float =if(mediaSize==null) {println("Mediasize "+mn+" = null");210f} else mediaSize.getX(MediaMap.mmUnit)
	lazy val height: Float =if(mediaSize==null) 297f else mediaSize.getY(MediaMap.mmUnit)
	
	override def toString: String =name+" ("+math.round(width)+" x "+math.round(height)+") mm"
	
	def cutString(st:String,len:Int): String = {
		if(st.length>len) st.substring(0,len)
		else st
	}
}



object TrayMap {	
	
	val altClass: Class[_] =Class.forName("sun.print.SunAlternateMedia")
	val altConst: Constructor[_] =altClass.getDeclaredConstructor(classOf[Media])
	
  private val trayHash=collection.mutable.HashMap[MediaTray,MediaTrayWrapper]()
  
  def getMediaTray(mt:MediaTray): MediaTrayWrapper = {
  	if(trayHash.contains(mt))trayHash(mt)
  	else {
  		val newWrapper=new MediaTrayWrapper(mt)
  		trayHash(mt)=newWrapper
  		newWrapper
  	}
  }
}


class MediaTrayWrapper(val mt:MediaTray){
	lazy val altValue:PrintRequestAttribute=TrayMap.altConst.newInstance(mt).asInstanceOf[PrintRequestAttribute]	
	
	private lazy val name = if(OutputDefinition.trayTranslations.contains(mt.toString))OutputDefinition.trayTranslations(mt.toString) else mt.toString
	
	override def toString: String = name+" "+mt.getValue
}

object AutoTray extends MediaTrayWrapper(null) {
	override def toString= "Automatische Auswahl"
	override lazy val altValue:PrintRequestAttribute=null
}