package client.icons
import java.io.File
import java.net.{URL, URLDecoder}
import java.util.jar.JarFile
import javax.swing.ImageIcon

import scala.collection.JavaConverters._

object IconManager {
  val startPath="client/icons/"
  val cl: ClassLoader =this.getClass.getClassLoader()
  val iconResource: URL =cl.getResource(startPath)
  val ending="png"
    
  def namePart(fileName:String): String = fileName.substring(fileName.lastIndexOf('/')+1,fileName.length)
  
  val iconMap: Map[(String, String), ImageIcon] =if(iconResource.getProtocol()=="jar") {
    val jarFileN = URLDecoder.decode(iconResource.getFile(),"UTF-8")
    val jarFileName = jarFileN.substring(5,jarFileN.indexOf("!"))
    val jf=new JarFile(jarFileName)    
     (for(entry<-jf.entries.asScala;eName=entry.getName()
          if !entry.isDirectory() && eName.startsWith(startPath);
     parts= eName.split('.')
          if parts.last.equalsIgnoreCase(ending);
     splits=namePart(parts.head).split('$')
     ) yield (splits(0), splits(1)) -> new ImageIcon(cl.getResource(entry.getName()))).toMap
    
  }else {
    val dirFile=new File(iconResource.getFile())
    (for(file<-dirFile.listFiles;if file.isFile;parts=file.getName.split('.')
         if parts.last.equalsIgnoreCase(ending);
      splits=parts.head.split('$');if splits.length == 2) yield (splits(0), splits(1)) -> new ImageIcon(file.toURI.toURL)).toMap
  }
  
   def getIcon(group:String,command:String):Option[ImageIcon]={
    val key=(group,command)
    //if(!iconMap.contains(key)) System.err.println("Cant find Icon:"+key)
    iconMap.get(key)    
  }
  
  
  def  createImageIcon(path:String):ImageIcon = {
		val imgURL:URL   = this.getClass.getResource(path)
		if (imgURL != null) {			
			new ImageIcon(imgURL)
    } else {
      util.Log.e("Couldn't find file: " + path)
      null
    }
	}

}