package transaction.handling

import java.util.TimerTask
import java.util.Date
import server.config.FSPaths
import java.io.FileInputStream
import java.util.zip.ZipEntry
import java.io.File
import java.util.zip.ZipOutputStream
import java.io.FileOutputStream
import java.util.GregorianCalendar
import definition.data.EMPTY_REFERENCE
import server.storage.ActionNameMap
import scala.util.control.NonFatal


trait BackupThread {
  self:{def setProgress(value:Int):Unit} =>
  def isCancelled:Boolean
  
  def getTodayName={    
	 FSPaths.backupDir+SessionManager.backupFormatter.format((new GregorianCalendar).getTime())+".zip"
  }
  
  def createBackupZipFile(theFile:File)={
    theFile.createNewFile()
		val zipFile=new ZipOutputStream(new FileOutputStream(theFile))
		zipFile.setMethod(ZipOutputStream.DEFLATED)
		zipFile
  }
  def updateProgress(value:Int)= {}
  
  val buffer=new Array[Byte](1024)  
  
  def loopDirectory(dirName:String,zipFile:ZipOutputStream):Unit={    
    val dirFile=new File(dirName)
    val files=dirFile.listFiles
	  for(i<-0 until files.size)  try {			    
		    if (isCancelled) {  return}
		    println("Backup "+i+" "+files(i))
		    val entry=new ZipEntry(dirFile.getName()+File.separator+files(i).getName())
		    zipFile.putNextEntry(entry)
		    var inFile:FileInputStream=null
		    var readBytes=0
		    try {
		      inFile=new FileInputStream(files(i))
		      while(readBytes != -1  && ! isCancelled) {
		        readBytes = inFile.read(buffer, 0, 1024) 
		        if(readBytes>0)zipFile.write(buffer, 0, readBytes)
          }
			  } 
		    catch {case NonFatal(ex) =>println(ex+" "+ex.getMessage())}
			    finally if (inFile != null) inFile.close
			    zipFile.closeEntry()
			    updateProgress(100*i/files.size)					    
			  }catch {case NonFatal(ex)=>println(ex+" "+ex.getMessage())} 
		}  
 }


class BackupTask extends TimerTask with BackupThread {
   val isCancelled=false
   var backupDoneListener:Option[()=>Unit] = None
   def setProgress(value:Int)={}
   def run()= {
     var zipFile:ZipOutputStream=null
     try {
       TransactionManager.doTransaction(0, ActionNameMap.getActionID("backup"), EMPTY_REFERENCE, true, 0, {
         val byFile=new File(FSPaths.backupDir+"beforeYesterday.zip")
         if(byFile.exists) byFile.delete()
         val yesterdayFile=new File(FSPaths.backupDir+"yesterday.zip")
         if(yesterdayFile.exists) yesterdayFile.renameTo(byFile)
         val currentFile=new File(FSPaths.backupDir+"Current.zip")
         if(currentFile.exists)currentFile.renameTo(yesterdayFile)
         zipFile=createBackupZipFile(currentFile)
	       loopDirectory(FSPaths.configDir,zipFile)
				 loopDirectory(FSPaths.dataDir,zipFile)         
       })       
     }
     catch {case NonFatal(ex)=>println(ex+" "+ex.getMessage())}
     finally if (zipFile != null) zipFile.close
     util.Log.w("backup at "+SessionManager.backupFormatter.format(new Date(System.currentTimeMillis())))
     for(l<-backupDoneListener)l()
     FSPaths.setLastBackup(System.currentTimeMillis())     
   }
     
}