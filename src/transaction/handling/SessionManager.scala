/**
 * Author: Peter Started:29.08.2010
 */
package transaction.handling


import java.io.File
import java.text.SimpleDateFormat
import java.util.{Calendar, Date, GregorianCalendar, Timer}
import javax.swing.JOptionPane

import definition.expression.DateConstant
import definition.typ.{AllClasses, SystemSettings}
import management.databrowser.ConsolePanel
import server.comm.{MainServerSocket, UserList}
import server.config.{FSPaths, ServerSystemSettings}
import server.storage.{ActionNameMap, ServerClassList, StorageManager, TransLogHandler, UsageStatFileHandler}
import server.webserver.WebServer

import scala.swing.Swing
import scala.util.control.NonFatal
;

/** the main class for server
 * 
 */
object SessionManager {
  var scl:ServerClassList=null  
  var isSetup=false  
  val setupListeners=collection.mutable.ArrayBuffer[()=>Unit]()  
  val backupTimer=new Timer(true)
  val backupTask=new BackupTask
  val backupFormatter=new SimpleDateFormat("yyyy.MM.dd_HH.mm.ss")
  //def main(args: Array[String]): Unit = 	init()
  
  def registerSetupListener(func:()=>Unit) =  	
  	if(isSetup) func() // if the classes List is ready, call the func  	
  	else setupListeners +=func // else wait for getting set up
  
  //def registerBackupDoneListener(func:()=>Unit)= backupTask.backupDoneListener=Some(func)
  
  def init(logConsole:ConsolePanel) = {
    try {
      MainServerSocket.setup()
      print("Sessionmanager init ..")
      val configFile = new File(FSPaths.configDir + "types.xml")
      if (!configFile.exists) inputMissingData()
      println("configfile exists")
      val xmlData = xml.XML.loadFile(FSPaths.configDir + "types.xml")

      scl = new ServerClassList(xmlData)

      AllClasses.set(scl)

      UserList.fromXML(xml.XML.loadFile(FSPaths.configDir + "users.xml"))

      StorageManager.init(scl.classList)

      ActionNameMap.read()
      //System.out.println("ActionNames read")
      SystemSettings.settings = new ServerSystemSettings(FSPaths.settingsObjectRef)
      for (li <- setupListeners)
        li() // call listeners
      isSetup = true
      Runtime.getRuntime.addShutdownHook(new Thread {
        override def run() = {
          shutDown()
          backupTimer.cancel()
        }
      })

      MainServerSocket.start()

      val now = new Date()
      val thisDate =DateConstant()
      val gc = new GregorianCalendar
      gc.setTime(now)
      gc.set(Calendar.HOUR_OF_DAY, 23)
      gc.set(Calendar.MINUTE, 59)
      gc.set(Calendar.SECOND, 50)
      //gc.add(Calendar.DAY_OF_MONTH, 1)

      backupTask.backupDoneListener = Some(UsageStatFileHandler.updateStatistics _)
      if (isBackupDue(thisDate)) backupTask.run()
      backupTimer.scheduleAtFixedRate(backupTask, gc.getTime(), 1000 * 60 * 60 * 24)
      //UsageStatFileHandler.updateStatistics()

      println("webserver "+WebServer)
      WebServer.setLogConsole(logConsole)

      WebServer.setup()

      WebServer.start()
      util.Log.w("Max Trans:" + TransLogHandler.getTransID)
      util.Log.w("Init done")
    }  catch { case NonFatal(e)=> util.Log.e("init",e)}
  }
  
  def inputMissingData()= Swing.onEDT{
    val newDirName=JOptionPane.showInputDialog(null,"Cant find config file \"Types.xml\". Enter config directory that contains it, leave empty to break: ", 
    "c:\\database\\config")	  
	  if(newDirName.trim.length==0) System.exit(0)
	  FSPaths.dirNode.put("ConfigDir",newDirName)	  
	  val newDataDir=JOptionPane.showInputDialog(null,"Enter data directory","c:\\database\\data")
	  FSPaths.dirNode.put("DataDir",newDataDir)	  
	  val port=JOptionPane.showInputDialog(null,"Enter server port","9000")
	  FSPaths.setupNode.put("ServerPort",port)  
	  val so=JOptionPane.showInputDialog(null,"Enter settings object","30,1")
	  if(so.trim.length>0) FSPaths.setupNode.put("SettingsObject",so)
    util.Log.e("starting Database now ")
  }
  
  protected def shutDown() = MainServerSocket.synchronized{
    MainServerSocket.isRunning=false    
    MainServerSocket.listener.close()
    util.Log.w("Shutting Down Server")
  	UserList.shutDown(() => {
  	 StorageManager.shutDown()	
  	}  	)
  	Thread.sleep(500)
    util.Log.w("finish")
    WebServer.stop()
  }  
  
  
  def getLastBackupTime=DateConstant.from1970Millis(FSPaths.lastBackup)
  	
  def isBackupDue(thisTime:DateConstant)= getLastBackupTime!=thisTime
  
  
  
}