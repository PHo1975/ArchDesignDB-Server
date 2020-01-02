/**
 * Author: Peter Started:29.08.2010
 */
package transaction.handling


import java.io.File
import java.text.SimpleDateFormat
import java.util.{Calendar, Date, GregorianCalendar, Timer}

import definition.expression.DateConstant
import definition.typ.{AllClasses, SystemSettings}
import javax.swing.JOptionPane
import management.databrowser.ConsolePanel
import org.eclipse.jetty.http.HttpVersion
import org.eclipse.jetty.server._
import org.eclipse.jetty.util.ssl.SslContextFactory
import server.comm.{MainServerSocket, UserList}
import server.config.{FSPaths, ServerSystemSettings}
import server.storage._
import server.webserver.WebServer

import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing
import scala.util.control.NonFatal


/** the main class for server
 * 
 */
object SessionManager {
  System.setProperty("jdk.tls.ephemeralDHKeySize", "2048")
  System.setProperty("org.eclipse.jetty.util.log.class","server.webserver.MyLogger")
  System.setProperty("org.eclipse.jetty.servlet.LEVEL","INFO")
  System.setProperty("org.eclipse.jetty.LEVEL","WARN")
  var scl: ServerClassList = _
  var isSetup=false
  protected val setupListeners: ArrayBuffer[() => Unit] = collection.mutable.ArrayBuffer[() => Unit]()
  val backupTimer=new Timer(true)
  val backupTask=new BackupTask
  val backupFormatter=new SimpleDateFormat("yyyy.MM.dd_HH.mm.ss")
  //def main(args: Array[String]): Unit = 	init()

  def registerSetupListener(func: () => Unit): Unit =
  	if(isSetup) func() // if the classes List is ready, call the func  	
  	else setupListeners +=func // else wait for getting set up
  
  //def registerBackupDoneListener(func:()=>Unit)= backupTask.backupDoneListener=Some(func)

  def init(logConsole: ConsolePanel): Unit = {
    try {
      println("Session init")
      MainServerSocket.setup()
      val configFile = new File(FSPaths.configDir + "types.xml")
      if (!configFile.exists) inputMissingData()
      val xmlData = xml.XML.loadFile(FSPaths.configDir + "types.xml")
      scl = new ServerClassList(xmlData)
      AllClasses.set(scl)
      println("All Classes set")
      UserList.fromXML(xml.XML.loadFile(FSPaths.configDir + "users.xml"))
      println("Userlist read")
      StorageManager.init(scl)
      println("StorageManager Init")
      ActionNameMap.read()
      println("ActionNames read")
      SystemSettings.settings = new ServerSystemSettings(FSPaths.settingsObjectRef)
      println("Server System Settings")
      //println("Setup Listeners "+setupListeners.size)
      for (li <- setupListeners) {
        li() // call listeners

      }
      println("SetupListener done")
      isSetup = true
      Runtime.getRuntime.addShutdownHook(new Thread {
        override def run(): Unit = {
          shutDown()
          backupTimer.cancel()
        }
      })

      MainServerSocket.start()
      println("Socket startet")
      val now = new Date()
      val thisDate =DateConstant()
      val gc = new GregorianCalendar
      gc.setTime(now)
      gc.set(Calendar.HOUR_OF_DAY, 23)
      gc.set(Calendar.MINUTE, 59)
      gc.set(Calendar.SECOND, 50)
      //gc.add(Calendar.DAY_OF_MONTH, 1)
      //println("Backup")
      backupTask.backupDoneListener = Some(UsageStatFileHandler.updateStatistics _)
      if (isBackupDue(thisDate)) backupTask.run()
      backupTimer.scheduleAtFixedRate(backupTask, gc.getTime, 1000 * 60 * 60 * 24)
      //UsageStatFileHandler.updateStatistics()
      util.Log.i("Max Trans:" + TransLogHandler.getTransID)
      WebServer.setLogConsole(logConsole)
      println("ssl:"+ management.databrowser.MainWindow.webSocketSSL)
      if (management.databrowser.MainWindow.webSocketSSL) {
        try {
          println("Start SSL")
          val http_config = new HttpConfiguration()
          http_config.setSecureScheme("https")
          http_config.setSecurePort(FSPaths.webServerPortSSL )
          http_config.setSendServerVersion(false)
          http_config.addCustomizer(new SecureRequestCustomizer())

          val https_config = new HttpConfiguration(http_config)
          https_config.addCustomizer(new SecureRequestCustomizer(true, 31536000L, true))

          val sslContextFactory = new SslContextFactory.Server()
          if (!FSPaths.keyStoreFile.exists) util.Log.e("keystore " + FSPaths.keyStoreFile + " existiert nicht")
          sslContextFactory.setKeyStorePath(FSPaths.keyStoreFile.toString)
          sslContextFactory.setKeyStorePassword("Pitpass1#")
          sslContextFactory.setCertAlias("1")
          sslContextFactory.setRenegotiationAllowed(false)
          sslContextFactory.addExcludeCipherSuites("TLS_DHE.*", "TLS_EDH.*")
          sslContextFactory.setIncludeCipherSuites("TLS_ECDHE.*")

          val httpConnector = new ServerConnector(WebServer, new HttpConnectionFactory(http_config))
          httpConnector.setPort(FSPaths.webServerPortNoSSL)
          val sslConnector = new ServerConnector(WebServer, new SslConnectionFactory(sslContextFactory,
            HttpVersion.HTTP_1_1.asString), new HttpConnectionFactory(https_config))
          sslConnector.setPort(FSPaths.webServerPortSSL)
          WebServer.setConnectors(Array(httpConnector, sslConnector))
          WebServer.setup()
          WebServer.start()
          println("WebServer mit ssl gestartet")
        } catch {
          case NonFatal(e) => util.Log.e("Fehler beim Serverstart:" + e.getMessage, e)
        }

      } else  try {
        val connector = new ServerConnector(WebServer)
        connector.setPort(FSPaths.webServerPortNoSSL)
        println("Webserver Port:"+ FSPaths.webServerPortNoSSL)
        WebServer.setConnectors(Array(connector))

        WebServer.setup()
        WebServer.start()
        println("WebServer gestartet")
      } catch {
        case NonFatal(e) => util.Log.e("fehler webserver:" + e.getMessage, e)
      }
      println("init done")
    }  catch {
      case NonFatal(e)=> util.Log.e("init",e);println("e:"+e)
      case g:Throwable=> println("Fatal "+g+" "+g.getSuppressed.mkString+" ");g.printStackTrace()}
  }

  def inputMissingData(): Unit = Swing.onEDT {
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

  protected def shutDown(): Unit = MainServerSocket.synchronized {
    MainServerSocket.isRunning=false
    for (l <- MainServerSocket.listener) l.close()
    util.Log.w("Shutting Down Server")
  	UserList.shutDown(() => {
  	 StorageManager.shutDown()	
  	}  	)
  	Thread.sleep(500)
    util.Log.w("finish")
    WebServer.stop()
  }


  def getLastBackupTime: DateConstant = DateConstant.from1970Millis(FSPaths.lastBackup)

  def isBackupDue(thisTime: DateConstant): Boolean = getLastBackupTime != thisTime
  
  
  
}