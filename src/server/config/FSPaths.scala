/**
 * Author: Peter Started:27.06.2010
 */
package server.config

import java.io.File
import java.util.prefs.Preferences

import definition.data.Reference

/** Contains the directories in the file system
 * 
 */
object FSPaths 
{
  val systemNode: Preferences = Preferences.userNodeForPackage(this.getClass)
	systemNode.flush()

  val dirNode: Preferences = systemNode.node("directories")

  val setupNode: Preferences = systemNode.node("setup")

  def configDir: String = checkSeparator(dirNode.get("ConfigDir", "config"))

  def dataDir: String = checkSeparator(dirNode.get("DataDir", "data"))

  def backupDir: String = checkSeparator(dirNode.get("BackupDir", "backup"))

  def logDir: String = checkSeparator(dirNode.get("LogDir", "log"))

  def deployDir: String = checkSeparator(dirNode.get("DeployDir", "deploy"))

  def serverPort: Int = setupNode.getInt("ServerPort", 9000)

  def webServerPort: Int = setupNode.getInt("WebServerPort", 80)

  def certDomain: String = setupNode.get("CertDomain", "")

  def certAcmeURL: String = setupNode.get("CertAcmeURL", "acme://letsencrypt.org")

  def certRootFolder: String = setupNode.get("CertRootFolder", "")

  def certEmail: String = setupNode.get("CertEmail", "")


  def keyStoreFile = new File(FSPaths.configDir + "keystore.p12")
  
  //def backupHour=setupNode.getInt("BackupHour",1)

  def lastBackup: Long = setupNode.getLong("LastBackup", 0)

  def lastStatTransID: Int = setupNode.getInt("LastStatTransID", 0)
  
  lazy val settingsObjectRef=Reference(setupNode.get("SettingsObject","30,1"))  
  
  lazy val projectRootRef=Reference(setupNode.get("ProjectRoot","110,6"))
  
  lazy val libraryRootRef=Reference(setupNode.get("LibraryRoot","110,11"))
  
  lazy val userRootRef=Reference(setupNode.get("UserRoot","110,413"))
  
  private def checkSeparator(path:String) =  	
  	if(path.last==File.pathSeparatorChar) path else path+File.separator

  def setConfigDir(newDir: String): Unit = dirNode.put("ConfigDir", newDir)

  def setDataDir(newDir: String): Unit = dirNode.put("DataDir", newDir)

  def setBackupDir(newDir: String): Unit = dirNode.put("BackupDir", newDir)

  def setDeployDir(newDir: String): Unit = dirNode.put("DeployDir", newDir)

  def setLogDir(newDir: String): Unit = dirNode.put("LogDir", newDir)

  def setServerPort(newPort: Int): Unit = setupNode.putInt("ServerPort", newPort)

  def setWebServerPort(newPort: Int): Unit = setupNode.putInt("WebServerPort", newPort)
  //def setBackupHour(newTime:Int)=setupNode.putInt("BackupHour",newTime)
  def setSettingsRef(name: String, newRef: Reference): Unit = setupNode.put(name, newRef.sToString)

  def setLastBackup(time: Long): Unit = setupNode.putLong("LastBackup", time)

  def setLastStatTransID(newID: Int): Unit = setupNode.putInt("LastStatTransID", newID)

  def setCertDomain(n: String): Unit = setupNode.put("CertDomain", n)

  def setCertAcmeURL(n: String): Unit = setupNode.put("CertAcmeURL", n)

  def setCertRootFolder(n: String): Unit = setupNode.put("CertRootFolder", n)

  def setCertEmail(n: String): Unit = setupNode.put("CertEmail", n)

}