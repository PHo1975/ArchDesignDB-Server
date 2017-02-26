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
	val systemNode=Preferences.userNodeForPackage(this.getClass)
	systemNode.flush()
	
	val dirNode =systemNode.node("directories")
	
	val setupNode=systemNode.node("setup")
	
  def configDir=checkSeparator( dirNode.get("ConfigDir","config"))
  
  def dataDir=checkSeparator( dirNode.get("DataDir","data"))
  
  def backupDir=checkSeparator( dirNode.get("BackupDir","backup"))
  
  def logDir=checkSeparator( dirNode.get("LogDir","log"))

  def deployDir=checkSeparator(dirNode.get("DeployDir","deploy"))
  
  def serverPort=setupNode.getInt("ServerPort",9000)
  
  //def backupHour=setupNode.getInt("BackupHour",1)
  
  def lastBackup=setupNode.getLong("LastBackup",0)
  def lastStatTransID=setupNode.getInt("LastStatTransID",0)
  
  lazy val settingsObjectRef=Reference(setupNode.get("SettingsObject","30,1"))  
  
  lazy val projectRootRef=Reference(setupNode.get("ProjectRoot","110,6"))
  
  lazy val libraryRootRef=Reference(setupNode.get("LibraryRoot","110,11"))
  
  lazy val userRootRef=Reference(setupNode.get("UserRoot","110,413"))
  
  private def checkSeparator(path:String) =  	
  	if(path.last==File.pathSeparatorChar) path else path+File.separator
  
  def setConfigDir(newDir:String)=dirNode.put("ConfigDir", newDir)
  def setDataDir(newDir:String)=dirNode.put("DataDir", newDir)
  def setBackupDir(newDir:String)=dirNode.put("BackupDir", newDir)
  def setDeployDir(newDir:String)=dirNode.put("DeployDir", newDir)
  def setLogDir(newDir:String)=dirNode.put("LogDir",newDir)
  def setServerPort(newPort:Int)=setupNode.putInt("ServerPort", newPort)
  //def setBackupHour(newTime:Int)=setupNode.putInt("BackupHour",newTime)
  def setSettingsRef(name:String,newRef:Reference)=setupNode.put(name,newRef.sToString)
  def setLastBackup(time:Long)=setupNode.putLong("LastBackup",time)
  def setLastStatTransID(newID:Int)=setupNode.putInt("LastStatTransID", newID)
  
  
}