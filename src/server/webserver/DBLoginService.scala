package server.webserver


import java.io.{DataInputStream, File, FileInputStream}

import org.eclipse.jetty.security.{DefaultIdentityService, MappedLoginService}
import org.eclipse.jetty.server.UserIdentity
import org.eclipse.jetty.util.security.Credential
import server.comm.UserList
import definition.comm.UserSetting
import org.eclipse.jetty.security.MappedLoginService.KnownUser
import server.config.FSPaths
import util.CollUtils

import scala.collection.mutable


object DBLoginService extends MappedLoginService {
  protected val identityService = new DefaultIdentityService()

  override def loadUser(s: String): UserIdentity = null

  override def loadUsers(): Unit = { }

  override def doStart(): Unit = for(u<-UserList.userIterator)
     this.putUser(u.name,Credential.getCredential(u.password),u.roles)

  def getUser(name:String)=  UserList.getUserByName(name)

  /*def readUserSetting(userName:String)={
    val file=new File(FSPaths.configDir+userName+"_Table.set")
    if(!file.exists||file.length==0) None
    else {
      CollUtils.tryWith(new DataInputStream(new FileInputStream(file)))(stream=>{
        val setting=new UserSetting
        setting.readFromStream(stream,file.length.toInt,false)
        setting
      })
    }
  }*/
  override def loadUserInfo(s: String): KnownUser = null

  override def loadRoleInfo(knownUser: KnownUser): Array[String] = null
}


