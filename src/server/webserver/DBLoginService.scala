package server.webserver


import org.eclipse.jetty.security.{DefaultIdentityService, HashLoginService, UserStore}
import org.eclipse.jetty.util.security.Credential
import server.comm.{UserInfo, UserList}




object DBLoginService extends HashLoginService {
  protected val identityService = new DefaultIdentityService()


  override def doStart(): Unit = {
    val userStore=new UserStore
    for(u<-UserList.userIterator)
      userStore.addUser(u.name,Credential.getCredential(u.password),u.roles)
    this.setUserStore(userStore)
  }

  def getUser(name:String): Option[UserInfo] =  UserList.getUserByName(name)


}


