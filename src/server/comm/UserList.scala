/**
 * Author: Peter Started:29.08.2010
 */
package server.comm

import definition.data.Reference
import server.config.FSPaths
import server.storage.StorageManager
import transaction.handling.TransactionManager
import util.Log

import scala.collection.mutable

trait AbstractConnectionEntry{
  def queryHandler:AbstractQueryHandler
  def app:String
  def userName:String
  def tellToQuit():Unit
  def sendUndoLockInfo(undoName:String): Unit
  def releaseUndoLock():Unit

  def getRemoteAddress: String
  def getPort:String
  def flushTransactionBuffers():Unit
  def shutDown():Unit
}

case class ConnectionEntry(app:String, thread:JavaClientSocket, queryHandler:AbstractQueryHandler) extends AbstractConnectionEntry {
  override def toString: String = "Connection " + app
  def tellToQuit(): Unit =thread.tellToQuit()
  def sendUndoLockInfo(undoName:String): Unit =thread.sendUndoLockInfo(undoName)
  def releaseUndoLock(): Unit =thread.releaseUndoLock()

  def getRemoteAddress: String = thread.socket.getRemoteSocketAddress.toString + " " + thread.socket.getInetAddress.getCanonicalHostName

  def getPort: String = thread.socket.getPort.toString
  def shutDown(): Unit ={
    //thread.userEntry=null
    thread.wantRun=false
    thread.socket.close()
    thread.interrupt()
  }
  def userName: String =thread.userName
  def flushTransactionBuffers():Unit= thread.flushTransactionBuffers()
}


/**
 * 
 */
case class UserInfo(name:String,id:Short,password:String,shortName:String, roles:Array[String],startRef:Reference) {
  var connections:List[AbstractConnectionEntry]=Nil
  def toXML:scala.xml.Node = {
    <User name={name} id={id.toString} password={password} shortName={shortName} roles={roles.mkString(",")} startRef= {startRef.bToString}> </User>
  }

  lazy val isEditable: Boolean = !roles.contains("guest")
  override def toString: String =name
  def flattenSize: Int =if(connections.isEmpty)1 else connections.size
}


object UserInfo {
  def fromXML(node: scala.xml.Node): UserInfo = {
    val name=(node \"@name").text
    val id=(node \"@id").text.toShort
    val password = (node \"@password").text
    val shortName=(node \"@shortName").text
    UserInfo(name,id,password,shortName,(node \"@roles").text.split(","),Reference((node\"@startRef").text))
  }
}

trait ChangeSender {
  val changeListeners: mutable.HashSet[() => Unit] = collection.mutable.HashSet[() => Unit]()
  def notifyListeners():Unit= for(li<-changeListeners) li()
  def registerListener(listener:()=>Unit): Unit = changeListeners+= listener

}


object UserList extends ChangeSender {

  protected var theMap: mutable.Map[Short, UserInfo] = mutable.Map[Short, UserInfo]()
  protected var wantQuit=false
  protected var finalFunc: ()=>Unit=_

  def toXML:scala.xml.Node = {
    val l=theMap.map(x => x._2.toXML)
    <UserList> {for (u <-theMap) yield u._2 .toXML} </UserList>
  }

  def fromXML(node: scala.xml.Node): Unit = {
    theMap = collection.mutable.Map[Short,UserInfo]()++= (for(userNode <- node \\ "User";newUser=UserInfo.fromXML(userNode))
    yield {newUser.id -> newUser }).sortBy(_._1)
    //orderedList=list.values.toSeq.sortBy(_.id)
    notifyListeners()
  }

  def addUser(newUser:UserInfo): Unit = {
    theMap(newUser.id)=newUser		//orderedList=list.values.toSeq.sortBy(_.id)
    notifyListeners()
  }


  def getUserName(id:Short): String = theMap.get(id) match {
    case Some(entry)=> entry.name
    case None=>"unknown"
  }

  def getUserByName(name:String): Option[UserInfo] = theMap.values.find(_.name==name)


  def isOnline(userID:Short,appName:String):Boolean = theMap.get(userID) match {
    case Some(user)=>user.connections.exists(_.app==appName)
    case None=>false
  }

  def addConnection(userID:Short,connection:AbstractConnectionEntry):Boolean =  theMap.get(userID) match {
    case Some(user)=> if(user.connections.exists(_.app==connection.app)) false
    else {
      user.connections=connection::user.connections
      //System.out.println("addUser "+user.name+" app:"+connection.app)
      notifyListeners()
      true
    }
    case None=>  false
  }

  def removeConnection(userID:Short,connection:AbstractConnectionEntry): Unit = {
    theMap.get(userID) match {
      case Some(user)=> if(user.connections.contains(connection)){
        user.connections=user.connections.filterNot(_ ==connection)
        TransactionManager.userLogsOff(userID)
        StorageManager.safeFlush()
        CommonSubscriptionHandler.userLogsOff(connection)
        //System.out.println("Remove user "+userID+" " +wantQuit)
        connection.shutDown()
        checkFinish()
        notifyListeners()
      } else Log.e("removing Connection for "+user+" but no active connection found")
      case None => println("Remove connection for unknown user :"+userID)
    }
  }

  def flushTransactionBuffers():Unit= for(entry<-theMap.valuesIterator){
    for (connection<-entry.connections)
      connection.flushTransactionBuffers()
  }


  def shutDown(nf:()=>Unit): Unit = {
    finalFunc=nf
    wantQuit=true
    checkFinish()
    System.out.println("Shutting Down UserList size:" +theMap.size)
    for (u <-theMap.values;c<-u.connections)
      c.tellToQuit()
    theMap.clear()
  }

  def disconnectUsers(): Unit ={
    for (u <-theMap.values;c<-u.connections)
      c.tellToQuit()
    theMap.clear()
  }

  def checkFinish(): Unit = if(wantQuit && !usersOnline && (finalFunc!=null)) finalFunc()

  def lockUsersForUndo(undoUser:ConnectionEntry): Unit = {
    val undoName=undoUser.thread.userName
    for( u<-theMap.values;c<-u.connections/*;if(c!=undoUser)*/)
      c.sendUndoLockInfo(undoName)
  }

  def releaseUsersForUndo(undoUser:ConnectionEntry): Unit = {
    for( u<-theMap.values;c<-u.connections/*;if(c !=undoUser)*/)
      c.releaseUndoLock()
  }

  def usersOnline: Boolean = theMap.values.exists(_.connections.nonEmpty)

  def findConnection(row:Int):(UserInfo,AbstractConnectionEntry)= {
    var pos=0
    for(u<-theMap.values) {
      pos+=u.flattenSize
      if(pos>row) return (u,if(u.connections.isEmpty) null else u.connections(row-(pos-u.flattenSize)))
    }
    (null,null)
  }

  def saveUserList(): Unit = scala.xml.XML.save(FSPaths.configDir+"users.xml",toXML,"UTF-8",xmlDecl = true,null)

  def userIterator: Iterator[UserInfo] =theMap.valuesIterator

  def numUsers: Int =theMap.size
}