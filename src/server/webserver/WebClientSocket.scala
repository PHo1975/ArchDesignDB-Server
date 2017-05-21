package server.webserver


import java.io.{ByteArrayOutputStream, DataOutput, DataOutputStream}
import java.nio._
import java.util.concurrent._

import definition.comm.{ClientCommands, CommandError, ServerCommands}
import definition.data._
import definition.expression.{Constant, Decode, Expression, IntConstant}
import definition.typ.{AbstractObjectClass, AllClasses, ParamQuestion, SystemSettings}
import org.eclipse.jetty.websocket.api.{Session, WebSocketAdapter}
import org.eclipse.jetty.websocket.servlet.{WebSocketServlet, WebSocketServletFactory}
import server.comm._
import server.config.ServerSystemSettings
import server.storage._
import transaction.handling.{ActionList, TransactionManager}
import util.{Log, StrToInt}

import scala.util.control.NonFatal
import scala.util.matching.Regex

object WebClientSocket{
  val executor=new ScheduledThreadPoolExecutor(4)


  def startDelayed(task: =>Unit): ScheduledFuture[_] = executor.schedule(new Runnable(){override def run(): Unit = task},10,TimeUnit.SECONDS)
  def startRepeated(task: =>Unit): Runnable ={
    val runnable = new Runnable() {override def run(): Unit = { task }}
    executor.scheduleAtFixedRate(runnable,1,1,TimeUnit.MINUTES)
    runnable
  }

  val byteBuffer: ByteBuffer =ByteBuffer.wrap(Array[Byte](10))

  val PrFieldPattern: Regex ="""(\d+),(\d+),(-?\d+)(.*)""".r
  val classes: AllClasses[_ <: AbstractObjectClass] =AllClasses.get

}

/**
 * Created by Peter on  21.03.2015.
 */
@SerialVersionUID(4444L) class DBServlet extends WebSocketServlet{
  override def configure(webSocketServletFactory: WebSocketServletFactory): Unit =
    webSocketServletFactory.register(classOf[WebClientSocket])
}

class TransBuffers {
  val byteStream = new ByteArrayOutputStream()
  val output = new DataOutputStream(byteStream)
}



class WebClientSocket extends WebSocketAdapter with AbstractConnectionEntry with  AbstractQueryHandler with AbstractUserSocket{
  var userEntry:UserInfo=_
  var pinger:Option[Runnable]=None
  var mySession:Option[Session]=None
  private val outStreamLock : AnyRef = new Object()

  protected var transBuffers:Option[TransBuffers]=None

  def userSocket: WebClientSocket =this

  def isAllowed(ref:Reference,user:UserInfo): Boolean = StorageManager.isChildOf(ref,user.startRef)

  // Interfcace WebSocketAdaber
  override def onWebSocketConnect(sess: Session): Unit = {
    super.onWebSocketConnect(sess)

    Log.w("connect sess address:" + sess.getRemoteAddress + ", user:" + sess.getUpgradeRequest.getUserPrincipal)
    if (sess.getUpgradeRequest.getUserPrincipal != null) {
      val userName = sess.getUpgradeRequest.getUserPrincipal().getName()
      UserList.getUserByName(userName) match {
        case Some(u) => userEntry = u
        case None => Log.e("Login unknown user " + userName); userEntry = null; return;
      }
      mySession = Some(sess)
      removePinger()
      pinger = Some(WebClientSocket.startRepeated {
        val remote = getRemote()
        if (remote != null) remote.sendPing(WebClientSocket.byteBuffer)
        else throw new IllegalArgumentException("ping connection closed")
      })
      UserList.addConnection(userEntry.id, this)
    }
  }

  override def onWebSocketText(message: String): Unit = if (mySession.isDefined) {
    super.onWebSocketText(message)
    //println("Received:"+message+" "+Thread.currentThread().getName)
    handleCommands(message)
  }

  override def onWebSocketError(cause: Throwable): Unit ={
    super.onWebSocketError(cause)
    Log.e(cause)
  }

  override def hashCode: Int = userEntry.hashCode()+mySession.hashCode()

  override def equals(other: Any): Boolean = other match {
    case that: WebClientSocket=> this.userEntry == that.userEntry && this.mySession==that.mySession
    case _ => false
  }

  /*override def onWebSocketBinary(array:Array[Byte],offset:Int,len:Int)= {
    super.onWebSocketBinary(array,offset,len)
    println("binary:"+array.mkString("\\|")+"off:"+offset+" len:"+len)
  }*/

  private def removePinger()= for(p<-pinger){
    //println("removing old pinger")
    WebClientSocket.executor.remove(p)
  }

  private def cleanup()={
    if(userEntry!=null){
      removePinger()
      mySession=None
      pinger=None
      UserList.removeConnection(userEntry.id,this)
      userEntry=null
    }
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    Log.w("Sock user:" + userEntry.name + " adress:" + getRemoteAddress + " closed status:" + statusCode + " reason:" + reason)
    super.onWebSocketClose(statusCode, reason)
    cleanup()
  }


  protected def handleCommands(message: String): Unit =
    message.split("\\|",2) match {
      case Array(command,data)=> command match {
        case "CreateSubscription"=> initSubscription(data)
        case "RemoveSubscription"=> removeSubscription(data)
        case "ChangeRef"=>
        case "SubscribePath"=>subscribePath(data)
        //case "Query"=> queryInstances(data)
        case "OpenChild"=> openChild(data)
        case "JumpUp"=>jumpUp(data)
        case "ChangeTableSetup"=>changeTableSetup(data)
        case "WriteField"=> writeField(data)
        case "Execute"=> executeAction(data,false)
        case "CreateInstance"=>createInstance(data)
        case "DeleteInstance"=>deleteInstance(data)
        case "Root"=> sendRoot(data)
        case _=>Log.e("unknown Command "+command+" data:"+data)
      }
      case Array("SendTypes")=> sendTypes()
      case Array("SendSystemSettings")=> sendSystemSettings()
      case Array("CalendarRoots")=>sendCalendarRoots()
      case _=> Log.e("Wrong message Data:"+message)
    }



  // Interface ConnectionEntry
  override def queryHandler: AbstractQueryHandler = this

  override def releaseUndoLock(): Unit = {}

  override def shutDown(): Unit =
    if(userEntry!=null) {
      Log.w("shut down name:" + userEntry + " adress:" + mySession.get.getRemoteAddress)
      cleanup()
      val session=this.getSession
      if(session!=null) session.close()
    }


  override def userName: String = userEntry.name

  override def tellToQuit(): Unit = shutDown()

  override def getRemoteAddress: String = mySession match {case Some(s) => s.getRemoteAddress.toString(); case _ => ""}

  override def sendUndoLockInfo(undoName: String): Unit = {}

  override def app: String = "Web"

  override def getPort: String = "8080"


  //*****************************************************************
  // Interface QueryHandler


  def sendData(command:ServerCommands.Value)(func:DataOutput=>Unit): Unit = try {
    //println("send data "+command)
    outStreamLock.synchronized {
      val buffer=if(ActionList.isBufferingUpdates) getTransBuffers else new TransBuffers()
      buffer.output.writeInt(command.id)
      func(buffer.output)
      //output.flush()
      //println("send command " + command + " " + buffer.byteStream.size() + " bytes buffering:"+ActionList.isBufferingUpdates)
      if(!ActionList.isBufferingUpdates) {
        val remote = getRemote()
        if (remote != null) remote.sendBytesByFuture(java.nio.ByteBuffer.wrap(buffer.byteStream.toByteArray))
        else util.Log.e("senddata getRemote == null")
      }
    }
    } catch {
        case NonFatal(e) => util.Log.e(e)
    }



  // Client requests

  protected def sendRoot(nappName:String): Unit =  if(userEntry!=null){
    appName=nappName
    sendUserSettings(null)
  }

  def sendCalendarRoots(): Unit = if(userEntry!=null){
    SystemSettings() match {
      case ses: ServerSystemSettings =>
        val rootRef = ses.getUserRoot(userEntry.name)
        val projects=StorageManager.loadChildren(rootRef,202,0)
        sendData(ServerCommands.sendCalendarData){out=>{
          out.writeInt(projects.size)
          for(p<-projects) StorageManager.getInstanceProperties(p.ref) match {
            case Some(props)=>
              p.ref.write(out)
              p.writeWithChildInfo(out)
              props.propertyFields(0).propertyList.head.write(out)
              props.propertyFields(1).propertyList.head.write(out)
            case None => throw new IllegalArgumentException("No Project-Props found " + p.ref + " " + p.fieldValue.head.toString)
          }
        }}
      case _ => Log.e("no Systemsettings found")
    }
  }

  protected def sendTypes(): Unit = {
    val ac=AllClasses.get.classList
    sendData(ServerCommands.sendTypes){out=>{
      out.writeInt(ac.size)
      for(o<-ac.valuesIterator)o.writeToStream(out)
      out.writeUTF(WebUserSetting.getTableSettingString(userEntry))
    }
    }
    //println("types send")
  }

  protected def initSubscription(data:String):Unit=
    data match {
      case WebClientSocket.PrFieldPattern(StrToInt(typ), StrToInt(inst), StrToInt(pr), pdata) =>
        val ref = Reference(typ, inst)
        if (userEntry!=null && isAllowed(ref, userEntry))
          createSubscription(ref,pr.toByte,this)
        else {
          userSocket.sendData(ServerCommands.acceptSubscription ) { out=>
            out.writeInt(-1)
            out.writeUTF("Acess to "+ref.sToString+" is not allowed for user "+userEntry)
          }
          Log.e("ref " + ref + " not allowed for " + userEntry)
        };
    }


  protected def removeSubscription(data:String): Unit =data match {
    case StrToInt(subsID)=> CommonSubscriptionHandler.removeSubscription(subsID)
    case a=> Log.e("wrong data:"+a)
  }

  protected def subscribePath(data:String): Unit ={
    val ref= data match {
      case Reference(r1) => r1
      case "root" => userEntry.startRef
      case o => Log.e("Wrong reference format "+o);userEntry.startRef
    }
    if(isAllowed(ref, userEntry))
      createPathSubscription((ref :: StorageManager.getPathToParent(ref,userEntry.startRef)).reverse,this)
    else {
      userSocket.sendData(ServerCommands.acceptSubscription) { out =>
        out.writeInt(-1)
        out.writeUTF("Acess to " + ref.sToString + " is not allowed for user " + userEntry.name)
      }
      Log.e("ref" + ref + " not allowed for " + userEntry)
    }
  }

  protected def openChild(data:String):Unit=    data.split("\\|") match {
    case Array(StrToInt(subsID),Reference(newRef))=> openChild(subsID,newRef)
    case o => Log.e("wrong ref style "+data)
  }

  protected def jumpUp(data:String): Unit = data.split("\\|") match {
    case Array(StrToInt(subsID),StrToInt(pos))=>
      CommonSubscriptionHandler.jumpUp(subsID,pos)
    case o=> Log.e("Wrong style "+o)
  }

  protected def changeTableSetup(data:String): Unit =data.split("§") match {
    case Array (StrToInt(typeID),settingsString)=> WebUserSetting.writeTableSetting(userEntry,typeID,settingsString)
    case other => Log.e("set Table Setup wrong string:"+data)
  }

  protected def writeField(data:String):Unit = data.split("\\|") match {
    case Array(Reference(ref),StrToInt(field),Decode(expression)) => try {
      TransactionManager.doTransaction(userEntry.id,ClientCommands.writeField.id.toShort,
      ref,false,0,{
        if (!TransactionManager.tryWriteInstanceField(ref,field.toByte,expression))
          Log.e("cant write field "+field+" in ref:"+ref+" ex:"+expression)
      })
    }
    catch {
      case NonFatal(e) => Log.e("write Field "+field+" wrong String: " + data+" in ref:"+ref,e)
    }
    case o:Array[String]=> Log.e("writeField wrong data "+o.mkString(" | "))
  }

  protected def strToParam(st:String): (String, Constant) = {
    val ix=st.indexOf("\u2192")
    (st.substring(0,ix),Expression.decode(st.substring(ix+1,st.length))._1.getValue)
  }

  protected def executeAction(data:String,createAction:Boolean):Unit= data.split("\\|") match {
    case Array(RefList(rList), actionName, paramText) => intExecute(rList, actionName, paramText, createAction)
    case Array(RefList(rList), actionName) => intExecute(rList,actionName,"",createAction)
    case _=> Log.e("Execute action wrong syntax:"+data)
  }

  def intExecute (rList: Array[Reference], actionName: String, paramText: String, createAction: Boolean): Unit = {
    val paramList:Array[(String,Constant)] =if(paramText.trim.length==0)Array.empty else paramText.split ('\u01c1').map (strToParam)
    val newType = 0
    val propField = 0.toByte
    val formList: Seq[(Int, Constant)] = Nil
    val owner = new OwnerReference (0, EMPTY_REFERENCE)
    var error: CommandError = null
    val instList = for (r <- rList
                        if StorageManager.instanceExists (r.typ, r.instance) ) yield StorageManager.getInstanceData (r)
    try {
      if (instList.isEmpty) throw new IllegalArgumentException ("Instanzliste ist leer  bei " + actionName + " " + paramText)
      val ret = TransactionManager.doTransaction (userEntry.id, ActionNameMap.getActionID (actionName),
        instList.head.ref, instList.length > 1, newType, {
          if (actionName == "*" && createAction && instList.length == 1) simplyCreateInstance (instList, newType, propField)
          else {
            val theAction = if (createAction) AllClasses.get.getClassByID (newType).asInstanceOf[ServerObjectClass].getCreateAction (actionName)
            else {
              val theClass = AllClasses.get.getClassByID (instList.head.ref.typ)
              if (theClass.actions.contains (actionName) ) theClass.actions (actionName)
              else throw new IllegalArgumentException ("Unknown Action '" + actionName + "' in class " + theClass.name)
            }
            theAction match {
              case a: ActionImpl => // simple action, order of execution is not important
                for ((typ, partList) <- instList.groupBy (_.ref.typ) ) {
                  val theAction = AllClasses.get.getClassByID (typ).actions (actionName).asInstanceOf[ActionImpl]
                  partList.foreach (a => theAction.func (this, a, paramList) )
                }

              case b: ActionIterator => // Iterator, runs through all instances in given order
                b.func (this, owner, instList, paramList)

              case c: CreateActionImpl if createAction => c.func (this, instList, paramList, newType, formList)
              case e => Log.e ("unknown type " + e + " " + createAction)
            }
          }
        })
      for (transError <- ret) error = new CommandError (transError.getMessage, ClientCommands.executeAction.id, 0)
    }
    catch {
      case e: Exception =>
        Log.e("intExecute action:"+actionName+" paramText:"+paramText,e)
        error = new CommandError (e.toString, ClientCommands.executeAction.id, 0)
      case e: Error => Log.e("intExecute action:"+actionName+" paramText:"+paramText,e)
        error = new CommandError (e.toString, ClientCommands.executeAction.id, 0)
    }
    sendData (ServerCommands.sendCommandResponse) {
      out =>
        if (error != null) {
          out.writeBoolean (true)
          error.write (out)
        }
        else {
          out.writeBoolean (false) // no errors
          out.writeBoolean (false) // no result
        }
    }
  }


  protected def simplyCreateInstance(parentList:Seq[InstanceData],newType:Int,propField:Byte): Unit = {
    val ownerRef=new OwnerReference(propField,parentList.head.ref)
   TransactionManager.tryCreateInstance(newType,Array(ownerRef),true)
  }

  def askEnquiry(question:ParamQuestion,continuation:(JavaClientSocket,Seq[(String,Constant)])=>Unit):Unit={

  }

  protected def createInstance(data:String): Unit =
    data.split("\\|") match {
      case Array(StrToInt(typ), OwnerRefList(ownerArray)) =>
        var error:CommandError=null
        var result:Int = -1
        try {
          val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.createInstance.id.toShort,
            ownerArray.head.ownerRef ,false,typ,{
              val inst= TransactionManager.tryCreateInstance(typ,ownerArray,true)
              if (inst==null)	error=new CommandError("Unknown Issue",ClientCommands.createInstance.id,0)
              else result=inst.ref.instance
            })
          for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.createInstance.id,0)
        }
        catch {
          case e:Exception =>
            Log.e("createInstance data:"+data,e)
            error=new CommandError(e.toString,ClientCommands.createInstance.id,0)
        }
        sendData(ServerCommands.sendCommandResponse ) {out =>
          if(error!=null) {
            out.writeBoolean(true)
            error.write(out)
          }
          else {
            out.writeBoolean(false) // no errors
            out.writeBoolean(true)
            IntConstant(result).write(out) // the result value
          }
        }

      case _=> Log.e("create wrong syntax:"+data)
    }


  protected def deleteInstance(data:String): Unit =
    data match {
      case Reference(ref)=>
        try {
          val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.deleteInstance.id.toShort,
            ref ,false,0,{
               TransactionManager.tryDeleteInstance(ref,None,None)
            })
          for(transError <-ret) Log.e(transError)
        }
        catch {
          case e:Exception =>
            Log.e("deleteInstance data:"+data,e)
        }
      case _=> Log.e("create wrong syntax:"+data)
    }

  def getTransBuffers: TransBuffers =transBuffers match {
    case Some(b)=>b
    case None=> val b=new TransBuffers()
      transBuffers=Some(b)
      b

  }

  def flushTransactionBuffers():Unit={
    //println("flush "+userEntry.name)
    for(buffer<-transBuffers){
      val remote = getRemote()
      if (remote != null) remote.sendBytesByFuture(java.nio.ByteBuffer.wrap(buffer.byteStream.toByteArray))
      else util.Log.e("senddata flush getRemote == null")
    }
    transBuffers=None
  }

}