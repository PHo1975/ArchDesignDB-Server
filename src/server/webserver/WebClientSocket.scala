package server.webserver


import java.io.{ByteArrayOutputStream, DataOutput, DataOutputStream, EOFException}
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
import util.{CollUtils, Log, StrToInt, StringUtils}

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



class WebClientSocket extends WebSocketAdapter with AbstractConnectionEntry with  AbstractQueryHandler with AbstractUserSocket {
  private var _userEntry: UserInfo = _
  var pinger: Option[Runnable] = None
  var mySession: Option[Session] = None

  def userEntry: UserInfo = _userEntry

  private val outStreamLock: AnyRef = new Object()

  protected var transBuffers: Option[TransBuffers] = None

  def userSocket: WebClientSocket = this

  def isAllowed(ref: Reference, user: UserInfo): Boolean = StorageManager.isChildOf(ref, user.startRef)

  // Interfcace WebSocketAdaber
  override def onWebSocketConnect(sess: Session): Unit = {
    super.onWebSocketConnect(sess)

    Log.w("connect sess address:" + sess.getRemoteAddress + ", user:" + sess.getUpgradeRequest.getUserPrincipal + " ")
    if (sess.getUpgradeRequest.getUserPrincipal != null || management.databrowser.MainWindow.testWS) {
      val userName = if (management.databrowser.MainWindow.testWS) "Peter" else sess.getUpgradeRequest.getUserPrincipal.getName
      if (management.databrowser.MainWindow.testWS) println("Option testWS active, test user choosen")
      UserList.getUserByName(userName) match {
        case Some(u) => _userEntry = u
        case None => Log.e("Login unknown user " + userName); _userEntry = null; return;
      }
      mySession = Some(sess)
      removePinger()
      pinger = Some(WebClientSocket.startRepeated {
        val remote = getRemote
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

  override def onWebSocketError(cause: Throwable): Unit = {
    super.onWebSocketError(cause)
    Log.e(cause)
  }

  private def readInt(array: Array[Byte], pos: Int): Int = {
    val ch1 = (array(pos) & 0xff)
    val ch2 = (array(pos + 1) & 0xff)
    val ch3 = (array(pos + 2) & 0xff)
    val ch4 = (array(pos + 3) & 0xff)
    //println("readint "+ ch1+" "+ch2+" "+ch3+" "+ch4)
    if ((ch1 | ch2 | ch3 | ch4) < 0) {
      println("Lowww");
      throw new EOFException("Wrong Values")
    }
    else (ch1 << 24.toByte) + (ch2 << 16.toByte) + (ch3 << 8.toByte) + (ch4 << 0)
  }

  /** is called when a Block is changed
  *
*/
  override def onWebSocketBinary(payload: Array[Byte], offset: Int, len: Int): Unit = if (len >= 17) {
    super.onWebSocketBinary(payload, offset, len)
    println("OnWebSocketBinary p:" + payload.size + " offset:" + offset + " len:" + len)
    //println("payload:"+payload.mkString("|"))
    val blockTyp = readInt(payload, offset)
    println("blockTyp " + blockTyp)
    val blockClass = AllClasses.get.blockClassList(blockTyp)
    if (len != blockClass.blocksize + 17) Log.e("Block class " + blockClass + " wrong Size, expected:" + blockClass.blocksize + "+17, found:" + len)
    else {
      val blockInst = readInt(payload, offset + 4)
      println("BlockInst: " + blockInst)
      val propField: Byte = payload(offset + 8);
      println("PropField:" + propField)
      val parentType = readInt(payload, offset + 9);
      println("Parenttyp:" + parentType)
      val parentInst = readInt(payload, offset + 13);
      println("ParentInst:" + parentInst)
      val ownerRef = OwnerReference(propField, Reference(parentType, parentInst))

      val data = new Array[Byte](blockClass.blocksize)
      Array.copy(payload, offset + 17, data, 0, blockClass.blocksize)
      //payload.copyToArray(data,offset+17,blockClass.blocksize)
      //println("data:"+data.mkString("|"))
      if (blockInst == -1) { // new Block, create it
        createBlock(blockTyp, ownerRef, data)
      } else { // block changed
        writeBlock(ownerRef, Reference(blockTyp, blockInst), data)
      }
    }
  } else Log.e("OnWebSocket length too short " + len)

  override def hashCode: Int = userEntry.hashCode() + mySession.hashCode()

  override def equals(other: Any): Boolean = other match {
    case that: WebClientSocket => this.userEntry == that.userEntry && this.mySession == that.mySession
    case _ => false
  }


  private def removePinger(): Unit = for (p <- pinger) {
    //println("removing old pinger")
    WebClientSocket.executor.remove(p)
  }

  private def cleanup(): Unit =
    if (userEntry != null) {
      removePinger()
      mySession = None
      pinger = None
      val uid = userEntry.id
      _userEntry = null
      UserList.removeConnection(uid, this)
    }


  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {
    Log.w("Sock user:" + userEntry.name + " adress:" + getRemoteAddress + " closed status:" + statusCode + " reason:" + reason)
    super.onWebSocketClose(statusCode, reason)
    cleanup()
  }


  protected def handleCommands(message: String): Unit =
    message.split("\\|", 2) match {
      case Array(command, data) => command match {
        case "CreateSubscription" => initSubscription(data)
        case "RemoveSubscription" => removeSubscription(data)
        case "LoadData" => loadData(data)
        case "ChangeRef" =>
        case "SubscribePath" => subscribePath(data)
        //case "Query"=> queryInstances(data)
        case "OpenChild" => openChild(data)
        case "JumpUp" => jumpUp(data)
        case "ChangeTableSetup" => changeTableSetup(data)
        case "WriteField" => writeField(data)
        case "WriteInstancesField" => writeInstancesField(data)
        case "Execute" => executeAction(data)
        case "ExecuteCreate" => executeAction(data)
        case "CreateInstance" => createInstance(data)
        case "DeleteInstance" => deleteInstance(data)
        case "Root" => sendRoot(data)
        case "SubscribeBlocks" => subscribeBlocks(data)
        case "DeleteBlock" => deleteBlock(data)
        case _ => Log.e("unknown Command " + command + " data:" + data)
      }
      case Array("SendTypes") => sendTypes()
      case Array("SendSystemSettings") => sendSystemSettings()
      case Array("CalendarRoots") => sendCalendarRoots()
      case _ => Log.e("Wrong message Data:" + message)
    }


  // Interface ConnectionEntry
  override def queryHandler: AbstractQueryHandler = this

  override def releaseUndoLock(): Unit = {}

  override def shutDown(): Unit =
    if (userEntry != null) {
      Log.w("shut down name:" + userEntry + (if (mySession.isDefined) " adress:" + mySession.get.getRemoteAddress else ""))
      cleanup()
      val session = this.getSession
      if (session != null) session.close()
    }


  override def userName: String = userEntry.name

  override def tellToQuit(): Unit = shutDown()

  override def getRemoteAddress: String = mySession match {
    case Some(s) => s.getRemoteAddress.toString;
    case _ => ""
  }

  override def sendUndoLockInfo(undoName: String): Unit = {}

  override def app: String = "Web"

  override def getPort: String = "8080"


  //*****************************************************************
  // Interface QueryHandler


  def sendData(command: ServerCommands.Value)(func: DataOutput => Unit): Unit = try {
    //println("send data "+command)
    outStreamLock.synchronized {
      val buffer = if (ActionList.isBufferingUpdates) getTransBuffers else new TransBuffers()
      buffer.output.writeInt(command.id)
      func(buffer.output)
      //output.flush()
      //println("send command " + command + " " + buffer.byteStream.size() + " bytes buffering:"+ActionList.isBufferingUpdates)
      if (!ActionList.isBufferingUpdates) {
        val remote = getRemote
        if (remote != null) remote.sendBytesByFuture(java.nio.ByteBuffer.wrap(buffer.byteStream.toByteArray))
        else {
          util.Log.e("senddata " + command + " datasize:" + buffer.byteStream.size() + " getRemote == null")
          shutDown()
        }
      }
    }
  } catch {
    case NonFatal(e) => util.Log.e(e)
  }


  // Client requests

  protected def sendRoot(nappName: String): Unit = if (userEntry != null) {
    appName = nappName
    sendUserSettings(null)
  }

  def sendCalendarRoots(): Unit = if (userEntry != null) {
    SystemSettings() match {
      case ses: ServerSystemSettings =>
        val rootRef = ses.getUserRoot(userEntry.name)
        val projects = StorageManager.loadChildren(rootRef, 202, 0)
        sendData(ServerCommands.sendCalendarData) { out => {
          out.writeInt(projects.size)
          for (p <- projects) StorageManager.getInstanceProperties(p.ref) match {
            case Some(props) =>
              p.ref.write(out)
              p.writeWithChildInfo(out)
              props.propertyFields(0).propertyList.head.write(out)
              props.propertyFields(1).propertyList.head.write(out)
            case None => throw new IllegalArgumentException("No Project-Props found " + p.ref + " " + p.fieldValue.head.toString)
          }
        }
        }
      case _ => Log.e("no Systemsettings found")
    }
  }

  protected def sendTypes(): Unit = {
    val ac = AllClasses.get
    val classList = ac.classList
    val blockClassList = ac.blockClassList
    sendData(ServerCommands.sendTypes) { out => {
      out.writeInt(classList.size)
      for (o <- classList.valuesIterator)
        o.writeToStream(out)
      out.writeInt(blockClassList.size)
      for (b <- blockClassList.valuesIterator)
        CollUtils.writeToStream(out, b)
      out.writeUTF(WebUserSetting.getTableSettingString(userEntry))
    }
    }
    //println("types send")
  }

  protected def initSubscription(data: String): Unit =
    data match {
      case WebClientSocket.PrFieldPattern(StrToInt(typ), StrToInt(inst), StrToInt(pr), pdata) =>
        val ref = Reference(typ, inst)
        if (userEntry != null && isAllowed(ref, userEntry))
          createSubscription(ref, pr.toByte, this)
        else {
          userSocket.sendData(ServerCommands.acceptSubscription) { out =>
            out.writeInt(-1)
            out.writeUTF("Acess to " + ref.sToString + " is not allowed for user " + userEntry)
          }
          Log.e("ref " + ref + " not allowed for " + userEntry)
        };
    }

  protected def subscribeBlocks(data: String): Unit = data match {
    case WebClientSocket.PrFieldPattern(StrToInt(typ), StrToInt(inst), StrToInt(pr), pdata) =>
      val ref = Reference(typ, inst)
      println("subscribe blocks " + ref + " " + pr)
      if (userEntry != null && isAllowed(ref, userEntry))
        createBlockSubscription(ref, pr.toByte, this)
      else {
        userSocket.sendData(ServerCommands.acceptSubscription) { out =>
          out.writeInt(-1)
          out.writeUTF("Acess to " + ref.sToString + " is not allowed for user " + userEntry)
        }
        Log.e("ref " + ref + " not allowed for " + userEntry)
      };
  }

  protected def loadData(data: String): Unit = data.split("\\|") match {
    case Array(Reference(parentRef), StrToInt(propertyField), StrToInt(dataTicket)) =>
      userSocket.sendData(ServerCommands.sendQueryResponse) { out =>
        out.writeInt(dataTicket)
        sendQueryData(out, parentRef, propertyField.toByte)
      }
    case o => Log.e("Wrong format load data " + o)
  }


  protected def removeSubscription(data: String): Unit = data match {
    case StrToInt(subsID) => CommonSubscriptionHandler.removeSubscription(subsID)
    case a => Log.e("wrong data:" + a)
  }

  protected def subscribePath(data: String): Unit = {
    val ref = data match {
      case Reference(r1) => r1
      case "root" => userEntry.startRef
      case o => Log.e("Wrong reference format " + o); userEntry.startRef
    }
    if (isAllowed(ref, userEntry))
      createPathSubscription((ref :: StorageManager.getPathToParent(ref, userEntry.startRef)).reverse, this)
    else {
      userSocket.sendData(ServerCommands.acceptSubscription) { out =>
        out.writeInt(-1)
        out.writeUTF("Acess to " + ref.sToString + " is not allowed for user " + userEntry.name)
      }
      Log.e("ref" + ref + " not allowed for " + userEntry)
    }
  }

  protected def openChild(data: String): Unit = data.split("\\|") match {
    case Array(StrToInt(subsID), Reference(newRef)) => openChild(subsID, newRef)
    case o => Log.e("wrong ref style " + data)
  }

  protected def jumpUp(data: String): Unit = data.split("\\|") match {
    case Array(StrToInt(subsID), StrToInt(pos)) =>
      CommonSubscriptionHandler.jumpUp(subsID, pos)
    case o => Log.e("Wrong style " + o)
  }

  protected def changeTableSetup(data: String): Unit = data.split("§") match {
    case Array(StrToInt(typeID), settingsString) => WebUserSetting.writeTableSetting(userEntry, typeID, settingsString)
    case other => Log.e("set Table Setup wrong string:" + data)
  }

  protected def sendError(error:CommandError): Unit =
    sendData(ServerCommands.sendCommandResponse ) {out =>
      out.writeBoolean(true)
      error.write(out)
    }

  protected def writeField(data: String): Unit = {
    var error:CommandError=null
    data.split("\\|") match {
      case Array(Reference(ref), StrToInt(field), Decode(expression)) => try {
        for(ret<-TransactionManager.doTransaction(userEntry.id, ClientCommands.writeField.id.toShort,
          ref, false, 0, {
            if (!TransactionManager.tryWriteInstanceField(ref, field.toByte, expression))
              error=new CommandError("cant write field " + field + " in ref:" + ref + " ex:" + expression,ClientCommands.writeField.id,0)
          }))
          error=new CommandError(ret.getMessage,ClientCommands.writeField.id,0)
      }
      catch {
        case NonFatal(e) =>
          error=new CommandError("write Field " + field + " wrong String: " + data + " in ref:" + ref+" :"+ e,ClientCommands.writeField.id,0);
          Log.e(e)
      }
      case o: Array[String] => error=new CommandError("writeField wrong data " + o.mkString(" | "),ClientCommands.writeField.id,0)
    }
    if(error!=null) sendError(error)
  }



  protected def writeInstancesField(data:String):Unit ={
    var error:CommandError=null
    data.split("\\|") match {
      case Array(listString,StrToInt(field),Decode(expression)) => try {
        val refList: Array[Reference] =listString.split("#").flatMap { case Reference(ref) => Some(ref); case _ => None }
        if(refList.nonEmpty)
          for(ret<-TransactionManager.doTransaction(userEntry.id,ClientCommands.writeInstance.id.toShort,
            refList.head,false,0,{
              for(ref<-refList)
                if (!TransactionManager.tryWriteInstanceField(ref,field.toByte,expression))
                  error=new CommandError("cant write field "+field+" in ref:"+ref+" ex:"+expression,ClientCommands.writeInstance.id,0);Log.e(error.getMessage)
            }))
            error=new CommandError(ret.getMessage,ClientCommands.writeInstance.id,0)

      }
      catch {
        case NonFatal(e) => Log.e("write Fields "+field+" wrong String: " + data,e);error=new CommandError("write Fields "+field+" wrong String: " + data+" :"+e,ClientCommands.writeInstance.id,0)
      }
      case o:Array[String]=> Log.e("writeField wrong data "+o.mkString(" | "));error=new CommandError("writeField wrong data "+o.mkString(" | "),ClientCommands.writeInstance.id,0)
    }
    if(error!=null) sendError(error)
  }


  protected def writeBlock(owner:OwnerReference,ref:Reference,data:Array[Byte]): Unit={
    var error:CommandError=null
    try {

      for(ret<-TransactionManager.doTransaction(userEntry.id,ClientCommands.changeBlock.id.toShort,ref,false,-1,{
        TransactionManager.tryWriteBlock(owner,ref,data)
      }))
        error=new CommandError(ret.getMessage,ClientCommands.changeBlock.id,0)
    }catch {
      case NonFatal(e)=> Log.e("write Block "+ref,e);error=new CommandError(e.getMessage,ClientCommands.changeBlock.id,0)
    }
    if(error!=null) sendError(error)
  }


  protected def strToParam(st:String): (String, Constant) = {
    val ix=st.indexOf("\u2192")
    (st.substring(0,ix),Expression.decode(st.substring(ix+1,st.length))._1.getValue)
  }

  protected def strToFormatValues(st:String): (Int, Constant) = {
    val ix=st.indexOf("\u2192")
    (StringUtils.stringToInt(st.substring(0,ix)),Expression.decode(st.substring(ix+1,st.length))._1.getValue)
  }


  protected def executeAction(data:String):Unit= data.split("\\|") match {
    case Array(Reference(owner),StrToInt(propField),StrToInt(createType),actionName,paramText,formatText) =>
      val formatList:Array[(Int,Constant)]   = if(formatText.trim.length<2)Array.empty else formatText.split ('\u01c1').map (strToFormatValues)
      intExecute(Array(owner),actionName,paramText,createType,propField.toByte,formatList,createAction = true,EMPTY_OWNERREF)
    case Array(OwnerReference(owner),RefList(rList), actionName, paramText) => intExecute(rList, actionName, paramText,0,0,Array.empty, createAction = false,owner)
    case _=> Log.e("Execute action wrong syntax:"+data+"\n size:"+data.split("\\|").length)
  }

  def intExecute (rList: Array[Reference], actionName: String, paramText: String,newType:Int,propField:Byte,formList:Array[(Int,Constant)], createAction: Boolean,owner:OwnerReference): Unit = {
    //println("intExecute rlist:"+rList.mkString("|")+" create:"+createAction+" owner:"+owner)
    val paramList:Array[(String,Constant)] =if(paramText.trim.length==0)Array.empty else paramText.split ('\u01c1').map (strToParam)
    var error: CommandError = null
    val instList: Array[InstanceData] = for (r <- rList
                                             if StorageManager.instanceExists (r.typ, r.instance)) yield StorageManager.getInstanceData (r)
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

              case c: CreateActionImpl if createAction => c.func (this, instList, paramList.toSeq, newType, formList.toSeq)
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



  protected def simplyCreateInstance(parentList:Array[InstanceData],newType:Int,propField:Byte): Unit = {
    val ownerRef=new OwnerReference(propField,parentList.head.ref)
   TransactionManager.tryCreateInstance(newType,Array(ownerRef),notifyRefandColl = true)
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
              val inst= TransactionManager.tryCreateInstance(typ,ownerArray,notifyRefandColl = true)
              if (inst==null)	error=new CommandError("cant create Inst typ:"+typ,ClientCommands.createInstance.id,0)
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

      case _=> Log.e("create wrong syntax:"+data);sendError(new CommandError("create wrong syntax:"+data,ClientCommands.createInstance.id,0))
    }


  protected def createBlock(typ:Int,ownerRef:OwnerReference,data:Array[Byte]): Unit = {
    var error:CommandError=null
    var inst:Int = -1
    try {
      val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.createBlock.id.toShort,
        ownerRef.ownerRef ,false,typ, {
          inst=TransactionManager.tryCreateBlock(typ,ownerRef,data)
          if(inst== -1) error = new CommandError("cant create block "+typ+" owner:"+ownerRef,ClientCommands.createBlock.id,0)
        })
      for(transError<-ret) error=new CommandError(transError.getMessage,ClientCommands.createBlock.id,0)
    } catch {
      case NonFatal(e)=>
        Log.e("creating Block typ "+typ+" owner:"+ownerRef,e)
        error=new CommandError(e.getMessage,ClientCommands.createBlock.id,0)
    }
    sendData(ServerCommands.sendCommandResponse ) {out =>
      if(error!=null) {
        out.writeBoolean(true)
        error.write(out)
      }
      else {
        out.writeBoolean(false) // no errors
        out.writeBoolean(true)
        IntConstant(inst).write(out) // the result value
      }
    }
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

  def getTransBuffers: TransBuffers =transBuffers.getOrElse({
    val b=new TransBuffers()
    transBuffers=Some(b)
    b
  })

  protected def deleteBlock(data:String):Unit= {
    data.split("\\|") match{
      case Array(Reference(ref),OwnerReference(owner))=> try {
        val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.deleteBlock.id.toShort,
        ref,false,0,{
          TransactionManager.tryDeleteBlock(ref,owner)
        })
        for(error<-ret) Log.e("delete block",error)
      } catch {
        case NonFatal(e)=> Log.e("delete Block",e)
      }
      case _ => Log.e("delete bLock wrong syntax:"+data)
    }
  }



  def flushTransactionBuffers():Unit={
    //println("flush "+userEntry.name)
    for(buffer<-transBuffers){
      val remote = getRemote
      if (remote != null) remote.sendBytesByFuture(java.nio.ByteBuffer.wrap(buffer.byteStream.toByteArray))
      else util.Log.e("senddata flush getRemote == null")
    }
    transBuffers=None
  }

}