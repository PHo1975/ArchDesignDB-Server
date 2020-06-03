/**
  * Author: Peter Started:29.08.2010
  */
package server.comm

import java.io._
import java.net.{Socket, SocketException}
import java.util.zip.Deflater

import definition.comm.{ClientCommands, CommandError, ServerCommands}
import definition.data._
import definition.expression.{Constant, Expression, IntConstant}
import definition.typ.{AllClasses, ParamQuestion, SystemSettings}
import server.config.{FSPaths, ServerSystemSettings}
import server.storage._
import transaction.handling.{ActionList, TransactionManager}
import util.{CollUtils, Log, StringUtils}

import scala.util.control.NonFatal

/** manages communications with a client
  *
  */

trait AbstractUserSocket {
  def sendData(command:ServerCommands.Value)(func:  (DataOutput)=>Unit) :Unit
  def askEnquiry(question:ParamQuestion,continuation:(JavaClientSocket,Seq[(String,Constant)])=>Unit):Unit
  protected def userEntry:UserInfo
  def userName: String =userEntry.name
  def userID:Short=userEntry.id

  var appName:String=""

  def sendSystemSettings(): Unit = sendData(ServerCommands.sendSystemSettings) { out =>
    //println("Sending system settings")
    out.writeInt(userEntry.id)
    SystemSettings().asInstanceOf[ServerSystemSettings].write(out)
  }

  def getConfigFile =new File(FSPaths.configDir+userEntry.name+"_"+appName+".set")

  def sendUserSettings(st:DataInputStream):Unit = {
    val file=getConfigFile
    sendData(ServerCommands.sendUserSettings ) {out =>
      out.writeBoolean(userEntry.isEditable)
      out.writeInt(userEntry.id)
      userEntry.startRef.write(out)
      if(!file.exists||file.length==0) out.writeInt(0)
      else {
        //println("config file "+file)
        CollUtils.tryWith(new FileInputStream(file))(in=>{
          val l=file.length.toInt
          //System.out.println("read Settings: "+l)
          val readBuffer= new Array[Byte](l)
          in.read(readBuffer,0,l)
          out.writeInt(l)
          out.write(readBuffer,0,l)
        })
      }
      // readUserFolders
      SystemSettings() match {
        case ses:ServerSystemSettings=>
          val rootRef=ses.getUserRoot(userEntry.name)
          val folderList=StorageManager.getInstPropList(rootRef,3)
          rootRef.write(out)
          out.writeInt(folderList.size)
          for(fref<-folderList) {
            out.writeUTF(StorageManager.getInstanceData(fref).fieldValue.head.toString)
            fref.write(out)
          }
        case _ =>EMPTY_REFERENCE.write(out);out.writeInt(0)
      }
      // userlist
      out.writeInt(UserList.numUsers)
      for(userData<-UserList.userIterator){
        out.writeInt(userData.id)
        out.writeUTF(userData.name)
        out.writeUTF(userData.shortName)
      }

      // readKeyStrokes
      val ksFile=new File(FSPaths.configDir+userEntry.name+".kst")
      if(ksFile.exists) {
        //println("Keystrokes "+ksFile)
        val in=new DataInputStream(new FileInputStream(ksFile))
        val numGroups=in.readInt
        out.writeInt(numGroups)
        for(gr<-0 until numGroups) {
          out.writeUTF(in.readUTF)
          val numCommands=in.readInt
          out.writeInt(numCommands)
          for(c <-0 until numCommands) {
            out.writeUTF(in.readUTF)
            out.writeInt(in.readInt)
            out.writeInt(in.readInt)
          }
        }
        in.close()
      }else out.writeInt(0)
    }
  }
}

class JavaClientSocket(val socket: Socket) extends Thread with AbstractUserSocket {

  private val outStreamLock : AnyRef = new Object()
  private val out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
  protected var userEntry:UserInfo=_
  @volatile var wantRun=true
  var connectionEntry:ConnectionEntry=_


  type CommandHandlerFunc= (DataInputStream)=>Unit

  private val commandHandlerMap= scala.collection.mutable.HashMap[ClientCommands.Value,CommandHandlerFunc]()
  val queryHandler=new UserQueryHandler(this)
  var currentEnquiryContinuation:(JavaClientSocket,Seq[(String,Constant)])=>Unit = _

  def user: UserInfo =userEntry


  // register routines
  registerCommandHandler(ClientCommands.writeField) (wantWriteField)
  registerCommandHandler(ClientCommands.writeMultiFields) (wantWriteMultiFields)
  registerCommandHandler(ClientCommands.writeInstance)(wantWriteInstance)
  registerCommandHandler(ClientCommands.createInstance)(wantCreateInstance)
  registerCommandHandler(ClientCommands.createInstances)(wantCreateInstances)
  registerCommandHandler(ClientCommands.deleteInstance)(wantDeleteInstance)
  registerCommandHandler(ClientCommands.convertInstances)(convertTypes)
  registerCommandHandler(ClientCommands.copyInstances)(wantCopyInstances)
  registerCommandHandler(ClientCommands.moveInstances)(wantMoveInstances)
  registerCommandHandler(ClientCommands.executeAction  )(executeAction(createAction = false))
  registerCommandHandler(ClientCommands.executeCreateAction  )(executeAction(createAction = true))
  registerCommandHandler(ClientCommands.getUserSettings  )(sendUserSettings)
  registerCommandHandler(ClientCommands.writeUserSettings  )(writeUserSettings)
  registerCommandHandler(ClientCommands.requestUndoData  )(requestUndoData)
  registerCommandHandler(ClientCommands.undo  )(doUndo)
  registerCommandHandler(ClientCommands.stopUndo  )(stopUndo)
  registerCommandHandler(ClientCommands.secondUseInstances )(create2ndUseCopies)
  registerCommandHandler(ClientCommands.answerEnquiry  )(answerEnquiry)
  registerCommandHandler(ClientCommands.writeKeyStrokes) (writeKeyStrokes)


  override def run ():Unit = { // receiving loop
    try {
      val in = new DataInputStream( new BufferedInputStream( socket.getInputStream))
      try	{
        val userName=in.readUTF()
        val passWord=in.readUTF()
        appName=in.readUTF()
        System.out.print("user "+userName+" logged in ")
        Thread.`yield`()
        UserList.userIterator.find(_.name==userName) match {
          case Some(u) =>
            userEntry=u
            if(! (passWord==StringUtils.deobfuscate(userEntry.password)))
              writeOut("Wrong password")
            else
            if(userEntry.connections.exists(_.app==appName))
              writeOut("User "+userName+" with app:"+appName+" already online" )
            else {
              writeOut("welcome")
              System.out.println("and added")
              connectionEntry = ConnectionEntry(appName, this, queryHandler)
              UserList.addConnection(userEntry.id, connectionEntry)
              // start command loop
              handleCommands(in, userEntry)
            }

          case None=> writeOut("User "+userName+" not known")
        }
      }
      finally {
        out.close()
        in.close()
        socket.close()
      }
    }
    catch {
      case e: SocketException => Log.w("Client logged out: "+e.getMessage)
      // avoid stack trace when stopping a client with Ctrl-C
      case e: IOException =>
        Log.e("socked failed",e)
    }
  }

  private def writeOut(st: String): Unit = outStreamLock.synchronized {out.writeUTF(st); out.flush()}


  private def handleCommands(in:DataInputStream,user:UserInfo):Unit = {

    try {
      while(wantRun)
      {
        val command =ClientCommands(in.readByte.toInt)
        //System.out.println("User:"+userEntry.name+" ClientCommand:"+command)
        try {
          command match {
            case ClientCommands.getTypes => sendTypes()
            case ClientCommands.getSystemSettings=> sendSystemSettings()
            case ClientCommands.logOut => wantRun=false
            //case ClientCommands.getSetupData => sendSetupData()
            case ClientCommands.storeSetupData=> storeSetupData()
            case a => if (commandHandlerMap.contains(a))commandHandlerMap(a)(in)
            else Log.e("unhandled command "+a)
          }
        }
        catch {
          case NonFatal(e) => Log.e("handle Commands user:"+user.name+" command:"+command,e)
        }
      }
    }
    finally {
      logoutUser()
    }
  }

  private def logoutUser(): Unit = 	{
    Log.w("user " + userEntry.name + " logged off")
    UserList.removeConnection(userEntry.id,connectionEntry)
  }

  def sendData(command:ServerCommands.Value)(func:  (DataOutput)=>Unit): Unit =
  //print(" $S:"+command.toString().substring(0,10))
    try {
      outStreamLock.synchronized {
        out.writeByte(command.id.toByte)
        func(out)
        if (!ActionList.isBufferingUpdates) out.flush()
      }
    }
    catch {
      case e: IOException => Log.e("Send Data", e)
    }


  private def sendTypes(): Unit = 	{
    //System.out.println("Sending Types to "+userEntry.name)
    val outString=scala.xml.Utility.trim(AllClasses.get.asInstanceOf[ServerClassList].toXML).toString
    sendData(ServerCommands.sendTypes) {out=>
      val bytes=outString.getBytes("UTF-8")
      println("outString length:"+bytes.length)
      val df=new java.util.zip.Deflater(Deflater.BEST_COMPRESSION)
      df.setInput(bytes)
      df.finish()
      val newBuffer=Array.ofDim[Byte](bytes.size)
      val newBytes=df.deflate(newBuffer)
      println(" compr bytes:"+newBytes)
      out.writeInt(bytes.size)
      out.writeInt(newBytes)
      out.write(newBuffer,0,newBytes)
      //out.write(bytes)
      //println("ready sending Types to"+userEntry.name)
    }
  }



  private def storeSetupData(): Unit = {
    //System.out.println("store setup data")
  }

  def registerCommandHandler(command:ClientCommands.Value)(func:CommandHandlerFunc): Unit = {
    commandHandlerMap.put(command,func)
  }

  def sendError(error:CommandError): Unit =
    sendData(ServerCommands.sendCommandResponse ) {out =>
      out.writeBoolean(true)
      error.write(out)
    }

  def successNoResult():Unit =
    sendData(ServerCommands.sendCommandResponse ) {out =>
      out.writeBoolean(false) // no error
      out.writeBoolean(false) // no result
    }


  private def wantWriteField(in:DataInputStream): Unit = {
    var error:CommandError=null
    //util.Profile.start()
    val ref=Reference(in)
    val field=in.readByte
    val expr=Expression.read(in)
    //util.Profile.measure("params read")
    try {
      val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.writeField.id.toShort,
        ref,false,0,{
          if (!TransactionManager.tryWriteInstanceField(ref,field,expr))
            error=new CommandError("Unknown Issue",ClientCommands.writeField.id,0)
          //util.Profile.measure("try write")
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.writeField.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("write field "+ref+" field:"+field+" expr:"+expr,e)
        error=new CommandError(e.toString,ClientCommands.writeField.id,0)
    }
    if(error==null) successNoResult()
    else sendError(error)
  }


  private def wantWriteInstance(in:DataInputStream): Unit = {
    var error:CommandError=null
    val ref=Reference(in)
    val numFields=in.readByte
    val newValues=for(i<-0 until numFields) yield Expression.read(in)

    try {
      val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.writeInstance.id.toShort,
        ref,false,0,{
          val inst=StorageManager.getInstanceData(ref)
          TransactionManager.tryWriteInstanceData(inst.setFieldValues(newValues))
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.writeField.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("write instance ref:"+ref+" numFields:"+numFields+" numValues:"+newValues.size,e)
        error=new CommandError(e.toString,ClientCommands.writeField.id,0)
    }
    if(error==null) successNoResult()
    else sendError(error)
  }


  private def wantWriteMultiFields(in:DataInputStream): Unit = {
    var error:CommandError=null
    val numInst=in.readInt
    val refList=for(i <-0 until numInst) yield Reference(in)
    val field=in.readByte
    val expr=Expression.read(in)
    try {
      val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.writeMultiFields.id.toShort,
        refList.head,true,0,{
          for(ref<-refList)
            if (!TransactionManager.tryWriteInstanceField(ref,field,expr))
              error=new CommandError("Unknown Issue",ClientCommands.writeField.id,0)
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.writeMultiFields.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("WriteMultiFields numInst:"+numInst+" field:"+field+" ex:"+expr,e)
        error=new CommandError(e.toString,ClientCommands.writeField.id,0)
    }
    if(error==null) successNoResult()
    else sendError(error)
  }


  private def wantCreateInstance(in:DataInputStream): Unit = {
    var error:CommandError=null
    var result:Int = -1
    val typ=in.readInt
    val ownerCount=in.readInt
    val ownerArray:Array[OwnerReference]=(for (i <-0 until ownerCount) yield OwnerReference.read(in)).toArray

    try {
      val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.createInstance.id.toShort,
        ownerArray.head.ownerRef ,false,typ,{
          val inst= TransactionManager.tryCreateInstance(typ,ownerArray,notifyRefandColl = true)
          if (inst==null)	error=new CommandError("Unknown Issue",ClientCommands.createInstance.id,0)
          else result=inst.ref.instance
        })
      for (transError <- ret) {
        error = new CommandError(transError.getMessage, ClientCommands.createInstance.id, 0)
        Log.e("Create Instance " + transError.getMessage + " typ:" + typ + " owners:" + ownerArray.mkString(","))
      }
    }
    catch {
      case e:Exception =>
        Log.e("create Instance typ:"+typ+" owners:"+ownerArray.mkString(","),e)
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
  }

  private def wantCreateInstances(in:DataInputStream): Unit = {
    var error:CommandError=null
    val ownerCount=in.readInt
    val ownerArray:Array[OwnerReference]=(for (i <-0 until ownerCount) yield OwnerReference.read(in)).toArray
    val numInst=in.readInt
    val newObjects=new Array[(Int,IndexedSeq[Expression])](numInst)
    val checkLinks=in.readBoolean
    for(i<-0 until numInst) {
      val typ=in.readInt
      val numFields=in.readByte
      val fieldValues: IndexedSeq[Expression] =for(i<-0 until numFields) yield Expression.read(in)
      newObjects(i)=(typ,fieldValues)
    }
    try {
      val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.createInstances.id.toShort,
        ownerArray.head.ownerRef ,false,0,{
          for(i <- 0 until numInst;(typ,fieldValues)=newObjects(i)) {
            val inst= TransactionManager.tryCreateInstance(typ,ownerArray,notifyRefandColl = true)
            if (inst==null)	error=new CommandError("error creating typ "+typ,ClientCommands.createInstances.id,0)
            else {
              if(checkLinks)
                for(i<-fieldValues.indices; fv=fieldValues(i); if !fv.isNullConstant)
                  TransactionManager.tryWriteInstanceField(inst.ref,i.toByte, fv)
              else TransactionManager.tryWriteInstanceData(inst.setFieldValues(fieldValues))
            }
          }
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.createInstances.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("createInstances owners:"+ownerArray.mkString(",")+" numInst:"+numInst,e)
        error=new CommandError(e.toString,ClientCommands.createInstances.id,0)
    }
    if(error==null) successNoResult()
    else sendError(error)
  }


  private def wantDeleteInstance(in:DataInputStream): Unit = {
    var error:CommandError=null
    //var result:Constant=null
    val ref=Reference(in)
    val fromOwner=OwnerReference.read(in)
    try {
      val ret=TransactionManager.doTransaction (userEntry.id,ClientCommands.deleteInstance.id.toShort,
        ref,false,0,{
          if (!TransactionManager.tryDeleteInstance(ref,if(fromOwner.ownerRef.typ==0) None else Some(fromOwner),None))
            error=new CommandError("Unknown Issue",ClientCommands.deleteInstance.id,0)
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.deleteInstance.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("deleteInstance ref:"+ref+" from Owner:"+fromOwner,e)
        error=new CommandError(e.toString,ClientCommands.deleteInstance.id,0)
    }
    if(error==null) successNoResult()
    else sendError(error)
  }

  private def convertTypes(in:DataInputStream):Unit = {
    var error:CommandError=null
    val sources: Seq[Reference] =for(_<-0 until in.readInt) yield Reference(in)
    val ownerRef=Array(OwnerReference.read(in))
    val targetType=in.readInt
    val rules: Seq[(Int, Int)] = for(_<-0 until in.readInt) yield (in.readInt,in.readInt)
    val firstRef=sources.head
    try {
      val ret=TransactionManager.doTransaction (userEntry.id,ClientCommands.convertInstances.id.toShort,
        firstRef,false,0,{
          val sourcesInstances=sources.map(StorageManager.getInstanceData)

          for(sourceInst <-sourcesInstances){
            val targetInst =TransactionManager.tryCreateInstance(targetType,ownerRef,true)
            for((f1,f2)<-rules)
              TransactionManager.tryWriteInstanceField(targetInst.ref,f2.toByte,sourceInst.fieldData(f1).getValue)
          }
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.convertInstances.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("convertTypes ref:"+firstRef+" to Owner:"+ownerRef,e)
        error=new CommandError(e.toString,ClientCommands.convertInstances.id,0)
    }
    if(error==null) successNoResult()
    else sendError(error)
  }

  private def wantCopyInstances(in:DataInputStream): Unit = {
    copyOrMove(in,ClientCommands.copyInstances,move = false)
  }

  private def wantMoveInstances(in:DataInputStream): Unit = {
    copyOrMove(in,ClientCommands.moveInstances,move = true)
  }

  private def copyOrMove(in:DataInput,command:ClientCommands.Value,move:Boolean): Unit = {
    var error:CommandError=null
    val numInstances=in.readShort
    val refList:Seq[Reference] = for(i <- 0 until numInstances) yield Reference(in)
    val fromOwner:OwnerReference=OwnerReference.read(in)
    val toOwner:OwnerReference=OwnerReference.read(in)
    val atPos:Int = in.readInt
    //var instID=0
    //SimpleProfiler.startMeasure("start copy")
    try {
      val ret=TransactionManager.doTransaction(userEntry.id,command.id.toShort,
        refList.head,numInstances>1,0,{
          if(move)TransactionManager.tryMoveMultiInstances(refList,fromOwner,toOwner,atPos)
          else {
            val instID=TransactionManager.tryCopyMultiInstances(refList,fromOwner,toOwner,atPos)
            if(instID<0) error=new CommandError("Unknown Issue",command.id,0)
          }
          //System.out.println("actionList:"+ActionList.theList.mkString(" |"))
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,command.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("copy or move command:"+command+" move:"+move+" num:"+numInstances+" from:"+fromOwner+" to:"+toOwner+" at:"+atPos,e)
        error=new CommandError(e.toString,command.id,0)
    }
    sendData(ServerCommands.sendCommandResponse ) {out =>
      if(error!=null) {
        out.writeBoolean(true)
        error.write(out)
      }
      else {
        out.writeBoolean(false) // no errors
        out.writeBoolean(false)
        //IntConstant(instID).write(out)
      }
    }
  }

  private def create2ndUseCopies(in:DataInput): Unit = {
    var error:CommandError=null
    val numInstances=in.readShort
    val refList:Seq[Reference] = for(i <- 0 until numInstances) yield Reference(in)
    val fromOwner:OwnerReference=OwnerReference.read(in)
    val toOwner:OwnerReference=OwnerReference.read(in)
    val atPos:Int = in.readInt
    try {
      val ret=TransactionManager.doTransaction(userEntry.id,ClientCommands.secondUseInstances.id.toShort,
        refList.head,numInstances>1,0,{
          TransactionManager.trySecondUseInstances(refList,fromOwner,toOwner,atPos)
        })
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.secondUseInstances.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("create2ndUseCopies num:"+numInstances+" from:"+fromOwner+" to:"+toOwner,e)
        error=new CommandError(e.toString,ClientCommands.secondUseInstances .id,0)
    }
    sendData(ServerCommands.sendCommandResponse ) {out =>
      if(error!=null) {
        out.writeBoolean(true)
        error.write(out)
      }
      else {
        out.writeBoolean(false) // no errors
        out.writeBoolean(false) // no results
      }
    }
  }



  private def executeAction(createAction:Boolean)(in:DataInputStream): Unit = {

    //util.Profile.start()
    var error:CommandError=null
    val numInstances=in.readInt
    val refList=for(i <-0 until numInstances) yield Reference(in)
    val instList= for(r<-refList;if StorageManager.instanceExists(r.typ, r.instance)) yield StorageManager.getInstanceData(r)
    val newType=if(createAction)in.readInt else 0
    val propField=if(createAction)in.readByte else 0.toByte
    val actionName=in.readUTF
    //println("execute Action "+actionName+" num instances:"+numInstances+" reflist:"+refList.mkString("|")+"\n instList:"+instList.mkString("|"))
    val numParams=in.readInt
    val paramList=for(i <-0 until numParams)
      yield (in.readUTF,Expression.readConstant(in))
    val formList=if(createAction) {
      val numFormFields=in.readInt
      for(i<-0 until numFormFields)
        yield (in.readInt,Expression.readConstant(in))
    } else Nil
    //waitingForActionResult=true
    val owner=if(createAction)null else OwnerReference.read(in)
    //util.Profile.measure("Execute "+actionName+" params read")
    try {
      if(instList.isEmpty) throw new IllegalArgumentException("Instanzliste ist leer  bei "+actionName+" "+paramList.mkString("|"))
      val ret=TransactionManager.doTransaction(userEntry.id,ActionNameMap.getActionID(actionName),
        instList.head.ref,instList.size>1,newType,{
          if(actionName=="*" && createAction&&instList.size==1) simplyCreateInstance(instList,newType,propField)
          else {
            val theAction= if(createAction)	AllClasses.get.getClassByID(newType).asInstanceOf[ServerObjectClass].getCreateAction(actionName )
            else  {
              val theClass=AllClasses.get.getClassByID(instList.head.ref.typ).asInstanceOf[ServerObjectClass]
              if(theClass.actions.contains(actionName )) theClass.actions(actionName)
              else throw new IllegalArgumentException("Unknown Action '"+actionName+"' in class "+theClass.name+
                "\n module:"+theClass.actionModule+" name:"+theClass.moduleName)
            }
            theAction match {
              case a:ActionImpl => // simple action, order of execution is not important
                for ((typ,partList) <- instList.groupBy(_.ref.typ))              {
                  val theAction= AllClasses.get.getClassByID(typ).actions(actionName).asInstanceOf[ActionImpl]
                  partList.foreach(a => theAction.func(this,a,paramList))
                }
              case b:ActionIterator => // Iterator, runs through all instances in given order
                b.func(this,owner,instList,paramList)

              case c:CreateActionImpl if createAction => c.func(this,instList,paramList,newType,formList)
              case e => Log.e("unknown type "+e+" "+createAction)
            }
            //util.Profile.measure("Action tried")
          }
        })
      //util.Profile.measure("notification send")
      for(transError <-ret) error=new CommandError(transError.getMessage,ClientCommands.executeAction.id,0)
    }
    catch {
      case e:Exception =>
        Log.e("Execute action:"+actionName+" num:"+numInstances+" params:"+paramList.mkString(","),e)
        error=new CommandError(e.toString,ClientCommands.executeAction.id,0)
      case e:Error => Log.e("Execute action:"+actionName+" num:"+numInstances+" params:"+paramList.mkString(","),e)
        error=new CommandError(e.toString,ClientCommands.executeAction.id,0)
    }
    sendData(ServerCommands.sendCommandResponse ) { out =>
      if(error!=null) {
        out.writeBoolean(true)
        error.write(out)
      }
      else {
        out.writeBoolean(false) // no errors
        out.writeBoolean(false) // no result
      }
    }
    //util.Profile.measure("result send")
  }

  def simplyCreateInstance(parentList:Seq[InstanceData],newType:Int,propField:Byte): InstanceData = {
    val ownerRef=new OwnerReference(propField,parentList.head.ref)
    TransactionManager.tryCreateInstance(newType,Array(ownerRef),true)
  }


  def writeUserSettings(in:DataInputStream):Unit = {
    val file=getConfigFile
    val length=in.readInt
    val readBuffer=Array.ofDim[Byte](length)
    in.read(readBuffer,0,length)
    CollUtils.tryWith(new FileOutputStream(file))(out=>out.write(readBuffer,0,length))

  }

  def writeKeyStrokes(in:DataInputStream):Unit = {
    val numGroups=in.readInt
    val file=new File(FSPaths.configDir+userEntry.name+".kst")
    CollUtils.tryWith(new DataOutputStream(new FileOutputStream(file)))(out=>{
      out.writeInt(numGroups)
      for(gr<-0 until numGroups) {
        out.writeUTF(in.readUTF)
        val numCommands = in.readInt
        out.writeInt(numCommands)
        for (c <- 0 until numCommands) {
          out.writeUTF(in.readUTF)
          out.writeInt(in.readInt)
          out.writeInt(in.readInt)
        }
      }
    })
  }

  def tellToQuit(): Unit = {
    System.out.println("tell Quit to "+userEntry.name)
    sendData(ServerCommands.wantQuit ) {out =>}
  }


  def requestUndoData(in:DataInputStream): Unit = {
    //System.out.println("User "+userEntry.name+" requests Undo data")
    TransactionManager.requestUndoData(connectionEntry)
  }

  def doUndo(in:DataInputStream): Unit = {
    TransactionManager.doUndo(connectionEntry)
  }

  def stopUndo(in:DataInputStream): Unit = {
    System.out.println("Stop Undo ")
    TransactionManager.stopUndo(connectionEntry)
  }

  def denyUndoRequest(): Unit = {
    sendData(ServerCommands.sendUndoInformation ) {out =>
      out.writeBoolean(false)
    }
  }

  def sendUndoLockInfo(undoUserName:String): Unit = {
    sendData(ServerCommands.lockForUndo  ) {out =>
      out.writeUTF(undoUserName)
    }
  }

  def releaseUndoLock(): Unit = {
    println("rel "+userEntry.name +" "+appName)
    sendData(ServerCommands.releaseUndoLock  ) {out =>
    }
  }

  def sendUndoInformation(stepList:Seq[TransStepData]): Unit = {
    //System.out.println(stepList.mkString("\n"))
    sendData(ServerCommands.sendUndoInformation   ) {out =>
      out.writeInt(stepList.size)
      //System.out.println("StepList size:"+stepList.size)
      stepList.foreach(_.write(out))
    }
  }


  def askEnquiry(question:ParamQuestion,continuation:(JavaClientSocket,Seq[(String,Constant)])=>Unit): Unit = {
    /*if(waitingForActionResult) enquiryList+=((question,continuation))
    else*/
    //println("Ask Enquiry")
    sendData(ServerCommands.askEnquiry ) {out=>
      currentEnquiryContinuation=continuation
      out.writeUTF(question.toXML.toString)
    }
  }

  def answerEnquiry(in:DataInputStream): Unit = {
    val numParams=in.readInt
    val paramList=for(i <-0 until numParams)
      yield (in.readUTF,Expression.readConstant(in))
    if(currentEnquiryContinuation!=null)
      currentEnquiryContinuation(this,paramList)
  }

  def sendGeneratedData(func:(DataOutput)=>Unit): Unit = {
    sendData(ServerCommands.sendGeneratedData ) (func)
  }

  def flushTransactionBuffers(): Unit = outStreamLock.synchronized {
    try {
      out.flush()
    } catch {
      case e: IOException => Log.e("Send Data",e)
    }
  }
}

