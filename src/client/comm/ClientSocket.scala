/**
 * Author: Peter Started:29.08.2010
 */
package client.comm

import java.io._
import java.net._

import client.print.PrintQuestionHandler
import definition.comm._
import definition.data.{InstanceData, Reference}
import definition.typ.{AllClasses, ClientSystemSettings, SystemSettings}
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal


trait ChainedListener {
  def doIt(next:Option[ChainedListener]):Unit
}

object UserSettings extends UserSetting
/** manages the communication with the server
 * 
 */
class ClientSocket(serverAddress: InetAddress,port:Int,name:String,password:String,appName:String,readTypes:Boolean=true) extends Thread("Socket-Thread") {
	
	private val socket = new Socket(serverAddress, port)
	val out =  new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()))
	private val outStreamLock : AnyRef = new Object()
	private val inStreamLock = new Object()
	
	var wantRun=true	
	private var isSetup=false
	type CommandHandlerFunc= (DataInputStream)=>Unit
	
	var classesReadListener:()=>Unit = _

	val startupFinishListener: ArrayBuffer[() => Unit] = ArrayBuffer[() => Unit]()
  
  var connectionBrokenListener:()=>Unit = _
	
	private val commandHandlerMap=new scala.collection.mutable.HashMap[ServerCommands.Value,CommandHandlerFunc]()
	
	private val genDataHandlerMap=new collection.mutable.HashMap[GeneratorType.Value,GenDataReceiver]()
	
	
	
	override def run():Unit = {
		 try {			
			 val in = new DataInputStream(new BufferedInputStream(socket.getInputStream()))
			 // send user name and password
			 writeOut(name)
			 writeOut(password)
			 writeOut(appName)
			 //Thread.`yield`()
			 val retValue=in.readUTF()
			 if(retValue !="welcome" ) util.Log.e("not welcome, message: "+retValue)
       else {
         util.Log.w("Logged in to " + serverAddress)
         sendData(ClientCommands.getSystemSettings) { out =>}
         //Thread.`yield`()
         handleCommands(in)
       }
		 }
		 catch {
      case e: IOException =>
        Log.e(e)
     }
	}
	
	private def writeOut(st:String ) = {out.writeUTF(st);out.flush()}
	
	private def handleCommands(in:DataInputStream):Unit = inStreamLock.synchronized {
		while(wantRun)
			try {
			  //println("fetch ")
				val command =ServerCommands(in.readByte.toInt)
				//System.out.println("ServerCommand: "+command)
				command match {
					case ServerCommands.sendTypes  => readInTypes(in)
					case ServerCommands.sendSystemSettings=>readSystemSettings(in)
					case ServerCommands.sendUserSettings => readUserSettings(in)
					case ServerCommands.wantQuit => wantRun=false; quitApplication()
					case a => if (commandHandlerMap.contains(a)) commandHandlerMap(a)(in)
										else util.Log.e("ServerCommand "+a+" not handled")
				}
			}
			catch {
				case a:EOFException => if(wantRun){
					Log.e("handle commands",a)
          a.printStackTrace()
          connectionBroken()  
        }			
        case b:SocketException=> if(wantRun){
					Log.e("handle commands",b)
					println(b)
          connectionBroken()
        } 
				case NonFatal(e) => Log.e("handle commands",e)
				case other: Throwable => Log.e("handle commands", other); wantRun = false
			}
	}
  
  private def connectionBroken()={    
    if(connectionBrokenListener!=null)connectionBrokenListener()
		wantRun = false
		//System.exit(0)
  }
	
	private def readSystemSettings(in:DataInputStream):Unit= {
	  //val start=System.currentTimeMillis()
	  SystemSettings.settings=new ClientSystemSettings(in) {
			override def loadChildren(ref: Reference): IndexedSeq[InstanceData] = ClientQueryManager.queryInstance(ref,1)
		}
	  //println("read system settings "+(System.currentTimeMillis()-start))
	  sendData(if(readTypes)ClientCommands.getTypes else ClientCommands.getUserSettings ){out:DataOutputStream =>{}}
	}
		
	private def readInTypes(in: DataInputStream):Unit = 	{
    //val start=System.currentTimeMillis()
	  val uncompressedSize=in.readInt
		val numBytes:Int=in.readInt
		val buffer:Array[Byte]=Array.ofDim[Byte](numBytes)
		in.readFully(buffer)
		val inf=new java.util.zip.Inflater
		val fullBuffer=Array.ofDim[Byte](uncompressedSize)
		inf.setInput(buffer)
		inf.inflate(fullBuffer)
		inf.end()
    val cc=new ClientClasses(xml.XML.loadString(new String(fullBuffer,"UTF-8")))
		AllClasses.set(cc, resolve = false)
		//println(" Classes read "+(System.currentTimeMillis()-start)+" uncompressedsize:"+uncompressedSize+" size:"+numBytes)
		sendData(ClientCommands.getUserSettings  ){out:DataOutputStream =>{}}
	}
	
	
	private def readUserSettings(in:DataInputStream) = {	  
    //val start=System.currentTimeMillis()
		val editable=in.readBoolean()
		val userID=in.readInt()
		val startRef=Reference(in)
		UserSettings.readFromStream(in,in.readInt(),true)
		KeyStrokeManager.load(in)
		genDataHandlerMap(GeneratorType.printGenerator )=PrintQuestionHandler
		if(classesReadListener!=null)classesReadListener()
		ClientQueryManager.notifySetupListeners()
		for(s<-startupFinishListener)s()
	}		
	
	
	
	
	// ************************************ COMMANDS ***************************************

	def registerCommandHandler(command: ServerCommands.Value)(func: (DataInputStream) => Unit): Unit = {
		commandHandlerMap.put(command,func)
	}
	
	//def registerGenDataHandler(gType:GeneratorType.Value,handler:GenDataReceiver)= genDataHandlerMap(gType)=handler	

	def quitApplication(): Unit = {
		// Shutdown all data
		// save changes in user settings
		ClientQueryManager.notifyStoreSettingsListeners()		
		// store user settings
		//System.out.println("writing settings")
		sendData(ClientCommands.writeUserSettings ) {out =>
			val buffer =UserSettings.writeProperties.getBytes("UTF-8")
			out.writeInt(buffer.length)
			out.write(buffer,0,buffer.length)
		}
		// logout
    util.Log.w("Logging out")
		wantRun=false
		sendData(ClientCommands.logOut ) {out =>}
		System.exit(0)
	}

	def sendData(command: ClientCommands.Value)(func: (DataOutputStream) => Unit): Unit = {
		try {
			outStreamLock.synchronized {
				out.writeByte(command.id.toByte)
				func(out)
				out.flush()
			}
		}
		catch {			
			case e: IOException =>
			Log.e("send data command:"+command,e)
		}
	}	
	
	
	// ****************************** COMMAND HANDLER ****************************************+++++
	
	registerCommandHandler(ServerCommands.sendGeneratedData )(in => {	  
	  val gt=GeneratorType(in.readInt)
	  //println("Receive generated data "+gt )
	  if(genDataHandlerMap.contains(gt))
		genDataHandlerMap(gt).receiveData(in)
		else util.Log.e("Cant find Generator of type "+gt)
	})
	
}

