/**
 * Author: Peter Started:01.11.2010
 */
package server.storage

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, File, RandomAccessFile}

import definition.data.{InstanceData, Reference, TransStepData}
import server.comm.UserList
import server.config.FSPaths
import util.UnsyncBAInputStream

import scala.Option.option2Iterable
import scala.util.control.NonFatal



object TransDetailLogHandler {
	//var lastLoggedTime:Int=0
	private var lastLoggedID:Int= -1
	val fileName=FSPaths.dataDir+"TransDetailIndex.log"
	var theFile= new RandomAccessFile(fileName,"rwd")
	private var insertPos:Int =0
	val recordSize=27
	val readBuffer= Array.ofDim[Byte](recordSize)
	val inBufferStream=new UnsyncBAInputStream(readBuffer)
	val dataInStream=new DataInputStream(inBufferStream)
	val bufferStream=new MyByteStream(recordSize)
	val outStream=new DataOutputStream(bufferStream)
	
	if(theFile.length>0) {
		insertPos=theFile.readInt// ((theFile.length-4)/recordSize).toInt
    //if(insertPos!=(theFile.length-4)/recordSize) println(" insert pos diff "+insertPos+" "+(theFile.length-4)/recordSize)
		lastLoggedID= if(insertPos==0) 0 else {
      theFile.seek(filePosToIndex(insertPos-1))		
			theFile.readInt		
		}
		System.out.println("DetailLog last logged ID:"+lastLoggedID)		
		//System.out.println("Last logged Time:"+new java.util.Date(lastLoggedTime*60000L)+" id:"+lastLoggedID)
	} else {
      theFile.writeInt(0)
		System.out.println("DetailLog is empty ")
    }
	
	
	def filePosToIndex(pos:Int) = 4+pos*recordSize 
  
  def getInsertPos= insertPos
  def getLastLoggedID = lastLoggedID
	
	def log(trID:Int,userID:Int,firstInst:Reference,multiInst:Boolean,action:Short,createType:Int ) = {
    theFile.seek(filePosToIndex(insertPos))
		val time=(System.currentTimeMillis/60000).toInt
		lastLoggedID=trID
		//System.out.println("timediff:"+(time-lastLoggedTime))
		bufferStream.reset()		
	  outStream.writeInt(trID)
	  outStream.writeInt(time)
	  outStream.writeInt(userID)
	  outStream.writeInt(firstInst.typ)
	  outStream.writeInt(firstInst.instance)
	  outStream.writeBoolean(multiInst)
	  outStream.writeShort(action)
	  outStream.writeInt(createType)
	  theFile.write(bufferStream.buffer,0,recordSize)	  
	  insertPos +=1
    flush()
	}
  
  def flush()={
    theFile.seek(0)
    theFile.writeInt(insertPos)
    theFile.getChannel().force(false)
  }
	
	def shutDown() = {		
		theFile.close()
	}
	
	private def readTransStepData(trID:Int):Option[TransStepData] = {
	  //val startTime=System.currentTimeMillis()
		if (trID>lastLoggedID){
			System.out.println("TRID>lastLoggedID  trID:"+trID+" lastLoggedID"+lastLoggedID)
			None
		}
		else if(trID<0) {
			System.out.println("trid<0  "+trID)
			None
		}
		else {
			System.out.println("read transStep "+trID+" insPos:"+insertPos+" lastLoggedID:"+lastLoggedID)
			if(insertPos-(lastLoggedID-trID)<0) None
			else {
				theFile.seek(filePosToIndex(insertPos-(lastLoggedID-trID)-1))
				val result=internRead(true)
        for(r<-result)if(r.trID!=trID) throw new IllegalArgumentException("wrong trid:"+r+" wanted:"+trID)
        result
			}
		}
	}
  
  private def internRead(removeZombies:Boolean):Option[TransStepData]={
    theFile.read(readBuffer,0,recordSize)
    inBufferStream.reset()
    val ntrID=dataInStream.readInt        
    val time=dataInStream.readInt
    val userID=UserList.getUserName(dataInStream.readInt.toShort)
    val ref=Reference(dataInStream.readInt,dataInStream.readInt)
    val multi=dataInStream.readBoolean
    val action=ActionNameMap.getActionName(dataInStream.readShort)
    val ct=dataInStream.readInt
    //val midTime=System.currentTimeMillis()    
    val result= try {Some(new TransStepData(ntrID,time,userID,if(removeZombies)StorageManager.getZombieInstanceData(ref)
			else new InstanceData(ref,IndexedSeq(),Array(),Seq.empty,false),multi,action,ct))}
    catch {case NonFatal(e) => if(removeZombies){
      util.Log.e("internRead",e);None}
    else Some(new TransStepData(ntrID,time,userID,new InstanceData(ref,IndexedSeq(),Array(),Seq.empty,false),multi,action,ct))
      }
    //println("readTrans "+trID+" "+(System.currentTimeMillis-startTime)+" "+(midTime-startTime))
    result
  }
  
	
	def readTransStepData(fromID:Int,toID:Int):Seq[TransStepData] = {
    println("read transstep from:"+fromID+" to:"+toID)
	  //val startTime=System.currentTimeMillis()
	  val rtoID=if(toID<2) 2 else toID
	  if(fromID>lastLoggedID) {println("read transsstep after lastLoggedID fromID:"+fromID+" lastl:"+lastLoggedID); Nil }
	  else {
	    val ret= for(id <-fromID to rtoID by -1)
	    	yield readTransStepData(id)	   
	    //println("readTransStrep "+(System.currentTimeMillis-startTime)+" "+ret.size)
	    ret.flatten
	  }
	}
  
  def loop(startTransID:Int,endTransID:Int,callBack: (Long,Short,Reference)=>Unit):Unit = {
    val firstID=lastLoggedID-insertPos
    val start =if(startTransID<firstID) 0 else insertPos-(lastLoggedID-startTransID)-1 //startTransID-firstID
		println("startID:"+startTransID+" endID:"+endTransID+" firstID:"+firstID+" start:"+
			start+" lastLogged:"+lastLoggedID+" insertPos:"+insertPos)
		theFile.seek(filePosToIndex(start))
		var trShouldBe=startTransID
		var trID:Int=0
		do {
			theFile.read(readBuffer,0,recordSize)
			inBufferStream.reset()
			trID=dataInStream.readInt
			//print(" trID:"+trID)
			val time=dataInStream.readInt
			val userID=dataInStream.readInt.toShort
			val ref=Reference(dataInStream.readInt,dataInStream.readInt)
			val multi=dataInStream.readBoolean
			val action=dataInStream.readShort
			val ct=dataInStream.readInt
			callBack(time*60000l,userID,ref)
			if(trID!=trShouldBe) {println("wrong trID "+trID+" should be:"+trShouldBe); return }
			trShouldBe+=1
		} while(trID<endTransID&& trID<lastLoggedID)
		//println("|")
  }
  
  /*private def loopAll(callBack: (Long,Short,Reference)=>Unit)= {
    println("loop size "+((theFile.length-4)/recordSize).toInt)
    theFile.seek(4)
    for (i <-0 until ((theFile.length-4)/recordSize).toInt){
      //try {                         
        theFile.read(readBuffer,0,recordSize)
        inBufferStream.reset()
        val trID=dataInStream.readInt
        print(" "+trID)
        val time=dataInStream.readInt
        val userID=dataInStream.readInt.toShort
        val ref=Reference(dataInStream.readInt,dataInStream.readInt)
        val multi=dataInStream.readBoolean
        val action=dataInStream.readShort
        val ct=dataInStream.readInt
        if(StorageManager.instanceExists(ref.typ, ref.instance))
          callBack(time*60000l,userID,ref)
        else print(" nope "+ref+" ")
      //} finally readFinished()  
    }
  }*/
	
	/*private def readFinished() = {
		theFile.seek(filePosToIndex(insertPos))
	}*/
	
	def readFully:IndexedSeq[TransStepData] = {
		System.out.println("readfully "+(theFile.length-4)/recordSize)
		theFile.seek(4)
		val ret=for (i <-0 until ((theFile.length-4)/recordSize).toInt) 
			yield	{internRead(false)
				/*System.out.print(i+"->"+(if(res.isDefined) res.get.trID else "N")+
					(if(i%10==0)"\n" else " "));res*/}
		System.out.println("read finished "+ret.size)
		ret.flatten
	}	
	
	def undoLastStep()= {
	  insertPos -=1
	  lastLoggedID-=1
    flush()
	  //readFinished()	
	}
	
	def deleteLogFile()={
	  theFile.close()
		new File(fileName).delete
	  theFile=new RandomAccessFile(fileName,"rwd")
		theFile.writeInt(0)
	  insertPos=0
	} 
	
		
}