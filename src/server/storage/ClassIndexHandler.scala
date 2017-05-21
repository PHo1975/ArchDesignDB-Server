/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import java.io._

import definition.data.{InstanceData, Reference, TransType}
import server.config.FSPaths
import util.UnsyncBAInputStream

import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.collection.immutable

trait RecordListener {
 def nextRecord(inst:Int,dataPos:Long,dataLen:Int,propPos:Long,propLen:Int,linkPos:Long,linkLen:Int,collPos:Long,collLen:Int): Unit
}

/** Manages the index file for a certain class
 * 
 */

class ClassIndexHandler(val theClass:ServerObjectClass,val extension:String=".idx") {
	val fileName=new File(FSPaths.dataDir+theClass.name+extension)	
	var theFile= new RandomAccessFile(fileName,"rwd")
  final val recordSize: Int = 8 * 4 + 4 * 5
	var numRecords:Int=(theFile.length/recordSize).toInt
	//System.out.println("Typ: "+theClass.id+" numRecords:"+numRecords)
  var firstID: Int = if (numRecords > 0) readIxInst(0) else 0
	var lastID:Int= if(numRecords>0) readIxInst(numRecords-1) else 0	
	
	var lastReadID:Int= -2	
	var lastSeekID:Int = -2
	var lastSeekPos:Option[Int] = None
	
	val bufferStream=new MyByteStream(recordSize)
	val miniBufferStream=new MyByteStream(12)
	val miniOutStream=new DataOutputStream(miniBufferStream)	
	
	val outStream=new DataOutputStream(bufferStream)
	for(i <-0 until 6 )	outStream.writeLong(0)
	outStream.writeInt(0)

  val readBuffer: Array[Byte] = Array.ofDim[Byte](recordSize)
  var bulkReadBuffer: Array[Byte] = readBuffer
	val inBufferStream=new UnsyncBAInputStream(readBuffer)
	val dataInStream=new DataInputStream(inBufferStream)
	
	//val instCache=new Cache[InstanceData](theClass.id)
	//val propCache=new Cache[InstanceProperties](theClass.id)
	
	var counter=0
	
	
	// stores Information about the instance data in the index
	
	def createInstance():Int =	{
		val inst=lastID+1		
		theFile.seek(theFile.length)
		resetLastReadID()
		bufferStream.reset()
		outStream.writeInt(inst)
		outStream.writeLong(0)
		outStream.writeInt(0)
		outStream.writeLong(0)
		outStream.writeInt(0)
		outStream.writeLong(0)
		outStream.writeInt(0)
		outStream.writeLong(0)
		outStream.writeInt(0)
		theFile.write(bufferStream.buffer,0,recordSize)
		numRecords+=1
		lastID=inst		
		inst
	}

  def resetLastReadID(): Unit = if (lastReadID != -2) lastReadID = -2


  def writeData(inst: Int, dataPos: Long, dataLength: Int, created: Boolean, withLog: Boolean = true): Unit = {
	  internalWrite(inst,dataPos,dataLength,0) 
	  if(withLog){
	  	if(created) TransLogHandler.dataChanged(TransType.created,theClass.id,inst,dataPos,dataLength) 
	  	else        TransLogHandler.dataChanged(TransType.dataChanged,theClass.id,inst,dataPos,dataLength)
	  }
	}


  def reorgWriteRecord(inst: Int, dataPos: Long, dataLen: Int, propPos: Long, propLen: Int, linkPos: Long, linkLen: Int, collPos: Long, collLen: Int): Unit = {
	  bufferStream.reset()
	  outStream.writeInt(inst)
	  outStream.writeLong(dataPos)
	  outStream.writeInt(dataLen)
	  outStream.writeLong(propPos)
	  outStream.writeInt(propLen)
	  outStream.writeLong(linkPos)
	  outStream.writeInt(linkLen)
	  outStream.writeLong(collPos)
	  outStream.writeInt(collLen)
	  theFile.write(bufferStream.buffer,0,recordSize)
	}	
	
	
	private def internalWrite(inst:Int,dataPos:Long,dataLength:Int,
	                          ixOffset:Int ) = { // Offset position in the index record	
		if(inst>lastID)  // create new Instance
	  	throw new IllegalArgumentException("Storing wrong instance" +inst+ " in class "+theClass.name)
	  
		theFile.seek(findIxRecord(inst)*recordSize+4+ixOffset)
		resetLastReadID()
		miniBufferStream.reset()
		miniOutStream.writeLong(dataPos)
	  miniOutStream.writeInt(dataLength)
	  theFile.write(miniBufferStream.buffer,0,12)
	}

  def writePropertiesData(inst: Int, dataPos: Long, dataLength: Int, withLog: Boolean = true): Unit = {
    internalWrite(inst,dataPos,dataLength,8+4)
    if(withLog)TransLogHandler.dataChanged(TransType.propertyChanged ,theClass.id,inst,dataPos,dataLength)
	}

  def writeLinksData(inst: Int, dataPos: Long, dataLength: Int, withLog: Boolean = true): Unit = {
    internalWrite(inst,dataPos,dataLength,(8+4)*2)
    if(withLog)TransLogHandler.dataChanged(TransType.linksChanged ,theClass.id,inst,dataPos,dataLength)
	}

  def writeCollFuncData(inst: Int, dataPos: Long, dataLength: Int, withLog: Boolean = true): Unit = {
		internalWrite(inst,dataPos,dataLength,(8+4)*3)
		if(withLog)TransLogHandler.dataChanged(TransType.collFuncChanged ,theClass.id,inst,dataPos,dataLength)
	}


  def foreachInstance(func: (Reference) => Unit): Unit = {
		for(i <-1 to lastID; if instanceExists(i))
			func(Reference(theClass.id,i))
	}

  def foreachInstance(listener: RecordListener): Unit = {
	  theFile.seek(0)
		for(i <-0 until numRecords) {
	    theFile.read(readBuffer,0,recordSize)
	    inBufferStream.reset()
	    val inst=dataInStream.readInt
	    val dataPos=dataInStream.readLong
	    if(dataPos>=0) listener.nextRecord(inst,dataPos,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,
			dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)
	  }
	}

  def getInstancesRefs: Array[Reference] = readFully.filter(_.dataPos >= 0).map(record => new Reference(theClass.id, record.inst))


  def deleteInstance(inst: Int, withLog: Boolean = true): Unit = {
		internalWrite(inst,-1,0,0)
		if(withLog)TransLogHandler.dataChanged(TransType.deleted,theClass.id,inst,0,0)
	}
	
	
	// does the specified instance exist ?
	def instanceExists(inst:Int):Boolean = if(inst<firstID||inst>lastID) false
	  else {	  
			resetLastReadID()
			for(i<-internFindIxRecord(inst)){
			   val r=getInstanceRecord(inst)		   
		     return r.dataPos > -1 && r.dataLength != 0
			}
			false
	  }
	
	
	
	def shutDown():Unit = theFile.close()


  def bulkGetInstanceRecords(startInst: Int, endInst: Int, dataFileHandler: BoolContFileHandler[InstanceData]): immutable.IndexedSeq[InstanceData] = {
		if (bulkReadBuffer.length<(endInst-startInst+1)*recordSize)
			bulkReadBuffer=Array.ofDim[Byte](recordSize* (endInst-startInst+1))
		
		theFile.seek(findIxRecord(startInst)*recordSize)
		theFile.read(bulkReadBuffer,0,recordSize* (endInst-startInst+1))
		//System.out.println("buffer read "+(endInst-startInst+1).toInt)
		//inBufferStream.reset
		for(i<-startInst to endInst;offset= (i-startInst) *recordSize+4;
			rPos=getLong(bulkReadBuffer,offset);
			rSize=getInt(bulkReadBuffer,offset+8);if rSize != 0 && rPos >= 0) yield
		  	dataFileHandler.readWithBool(Reference(theClass.id,i),rPos,rSize,getLong(bulkReadBuffer,offset+8+4)!=0 	)	
	}
	
	
	def bulkPushInstanceRecords(startInst:Int, endInst:Int, dataFileHandler:BoolContFileHandler[InstanceData],
                              out: DataOutput): Unit = {
		if (bulkReadBuffer.length<(endInst-startInst+1)*recordSize)
			bulkReadBuffer=Array.ofDim[Byte](recordSize* (endInst-startInst+1))
		
		theFile.seek(findIxRecord(startInst)*recordSize)
		theFile.read(bulkReadBuffer,0,recordSize* (endInst-startInst+1))
		//System.out.println("buffer push "+(endInst-startInst+1).toInt)
		//inBufferStream.reset
		for(i<-startInst to endInst){
			val offset= (i-startInst) *recordSize+4
			out.writeInt(theClass.id) // reference
			out.writeInt(i)
			val rpos=getLong(bulkReadBuffer,offset)
			//if(rpos<0) println("bulkPush typ:"+theClass.id+" "+theClass.name +" inst:"+i+" pos ="+rpos)
			dataFileHandler.pushData(rpos,getInt(bulkReadBuffer,offset+8) ,out	)
			out.writeBoolean(getLong(bulkReadBuffer,offset+8+4)!=0)
		}
			
	}
	
	
	def getLong(readBuffer:Array[Byte],pos:Int):Long = 
	 (readBuffer(pos).toLong << 56) +
		 ((readBuffer(pos + 1) & 255).toLong << 48) +
		 ((readBuffer(pos + 2) & 255).toLong << 40) +
		 ((readBuffer(pos + 3) & 255).toLong << 32) +
		 ((readBuffer(pos + 4) & 255).toLong << 24) +
		 ((readBuffer(pos + 5) & 255).toLong << 16) +
		 ((readBuffer(pos + 6) & 255).toLong << 8) +
		 ((readBuffer(pos + 7) & 255).toLong << 0)
    
		
	def getInt (readBuffer:Array[Byte],pos:Int):Int =
		((readBuffer(pos+0) & 255) << 24) +
    ((readBuffer(pos+1) & 255) << 16) +
    ((readBuffer(pos+2) & 255) << 8) +
    ((readBuffer(pos+3) & 255) << 0)
	
	
	 def getInstanceRecord (inst:Int):IndexRecord =	{	
		 //System.out.println("getinstance Record inst"+inst+" lastReadID:"+lastReadID)
		if(lastReadID!=(inst-1)) // optimize: if this call is the subsequent inst of the last call, try not to seek
			theFile.seek(findIxRecord(inst)*recordSize)
		//else print("o ")
		lastReadID=inst
		theFile.read(readBuffer,0,recordSize)
		inBufferStream.reset()
		new IndexRecord(dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,
			dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)
	}
	
	
	 def readFully():Array[IndexRecord] = {
		 val retArray=Array.ofDim[IndexRecord](numRecords)
		 theFile.seek(0)
		 for(i<- 0 until numRecords) {
			 theFile.read(readBuffer,0,recordSize)
			 inBufferStream.reset()
			 retArray(i)=new IndexRecord(dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt,
				 dataInStream.readLong,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)			 
		 }
		 retArray
			 
	 }
	
	// internal routines
	
	// gets the Position of the given Instance in the index file
	private def internFindIxRecord(inst:Int):Option[Int] =	{
		if(inst==lastSeekID) lastSeekPos
		else {			
		  if(numRecords==0||inst<firstID||inst>lastID) return None
		  if(inst==lastID) return Some(numRecords-1)		  
		  lastSeekID=inst		  		
		  // binary search
			@tailrec
			def finder(lower:Int,upper:Int):Option[Int] =  {
		  	if (upper<lower) return None
		  	val mid = (upper + lower) / 2
		  	val instAtMid=readIxInst(mid)		  	
		  	if (instAtMid > inst)finder(lower, mid-1)
		  		else if (instAtMid < inst)finder( mid+1, upper)
		  	else Some(mid)
		  }
		  lastSeekPos=finder(0,numRecords)
		  lastSeekPos
		}				
	}
	
	private def findIxRecord(inst:Int):Int =	{
    resetLastReadID()
		internFindIxRecord(inst) match		{ 
			case Some(v)=>v ;
			case None => throw new IllegalArgumentException("Instance "+inst+" not found in class "+theClass.name )
		}
	}
	
	// reads the Instance # at the given position 
	private def readIxInst(pos:Int) =	{
		theFile.seek(pos*recordSize)
		theFile.readInt
	}


  def takeOverFromReorgFile(reorgFile: File): Unit = {
	  theFile.close()
		val backupFile=new File(fileName.toString+".bak")
	  if(backupFile.exists()) backupFile.delete
	  fileName.renameTo(backupFile)
	  reorgFile.renameTo(fileName)
	  theFile= new RandomAccessFile(fileName,"rwd")
	  numRecords=(theFile.length/recordSize).toInt
	  firstID= if(numRecords>0) readIxInst(0) else 0
	  lastID= if(numRecords>0) readIxInst(numRecords-1) else 0	
	  lastReadID= -2	
	  lastSeekID = -2
	  lastSeekPos = None
	}
}



/** Index record for instance data
 * 
 */
class IndexRecord (val inst:Int,val dataPos:Long,val dataLength:Int,val propPos:Long,val propLength:Int,
	val linkPos:Long,val linkLength:Int,val collPos:Long,val collLength:Int)
{

}