/**
 * Author: Peter Started:26.07.2010
 */
package server.storage

import java.io.ByteArrayInputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.RandomAccessFile
import definition.data.LogIndexSet
import definition.data.Reference
import definition.data.TransType
import server.config.FSPaths
import scala.collection.mutable.ArrayBuffer

/** manages the transaction log
 * 
 */
object TransLogHandler 
{
	val fileName=FSPaths.dataDir+"transaction.log"	
	var theFile= new RandomAccessFile(fileName,"rwd")
	val recordSize=25
	val bufferStream= new MyByteStream(recordSize)
	val outStream=new DataOutputStream(bufferStream)		
	val readBuffer= new Array[Byte](recordSize)	
	val inBufferStream=new ByteArrayInputStream(readBuffer)
	val dataInStream=new DataInputStream(inBufferStream)
	
	lazy val largeReadBuffer= new Array[Byte](recordSize*10)
	lazy val largeInBufferStream=new ByteArrayInputStream(largeReadBuffer)
	lazy val largeDataInStream=new DataInputStream(largeInBufferStream)
	
	private var insertPos:Int=0
		
	private var transID:Int =
		if(theFile.length==0) { theFile.writeInt(1); 1 } 
	  else {
	  	theFile.seek(0)	  	
	  	insertPos=theFile.readInt// ((theFile.length()-4)/recordSize).toInt
      println("translog insertPos:"+insertPos+" size:"+theFile.length)
	  	if(insertPos==0) {
	  	  println("Warning: InsertPos==0 !")
	  	  theFile.seek(0)
	  	  theFile.writeInt(1)
				1
	  	} else {
	  		theFile.seek(getSeekPos(insertPos-1)+1)        
	  		val tr=theFile.readInt
	  		System.out.println("TransLog insPos:"+insertPos+" transID:"+tr)
	  		tr
	  	}
	  }
	//readFinished()  
	
	def getSeekPos(inPos:Int)= 4+inPos*recordSize
  
  def getTransID=transID
  
  def getInsertPos=insertPos
	
  /** prepares the LogHander for writing of Log data
   *  
   */
	def increaseTransID():Unit={
    transID+=1
    theFile.seek(getSeekPos(insertPos)) 
  } 
	
	//def resetTransID()= transID -=1
	
	def instanceCreated(ref: Reference ):Unit = dataChanged(TransType.created,ref.typ,ref.instance,0,0)		
		
	
	def dataChanged(transTyp: TransType.Value,typ:Int,inst:Int,dataPos:Long,dataLength:Int):Unit =	{		
		//System.out.println("TransLog changed ID:" + transID+ " transtyp:"+transTyp+" "+typ+", "+inst )    
		bufferStream.reset()		
		outStream.writeByte(transTyp.id)
		outStream.writeInt(transID)		
		outStream.writeInt(typ)		
		outStream.writeInt(inst )
		outStream.writeLong(dataPos)
		outStream.writeInt(dataLength)    
		theFile.write(bufferStream.buffer,0,bufferStream.size())
		insertPos+=1
	}
	
	def shutDown():Unit =	{		
		theFile.close()
	}
	
	def deleteLogFile():Unit= {
	  theFile.close()
	  new File(fileName).delete()
	  theFile=new RandomAccessFile(fileName,"rwd")
	  theFile.seek(0)
	  theFile.writeInt(1)
	  transID=1
	  insertPos=0
	}
	
	def readFullIndex():IndexedSeq[LogIndexSet] = {
		theFile.seek(4)
    val retList= ArrayBuffer[LogIndexSet]()		
		val endPos=getSeekPos(insertPos)
		while(theFile.getFilePointer < theFile.length) {
			theFile.read(readBuffer,0,recordSize)
			inBufferStream.reset()
			retList+= LogIndexSet(TransType(dataInStream.readByte),dataInStream.readInt,
				dataInStream.readInt,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)
		}
		//readFinished()	
		retList
	}	
	
	def readPosition(pos:Int):LogIndexSet = {
		theFile.seek(getSeekPos(pos))
		theFile.read(readBuffer,0,recordSize)
		inBufferStream.reset()
		LogIndexSet(TransType(dataInStream.readByte),dataInStream.readInt,
				dataInStream.readInt,dataInStream.readInt,dataInStream.readLong,dataInStream.readInt)
	}
	
	/** finds the last occurrence of a certain data type of a certain instance
	 * @param typ the type of the instance
	 * @param inst the instance id of the instance
	 * @param startPos the position in the log where the search should start backwards
	 * @param fittingTrActions TransType actions that we are looking for 
	 * @returns (transType.id,dataPos,dataLength)
	 */
	def getLastLivingData(typ:Int,inst:Int,startPos:Int,fittingTrActions:Seq[Int],skipDeleteEntries:Boolean=false):(Int,Long,Int)= {
	  //val stt=System.currentTimeMillis()
		var currPosition=startPos
		while (currPosition>10){
		  theFile.seek(4+(currPosition-9)*recordSize)
			theFile.read(largeReadBuffer,0,recordSize*10)
			//largeInBufferStream.reset
		  for(si<-9 to 0 by -1) {
		    largeInBufferStream.reset()
		    largeInBufferStream.skip(recordSize*si)
		    val trans=largeDataInStream.readByte
				val trid=largeDataInStream.readInt 
				val ttype=largeDataInStream.readInt
				val tinst=largeDataInStream.readInt
				val rpos =largeDataInStream.readLong
				val rlen =largeDataInStream.readInt
				if(ttype==typ&& tinst==inst &&	fittingTrActions.contains(trans)) {
				  /*theFile.seek(4+(currPosition-9+si)*recordSize+1+4+4+4)				  
				  val rpos=theFile.readLong
				  val rinst=theFile.readInt*/	
          if(!skipDeleteEntries||( rpos> -1&& rlen>0))     		  				  
				    return (trans,rpos,rlen)		
				}
		  }
		  currPosition-= 10
		}		
		while(currPosition>=0){
			theFile.seek(4+currPosition*recordSize)
			theFile.read(readBuffer,0,recordSize)
			inBufferStream.reset()
			val trans=dataInStream.readByte
			val trid=dataInStream.readInt // skipped
			val ttype=dataInStream.readInt
			val tinst=dataInStream.readInt
      val rpos=theFile.readLong
      val rlen=theFile.readInt
			if(ttype==typ&& tinst==inst &&	fittingTrActions.contains(trans)) {
			  /*theFile.seek(4+currPosition*recordSize+1+4+4+4)				  
			  val rpos=theFile.readLong
			  val rinst=theFile.readInt*/
			  if(!skipDeleteEntries||( rpos> -1&& rlen>0)) {				  
				  //println(" gll trid:"+trid+" time:"+(System.currentTimeMillis-stt)+" b:"+(startPos-currPosition))
				  return (trans,rpos,rlen)		
			  } 
			}
			currPosition-=1			
		}		
		System.out.println(" gll nope "/*+(System.currentTimeMillis-stt)*/+" b:"+(startPos-currPosition))
		throw new IllegalArgumentException("Cant find last living Data typ:"+typ+" inst:"+inst+" startPos:"+startPos+
		    " fittingActions:"+fittingTrActions.mkString(","))
		//return (-1,0L,0)
	}
	
	/*def readFinished() = {
		theFile.seek(getSeekPos(insertPos))
	}	*/
	
	/** notifies the Log Handler to reset the pointers behind the removed transaction
	 * @param numStepsBack num of transaction steps to be removed
	 */
	def undoLastStep(numStepsBack:Int):Unit = {	
	  if(numStepsBack>insertPos) insertPos=0
	  else insertPos -=numStepsBack
		System.out.println("translog undo "+insertPos)		
		transID -=1
    flush()
	}
	
	def flush():Unit= {	  
    theFile.seek(0)
    theFile.writeInt(insertPos)
    theFile.getChannel().force(false)
	}
	
}
