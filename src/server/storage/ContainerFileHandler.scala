/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.DataInput
import java.io.DataInputStream
import java.io.DataOutput
import java.io.DataOutputStream
import java.io.File
import java.io.RandomAccessFile

import definition.data.Referencable
import definition.data.Reference
import server.config.FSPaths
import util.UnsyncBAInputStream

/** manages file access to the data file
 * 
 */
class ContainerFileHandler [T <: Referencable] (val fileName:String,factory: (Reference,DataInput) => T) {
  val compFileName=new File(FSPaths.dataDir+fileName)
	var theFile= new RandomAccessFile(compFileName,"rwd")	
	val bufferStream= new MyByteStream(256)
	val outStream=new DataOutputStream(bufferStream)
	
	var readBuffer= Array.ofDim[Byte](256)
	var inBufferStream=new UnsyncBAInputStream(readBuffer)
	var dataInStream=new DataInputStream(inBufferStream)
	
	var lastReadPos:Long= -2 
	var lastReadSize:Int= -2
	//var followCount=0
	
	/** Stores an Instance in the Data File
	 @param data Instance to Store
	 @return the position in the Data file and the num of bytes written 
	 */
	def writeInstance(data:T):(Long,Int) =	{
		if(lastReadPos!= -2) lastReadPos= -2 // reset cache marker
		bufferStream.reset()
		val pos=theFile.length
		theFile.seek(pos)
		data.write(outStream)
		theFile.write(bufferStream.buffer,0,bufferStream.size())
		(pos,bufferStream.size())
	}
	
	def writeBuffer(buffer:Array[Byte], size:Int):Long = {
	  val pos=theFile.length
	  theFile.write(buffer,0,size)
	  pos
	}
	
	/**
	 *  reads one instance out of the data File
	 */
	def readInstance(ref:Reference,pos:Long,size:Int):T =	{
	  internReadInBuffer(pos,size)
		factory(ref,dataInStream)
	}
	
	def readInBuffer(pos:Long, size:Int):Array[Byte] = {
	  internReadInBuffer(pos,size)
	  readBuffer
	}

	def readInStream(pos:Long,size:Int):DataInput= {
		internReadInBuffer(pos,size)
		dataInStream
	}
	
	protected def internReadInBuffer(pos:Long, size:Int) = {
	  if(pos<0) throw new IllegalArgumentException(" cant read Buffer at Pos :"+pos)
		if(size>readBuffer.length) {
			readBuffer=Array.ofDim[Byte](size+128)
			inBufferStream=new UnsyncBAInputStream(readBuffer)
			dataInStream=new DataInputStream(inBufferStream)
		}	
		//print(" R:"+pos+","+size )
		if(pos!=lastReadPos+lastReadSize)  // dont seek for subsequent instances			
			theFile.seek(pos)
				
		theFile.read(readBuffer,0,size)
		inBufferStream.reset()
		lastReadPos=pos
		lastReadSize=size		
	}	
	
		
	def shutDown()=	{
		theFile.close()
	}
	
	def takeOverReorgFile(reorgFile:File)={
	  theFile.close()
	  val backupFile=new File(compFileName.toString()+".bak")
	  if(backupFile.exists()) backupFile.delete
	  compFileName.renameTo(backupFile)
	  reorgFile.renameTo(compFileName)
	  theFile=new RandomAccessFile(compFileName,"rwd")	
	}

}

class BoolContFileHandler [T <: Referencable] (override val fileName:String,withBoolFactory: (Reference,DataInput,Boolean) => T) extends
	  ContainerFileHandler[T](fileName,null)	
	{
		def readWithBool(ref:Reference,pos:Long,size:Int,boolValue:Boolean):T = {
			internReadInBuffer(pos,size)
			withBoolFactory(ref,dataInStream,boolValue)
		}		
		def pushData(pos:Long,size:Int,out:DataOutput) = {		  
			internReadInBuffer(pos,size)
			//System.out.println("pushData "+fileName)
			out.write(readBuffer,0,size)			
		}
	}


class MyByteStream(nsize:Int) extends ByteArrayOutputStream(nsize) {
	def buffer=buf

	override def write(b: Array[Byte], off: Int, len: Int) ={
		if ((off < 0) || (off > b.length) || (len < 0) || ((off + len) - b.length > 0)) {
			throw new IndexOutOfBoundsException
		}
		ensureCapacity(count + len)
		System.arraycopy(b, off, buf, count, len)
		count += len
	}

	override def write (b: Int)= {
		ensureCapacity(count + 1)
		buf(count) = b.toByte
		count += 1
	}

	override def reset()= {
		count = 0
	}

	protected def ensureCapacity (minCapacity: Int)=
		if (minCapacity - buf.length > 0) grow(minCapacity)


	protected def  grow(minCapacity:Int)= {
		if (minCapacity < 0) // overflow
			throw new OutOfMemoryError();
		val oldCapacity = buf.length;
		val newCapacity= {
			val nc=oldCapacity << 1
			if(nc-minCapacity < 0) minCapacity
			else nc
		}
		buf = java.util.Arrays.copyOf(buf, newCapacity);
	}
}