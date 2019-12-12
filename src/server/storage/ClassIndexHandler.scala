/**
 * Author: Peter Started:25.07.2010
 */
package server.storage

import java.io._

import definition.data.{InstanceData, Reference, TransType}
import server.config.FSPaths
import util.UnsyncBAInputStream

import scala.Array.canBuildFrom
import scala.collection.immutable

trait RecordListener {
	def nextRecord(inst: Int, dataPos: Long, dataLen: Int, propPos: Long, propLen: Int, linkPos: Long, linkLen: Int, collPos: Long, collLen: Int): Unit
}


object ClassIndexHandler extends InstanceHandlerStaticData {
	final val recordSize: Int = 8 * 4 + 4 * 5
	final val appendBufferSize = 10
	val appendBufferHandler=new AppendBufferHandler(recordSize,appendBufferSize)
}


class AppendBufferHandler(nrecordSize:Int,nappendBufferSize:Int){
	val appendInBuffer: Array[Byte] = Array.fill[Byte](nrecordSize * nappendBufferSize)(0.toByte)
	val appendInStream = new UnsyncBAInputStream(appendInBuffer)
	val appendDataIn = new DataInputStream(appendInStream)
	val appendOutStream = new MyByteStream(nrecordSize * nappendBufferSize)
	val appendDataOut = new DataOutputStream(appendOutStream)

	/**  writes one record and adds appendBufferSize number of records at the end of the file so the file is only increased 1 in appendBufferSize times.
	* @param b record data
*/
	def addTail(theFile: RandomAccessFile, b: Array[Byte]): Unit = {
		//println("add Tail ")
		theFile.seek(theFile.length)
		appendOutStream.reset()
		appendOutStream.write(b, 0, nrecordSize)
		for (i <- 0 until nappendBufferSize - 1) {
			appendDataOut.writeInt(-1)
			appendDataOut.writeLong(-1); appendDataOut.writeInt(0); appendDataOut.writeLong(0); appendDataOut.writeInt(0)
			appendDataOut.writeLong(0); appendDataOut.writeInt(0); appendDataOut.writeLong(0); appendDataOut.writeInt(0)
		}
		theFile.write(appendOutStream.buffer, 0, appendOutStream.buffer.length)
	}

	/** Reads the end of an indexFile and tries to find out how many free record lines are left in the end.
	* @param theFile indexFile
  *
	*/
	def readTailSpace(theFile: RandomAccessFile): Int = {
		if (theFile.length() >= nrecordSize * nappendBufferSize) {
			theFile.seek(theFile.length() - nrecordSize * nappendBufferSize)
			theFile.read(appendInBuffer)
			appendInStream.reset()
			var line = -1
			for (i <- 0 until nappendBufferSize; if line == -1) {
				val inst = appendDataIn.readInt
				if (inst == -1 || inst == 0) line = i
				appendDataIn.readLong; appendDataIn.readInt; appendDataIn.readLong; appendDataIn.readInt
				appendDataIn.readLong; appendDataIn.readInt; appendDataIn.readLong; appendDataIn.readInt
			}
			if (line == -1) 0
			else if (line < nappendBufferSize) {
				//println("TailSpace line:"+line)
				nappendBufferSize - line
			}
			else throw new IllegalArgumentException("Wrong tail number " + line)
		} else 0
	}
}

/** Manages the index file for a certain class
 * 
 */

class ClassIndexHandler(val theClass: ServerObjectClass, val extension: String = ".idx") extends AbstractInstanceHandler {

	def fileName = new File(FSPaths.dataDir + theClass.name + extension)
	def staticData=ClassIndexHandler

	def className: String =theClass.name

	val benchmarkBuffer: Array[Int] = Array.fill(50)(0)
	protected var currentBenchPos = 0

	for (i <- 0 until 6) outStream.writeLong(0)
	outStream.writeInt(0)

	protected def writeBench(value: Int): Unit = {
		benchmarkBuffer(currentBenchPos) = value
		currentBenchPos = if (currentBenchPos < benchmarkBuffer.length - 1) currentBenchPos + 1 else 0
	}

	// stores Information about the instance data in the index
	def createInstance(): Int = {
		val inst = lastID + 1
		writeBufferStream.reset()
		outStream.writeInt(inst)
		outStream.writeLong(0)
		outStream.writeInt(0)
		outStream.writeLong(0)
		outStream.writeInt(0)
		outStream.writeLong(0)
		outStream.writeInt(0)
		outStream.writeLong(0)
		outStream.writeInt(0)
		append(writeBufferStream.buffer)
		lastID = inst
		inst
	}


	def writeData(inst: Int, dataPos: Long, dataLength: Int, created: Boolean, withLog: Boolean = true): Unit = {
		internalWrite(inst, dataPos, dataLength, 0)
		if (withLog)
			TransLogHandler.dataChanged(if (created) TransType.created else TransType.dataChanged, theClass.id, inst, dataPos, dataLength)
	}


	def writeAllFields(inst: Int, dataPos: Long, dataLen: Int, propPos: Long, propLen: Int, linkPos: Long, linkLen: Int, collPos: Long, collLen: Int, created: Boolean): Unit = {
		resetLastReadID()
		if (inst > lastID) // create new Instance
			throw new IllegalArgumentException("Storing wrong instance" + inst + " in class " + theClass.name)
		theFile.seek(findIxRecord(inst) * staticData.recordSize)
		reorgWriteRecord(inst, dataPos, dataLen, propPos, propLen, linkPos, linkLen, collPos, collLen)
		TransLogHandler.startCombiWrite()
		TransLogHandler.dataChanged(if (created) TransType.created else TransType.dataChanged, theClass.id, inst, dataPos, dataLen, writeInstant = false)
		if (propLen != 0) TransLogHandler.dataChanged(TransType.propertyChanged, theClass.id, inst, propPos, propLen, writeInstant = false)
		if (linkLen != 0) TransLogHandler.dataChanged(TransType.linksChanged, theClass.id, inst, linkPos, linkLen, writeInstant = false)
		if (collLen != 0) TransLogHandler.dataChanged(TransType.collFuncChanged, theClass.id, inst, collPos, collLen, writeInstant = false)
		TransLogHandler.flushCombiWrite()
	}


	def reorgWriteRecord(inst: Int, dataPos: Long, dataLen: Int, propPos: Long, propLen: Int, linkPos: Long, linkLen: Int, collPos: Long, collLen: Int): Unit = {
		writeBufferStream.reset()
		outStream.writeInt(inst)
		outStream.writeLong(dataPos)
		outStream.writeInt(dataLen)
		outStream.writeLong(propPos)
		outStream.writeInt(propLen)
		outStream.writeLong(linkPos)
		outStream.writeInt(linkLen)
		outStream.writeLong(collPos)
		outStream.writeInt(collLen)
		theFile.write(writeBufferStream.buffer, 0, staticData.recordSize)
	}

	private def internalWrite(inst: Int, dataPos: Long, dataLength: Int,
														ixOffset: Int): Unit = { // Offset position in the index record
		if (inst > lastID) // create new Instance
			throw new IllegalArgumentException("Storing wrong instance" + inst + " in class " + theClass.name)

		theFile.seek(findIxRecord(inst) * staticData.recordSize + 4 + ixOffset)
		resetLastReadID()
		miniwriteBufferStream.reset()
		miniOutStream.writeLong(dataPos)
		miniOutStream.writeInt(dataLength)
		theFile.write(miniwriteBufferStream.buffer, 0, 12)
	}

	def writePropertiesData(inst: Int, dataPos: Long, dataLength: Int, withLog: Boolean = true): Unit = {
		internalWrite(inst, dataPos, dataLength, 8 + 4)
		if (withLog) TransLogHandler.dataChanged(TransType.propertyChanged, theClass.id, inst, dataPos, dataLength)
	}

	def writeLinksData(inst: Int, dataPos: Long, dataLength: Int, withLog: Boolean = true): Unit = {
		internalWrite(inst, dataPos, dataLength, (8 + 4) * 2)
		if (withLog) TransLogHandler.dataChanged(TransType.linksChanged, theClass.id, inst, dataPos, dataLength)
	}

	def writeCollFuncData(inst: Int, dataPos: Long, dataLength: Int, withLog: Boolean = true): Unit = {
		internalWrite(inst, dataPos, dataLength, (8 + 4) * 3)
		if (withLog) TransLogHandler.dataChanged(TransType.collFuncChanged, theClass.id, inst, dataPos, dataLength)
	}

	def foreachInstance(func: (Reference) => Unit): Unit =
		for (i <- 1 to lastID; if instanceExists(i))
			func(Reference(theClass.id, i))


	def foreachInstance(listener: RecordListener): Unit = {
		theFile.seek(0)
		for (i <- 0 until numRecords - tailSpace) {
			theFile.read(readBuffer, 0, staticData.recordSize)
			inBufferStream.reset()
			val inst = dataInStream.readInt
			val dataPos = dataInStream.readLong
			if (dataPos >= 0) listener.nextRecord(inst, dataPos, dataInStream.readInt, dataInStream.readLong, dataInStream.readInt,
				dataInStream.readLong, dataInStream.readInt, dataInStream.readLong, dataInStream.readInt)
		}
	}

	def getInstancesRefs: Array[Reference] = readFully().filter(_.dataPos >= 0).map(record => new Reference(theClass.id, record.inst))


	def deleteInstance(inst: Int, withLog: Boolean = true): Unit = {
		internalWrite(inst, -1, 0, 0)
		if (withLog) TransLogHandler.dataChanged(TransType.deleted, theClass.id, inst, 0, 0)
	}


	def bulkGetInstanceRecords(startInst: Int, endInst: Int, dataFileHandler: BoolContFileHandler[InstanceData]): immutable.IndexedSeq[InstanceData] = {
		if (bulkReadBuffer.length < (endInst - startInst + 1) * staticData.recordSize)
			bulkReadBuffer = Array.ofDim[Byte](staticData.recordSize * (endInst - startInst + 1))

		theFile.seek(findIxRecord(startInst) * staticData.recordSize)
		theFile.read(bulkReadBuffer, 0, staticData.recordSize * (endInst - startInst + 1))
		//System.out.println("buffer read "+(endInst-startInst+1).toInt)
		//inBufferStream.reset
		for (i <- startInst to endInst; offset = (i - startInst) * staticData.recordSize + 4;
				 rPos = getLong(bulkReadBuffer, offset);
				 rSize = getInt(bulkReadBuffer, offset + 8); if rSize != 0 && rPos >= 0) yield
			dataFileHandler.readWithBool(Reference(theClass.id, i), rPos, rSize, getLong(bulkReadBuffer, offset + 8 + 4) != 0)
	}


	def bulkPushInstanceRecords(startInst: Int, endInst: Int, dataFileHandler: BoolContFileHandler[InstanceData],
															out: DataOutput): Unit = {
		if (bulkReadBuffer.length < (endInst - startInst + 1) * staticData.recordSize)
			bulkReadBuffer = Array.ofDim[Byte](staticData.recordSize * (endInst - startInst + 1))

		theFile.seek(findIxRecord(startInst) * staticData.recordSize)
		theFile.read(bulkReadBuffer, 0, staticData.recordSize * (endInst - startInst + 1))
		//System.out.println("buffer push "+(endInst-startInst+1).toInt)
		//inBufferStream.reset
		for (i <- startInst to endInst) {
			val offset = (i - startInst) * staticData.recordSize + 4
			out.writeInt(theClass.id) // reference
			out.writeInt(i)
			val rpos = getLong(bulkReadBuffer, offset)
			//if(rpos<0) println("bulkPush typ:"+theClass.id+" "+theClass.name +" inst:"+i+" pos ="+rpos)
			dataFileHandler.pushData(rpos, getInt(bulkReadBuffer, offset + 8), out)
			out.writeBoolean(getLong(bulkReadBuffer, offset + 8 + 4) != 0)
		}
	}


	protected def getLong(readBuffer: Array[Byte], pos: Int): Long =
		(readBuffer(pos).toLong << 56) +
			((readBuffer(pos + 1) & 255).toLong << 48) +
			((readBuffer(pos + 2) & 255).toLong << 40) +
			((readBuffer(pos + 3) & 255).toLong << 32) +
			((readBuffer(pos + 4) & 255).toLong << 24) +
			((readBuffer(pos + 5) & 255).toLong << 16) +
			((readBuffer(pos + 6) & 255).toLong << 8) +
			((readBuffer(pos + 7) & 255).toLong << 0)


	protected def getInt(readBuffer: Array[Byte], pos: Int): Int =
		((readBuffer(pos + 0) & 255) << 24) +
			((readBuffer(pos + 1) & 255) << 16) +
			((readBuffer(pos + 2) & 255) << 8) +
			((readBuffer(pos + 3) & 255) << 0)


	def getInstanceRecord(inst: Int, pos: Option[Int] = None): IndexRecord = {
		//System.out.println("getinstance Record inst"+inst+" lastReadID:"+lastReadID)
		if (lastReadID != (inst - 1)) // optimize: if this call is the subsequent inst of the last call, try not to seek
			theFile.seek((pos match {
				case Some(npos) => npos
				case None => findIxRecord(inst)
			}) * staticData.recordSize)
		//else print("o ")
		lastReadID = inst
		theFile.read(readBuffer, 0, staticData.recordSize)
		inBufferStream.reset()
		new IndexRecord(dataInStream.readInt, dataInStream.readLong, dataInStream.readInt, dataInStream.readLong, dataInStream.readInt,
			dataInStream.readLong, dataInStream.readInt, dataInStream.readLong, dataInStream.readInt)
	}


	def readFully(): Array[IndexRecord] = {
		val retArray = Array.ofDim[IndexRecord](numRecords)
		theFile.seek(0)
		for (i <- 0 until numRecords) {
			theFile.read(readBuffer, 0, staticData.recordSize)
			inBufferStream.reset()
			retArray(i) = new IndexRecord(dataInStream.readInt, dataInStream.readLong, dataInStream.readInt, dataInStream.readLong, dataInStream.readInt,
				dataInStream.readLong, dataInStream.readInt, dataInStream.readLong, dataInStream.readInt)
		}
		retArray
	}
}


/** Index record for instance data
 * 
 */
class IndexRecord(val inst: Int, val dataPos: Long, val dataLength: Int, val propPos: Long, val propLength: Int,
									val linkPos: Long, val linkLength: Int, val collPos: Long, val collLength: Int) {

}