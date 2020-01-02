package server.storage

import java.io.{DataInputStream, DataOutputStream, File, RandomAccessFile}

import util.UnsyncBAInputStream

import scala.annotation.tailrec

trait InstanceHandlerStaticData{
  val recordSize:Int
  val appendBufferSize:Int
  val appendBufferHandler:AppendBufferHandler
}

trait AbstractInstanceHandler {
  protected def fileName:File
  protected def className:String
  protected def staticData:InstanceHandlerStaticData
  protected var theFile:RandomAccessFile=new RandomAccessFile(fileName, "rw")
  protected var tailSpace: Int = staticData.appendBufferHandler.readTailSpace(theFile)
  protected var numRecords: Int = (theFile.length / staticData.recordSize).toInt
  protected var firstID: Int = if (numRecords > 0) readIxInst(0) else 0
  protected var lastID: Int = if (numRecords > 0) readIxInst(numRecords - 1 - tailSpace) else 0
  protected var lastReadID: Int = -2
  protected var lastSeekID: Int = -2
  protected var lastSeekPos: Option[Int] = None
  protected val writeBufferStream = new MyByteStream(staticData.recordSize)
  protected val miniwriteBufferStream = new MyByteStream(12)
  protected val miniOutStream = new DataOutputStream(miniwriteBufferStream)
  protected val outStream = new DataOutputStream(writeBufferStream)

  protected val readBuffer: Array[Byte] = Array.ofDim[Byte](staticData.recordSize)
  protected var bulkReadBuffer: Array[Byte] = readBuffer
  protected val inBufferStream = new UnsyncBAInputStream(readBuffer)
  protected val dataInStream = new DataInputStream(inBufferStream)

  def flush(): Unit = theFile.getChannel.force(true)

  protected def writeBench(value: Int): Unit

  protected def append(b: Array[Byte]): Unit = {
    resetLastReadID()
    //println("append "+tailSpace)
    if (tailSpace > 0) {
      theFile.seek(theFile.length - tailSpace * staticData.recordSize)
      theFile.write(b, 0, staticData.recordSize)
      tailSpace -= 1
    } else {
      staticData.appendBufferHandler.addTail(theFile, b)
      numRecords += staticData.appendBufferSize
      tailSpace = staticData.appendBufferSize - 1
    }
  }

  def resetLastReadID(): Unit = lastReadID = -2


  protected def readIxInst(pos: Int): Int = {
    theFile.seek(pos * staticData.recordSize)
    theFile.readInt

  }

  // does the specified instance exist ?
  def instanceExists(inst: Int): Boolean = {
    //println("Exist  "+inst+" first:"+firstID+" last:"+lastID)
    if (inst < firstID || inst > lastID) false
    else {
      resetLastReadID()
      internFindIxRecord(inst) match {
        case sp@Some(pos) =>
          getDataPos(inst, sp)  > -1
        case None => false
      }
    }
  }

  def getDataPos(inst: Int, pos: Option[Int] = None): Long = {
    if (lastReadID != (inst - 1)) // optimize: if this call is the subsequent inst of the last call, try not to seek
      theFile.seek((pos match {
        case Some(npos) => npos
        case None => findIxRecord(inst)
      }) * staticData.recordSize)
    lastReadID = inst
    theFile.read(readBuffer, 0, staticData.recordSize)
    inBufferStream.reset()
    dataInStream.readInt
    dataInStream.readLong
  }

  protected def internFindIxRecord(inst: Int): Option[Int] =
    if (inst == lastSeekID) {
      writeBench(-1)
      lastSeekPos
    }
    else {
      if (numRecords == 0 || inst < firstID || inst > lastID) None
      else if (inst == lastID) {
        writeBench(-2)
        Some(numRecords - 1 - tailSpace)
      }
      else {
        lastSeekID = inst
        // binary search
        var seektime = 0

        @tailrec
        def finder(lower: Int, upper: Int): Option[Int] = {
          seektime += 1
          if (upper < lower) return None
          val mid = (upper + lower) / 2
          val instAtMid: Int = readIxInst(mid)
          if (instAtMid > inst) finder(lower, mid - 1)
          else if (instAtMid < inst) finder(mid + 1, upper)
          else Some(mid)
        }

        lastSeekPos = finder(0, numRecords - tailSpace)
        writeBench(seektime)
        lastSeekPos
      }
    }

  protected def findIxRecord(inst: Int): Int = {
    resetLastReadID()
    internFindIxRecord(inst) match {
      case Some(v) => v
      case None => throw new IllegalArgumentException("Instance " + inst + " not found in class " + className)
    }
  }

  def shutDown(): Unit = theFile.close()

  def takeOverFromReorgFile(reorgFile: File): Unit = {
    theFile.close()
    val backupFile = new File(fileName.toString + ".bak")
    if (backupFile.exists()) backupFile.delete
    fileName.renameTo(backupFile)
    reorgFile.renameTo(fileName)
    theFile= new RandomAccessFile(fileName, "rwd")
    numRecords = (theFile.length / staticData.recordSize).toInt
    firstID = if (numRecords > 0) readIxInst(0) else 0
    lastID = if (numRecords > 0) readIxInst(numRecords - 1) else 0
    lastReadID = -2
    lastSeekID = -2
    lastSeekPos = None
  }
}
