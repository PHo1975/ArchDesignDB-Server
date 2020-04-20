package server.storage

import java.io.File

import definition.data.{Reference, TransType}
import definition.typ.BlockClass
import server.config.FSPaths

trait BlockRecordListener {
  def nextRecord(inst: Int, dataPos: Long): Unit
}


object BlockIndexHandlerStaticData extends InstanceHandlerStaticData {
  final val recordSize: Int = 8 + 4
  val appendBufferSize = 10
  val appendBufferHandler: AppendBufferHandler =new AppendBufferHandler(recordSize,appendBufferSize){
    def readTail(): Unit = {
      appendDataIn.readLong
    }
    def writeTail():Unit={
      appendDataOut.writeInt(-1)
      appendDataOut.writeLong(-1)
    }
  }
}


class BlockIndexHandler(val theClass: BlockClass) extends AbstractInstanceHandler {
  def fileName = new File(FSPaths.dataDir + theClass.name + ".bix")
  def staticData:InstanceHandlerStaticData  =BlockIndexHandlerStaticData

  val blockBuffer=new Array[Byte](theClass.blocksize)

  outStream.writeLong(0)
  outStream.writeInt(0)

  println(getInfoString)

  def className: String =theClass.name

  def getInfoString:String="<html>BlockIndex "+className+" tailSpace:"+tailSpace+" numRecords:"+numRecords+"<br> firstID:"+firstID+" lastID:"+lastID+"</html>"

  def writeBench(value:Int): Unit = {}

  def createInstance(): Int = {
    val inst = lastID + 1
    writeBufferStream.reset()
    outStream.writeInt(inst)
    outStream.writeLong(0)
    append(writeBufferStream.buffer)
    lastID = inst
    inst
  }

  def writeData(inst: Int, dataPos: Long, created: Boolean, withLog: Boolean = true): Unit = {
    internalWrite(inst, dataPos)
    if (withLog)
      TransLogHandler.dataChanged(if (created) TransType.created else TransType.dataChanged, -1*theClass.id, inst, dataPos, theClass.blocksize)
  }

  private def internalWrite(inst: Int, dataPos: Long ): Unit = {
    if (inst > lastID) // create new Instance
      throw new IllegalArgumentException("Storing wrong instance" + inst + " in class " + theClass.name)

    resetLastReadID()
    //miniwriteBufferStream.reset()
    //miniOutStream.writeLong(dataPos)
    theFile.seek(findIxRecord(inst) * staticData.recordSize + 4 )
    theFile.writeLong(dataPos)
  }

  /*def reorgWriteRecord(inst: Int, dataPos: Long): Unit = {
    writeBufferStream.reset()
    outStream.writeInt(inst)
    outStream.writeLong(dataPos)
    theFile.write(writeBufferStream.buffer, 0, staticData.recordSize)
  }*/

  def foreachInstance(func: Reference => Unit): Unit =
    for (i <- 1 to lastID; if instanceExists(i))
      func(Reference(theClass.id, i))


  def foreachInstance(listener: (Int,Long)=>Unit): Unit = {
    theFile.seek(0)
    resetLastReadID()
    for (_ <- 0 until numRecords - tailSpace) {
      theFile.read(readBuffer, 0, staticData.recordSize)
      inBufferStream.reset()
      val inst = dataInStream.readInt
      val dataPos = dataInStream.readLong
      if (dataPos >= 0)
        listener(inst, dataPos)
    }
  }

  def deleteInstance(inst: Int, withLog: Boolean = true): Unit = {
    internalWrite(inst, -1 )
    if (withLog) TransLogHandler.dataChanged(TransType.deleted, -1*theClass.id, inst, 0, 0)
  }

  def getInstanceRecord(inst: Int, pos: Option[Int] = None):(Int,Long)=
    (inst,getDataPos(inst,pos))

  def readFully(): Array[(Int,Long)] = {
    val retArray = Array.ofDim[(Int,Long)](numRecords)
    resetLastReadID()
    theFile.seek(0)
    for (i <- 0 until numRecords) {
      theFile.read(readBuffer, 0, staticData.recordSize)
      inBufferStream.reset()
      retArray(i) = (dataInStream.readInt, dataInStream.readLong)
    }
    retArray
  }



}
