package server.storage

import java.io._

object RafTest {
  protected var theFile= new RandomAccessFile("/home/peter/test.txt","rwd")
  final val recordSize: Int = 100
  val numBuffers=20
  var currBufferPos=20
  val buffer: Array[Byte] =Array.fill[Byte](recordSize)((Math.random*256d).toByte)
  val bigBuffer: Array[Byte] =Array.fill[Byte](recordSize*numBuffers)(0)
  var time:Long=0l

  def log(st:String): Unit ={
    val newTime=System.currentTimeMillis()
    println(st+" "+(newTime-time))
    time=newTime
  }

  def append(b:Array[Byte]):Unit = {
    /*if(currBufferPos>numBuffers-1) {
      Array.copy(b,0,bigBuffer,0,recordSize)
      theFile.seek(theFile.length())
      theFile.write(bigBuffer)
      currBufferPos=1
    } else {
      theFile.seek(theFile.length()-recordSize*(20-currBufferPos))*/
      theFile.write(b)
      /*currBufferPos+=1
    }*/
  }


  def main(args:Array[String]):Unit = {
    println("start "+File.listRoots().mkString(", "))
    try {
      theFile.seek(theFile.length())
      for (i <- 0 to 20) append(buffer)
      time = System.currentTimeMillis()
      append(buffer)
      log("write one")
      for (i <- 0 to 10) append(buffer)
      log("write 10")
      for (i <- 0 to 20) append(buffer)
      log("write 10")
      for (i <- 0 to 40) append(buffer)
      log("write 10")
      for (i <- 0 to 100) append(buffer)
      log("write 10")
    } catch {
      case e:Throwable => println("Fehler "+e)
    }
  }
}
