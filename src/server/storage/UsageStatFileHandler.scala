package server.storage

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, RandomAccessFile}
import java.util.{Calendar, Date, GregorianCalendar}

import server.config.FSPaths
import server.print.PrintEngine
import util.Log

import scala.collection.mutable




object UsageStatFileHandler {
  val fileName=FSPaths.dataDir+"UsageStat.log"
  var theFile= new RandomAccessFile(fileName,"rwd")
  val recordSize=2+2+4+36
  val readBuffer= new Array[Byte](recordSize) 
  val inBufferStream=new ByteArrayInputStream(readBuffer)
  val dataInStream=new DataInputStream(inBufferStream)
  val bufferStream=new MyByteStream(recordSize)
  val outStream=new DataOutputStream(bufferStream)
  val gc=new GregorianCalendar
  val mins=1000*60*5
  val dayMins=1000l*60*60*24
  
  var resultMap=mutable.HashMap[(Short,Int),Array[Byte]]()
  
  def getArray =new Array[Byte](36)
  
  def prepareWrite()=theFile.seek(theFile.length())
  
  def readArray()={
    val rArray=getArray
    dataInStream.read(rArray)
    rArray
  }
  
  def updateStatistics():Unit={    
    prepareWrite()
    val currentTransID=TransLogHandler.getTransID
    val lastStatTransID=FSPaths.lastStatTransID
    Log.w("updating statistics ... currID:"+currentTransID+" lastStatTransID:"+lastStatTransID)
    if(currentTransID>lastStatTransID)
    try {
      var currentDay:Option[Long]=None
      resultMap.clear()
      TransDetailLogHandler.loop(lastStatTransID+1,currentTransID, (time,user,ref)=>
        if(time==0) print(" n ") else{
        val prInst= StorageManager.getNextParentOfType(ref,PrintEngine.projectType) match {
          case Some(projectRef)=> projectRef.instance
          case None => -1
        }
        val theDate=new Date(time)
        val theDateConst=time /dayMins
        //println(f" data $theDate%tD $theDate%tH:$theDate%tM u:"+user+" r:"+ref+" pr:"+prInst+" "+currentDay)
        
        currentDay match {
          case None => currentDay=Some(theDateConst);print(" "+theDateConst)
          case Some(cd)=> if(theDateConst!=cd) {
            Log.w(" "+theDateConst)
            writeDay((cd).toShort)
            resultMap.clear()
            currentDay=Some(theDateConst)
          }          
        }
        val key=(user,prInst)
        val oldData=resultMap.getOrElse(key,getArray)
        putTime(theDate,oldData)
        resultMap(key)=oldData
      })
      for(cd<-currentDay) writeDay((cd).toShort)
    }    
    finally {
      flush()
      FSPaths.setLastStatTransID(currentTransID)
      println(" Statistics done")
    }
  }
  
  def putTime(time:Date,array:Array[Byte])={
    gc.setTime(time)
    gc.set(Calendar.HOUR_OF_DAY,0)
    gc.set(Calendar.MINUTE,0)
    gc.set(Calendar.SECOND,0)    
    val millis=time.getTime-gc.getTimeInMillis    
    val block=(millis/mins).toInt
    val theByte=block/8   
    array(theByte)=(array(theByte) | (1<< (block % 8))).toByte    
  }
  
  def decodeTime(day:Short,array:Array[Byte]):List[Date]={
    var ret:List[Date]=Nil 
    gc.setTimeInMillis(day*dayMins)
    gc.set(Calendar.HOUR_OF_DAY,0)
    val timeBase = gc.getTimeInMillis
    for(i<-35 to 0 by -1;b=array(i); if b > 0;
      bit<- 0.toByte until 8.toByte; if ((1 << bit) & b) > 0)
        ret=new Date(timeBase+(i*8+bit)*mins)::ret
    ret
  }
    
  
  def writeDay(day:Short):Unit=
    for (((user, prInst), array) <- resultMap) {
      /*print(" U:"+user+" pr:"+prInst)
      val dec=decodeTime(day,array)
      for(d<-dec)print(f" $d%tH:$d%tM,")
      println()*/
      bufferStream.reset()
      outStream.writeShort(day)
      outStream.writeShort(user)
      outStream.writeInt(prInst)
      outStream.write(array)
      theFile.write(bufferStream.buffer, 0, bufferStream.size())
    }

  
  def flush():Unit= theFile.getChannel().force(false)
  
  def loopAll(callBack:(Long,Short,Int,Array[Byte])=>Unit)= {
    theFile.seek(0)    
    var pos=0
    while(pos<theFile.length) {
      theFile.read(readBuffer,0,recordSize)
      inBufferStream.reset()
      gc.setTimeInMillis(dataInStream.readShort*dayMins)
      gc.set(Calendar.HOUR_OF_DAY,0)      
      callBack(gc.getTimeInMillis,dataInStream.readShort,dataInStream.readInt,readArray())
      pos+=recordSize
    }
  }
}