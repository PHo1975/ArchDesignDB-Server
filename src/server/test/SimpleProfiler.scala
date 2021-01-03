/**
 * Author: Peter Started:06.09.2010
 */
package server.test

/**
 * 
 */
object SimpleProfiler {
	val lock=new Object
  var startTime:Long=0
  @scala.volatile var lastTime:Long=0
  var dprint=false
  @scala.volatile var timeList:List[(String,Long)]=Nil
  
  def startMeasure(label:String): Unit = lock.synchronized{
  	if(dprint){
  		println("["+label+" ")  		
  	} else {
  		if(timeList.nonEmpty )printTimeList()
  		timeList=(label,0L)::Nil
  	}
  	startTime=System.currentTimeMillis()
		lastTime=startTime
  }
  
  def measure(label:String): Unit = lock.synchronized{
  	val between=System.currentTimeMillis()
		if(dprint) {
  		println(label+":"+(between-lastTime)+" ")  		
  	} else timeList=(label, between - lastTime)::timeList
  	lastTime=between
  }
  
  def measureEx(label:String): Unit = lock.synchronized{
  	val between=System.currentTimeMillis()
		if(dprint) {
  	    val dist= between - lastTime
  	    if(dist>10)
  	    	print(label+":"+dist+" ")  		
  	} else timeList=(label, between - lastTime)::timeList
  	lastTime=between
  }
  
  def finish(label:String): Unit = lock.synchronized{if(dprint) {
  	measure(label)
  	System.out.println(label+"= "+(lastTime-startTime))  	
  } else printTimeList()}
  
  def printTimeList(): Unit = lock.synchronized{
  	println("\n timeList:")
  	println(timeList.reverseIterator.map(a => a._1 + " :" + a._2.toString + " ms").mkString("\n"))
  	println()
  	timeList=Nil
  }
  
}