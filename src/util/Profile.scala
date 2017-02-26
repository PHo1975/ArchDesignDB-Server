package util

/**
 * Created by Kathi on 10.01.2016.
 */
object Profile {
  @volatile var startTime:Long=0
  @volatile var lastTime:Long=0

  def start()= {
    startTime=System.currentTimeMillis()
    lastTime=startTime
  }

  def measure(st:String)={
    val ntime=System.currentTimeMillis()
    util.Log.w(">"+st+" "+(ntime-startTime)+" d:"+(ntime-lastTime))
    lastTime=ntime
  }
}
