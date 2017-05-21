package server.comm

import java.util.concurrent.{ExecutorService, Executors}

/**
 * Created by Peter Holzer on 25.03.2017 .
 */
object ThreadPool {
  val myPool: ExecutorService = Executors.newCachedThreadPool()

  def runInPool(a: => Unit): Unit = {
    myPool.execute(() => a)
  }
}
