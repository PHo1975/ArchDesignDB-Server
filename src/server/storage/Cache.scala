/**
 * Author: Peter Started:27.07.2010
 */
package server.storage

import scala.collection.mutable.ArraySeq
import scala.reflect.Manifest

import definition.data.Referencable


/** caches the Instances of a Type
 * 
 */
class Cache[T >: Null <: Referencable](val typ:Int)(implicit m: Manifest[T]) {
	final val cacheSize=20
  //val cache:Array[T]= Array(cacheSize)	
	lazy val cache=new Array[T](cacheSize)

	var used=false
  var pointer=0
  var cacheHit=0
  var cacheMiss=0
  var added=0
	
  def getInstanceData (inst:Int):Option[T] = if(!used)None else {
  		for(i <- 0 until cacheSize;c=cache(i);if c != null && c.ref.instance == inst) {
  			cacheHit +=1
  			return Some(c)
  		}
  	cacheMiss +=1
  	None
  }
	
	def putInstanceData(inst:T):Unit = {
		used=true
		added+=1
		for(i <- 0 until cacheSize;c=cache(i);if c != null && c.ref.instance == inst.ref.instance) {
  			cache(i)=inst
  			return 
  		}
		pointer += 1
		if(pointer >=cacheSize) pointer = 0
		cache(pointer)=inst
	}	
	
	def removeInstanceData (inst:Int):Unit = if(used) {
		for(i <- 0 until cacheSize;c=cache(i);if c != null && c.ref.instance == inst){
  			cache(i)=null.asInstanceOf[T]
  			return 
  		}
	}

	def clear()= if(used ){
		for(i<-0 until cacheSize)cache(i)= null
		pointer=0;
		used=false;
	}
	
	override def toString() = m.toString+ "-Cache for type "+typ+" miss:"+cacheMiss+" hit:"+cacheHit+ " add:"+added+" P:"+pointer
	
}


