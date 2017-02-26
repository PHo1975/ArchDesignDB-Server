/**
 * Author: Peter Started:10.11.2010
 */
package client.model

import client.comm.UserSettings
import definition.data._
import client.comm.ClientQueryManager
//import collection.immutable._
/** saves the last used paths
 * 
 */

class BMEntry (var name:String,var list:Seq[Reference]){
	  override def toString= "| "*(list.size-BookmarkFactory.lowestLevel)+name
	}

object BookmarkFactory {	
  var lowestLevel=0
  var hasSetup=false
  var _pathList= collection.mutable.ArrayBuffer[BMEntry]() 
  //var standardRef:Reference=null
  
  lazy val standardPath:Seq[Reference]=List(UserSettings.rootRef)
  
  def pathList:Seq[BMEntry]={
    if(!hasSetup) loadSettings()
    _pathList
  } 
  
  def loadSettings():Unit=  {
    //System.out.println("bookmarkFactory load settings ")
  	val num=UserSettings.getIntProperty("TablePaths","num",0)
  	for(i <- 0 until num){
  	  val name=UserSettings.getStringProperty("TablePaths","N"+i.toString)
  	  val path=UserSettings.getListProperty[Reference]("TablePaths",i.toString,collection.immutable.Seq.empty)
  	  _pathList += new BMEntry(name,path)
  	}
  	updateList()  	
  	/*standardPath=UserSettings.getStringProperty("TablePath","standard","") match {
  	  case Reference(ref)=>println("standardPath="+ref);List(ref)
  	  case _=>println("StandardPath=null"); List(UserSettings.rootRef)
  	}*/
  	ClientQueryManager.registerStoreSettingsListener (() => {
	  	//System.out.println("store path info "+pathList.mkString)
	  	UserSettings.setIntProperty("TablePaths","num",_pathList.size)
	  	for(i <-_pathList.indices;el=_pathList(i)){  	  
	  	  UserSettings.setListProperty[Reference]("TablePaths",i.toString,el.list)
	  	  UserSettings.setStringProperty("TablePaths","N"+i.toString,_pathList(i).name)
	  	}  		
	  }	)
  	hasSetup=true
  }
  
  def addBookmark(nname:String,npath:Seq[Reference])= {
    _pathList += new BMEntry(nname,npath)
    updateList()
  }
  
  private def updateList()={
    _pathList=_pathList.sortWith(comparePath)
    lowestLevel=_pathList.foldLeft(Int.MaxValue)((a,b)=>if(b.list.size<a)b.list.size else a)    
  }
  
  def deleteBookmark(ix:Int) = {
    _pathList.remove(ix)
    updateList()
  }
  
  def comparePath(entry1:BMEntry,entry2:BMEntry):Boolean = {
    //println("Compare "+entry1+" "+entry2)
    val path1=entry1.list
    val path2=entry2.list
    val compareLen=math.min(path1.size,path2.size)
    for(i <-0 until compareLen) {
      val result=path1(i).compareTo(path2(i))
      //println("Result "+i+": "+result)
      if(result<0) return true
      if(result>0) return false
    }
    //println("Size:" +(path1.size<path2.size))
    path1.size<path2.size
  }  
  
}