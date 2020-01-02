package client.calender

import client.comm.{ClientQueryManager, UserSettings}
import definition.comm.UserInfo
import javafx.collections.{FXCollections, ObservableList}
import javafx.scene.control.ListCell

import scala.jdk.CollectionConverters._



object UserList {
  
  val list: ObservableList[UserInfo] =FXCollections.observableList[UserInfo](new java.util.ArrayList[UserInfo])
  
  lazy val knownUsers:Array[Int]=UserSettings.basicFolders.get("KnownUsers") match {
      case Some(calRef)=>        
        (ClientQueryManager.queryInstance(calRef, -1).headOption match {
          case Some(data)=> data.fieldValue(1).toString 
          case None => util.Log.e("known users object not loaded");""
        }).split(",").map(st=> util.StringUtils.stringToInt(st))        
        
      case None=>util.Log.e("KnownUsers not found in UserSettings "); Array.empty
  }
  private lazy val intList=UserSettings.userList.filter(us=>knownUsers.contains(us.id)).sortBy(_.name)
  def load(): Boolean = {
    list.addAll(intList.asJava)
  }  
      
  def getShortName(userId:Int): String = intList.find(_.id==userId) match {
    case Some(user)=> user.shortName
    case _ => "-"
  } 
}


class UserCell extends ListCell[UserInfo] {
  override def updateItem(item:UserInfo,empty:Boolean): Unit = {
    super.updateItem(item,empty)
    if(item!=null) {	    
	    setText("("+item.shortName+") "+item.name)
	    getStyleClass.add("ProjectCell")
    }
  } 
}
