package server.webserver

import definition.data.{InstanceData, OwnerReference}
import definition.expression.StringConstant
import definition.typ.SystemSettings
import server.comm.UserInfo
import server.config.ServerSystemSettings
import server.storage.{ActionNameMap, StorageManager}
import transaction.handling.TransactionManager
import util.{BarSplitList, ColonSplit, StrToInt}

import scala.util.control.NonFatal

/**
 * Created by Kathi on 07.07.2015.
 */
object WebUserSetting {

  val tableSettingsName="TableSettings"
  lazy val folderType=SystemSettings.settings.systemTypes("Folder")
  lazy val tableSetActionID=ActionNameMap.getActionID(tableSettingsName)

    def getTableSettingsFolder(user:UserInfo):InstanceData={
    SystemSettings() match {
      case ses:ServerSystemSettings=>
        val rootRef=ses.getUserRoot(user.name)
        StorageManager.loadChildren(rootRef, -1, 3).
          find(_.fieldValue(0).toString.equalsIgnoreCase(tableSettingsName)) match {
          case Some(settingsFolder )=> settingsFolder
          case None =>
            println("create tablesettingsFolder ")
            var newInst:InstanceData=null
            TransactionManager.doTransaction(user.id,tableSetActionID,rootRef,false,folderType,{
              newInst=TransactionManager.tryCreateInstance(folderType,Array(OwnerReference(3,rootRef)),false)
              TransactionManager.tryWriteInstanceField(newInst.ref,0,StringConstant(tableSettingsName))
            })
            newInst
        }
      case o=> util.Log.e("cant find system settings");null
    }
  }

  def getTableSettingString(user: UserInfo) =
    getTableSettingsFolder(user).fieldValue(1).toString


  def writeTableSetting(user:UserInfo,typ:Int,setting:String)= {
    val settingsFolder =getTableSettingsFolder(user)
    var found=false
    val concat=settingsFolder.fieldValue(1).toString match {
      case BarSplitList(barList)=> {println("old list "+barList.mkString(" "))
        (for (b<-barList) yield b.trim match {
          case ret1@ ColonSplit(StrToInt(tableType),tableSet)=> if(tableType==typ) {
            found=true
            typ.toString+":"+setting
          } else ret1
          case other=>other
        }).mkString("|")
      }
      case other =>""
    }

    val resultString=if(found) concat else concat+( if(concat.length==0)"" else "|")+typ+":"+setting
    TransactionManager.doTransaction(user.id, tableSetActionID, settingsFolder.ref, false, folderType,{
      TransactionManager.tryWriteInstanceField(settingsFolder.ref, 1, StringConstant(resultString))
    })
  }

}
