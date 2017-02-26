package management.databrowser

import definition.data.Reference
import util.StringUtils
import scala.swing.BoxPanel
import javax.swing.{JOptionPane, BorderFactory}
import scala.swing.Button
import scala.swing.Table
import scala.swing.ScrollPane
import javax.swing.border.TitledBorder
import scala.swing.Orientation
import client.dataviewer.FieldColumnModel
import javax.swing.table.AbstractTableModel
import server.comm.{UserInfo, UserList}
import scala.swing.Swing
import scala.swing.event.ButtonClicked

class UserTableModel extends AbstractTableModel {
  def getColumnCount= 8

  def getRowCount= UserList.userIterator.foldLeft(0) ( (sum,user)=>
    sum+user.flattenSize)

  def getValueAt(row:Int,col:Int):Object = {
    if(row>=getRowCount) return null
    val (userID,connectionID)=UserList.findConnection(row)
    col match {
      case 0=> userID.id.toString
      case 1=> userID.name
      case 2=> userID.shortName
      case 3=> if (connectionID==null) "" else connectionID.app
      case 6=> userID.startRef.bToString()
      case 7=> userID.roles.mkString(",")
      case _=> if (connectionID==null) "" else {
        if(col==4) connectionID.getRemoteAndress
        else if(col==5)connectionID.getPort
        else null
      }
    }
  }
}


class UserPanel extends BoxPanel(Orientation.Vertical) {
  lazy val subsPanel=new SubscriptionPanel
  val fieldColMod=new FieldColumnModel{
    createColumn(1,"Name",85)
    createColumn(2,"Sh",25)
    createColumn(3,"App",45)
    createColumn(4,"Online address",100)
    createColumn(5,"port",45)
    createColumn(6,"startRef",65)
    createColumn(7,"roles",100)
  }

  this.xLayoutAlignment=0.5f
  val belowTopBorder = BorderFactory.createTitledBorder("User")
  belowTopBorder.setTitlePosition(TitledBorder.BELOW_TOP)
  border = belowTopBorder
  val logOffUserBut=new Button("Log user off")
  val shutDownUserBut=new Button("ShutDown user")
  val createUserBut=new Button("Create User")
  val changePasswordBut=new Button("Change Password")
  val changestartRefBut=new Button("Change startRef")
  val changeRoleBut=new Button("Change Roles")
  val deleteUserBut=new Button("Delete User")
  val showSubsBut=new Button("Show Subscriptions")
  val fixBookmarksBut=new Button("Fix Bookmarks")
  listenTo(shutDownUserBut,logOffUserBut,createUserBut,changePasswordBut,changestartRefBut,deleteUserBut,showSubsBut,fixBookmarksBut,changeRoleBut)

  val table=new Table()
  table.autoResizeMode=Table.AutoResizeMode.Off
  table.selection.intervalMode=Table.IntervalMode.Single
  table.selection.elementMode=Table.ElementMode.Row
  table.peer.setAutoCreateColumnsFromModel(false)
  table.peer.setColumnModel(fieldColMod)

  val model=new UserTableModel
  table.model=model
  val receiver= ()=>{MainWindow.runSw{
    //println("Fire user :"+UserList.theMap.size)
    model.fireTableDataChanged()
  }}
  UserList.registerListener(receiver )
  val scroller=new ScrollPane {
    viewportView=table
  }
  contents+=scroller+=Swing.VStrut(10)+=logOffUserBut+=
    shutDownUserBut+=createUserBut+=changePasswordBut+=changestartRefBut+=changeRoleBut+=deleteUserBut+=
    showSubsBut+=fixBookmarksBut


  def changeProperty(prompt:String,checkFunc:(String,UserInfo)=>Option[UserInfo])=table.selection.rows.headOption match {
    case Some(ix) => JOptionPane.showInputDialog(table.peer,prompt) match {
      case null | "" =>
      case text => val (oldUser,_)=UserList.findConnection(ix)
        for(newUser<-checkFunc(text,oldUser)){
          UserList.addUser(newUser)
          UserList.saveUserList()
        }
    }
    case None =>
  }

  reactions += {
    case ButtonClicked(`logOffUserBut`)=>table.selection.rows.headOption match {
      case Some(ix)=>UserList.findConnection(ix) match {
        case (_,null)=>
        case (user,connection)=>MainWindow.runAsThread{connection.tellToQuit()}
      }
      case None=>
    }

    case ButtonClicked(`shutDownUserBut`)=>table.selection.rows.headOption match {
      case Some(ix)=>UserList.findConnection(ix) match {
        case (_,null)=>
        case (user,connection)=>
          MainWindow.runAsThread{
            UserList.removeConnection(user.id,connection)
            connection.shutDown()
          }
      }
      case None =>
    }
    case ButtonClicked(`showSubsBut`)=> table.selection.rows.headOption match {
      case Some(ix)=>UserList.findConnection(ix) match {
        case (_,null)=>
        case (user,connection)=>
          MainWindow.addRightPanelComponent(subsPanel)
          subsPanel.updateForUser(connection)
      }
      case None => println("cant find connection ")

    }
    case ButtonClicked(`changestartRefBut`)=> changeProperty("startRef eigeben typ,inst :",
      (text,oldUser)=>text match {
        case Reference(ref) => Some(oldUser.copy(startRef = ref))
        case _ => None
      })
    case ButtonClicked(`changeRoleBut`)=> changeProperty("Rollen (mit Komma getrennt):",
      (text,oldUser)=>Some(oldUser.copy(roles=text.trim.split(',').map(_.trim))))
    case ButtonClicked(`changePasswordBut`)=> changeProperty("neues Passwort:",
      (text,oldUser)=>Some(oldUser.copy(password=StringUtils.obfuscate(text.trim))))
  }
}