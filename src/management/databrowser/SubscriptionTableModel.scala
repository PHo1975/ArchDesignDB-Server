package management.databrowser

import client.dataviewer.{FieldColumnModel, ViewConstants}
import definition.typ.AllClasses
import javax.swing.table.AbstractTableModel
import server.comm._

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, BoxPanel, Button, Label, Orientation, ScrollPane, Table}

class SubscriptionTableModel extends AbstractTableModel {
  var subsList:Seq[SubscriptionInfo]=Nil
  def getColumnCount= 3

  def getRowCount: Int = subsList.size
  
  def getValueAt(row:Int,col:Int):Object = {
    if(row>=getRowCount) return null
    val subs=subsList(row)
    col match {
      case 0=> subs.id.toString
      case 1=> AllClasses.refToString(subs.parentRef)
      case 2=> subs match {
        case a:PathSubscription=>"Path:"+a.path.map(_.sToString).mkString("; ")
        case s:SingleSubscription=>"Single"
        case p:PropSubscription=>
          "PropField:"+p.propertyField+" "+AllClasses.get.getClassByID(subs.parentRef.typ).propFields(p.propertyField).name
      }
      case _=> null
    }
  }
}



class SubscriptionPanel extends BorderPanel() {
  var userID: AbstractConnectionEntry = _
  
  val fieldColMod=new FieldColumnModel{
    	createColumn(0,"ID",30)
    	createColumn(1,"ParentRef",175)
    	createColumn(2,"Data",460)    	    	
  }
  
  val updateBut=new Button("Update")
  val topLabel: Label = ViewConstants.label()
  val table=new Table()
  table.autoResizeMode=Table.AutoResizeMode.Off
	table.selection.intervalMode=Table.IntervalMode.Single
	table.selection.elementMode=Table.ElementMode.Row
	table.peer.setAutoCreateColumnsFromModel(false)
  table.peer.setColumnModel(fieldColMod)

  val model=new SubscriptionTableModel
  table.model=model
  
  val scroller=new ScrollPane {
  	viewportView=table
  }
  add(topLabel,BorderPanel.Position.North)
  add(scroller,BorderPanel.Position.Center)
  add(new BoxPanel(Orientation.Horizontal){
    contents+=updateBut  
  },BorderPanel.Position.South)
  listenTo(updateBut)
  reactions+= {
    case ButtonClicked(`updateBut`)=> if(userID!=null) {
      update()
    }
  }

  def updateForUser(nuserID: AbstractConnectionEntry): Unit = {
    topLabel.text=" Subscriptions for User "+nuserID.userName+" app:"+nuserID.app
    userID=nuserID
    update()
  }

  def update(): Unit = {
    model.subsList=CommonSubscriptionHandler.
    subscriptionList.values.filter(_.connectionEntry==userID).toSeq.sortBy(_.id)
    model.fireTableDataChanged()
  }
}