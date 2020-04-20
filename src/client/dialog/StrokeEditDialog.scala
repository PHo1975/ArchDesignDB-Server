package client.dialog

import java.awt.Dimension

import client.comm.{ClientQueryManager, KeyStrokeManager}
import client.ui.ViewConstants
import javax.swing.KeyStroke
import javax.swing.table.AbstractTableModel

import scala.swing.event.{ButtonClicked, KeyEvent, KeyPressed, ListSelectionChanged}
import scala.swing.{BorderPanel, BoxPanel, Button, Dialog, Label, ListView, Orientation, ScrollPane, Swing, Table, Window}


class CommandModel extends AbstractTableModel {
  var currentGroupName:String= _
  var currentCommands:Seq[(String,String,String)]=Nil

  override def getColumnName(column: Int): String = column match {
    case 0 => "Befehl"
    case 1 => "Tastenkürzel"
    case _ => "Belegt"
  }

  def getRowCount: Int = currentCommands.size
  def getColumnCount = 3
  def getValueAt(row: Int, col: Int): AnyRef = {
  	val com=currentCommands(row)
  	col match {
  	  case 0=> com._1
  	  case 1=> com._2
  	  case 2=> com._3
  	}  	 
  }

  override def getColumnClass(column: Int): Class[String] = classOf[String]
  override def isCellEditable(row: Int, column: Int) = false

  def loadGroup(groupName: String): Unit = {
    currentGroupName=groupName
    val group=KeyStrokeManager.getGroup(groupName)
    val allCommandSet=collection.immutable.SortedSet[String]()++group.receiverMap.keySet++group.commandMap.keySet
    val set=allCommandSet.map {commandName=> {
      (commandName,if(group.commandMap.contains(commandName)) KeyStrokeManager.keyStrokeToString(group.commandMap(commandName)) else " - ",
          if(group.receiverMap.contains(commandName)) group.receiverMap(commandName).getClass.toString.split('.').last else " - ")
    }}
    //val set=collection.immutable.SortedMap[String,String]() ++ group.receiverMap.map(a=>(a._1," - ")) ++
    //group.commandMap.map(a=>(a._1,KeyStrokeManager.keyStrokeToString(a._2)))//.toSet
    currentCommands=set.toSeq 
    this.fireTableDataChanged()
  }

  def refreshGroup(): Unit = if (currentGroupName.length > 0) loadGroup(currentGroupName)
}

class StrokeEditDialog (w:Window) extends Dialog(w) {
  
  val storeBut=new Button("Speichern")
  val cancelBut=new Button("Abbrechen")
  val setBut=new Button("Tastenkürzel zuordnen ...")
  val groupListView=new ListView[String]()  
  val comModel=new CommandModel
  var keyListenReady=false
  val commandTable: Table =new Table {
    model=comModel  
    autoResizeMode=Table.AutoResizeMode.LastColumn		
		selection.elementMode=Table.ElementMode.None
  }
  
  val mainPanel: BorderPanel =new BorderPanel {
    add(new BoxPanel(Orientation.Horizontal){
      contents+=new BorderPanel{
        add(new ScrollPane(groupListView),BorderPanel.Position.Center)
        val bLabel: Label = ViewConstants.label("Befehlsgruppe")
        add(bLabel, BorderPanel.Position.North)
        preferredSize=new Dimension(190,50)
      } += Swing.HStrut(20)+=
        new BorderPanel {
         add(new ScrollPane(commandTable),BorderPanel.Position.Center)
         add(setBut,BorderPanel.Position.South)
      } 
    },BorderPanel.Position.Center)
    add(new BoxPanel(Orientation.Horizontal){
      contents+=storeBut+=Swing.HGlue+=cancelBut
    },BorderPanel.Position.South)
    listenTo(storeBut,cancelBut,setBut,groupListView.selection,setBut.keys)
    
    reactions+= {
      case ButtonClicked(`cancelBut`)=> close()
      case ButtonClicked(`storeBut`)=> storeStrokes()
      case ButtonClicked(`setBut`)=> if(commandTable.selection.rows.nonEmpty)keyListenReady=true
      case e:ListSelectionChanged[_]=> if(!e.live){
        keyListenReady=false
        groupListView.selection.indices.headOption match {
          case Some(ix)=> comModel.loadGroup(groupListView.listData(ix))
          case _=>
        }
      }
      case e:KeyPressed=> if(keyListenReady) setStroke(e)
    }
  }
  
  preferredSize=new Dimension(600,500)
  modal=true
  title="Tastaturkürzel bearbeiten"
  contents=mainPanel
  groupListView.selection.intervalMode=ListView.IntervalMode.Single


  def loadGroupList(): Unit = {
    //println("GroupKeys="+KeyStrokeManager.groupMap.keysIterator.mkString(" | "))
    groupListView.listData=(collection.immutable.SortedSet[String]()++ KeyStrokeManager.groupMap.keysIterator).toSeq
  }
  
  def setStroke(e:KeyEvent):Unit = {    
    e.peer.getKeyCode match {
      case java.awt.event.KeyEvent.VK_CONTROL | java.awt.event.KeyEvent.VK_SHIFT | java.awt.event.KeyEvent.VK_ALT => return
      case _=> keyListenReady=false
    }
    commandTable.selection.rows.headOption match {
      case Some(ix)=> KeyStrokeManager.addStroke(comModel.currentGroupName,
          comModel.currentCommands(ix)._1,KeyStroke.getKeyStroke(e.peer.getKeyCode,e.peer.getModifiersEx))
          comModel.refreshGroup()
      case _ =>
    }
  }

  def storeStrokes(): Unit = {
    ClientQueryManager.writeKeyStrokes()
    close()
  }

  def showDialog(): Unit = {
    loadGroupList()
    visible=true
  }
}