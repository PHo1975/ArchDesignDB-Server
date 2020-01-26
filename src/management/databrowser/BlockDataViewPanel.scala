package management.databrowser

import client.dataviewer.ViewConstants

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, GridPanel, ScrollPane, Table}

object BlockDataViewPanel extends BorderPanel {
  val ixTable: Table = new Table()	{
    model=BlockIndexTableModel
    selection.intervalMode=Table.IntervalMode.Single
  }


  add( new BorderPanel{
    add(ViewConstants.label("Index-Tabelle"),BorderPanel.Position.North)
    add (new ScrollPane()	{
      viewportView=ixTable
    },BorderPanel.Position.Center)
    add (new GridPanel(6,1)		{
      val openBut=new Button("Open Instance")
      val createBut=new Button("Create Instance")
      val deleteBut=new Button("Delete Instance")

      contents+=openBut+=createBut+=deleteBut
      listenTo(openBut,createBut,deleteBut)
      reactions += 	{
        case ButtonClicked(`openBut`) => openInstance()
        case ButtonClicked(`createBut`) => createInstance()
        case ButtonClicked(`deleteBut`) => deleteInstance()
      }
    },BorderPanel.Position.South)
  },BorderPanel.Position.West)

  def openInstance(): Unit ={

  }

  def createInstance(): Unit ={

  }
  def deleteInstance(): Unit ={

  }
}
