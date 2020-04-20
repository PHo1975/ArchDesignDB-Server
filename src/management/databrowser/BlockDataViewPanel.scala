package management.databrowser



import client.ui.ViewConstants
import server.storage.StorageManager

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Dimension, GridPanel, Label, ScrollPane, Table}

object BlockDataViewPanel extends BorderPanel {
  val ixTable: Table = new Table()	{
    model=BlockIndexTableModel
    selection.intervalMode=Table.IntervalMode.Single
  }

  val infoLabel: Label =ViewConstants.label("<html><br></html>")
  val dataArea=new scala.swing.TextArea()
  dataArea.wordWrap=true
  dataArea.lineWrap=true

  add( new BorderPanel{
    preferredSize=new Dimension(300,20)
    add(ViewConstants.label("Index-Tabelle"),BorderPanel.Position.North)
    add (new ScrollPane()	{
      viewportView=ixTable
    },BorderPanel.Position.Center)
    add (new GridPanel(6,1)		{
      val openBut=new Button("Open Instance")
      val createBut=new Button("Create Instance")
      val deleteBut=new Button("Delete Instance")
      val infoBut=new Button("Infos")

      contents+=infoLabel+=openBut+=createBut+=deleteBut+=infoBut
      listenTo(openBut,createBut,deleteBut,infoBut)
      reactions += 	{
        case ButtonClicked(`openBut`) => openInstance()
        case ButtonClicked(`createBut`) => createInstance()
        case ButtonClicked(`deleteBut`) => deleteInstance()
        case ButtonClicked(`infoBut`) => showInfo()
      }
    },BorderPanel.Position.South)
  },BorderPanel.Position.West)
  add( new BorderPanel{
    add(ViewConstants.label("Daten"),BorderPanel.Position.North)
    add(new ScrollPane(){
      viewportView=dataArea
    },BorderPanel.Position.Center)
  },BorderPanel.Position.Center )

  def openInstance(): Unit ={
     ixTable.selection.rows.headOption match {
       case Some(ix) =>
         val entry=BlockIndexTableModel.ixList(ix)
         if(entry._2 > -1) {
           val handler=BlockIndexTableModel.handler
           val block=new Array(handler.theClass.blocksize)
           dataArea.text=StorageManager.blockFileHandler.readInBuffer(entry._2,handler.theClass.blocksize).map(_.toString).mkString(" ")
         }
       case _ =>
     }
  }

  def createInstance(): Unit ={

  }
  def deleteInstance(): Unit ={

  }

  def showInfo():Unit= {
    infoLabel.text=BlockIndexTableModel.handler.getInfoString
  }
}
