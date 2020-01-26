package management.databrowser

import definition.typ.{AllClasses, BlockClass}
import javax.swing.JOptionPane
import javax.swing.table.DefaultTableModel
import management.databrowser.MainWindow.rightPanel
import server.config.FSPaths
import server.storage.StorageManager
import transaction.handling.SessionManager
import util.StrToInt

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Dimension, GridPanel, ScrollPane}

class BlockClassPanel extends BorderPanel {
  val blockTable=new SimpleTable
  var blockList:Array[Array[Object]]=Array()

  def dataInit():Unit= {
    blockList=AllClasses.get.blockClassList.valuesIterator.map(bc=>Array[Object](Integer.valueOf(bc.id),bc.name,Integer.valueOf(bc.blocksize))).toSeq.toArray
    val theModel = new DefaultTableModel(blockList, Array[Object]("ID", "Name", "BlockSize")) {
      override def getColumnClass(col: Int): Class[_] = {
        col match {
          case 0 => classOf[java.lang.Integer]
          case 2 =>classOf[java.lang.Integer]
          case _ => classOf[String]
        }
      }
    }
    blockTable.model=theModel
    MainWindow.updateBlockClassListeners(AllClasses.get.blockClassList.valuesIterator.map(bc=>(bc.id,bc.name)).toSeq)
  }



  add(new ScrollPane() {
    viewportView = blockTable
    preferredSize = new Dimension(380, 200)
  }, BorderPanel.Position.Center)
  add(new GridPanel(1, 3) {
    val newClassBut = new Button("Create Class")
    val removeeClassBut = new Button("Remove Class")
    val showDataBut = new Button("Show Data")

    contents += newClassBut += showDataBut += removeeClassBut
    listenTo(newClassBut, removeeClassBut, showDataBut)
    reactions += {
      case ButtonClicked(`newClassBut`) => createClass()
      case ButtonClicked(`removeeClassBut`) => removeClass()
      case ButtonClicked(`showDataBut`) => showData()
    }
  }, BorderPanel.Position.South)

  def createClass():Unit={
     JOptionPane.showInputDialog(this.peer,"Enter ClassID","CreateClass",JOptionPane.QUESTION_MESSAGE) match {
       case StrToInt(id)=>
         val name= JOptionPane.showInputDialog(this.peer,"Enter Name","CreateClass",JOptionPane.QUESTION_MESSAGE)
         JOptionPane.showInputDialog(this.peer,"EnterBlockSize","CreateClass",JOptionPane.QUESTION_MESSAGE) match {
           case StrToInt(blockSize)=>
             SessionManager.scl.addBlockClass(new BlockClass(name,id,"",blockSize))
             dataInit()
             scala.xml.XML.save(FSPaths.configDir+"types.xml",SessionManager.scl.saveToXML(),"UTF-8",xmlDecl = true,null)
           case st => println("wrong BlockSize value "+st)
         }
       case st=> println("wrong Value "+st)
     }
  }

  def removeClass():Unit= {

  }

  def showData():Unit = {
    if (blockTable.selection.rows.nonEmpty) {
      val typ = blockList(blockTable.selection.rows.head)(0).asInstanceOf[java.lang.Integer].intValue()
      //System.out.println("showData "+typ)
      BlockIndexTableModel.setTypeHandler(StorageManager.blockHandler(typ))
      rightPanel.addIt(BlockDataViewPanel, BorderPanel.Position.Center)
      rightPanel.peer.invalidate()
      rightPanel.peer.revalidate()
      rightPanel.repaint
    }
  }


}
