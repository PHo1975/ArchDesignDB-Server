/**
 * Author: Peter Started:03.11.2010
 */
package client.comm

import java.awt.event.{WindowAdapter, WindowEvent}

import client.ui.ViewConstants
import definition.data.TransStepData
import javax.swing.table.TableColumnModel

import scala.swing._
import scala.swing.event._
/**
 * 
 */
class UndoDialog(w:Window) extends Dialog(w) with StepListReader {
	val prefSize=new Dimension(900,350)
  preferredSize=prefSize
  modal=true
  title="Zuletzt ausgeführte Aktionen"

  val undoTableModel=new UndoLogTableModel
  val undoBut=new Button("Letzte Aktion rückgängig")
	val cancelBut=new Button("Abbruch")
	var oldLockListener:Option[(Boolean, String) => Unit]=None

	peer.addWindowListener (new WindowAdapter(){
    override def windowClosing(e: WindowEvent): Unit = ClientQueryManager.stopUndo()
	})

	listenTo(this)

  val undoTable=new Table(){
		model=undoTableModel
		autoResizeMode=Table.AutoResizeMode.LastColumn
		//selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None
    rowHeight = ViewConstants.defaultRowHeight
    font = ViewConstants.tableFont
	}

  val mainPanel=new BorderPanel(){
		add(new ScrollPane () {
			viewportView = undoTable
		},BorderPanel.Position.Center)
		add(new BoxPanel(scala.swing.Orientation.Horizontal){
	    contents+=undoBut += cancelBut
		},BorderPanel.Position.South)
		listenTo(undoBut,cancelBut)
		reactions += {
			case ButtonClicked(`undoBut`)=> doUndo()
			case ButtonClicked(`cancelBut`)=>doCancel()
		}
	}

  def prepare(): Unit = {
    //println("prepare ")
    oldLockListener=ClientQueryManager.undoLockListener
    ClientQueryManager.undoLockListener=Some(undoLockedInfo)
    undoBut.enabled=true
    cancelBut.enabled=true
  }

  def undoLockedInfo(locked:Boolean,user:String):Unit= ClientQueryManager.runInPool{
    //println("undolocked "+locked)
    if(!locked) {
      ClientQueryManager.undoLockListener=oldLockListener
      close()
    }
  }

  def loadStepList(list: Seq[TransStepData]): Unit = {
  	undoTableModel.loadStepList(list)
  	visible=true
  }


  contents=mainPanel
  val colMod: TableColumnModel = undoTable.peer.getColumnModel()
  undoTable.autoResizeMode=Table.AutoResizeMode.LastColumn
	colMod.getColumn(0).setPreferredWidth(70)
	colMod.getColumn(0).setMaxWidth(80)
	colMod.getColumn(1).setMaxWidth(80)
	colMod.getColumn(2).setMaxWidth(140)
	colMod.getColumn(2).setPreferredWidth(140)

  def doUndo(): Unit = {
    undoBut.enabled=false
    cancelBut.enabled=false
  	ClientQueryManager.doUndo()
  	//close
  }

  def doCancel(): Unit = {
  	ClientQueryManager.stopUndo()
  	//close
  }


}