/**
 * Author: Peter Started:03.11.2010
 */
package client.comm

import scala.swing._
import scala.swing.event._
import definition.data.TransStepData
import java.awt.event.{WindowAdapter,WindowEvent}
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
		override def windowClosing(e:WindowEvent)=	ClientQueryManager.stopUndo()
	})

	listenTo(this)

  val undoTable=new Table(){
		model=undoTableModel
		autoResizeMode=Table.AutoResizeMode.LastColumn
		//selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None

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

  def prepare()={
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

  def loadStepList(list:Seq[TransStepData]) = {
  	undoTableModel.loadStepList(list)
  	visible=true
  }


  contents=mainPanel
  val colMod=undoTable.peer.getColumnModel()
  undoTable.autoResizeMode=Table.AutoResizeMode.LastColumn
	colMod.getColumn(0).setPreferredWidth(70)
	colMod.getColumn(0).setMaxWidth(80)
	colMod.getColumn(1).setMaxWidth(80)
	colMod.getColumn(2).setMaxWidth(140)
	colMod.getColumn(2).setPreferredWidth(140)

  def doUndo() = {
    undoBut.enabled=false
    cancelBut.enabled=false
  	ClientQueryManager.doUndo()
  	//close
  }

  def doCancel() = {
  	ClientQueryManager.stopUndo()
  	//close
  }


}