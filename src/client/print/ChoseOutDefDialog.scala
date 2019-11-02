/**
 * Author: Peter Started:23.12.2010
 */
package client.print

import java.awt.Color
import java.awt.event.{WindowAdapter, WindowEvent}

import client.dataviewer.ViewConstants
import client.dialog.DialogManager
import definition.data.{OutputDefinition, Reference, ResultElement}
import definition.expression.{BoolConstant, IntConstant, StringConstant}
import javax.swing.BorderFactory
import javax.swing.border.TitledBorder

import scala.swing.event.{ButtonClicked, MouseClicked}
import scala.swing.{Alignment, BorderPanel, BoxPanel, Button, Dialog, Dimension, Label, ListView, Orientation, ScrollPane, Swing, Window}


class OutDefRenderer() extends BoxPanel(Orientation.Vertical ) {
	val topLabel: Label = ViewConstants.label()
	val bottomLabel: Label = ViewConstants.label()
	contents+=topLabel+=bottomLabel
	opaque=true
	topLabel.opaque=true
	bottomLabel.opaque=true
	val back=new Color(57,105,138)

	override def foreground_=(c: Color): Unit = {
		super.foreground_=(c)
		topLabel.foreground=c
		bottomLabel.foreground=c		
	}

	override def background_=(c: Color): Unit = {
		super.background_=(c)
		topLabel.background=c
		bottomLabel.background=c		
	}

	def config(list:ListView [_], isSelected: Boolean, focused: Boolean, a: OutputDefinition, index: Int): Unit = {
		topLabel .text=a.toString()
		topLabel.font=ViewConstants.tableFont
		topLabel.horizontalAlignment=Alignment.Left		
		bottomLabel .text=a.paramString		
		bottomLabel.horizontalAlignment=Alignment.Left
		if(isSelected||focused) {
			background = back
			foreground=Color.white
		}

	} 
	border=BorderFactory.createCompoundBorder(
		BorderFactory.createLineBorder(Color.gray),
		BorderFactory.createEmptyBorder(7, 10, 7, 10))
}

/**
 * 
 */
class ChoseOutDefDialog(w:Window) extends Dialog(w) {
	title="Ausgabe-Definition wählen:"
	
	val cancelBut=new Button("Abbruch")
	val nextBut=new Button("Weiter ->")	
	val createBut=new Button("Zusätzliche Definition Anlegen ...")
	val changeBut=new Button("Gewählte Definition ändern ...")
	val deleteBut=new Button("Gewählte Definition löschen")
	val archiveBut=new Button("Archiv ansehen ...")
	val outdefListView=new ListView[OutputDefinition](){
		selection.intervalMode=ListView.IntervalMode.Single		
		renderer=new ListView.AbstractRenderer[OutputDefinition,OutDefRenderer](new OutDefRenderer){
  		 def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: OutputDefinition, index: Int): Unit = {
			component.config(list,isSelected,focused,a,index)
		}
		}
	}
	private var resetOnClose=true	
	private var changedODInst:Int = -1
	
	val outdefScroller=new ScrollPane {
		viewportView=outdefListView	
		border=BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder,"Bereits definierte Ausgabe-Definitionen:",TitledBorder.LEFT,TitledBorder.ABOVE_TOP)
		maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
	}	
	
	preferredSize=new Dimension(750,550)
	
	val mainPanel=new BorderPanel(){
		add(new BoxPanel(Orientation.Vertical ) {
			contents+=Swing.VStrut(10)+= outdefScroller+=Swing.VStrut(20)
		},BorderPanel.Position.Center)
		
		add(new BoxPanel(Orientation.Vertical){
			contents += new BoxPanel(Orientation.Vertical) {
				contents+=createBut+=changeBut+=deleteBut+=archiveBut
			} += new BoxPanel(Orientation.Horizontal) {
				contents+=cancelBut+=Swing.HGlue+=nextBut
			}
				
		},BorderPanel.Position.South)
		listenTo(cancelBut,nextBut,createBut,changeBut,deleteBut,archiveBut,outdefListView.mouse.clicks)
		
		reactions+= {
			case ButtonClicked(`cancelBut`)=>closeWithReset()
			case ButtonClicked(`changeBut`) =>changeOutdef()
			case ButtonClicked(`createBut`)=>createOutdef()
			case ButtonClicked(`deleteBut`)=>deleteOutdef()
			case ButtonClicked(`nextBut`)=>choseOutdef()
			case ButtonClicked(`archiveBut`)=>showArchive()
				case ev:MouseClicked=> if(ev.clicks==2) choseOutdef()
		}
	}

	def closeWithReset(): Unit = {
	  close()
	  resetOnClose=true
	  DialogManager.reset()
	}
	
	contents=mainPanel

	def getIx: Int = if (outdefListView.selection.indices.isEmpty) -1 else outdefListView.selection.indices.head

	def changeOutdef(): Unit = /*if(!outdefListView.selection.items.isEmpty)*/ {
		val selOD=if(outdefListView.selection.items.isEmpty) PrintModel.outDefs.head else outdefListView.selection.items.head
		changedODInst=selOD.odInst
		PrintQuestionHandler.newDialog.setLocationRelativeTo(createBut )
		PrintQuestionHandler.newDialog.showEditDialog(whenOutputDefChanged,selOD)
	}

	def createOutdef(): Unit = {
		PrintQuestionHandler.newDialog.setLocationRelativeTo(createBut )
		PrintQuestionHandler.newDialog.showDialog("Neue Ausgabe definieren:",whenNewoutputDefined,false)
	}

	def deleteOutdef(): Unit = if (outdefListView.selection.indices.nonEmpty) {
		val selOD=outdefListView.selection.items.head		
		DialogManager.processCustomEnquiry(IndexedSeq(ResultElement("DeleteOutDef",IntConstant(selOD.odInst)) ))
		PrintModel.setOutDefs(PrintModel.outDefs.filterNot(_.odInst ==selOD.odInst))		
	}

	def choseOutdef(): Unit = if (outdefListView.selection.indices.nonEmpty) chooseOutdef(outdefListView.selection.items.head)


	protected def chooseOutdef(selOD: OutputDefinition): Unit = {
	  PrintQuestionHandler.newDialog.loadOutDefSettings(selOD)
		val sm=PrintModel.lastSelectedMedia
		DialogManager.processCustomEnquiry(IndexedSeq(ResultElement("ChoseOutDef",IntConstant(selOD.odInst)),
			ResultElement("PageWidth",IntConstant(sm.width.toInt)),ResultElement("PageHeight",IntConstant(sm.height .toInt)) ) )
		if(visible){
		  resetOnClose=false
		  close()
		}
	}


	def start(): Unit = {
	  if(PrintModel.outDefs.size==1) {
	    chooseOutdef(PrintModel.outDefs.head)
	  }else{	    
	    visible=true
	  } 
	}

	def showArchive(): Unit = if (outdefListView.selection.indices.nonEmpty) {
		val selOD=outdefListView.selection.items.head
		PrintQuestionHandler.newDialog.loadOutDefSettings(selOD)
		PrintQuestionHandler.previewWindow .showArchive("Druck-Archiv", new Reference(OutputDefinition.odefType,selOD.odInst ),PrintQuestionHandler)
	}

	def getCurrentOutDef: OutputDefinition = outdefListView.selection.items.head


	def loadOutdefs(newList: Seq[OutputDefinition]): Unit = {
	  if(newList.isEmpty) {
      util.Log.e("Load outdefs empty newList ")
      outdefListView.listData=PrintModel.outDefs
	    } 
	  else outdefListView.listData=newList
		outdefListView.selection.indices+=0
	}

	def whenNewoutputDefined(formIx: Int, printer: String, pageSetting: String, portrait: Boolean, w: Int, h: Int, paramData: Seq[ResultElement]): Unit = {
	  resetOnClose=false
	  close()
	  PrintQuestionHandler.outputDefined(formIx,printer,pageSetting,portrait,w,h,paramData)
	}

	def whenOutputDefChanged(formIx: Int, printer: String, pageSetting: String, portrait: Boolean, w: Int, h: Int, paramData: Seq[ResultElement]): Unit = {
	  resetOnClose=false
	  close()
	  //println("when outputdef changed "+formIx)
	  PrintModel.changeOutDef(new OutputDefinition(changedODInst,PrintModel.forms(formIx).inst,printer,pageSetting,portrait,paramData))
	  DialogManager.processCustomEnquiry(IndexedSeq(ResultElement("ChangeOutDef",IntConstant(changedODInst)),ResultElement("Form",IntConstant(formIx)),
			ResultElement("Printer",StringConstant(printer)),ResultElement("PageSettings",StringConstant(pageSetting)),
			ResultElement("Portrait",BoolConstant(portrait)),ResultElement("PageWidth",IntConstant(w)),ResultElement("PageHeight",IntConstant(h)) ) ++ paramData)
	}
	
	peer.addWindowListener (new WindowAdapter(){
		override def windowClosing(e: WindowEvent): Unit = closeWithReset()
	})
	
}