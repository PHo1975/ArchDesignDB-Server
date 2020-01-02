/**
 * Author: Peter Started:22.12.2010
 */
package client.print

import java.awt
import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{Color, Dimension}

import client.comm.ClientQueryManager
import client.dataviewer.{FieldColumnModel, MultilineEditor, ViewConstants}
import client.dialog.DialogManager
import definition.data.{FormDescription, OutputDefinition, ResultElement}
import definition.expression._
import javax.print.attribute.standard.{Media, MediaSizeName, MediaTray, OrientationRequested}
import javax.swing._
import javax.swing.border.TitledBorder
import javax.swing.table.TableCellEditor
import util.MyComboBox

import scala.swing.event.{ButtonClicked, ListSelectionChanged, SelectionChanged}
import scala.swing.{BorderPanel, BoxPanel, Button, ButtonGroup, CheckBox, Component, Dialog, Label, ListView, Orientation, RadioButton, ScrollPane, Swing, Table, Window}

/**
 * 
 */

class BoolRenderer extends CheckBox {
  opaque=true
	val bcolor: Color = Color.white
	//UIManager.getColor("Table.background") match{case null => Color.WHITE;case col=>col}
	val acolor: Color = UIManager.getColor("Table.alternateRowColor") match {case null => Color.WHITE; case col => col}

	def prepare(checked: Boolean, row: Int): Unit = {
    this.selected=checked
    background=if(row%2 != 0) acolor else bcolor
  }
  
}



class NewOutdefDialog (w:Window) extends Dialog(w)  {	
	
	private var outputDefinedFunc:(Int,String,String,Boolean,Int,Int,Iterable[ResultElement])=>Unit = _
	var combosAdjusting=false
	var resetOnClose=false
	val cancelBut=new Button("Abbruch")
	val nextBut=new Button("Weiter ->")
	val portraitBut=new RadioButton("Hoch")
	val landscapeBut=new RadioButton("Quer")

	val mediaCombo: MyComboBox[MediaSizeWrapper] = new MyComboBox(List(MediaSizeWrapper(MediaSizeName.ISO_A4))) {
		peer.setModel(PrintModel.mediaModel)
		listenTo(this.selection)
		reactions+= {
			case SelectionChanged(e) if !combosAdjusting => PrintModel.setMediaWrapper(mediaCombo.selection.item)
		}
	}
	
	val trayCombo:MyComboBox[MediaTrayWrapper]=new MyComboBox(List(new MediaTrayWrapper(MediaTray.MAIN))){
		peer.setModel(PrintModel.trayModel)
		listenTo(this.selection)
		reactions+= {
			case SelectionChanged(e) if !combosAdjusting =>
				val mediaTray=trayCombo.selection.item
				PrintModel.setMediaTray(mediaTray)
		}
	}
	
	peer.addWindowListener (new WindowAdapter(){
		override def windowClosing(e: WindowEvent): Unit = {
			if(resetOnClose)DialogManager.reset()
		}
	})			
	
	title="Neue Ausgabe definieren:"	
	preferredSize=new Dimension(550,650)	
	
	val pageFormGroup=new ButtonGroup(portraitBut,landscapeBut){
	 select(portraitBut)
	 listenTo(portraitBut,landscapeBut)
	 reactions+={
		 case ButtonClicked(`portraitBut`)|ButtonClicked(`landscapeBut`)=> PrintModel.pras.add(if(portraitBut.selected)
			 OrientationRequested.PORTRAIT else OrientationRequested.LANDSCAPE )
	 }
	}	
	
	val printerCombo=new MyComboBox[String](PrintModel.printerList)
	val formListView=new ListView[FormDescription](){
		selection.intervalMode=ListView.IntervalMode.Single
		maximumSize=new Dimension(Short.MaxValue,0)
	}
	formListView.border=BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder,"Druckformular auswählen:",TitledBorder.LEFT,TitledBorder.ABOVE_TOP)
	
	val fieldColMod=new FieldColumnModel{
    	createColumn(0,"Name",130)
    	createColumn(1,"Beschreibung",200)
    	createColumn(2,"Wert",150)
	}	
	
	val tcr = new Table.AbstractRenderer[BoolConstant, BoolRenderer](new BoolRenderer) {
		def configure(t: Table, sel: Boolean, foc: Boolean, o: BoolConstant, row: Int, col: Int): Unit =
	    component.prepare(o.toBoolean,row)	  
	}
		
	val paramTable=new Table {
		val stringEditor=new MultilineEditor(peer)	{
			def setEditorValue(value: Object): String = if (value == null) "" else value.toString
		}
		rowHeight = ViewConstants.defaultRowHeight
		font = ViewConstants.tableFont
		model=PrintModel.paramTabMod
		peer.setAutoCreateColumnsFromModel(false)
    peer.setColumnModel(fieldColMod)
    selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None  
		autoResizeMode=Table.AutoResizeMode.Off
		val boolEditor=new DefaultCellEditor(new JCheckBox){
			override def getTableCellEditorComponent(table: JTable, value: Object, isSelected: Boolean, row: Int, column: Int): awt.Component = {
		    super.getTableCellEditorComponent(table, value match {
		      case b:BoolConstant=> if(b.toBoolean) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE
		      case _=> java.lang.Boolean.FALSE
		    }, isSelected, row, column)
		  }
		    
		}
		
		override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int):Component = {
		  if(col==2) model.getValueAt(row,col) match {
	  	  case (boolValue:BoolConstant)=> return tcr.componentFor(this,sel, foc, boolValue, row, col)
	  	  case _=>
	  	}		  
		  super.rendererComponent(sel,foc,row,col)
		}
		
		override def editor(row: Int, column: Int):TableCellEditor = {
		  if(column==2) 
		     model.getValueAt(row,column) match {
		  	  case (boolValue:BoolConstant)=> boolEditor
		  	  case _=>  stringEditor
		  	}
      else super.editor(row,column)
		}
         
		peer.setDefaultEditor(classOf[String],stringEditor)
	}
	
	val paramScroller=new ScrollPane {
		viewportView=paramTable	
		border=BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder,"Parameter angeben:",TitledBorder.LEFT,TitledBorder.ABOVE_TOP)
	}
	
	val mainPanel=new BorderPanel(){
		formListView.background=background
		add(new BoxPanel(Orientation.Vertical ) {
			contents+=Swing.VStrut(10)+= formListView+=Swing.VStrut(10)+=paramScroller+=Swing.VStrut(10)+=
				new BoxPanel(Orientation.Horizontal) {
					val drLab = new Label("Drucker:")
					drLab.font = ViewConstants.labelFont
					contents += drLab += printerCombo
				listenTo(printerCombo.selection)
				reactions+= {
					case SelectionChanged(e)=> setPrinterName( printerCombo.selection.item)
				}
			}+=Swing.VStrut(10)+=
			new BoxPanel(Orientation.Horizontal) {
				val pfLab = new Label("Papierformat: ")
				pfLab.font = ViewConstants.labelFont
				contents += pfLab += mediaCombo += Swing.HStrut(20) += portraitBut += landscapeBut += Swing.HStrut(20)
			}+=Swing.VStrut(10)+=
			new BoxPanel(Orientation.Horizontal) {
				val shLab = new Label("Ausgabe-Schacht: ")
				shLab.font = ViewConstants.labelFont
				contents += shLab += trayCombo
			}+=Swing.VStrut(20)				
			
		},BorderPanel.Position.Center)
		add(new BoxPanel(scala.swing.Orientation.Horizontal){
	    contents+= cancelBut+=Swing.HGlue+=nextBut 
		},BorderPanel.Position.South)
		
		listenTo(nextBut,cancelBut,formListView.selection/*printerBut,pageBut,*/)
		reactions += {
			case ButtonClicked(`nextBut`)=> if(outputDefinedFunc!=null&& formListView.selection.indices.nonEmpty) {
				NewOutdefDialog.this.visible=false
				val paperSettings=PrintModel.lastSelectedMedia.mn.toString+(if(PrintModel.trayModel.getSize>0)"|"+trayCombo.selection.item.mt.toString else "")
				resetOnClose=false
				//println("next but "+formListView.selection.indices.head)
			  //println("Printservice \n"+PrintModel.theService.getAttributes().toArray().map(el=>el.getName()+"|"+el.getCategory().getName()+"|"+el.toString).mkString("\n"))
				outputDefinedFunc(formListView.selection.indices.head,PrintModel.getPrintServiceName(PrintModel.theService),paperSettings,portraitBut.selected,
				 PrintModel.lastSelectedMedia.width.toInt,PrintModel.lastSelectedMedia.height.toInt,	PrintModel.paramTabMod.getParams)
			}			
							
			case ButtonClicked(`cancelBut`)=>if(resetOnClose)DialogManager.reset(); close()
			
			case e:ListSelectionChanged[_]=> if(!e.live&& formListView.selection.indices.nonEmpty){
				val ix=formListView.selection.indices.head				
				if(ix > -1&& PrintModel.forms.nonEmpty) {
					val newForm=PrintModel.forms(ix)
					PrintModel.paramTabMod.loadForm( newForm)
				}
				if(PrintModel.forms.isEmpty) util.Log.w("Keine Formulare ")
			}			
		}
	}	
	contents=mainPanel


	def storePrintData(): Unit = {
	  close()
	  val paperSettings=PrintModel.lastSelectedMedia.mn.toString+(if(PrintModel.trayModel.getSize>0)"|"+trayCombo.selection.item.mt.toString else "")
	  DialogManager.processCustomEnquiry(IndexedSeq(ResultElement("StorePrintData",
	  		IntConstant(PrintModel.outputDefInst)),ResultElement("Form",IntConstant(formListView.selection.indices.head)),
			ResultElement("Printer",StringConstant(PrintModel.getPrintServiceName(PrintModel.theService))),ResultElement("PageSettings",StringConstant(paperSettings)),
			ResultElement("Portrait",BoolConstant(portraitBut.selected)),ResultElement("PageWidth",IntConstant(PrintModel.lastSelectedMedia.width.toInt)),
			ResultElement("PageHeight",IntConstant(PrintModel.lastSelectedMedia.height.toInt)) ) ++ PrintModel.paramTabMod.getParams)
	}


	def setPrinterName(newName: String): Unit = {
	  combosAdjusting=true
	  val (theService,defaultTrayIx)=PrintModel.setPrinterName(newName)		
		if(PrintModel.lastSelectedMedia!=null && theService.isAttributeValueSupported(PrintModel.lastSelectedMedia.mn, null, PrintModel.emptyAttributeSet))
			 mediaCombo.selection.index=PrintModel.mediaModel.getIndexOf(PrintModel.lastSelectedMedia)
		 else {
      util.Log.w("lastSelMedia not supported "+PrintModel.lastSelectedMedia)
			val defMed=theService.getDefaultAttributeValue(classOf[Media]).asInstanceOf[MediaSizeName]
			val newMap=MediaMap.getMediaSize(defMed)
			PrintModel.setMediaWrapper(newMap)
			mediaCombo.selection.index=PrintModel.mediaModel.getIndexOf(PrintModel.lastSelectedMedia)
		}
		if(PrintModel.trayModel.getSize>0&&defaultTrayIx> -1) {
			trayCombo.selection.index_=(defaultTrayIx)
			PrintModel.pras.add(PrintModel.trayModel.getElementAt(defaultTrayIx).altValue)
		}			
		combosAdjusting=false		
	}

	def getCurrentForm: FormDescription = formListView.selection.items(0)

	def showDialog(newTitle: String, noutputDefinedFunc: (Int, String, String, Boolean, Int, Int,Iterable[ResultElement]) => Unit, nresetOnClose: Boolean): Unit = {
		title=newTitle
		outputDefinedFunc=noutputDefinedFunc		
		resetOnClose=nresetOnClose
		visible=true
	}

	def showEditDialog(noutputDefinedFunc: (Int, String, String, Boolean, Int, Int, Iterable[ResultElement]) => Unit, odef: OutputDefinition): Unit = {
		loadOutDefSettings(odef)		
		showDialog("Ausgabedefinition ändern",noutputDefinedFunc,resetOnClose)
	}

	def loadForms(newList: Seq[FormDescription]): Unit = {
		formListView.listData=newList
		formListView.selection.indices+=0
	}

	def loadOutDefSettings(odef: OutputDefinition): Unit = {
		val sIx=PrintModel.printerList.indexOf(odef.printer)
		if(sIx<0) ClientQueryManager.printErrorMessage("cant find printer '"+odef.printer+"'")
		else printerCombo.selection.index=sIx			
		
		val paperS=odef.paperSetting .split('|')
		val paged=paperS(0)
		combosAdjusting=true
		PrintModel.theService.getSupportedAttributeValues(classOf[Media],null,PrintModel.emptyAttributeSet).asInstanceOf[Array[Media]].
			find(_.toString==paged) match {
			case Some(media)=>
				val wrapper=MediaMap.getMediaSize(media.asInstanceOf[MediaSizeName])
				val ix=PrintModel.mediaModel.getIndexOf(wrapper)
				if(ix> -1) mediaCombo.selection.index=ix
				PrintModel.setMediaWrapper(wrapper)
			case None => ClientQueryManager.printErrorMessage("Cant find Media: '"+paged+"'")
		}		
		if(paperS.length>1) { // tray
			val trayName=paperS(1)
			if(trayName.length>0){
				PrintModel.theService.getSupportedAttributeValues(classOf[Media],null,PrintModel.emptyAttributeSet).asInstanceOf[Array[Media]].
				find(_.toString==trayName) match {
					case Some(tray)=>
						val wrapper=TrayMap.getMediaTray(tray.asInstanceOf[MediaTray])
						val ix=PrintModel.trayModel.getIndexOf(wrapper)
						if(ix> -1) trayCombo.selection.index=ix
						PrintModel.pras.add(wrapper.altValue)
					case None =>ClientQueryManager.printErrorMessage("Cant find Tray: '"+trayName+"'")
				}
			}
		}
		if(odef.portrait )portraitBut.selected=true else landscapeBut.selected=true
		PrintModel.pras.add(if(odef.portrait) OrientationRequested.PORTRAIT else OrientationRequested.LANDSCAPE )
		// load params	
		
		combosAdjusting=false		
		val fix=PrintModel.forms.indexWhere(_.inst ==odef.formInst )
		if(fix> -1) {
		  val myForm=PrintModel.forms(fix)
		  
			def findParamValue(pname:String)= {		    
				odef.paramValues.find(_.paramName ==pname) match {
					case Some(param)=>  param.result
					case None =>myForm.params.find(_.name==pname) match {
					  case Some(paramDesc)=> paramDesc.defaultValue
					  case None=> EMPTY_EX
					}
				}		    
			}
			formListView.selection.indices+=fix
			//println("load form")
			PrintModel.paramTabMod.loadForm(myForm,findParamValue)
		}
		else ClientQueryManager.printErrorMessage("can't find form :"+odef.formInst)		
	} 
 
}



