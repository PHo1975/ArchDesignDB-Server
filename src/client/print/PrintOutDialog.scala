/**
 * Author: Peter Started:01.05.2011
 */
package client.print

import java.awt.{Color, Dimension}
import javax.print.attribute.standard.PageRanges
import javax.swing.{BorderFactory, ImageIcon, JOptionPane}
import javax.swing.event.{DocumentEvent, DocumentListener}

import client.dataviewer.ViewConstants
import client.icons.IconManager
import definition.data.Reference
import util.StrToInt

import scala.swing.{BorderPanel, BoxPanel, Button, ButtonGroup, Component, Dialog, Label, Orientation, RadioButton, ScrollPane, TextField}
import scala.swing.event.ButtonClicked
import scala.util.control.NonFatal

/**
 * 
 */
object PrintOutDialog{
  val printGroup="print"
	lazy val printerIcon: ImageIcon = IconManager.getIcon(printGroup, "printer-icon30").get
	lazy val printerMIcon: ImageIcon = IconManager.getIcon(printGroup, "printer-m").get
	lazy val portraitIcon: ImageIcon = IconManager.getIcon(printGroup, "portrait").get
	lazy val landscapeIcon: ImageIcon = IconManager.getIcon(printGroup, "landscape").get
	lazy val archiveIcon: ImageIcon = IconManager.getIcon(printGroup, "archive30").get
  lazy val dialog=new PrintOutDialog
}

class PrintOutDialog (/*preDialog:NewOutdefDialog*/)  extends BoxPanel(Orientation.Vertical){
  preferredSize=new Dimension(350,400)  
  
  var pageRange:PageRanges=_

	val dlabel: Label = ViewConstants.label()
	val plabel: Label = ViewConstants.label()
  val allPagesBut=new RadioButton("Alle Seiten drucken")
  val pageRangeBut=new RadioButton("Seitenbereich:")
  val currentPageBut=new RadioButton("Aktuelle Seite drucken")
  val pagesGroup=new ButtonGroup(allPagesBut,currentPageBut,pageRangeBut)
  val pageRangeEdit=new TextField
	val copyEdit=new TextField
  
  pageRangeEdit.peer .getDocument.addDocumentListener(new DocumentListener{
		def changedUpdate(e: DocumentEvent): Unit = checkPages()

		def insertUpdate(e: DocumentEvent): Unit = checkPages()

		def removeUpdate(e: DocumentEvent): Unit = checkPages()
  })
  
  val addReceiverBut=new Button("Hinzufügen...")
  
  var currPageable:APageable= _
  var currPage:Int = 0
  
  
  
  val outputPane=new BoxPanel(Orientation.Vertical){  	
  	border=BorderFactory.createTitledBorder("Ausgabe auf")
  	contents+=new BoxPanel(Orientation.Horizontal ){
			val olabel: Label = ViewConstants.label("Gerät: ")
  	 contents+=olabel+=dlabel
  	 xLayoutAlignment=0
  	}
  	contents+=new BoxPanel(Orientation.Horizontal ){
			val olabel: Label = ViewConstants.label("Papier: ")
  	 contents+=olabel+=plabel
  	 xLayoutAlignment=0
  	}
  	xLayoutAlignment=0  	
  	maximumSize=new Dimension(Short.MaxValue,preferredSize.height)
  }
  
  val pagesPane= new BoxPanel(Orientation.Vertical){  	
  	xLayoutAlignment=0
  	border=BorderFactory.createTitledBorder("Druckbereich")
		val exampleLab: Label = ViewConstants.label(" z.B.: 1-5, 8, 11 ")
  	exampleLab.foreground=Color.gray
  	val rangeBox=new BoxPanel(Orientation.Horizontal ){
  		contents+=pageRangeBut+=pageRangeEdit+=exampleLab
  	}
  	allPagesBut.xLayoutAlignment=0
  	currentPageBut.xLayoutAlignment=0
  	rangeBox.xLayoutAlignment=0
  	contents+=allPagesBut+=rangeBox+=currentPageBut  
  	maximumSize=new Dimension(Short.MaxValue,preferredSize.height)
  	allPagesBut.selected=true
  }

	val copyPan=new BoxPanel(Orientation.Horizontal){
		xLayoutAlignment=0
		border=BorderFactory.createTitledBorder("Kopien")
		val copyLab: Label = ViewConstants.label("Anzahl Kopien:")
		contents+=copyLab+=copyEdit
		maximumSize=new Dimension(Short.MaxValue,preferredSize.height)
	}
  
  val receiverPan=new BorderPanel(){
  	xLayoutAlignment=0
  	border=BorderFactory.createTitledBorder("Empfänger")
  	val receiverScroller=new ScrollPane (){
  		preferredSize=new Dimension(100,50)
  	}
  	add(receiverScroller,BorderPanel.Position.Center )
  	val managePanel=new BoxPanel(Orientation.Vertical ){
  		contents+=addReceiverBut
  	}
  	add(managePanel,BorderPanel.Position.Center )
  }
  
  
  contents+=outputPane+=pagesPane+=copyPan+=receiverPan
  
  listenTo(addReceiverBut)
  
  reactions += {
  	 case ButtonClicked(`addReceiverBut`)=> 
  }

	def checkPages(): Unit = {
  	if(currPageable!=null) 
  	pageRangeEdit.foreground=if(parsePages(pageRangeEdit.text)==null)Color.red else Color.black
  	if(!pageRangeBut.selected) pageRangeBut.selected=true
  }
  
  def parsePages(pageSt:String):PageRanges= {   
  	try {
  		new PageRanges(pageSt)
  	}
  	catch {case NonFatal(e)=>util.Log.e("parsing pages :",e); null
		case other:Throwable =>println(other);System.exit(0);null}
  }
  
  def getPages: PageRanges = {
  	if(currPageable==null) null
  	else if(allPagesBut.selected) new PageRanges(1, currPageable.getNumberOfPages())
  	else if(pageRangeBut.selected) new PageRanges(pageRangeEdit.text)
  	else new PageRanges(currPage)
  }

	def getCopies:Int = {
	  copyEdit.text.trim match {
			case StrToInt(num)=> num
			case _ => 1
		}
	}
  
  
  def showPrintOutDialog(parent:Component,nPageable:APageable,ncurrPage:Int,printerName:String,mediaName:String):(PageRanges,Int,Seq[Reference])={
  	currPageable=nPageable
  	currPage=ncurrPage
  	dlabel.text=printerName
  	plabel.text=mediaName
		copyEdit.text="1"
  	//val dr="Drucken"
  	if(nPageable.getNumberOfPages()==1){
  	   allPagesBut.enabled=false
       pageRangeBut.enabled=false
       currentPageBut.selected=true
       pageRangeEdit.enabled=false
  	} else {
  	  allPagesBut.enabled=true
      pageRangeBut.enabled=true
      allPagesBut.selected=true
      pageRangeEdit.enabled=true
  	}
  	if (JOptionPane.YES_OPTION== JOptionPane.showConfirmDialog(parent.peer,peer,"Druckausgabe",Dialog.Options.OkCancel.id,
  		Dialog.Message.Question.id,PrintOutDialog.printerIcon))  	
  		(getPages,getCopies,Seq.empty)
  	else (null,getCopies,Seq.empty)
  }
  
  
  
  
}