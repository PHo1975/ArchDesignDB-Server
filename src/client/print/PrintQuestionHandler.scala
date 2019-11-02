/**
 * Author: Peter Started:21.12.2010
 */
package client.print

import java.io.DataInput
import java.util.Date

import client.comm.GenDataReceiver
import client.dialog.{ActionPanel, CustomQuestionHandler, DialogManager, FocusContainer}
import client.ui.ClientApp
import definition.data.{FormDescription, PageData, ResultElement}
import definition.expression._
import definition.typ.{ParamQuestion, XMLQuestion}
import javax.print.attribute.standard.PageRanges
import util.Log

import scala.swing.Swing

/**
 * 
 */
object PrintQuestionHandler extends CustomQuestionHandler with GenDataReceiver with PrintReceiver {	
  lazy val newDialog= new NewOutdefDialog(ClientApp.top)
  lazy val outdefDialog=new ChoseOutDefDialog(ClientApp.top)
  lazy val previewWindow=new PreviewWindow(ClientApp.top)
  //val dateFormat=DateFormat.getDateInstance(DateFormat.SHORT)
  val dateParamKey="Date"
  
  def load(question: ParamQuestion,container:FocusContainer): Unit = PrintQuestionHandler.synchronized{
    //System.out.print("Load Question Handler "+Thread.currentThread().getName())
		question match {
			case x:XMLQuestion => PrintModel.readInXML(x.customData)
			case o=> Log.e("wrong question type "+o)
		}
  	//System.out.print("readInXML")
  	newDialog.loadForms( PrintModel.forms)
  	//System.out.println("load done "+PrintModel.outDefs.map(_.formName).mkString(","))
  	if(PrintModel.outDefs.isEmpty) {  
  	  //println("Outdefs empty, show newdialog")
  		newDialog.setLocationRelativeTo(ActionPanel )
  		newDialog.showDialog("Neue Ausgabedefinition anlegen",outputDefined,true)
  	}
  	else {  		
  	  //println("Oudefs exist")
  		outdefDialog.setLocationRelativeTo(ActionPanel)
  		outdefDialog.start()
  	}    
  }
  
  
  def outputDefined(formIx:Int,printer:String,pageSetting:String,portrait:Boolean,w:Int,h:Int,paramData:Seq[ResultElement])= PrintQuestionHandler.synchronized{
   //println("output Defined:"+formIx+" "+printer+" paramData:"+paramData)
  	DialogManager.processCustomEnquiry(IndexedSeq(ResultElement("NewOutDef",IntConstant(formIx)),
			ResultElement("Printer",StringConstant(printer)),ResultElement("PageSettings",StringConstant(pageSetting)),
			ResultElement("Portrait",BoolConstant(portrait)),ResultElement("PageWidth",IntConstant(w)),ResultElement("PageHeight",IntConstant(h))) ++ paramData)
  }
  
    
  def receiveData(in:DataInput)=  PrintQuestionHandler.synchronized{
    //System.out.print("Receive print data "+Thread.currentThread().getName())
  	val form=newDialog.getCurrentForm  	
  	val jobTitle=in.readUTF+" "+util.JavaUtils.shortDateFormat.format(new Date())
  	val oInst=in.readInt
  	val numPages=in.readInt
  	//System.out.print(" numPages:"+numPages)
  	val pagesList=for( i <-0 until numPages) yield PageData(in)  	
  	//val printableArea=PrintModel.getPrintableArea
    //System.out.println(" read done")
    Swing.onEDT{
	  	val pageFormat=PrintModel.getPageFormat  	
	  	MyContext.fontStyleList=form.fonts  	
	  	PrintModel.myPageable.setData(pageFormat,pagesList)  	
	  	print(jobTitle, oInst,form,true)
  	}
  }
  /** @param setForm should set Form before print (not for archives)
   * 
   */
  def print(ntitle:String,odefInt:Int,currentForm:FormDescription,setForm:Boolean)= PrintQuestionHandler.synchronized{    
		PrintModel.outputDefInst=odefInt
		PrintModel.printJob.setJobName(ntitle)
		if(setForm)PrintModel.myPageable.form=currentForm		
		previewWindow.showPreview(ntitle,PrintModel.myPageable,this)		
	}
  
  // interface PrintReceiver 
	def printOut(pageable:APageable,copies:Int,pages:PageRanges):Unit= PrintQuestionHandler.synchronized{
	  //println("Printout "+pageable+" pages:"+pages)
	  pageable.tempContext=null 	
  	PrintModel.printJob.setPageable(pageable)
  	PrintModel.pras.add(pages)
		for(i<-0 until copies)
  	  PrintModel.printJob.print(PrintModel.pras)
  	newDialog.storePrintData()
  	DialogManager.reset()
	}
  
  def printerName:String=PrintModel.printerName
  def mediaName:String=PrintModel.lastSelectedMedia.toString

}