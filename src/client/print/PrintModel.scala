package client.print

import java.awt.print.{PageFormat, PrinterJob}

import definition.data.{FormDescription, OutputDefinition}
import javax.print.{PrintService, PrintServiceLookup}
import javax.print.attribute.{HashPrintRequestAttributeSet, PrintRequestAttributeSet}
import javax.print.attribute.standard.{Media, MediaPrintableArea, MediaSizeName, MediaTray}
import javax.swing.DefaultComboBoxModel

object PrintModel {
  val myPageable=new MyPageable
  var forms:Seq[FormDescription]=Seq.empty
  var outDefs:Seq[OutputDefinition]=Seq.empty
  var choosenOutDef:OutputDefinition= _
  val emptyAttributeSet=new HashPrintRequestAttributeSet()
  var pras:PrintRequestAttributeSet  = new HashPrintRequestAttributeSet()
  var outputDefInst:Int=0
	val printJob=PrinterJob.getPrinterJob
	printJob.setPageable(myPageable)
	val printServices=  PrintServiceLookup.lookupPrintServices( null,null)
  if(printServices==null || printServices.size==0) util.Log.e("no print services found !")
	var printerName:String=null
	var theService:javax.print.PrintService= PrintServiceLookup.lookupDefaultPrintService
	println("before crash")
	if(theService==null) println("Printservice == null")
	else if (theService.getDefaultAttributeValue(classOf[Media])==null) println("Default AttributeValue == null")
	var lastSelectedMedia:MediaSizeWrapper=MediaMap.getMediaSize(theService.getDefaultAttributeValue(classOf[Media]).asInstanceOf[MediaSizeName])
	val mediaModel=new DefaultComboBoxModel[MediaSizeWrapper]
	val trayModel=new DefaultComboBoxModel[MediaTrayWrapper]
	//var formList:Seq[FormDescription]=Seq.empty	
	var printerList=  printServices.map(s=>s.getName).toSeq
	val paramTabMod=new ParamTabMod
	
	setMediaWrapper(lastSelectedMedia)
	setPrinterName(getPrintServiceName(theService))
	
	def setOutDefs(newList:Seq[OutputDefinition]): Unit = {
	  outDefs=newList
	  for(od<-outDefs;formID=od.formInst )
			 forms.find(_.inst==formID) match {
			case Some(form)=> od.formName=form.name
			case None => util.Log.e("Cant find form with id:"+formID)
		} 
	  PrintQuestionHandler.outdefDialog.loadOutdefs(outDefs)
	}
	
	def changeOutDef(newOutDef:OutputDefinition): Unit = {
	  outDefs.indexWhere(_.odInst==newOutDef.odInst) match {
	    case -1 =>
	    case index => setOutDefs(outDefs.updated(index, newOutDef))
	  }
	}
  
  def readInXML(xmlData:Seq[scala.xml.Node]): Unit = {
    forms=for (f<- xmlData \\ "Form") yield FormDescription.fromXML(f)
    //println("readInXML outdefs "+(xmlData \\"OutDef").mkString)
    setOutDefs(for (f<- xmlData \\ "OutDef") yield OutputDefinition.fromXML(f))
  }
	
	def setMediaWrapper(newWrapper:MediaSizeWrapper): Unit = if(newWrapper!=null){
		pras.remove(classOf[MediaPrintableArea])
		pras.add(newWrapper.mn)		
		lastSelectedMedia=newWrapper		
	}
	
	def setMediaTray(mediaTray:MediaTrayWrapper): AnyVal = if(mediaTray!=null){
	  if(mediaTray==AutoTray) {
	  	pras.remove(classOf[MediaTray])
	  	pras.remove(TrayMap.altClass)
	  }
	  else pras.add(mediaTray.altValue)
	}	
	
	
	/** set Printer Name
	 * return (Printservice for given Name, Index of the Default Tray. -1 when none available)
	 */	
	def setPrinterName(newName:String):(PrintService,Int)= {
		var defaultTrayIx= -1
		printerName=newName		
		mediaModel.removeAllElements()
		trayModel.removeAllElements()
		//println("SetPrinterName:"+newName)
		
		theService=printServices.find(_.getName==printerName) match{
    	case Some(serv) =>serv
    	case None => println(printerName + " not found "); PrintServiceLookup.lookupDefaultPrintService()
		}	
		
		
		theService.getSupportedAttributeValues(classOf[Media], null, emptyAttributeSet).asInstanceOf[Array[_]].
      foreach {
      case n: MediaSizeName => if (n.getValue < 40 && !(n.toString.length > 4 && n.toString.substring(0, 3).equalsIgnoreCase("JIS"))) mediaModel.addElement(MediaMap.getMediaSize(n))
      case t: MediaTray =>
				if (t.toString == "Automatic-Feeder") defaultTrayIx = trayModel.getSize
				trayModel.addElement(TrayMap.getMediaTray(t))
			//case p:MediaName=> println("*** MediaName:"+p)
      case e => //println("*** Unknown element:"+e)
    }
		(theService,defaultTrayIx)	
	}
	
	def getPrintableArea: MediaPrintableArea = {
		val pList=theService.getSupportedAttributeValues(classOf[javax.print.attribute.standard.MediaPrintableArea],
    		null,pras).asInstanceOf[Array[MediaPrintableArea]]	
		if(pList.isEmpty) throw new IllegalArgumentException("Cant find Supported PrintableArea ")
		else pList.head
	}
	
	def getPageFormat: PageFormat = {
		printJob.setPrintService(theService)
		printJob.getPageFormat(pras)		
	}
	def getPrintServiceName(ps:PrintService): String ={
	  ps.getAttribute(classOf[javax.print.attribute.standard.PrinterName]).toString
	}

}