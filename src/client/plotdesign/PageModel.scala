package client.plotdesign
import definition.data.Reference
import client.comm.ClientQueryManager
import definition.comm.NotificationType
import javax.print.PrintService
import javax.print.PrintServiceLookup
import definition.expression.StringConstant
import javax.swing.DefaultComboBoxModel
import javax.print.attribute.HashPrintRequestAttributeSet
import client.print.{MediaSizeWrapper,MediaMap,AutoTray}
import javax.print.attribute.PrintRequestAttributeSet
import javax.print.attribute.standard.MediaPrintableArea
import javax.print.attribute.standard.Media
import javax.print.attribute.standard.MediaSizeName
import client.print.{MediaTrayWrapper,TrayMap}
import javax.print.attribute.standard.MediaTray
import javax.print.attribute.standard.MediaName
import javax.print.attribute.standard.OrientationRequested
import definition.expression.BoolConstant
import java.awt.geom.Rectangle2D
import definition.data.FormDescription
import definition.data.InstanceData
import definition.data.OutputDefinition
import definition.data.OwnerReference
import definition.typ.SystemSettings
import definition.expression.IntConstant
import scala.swing.Swing

class PageModel(val controller:PlotDesignController) {
  @volatile var listLock=new Object
  var portrait:Boolean=false
  var printer:String=_
  var pageSettings:String=_  
  var thisSubsID= -1
  var odefSubsID= -1
  var odefRef:Option[Reference]=None
  var currentService:Option[PrintService]=None
  val emptyAttributeSet=new HashPrintRequestAttributeSet()
  val defaultPrinter= PrintServiceLookup.lookupDefaultPrintService
  var selectedMedia:MediaSizeWrapper=MediaMap.getMediaSize(defaultPrinter.getDefaultAttributeValue(classOf[Media]).asInstanceOf[MediaSizeName])
  var selectedTray:Option[MediaTrayWrapper]=None
  var printableArea:Option[MediaPrintableArea]=None
  def pageWidth:Float=/*FormDescription.toMM*/ if (portrait) selectedMedia.width else selectedMedia.height
  def pageHeight:Float=/*FormDescription.toMM*/ if (portrait) selectedMedia.height else selectedMedia.width
  val bounds=new Rectangle2D.Float()
  val clipBounds= new Rectangle2D.Float()
  @volatile var ignoreChangesAfterCreating=0
  //@volatile var isCreating=false
  
 
 
  @volatile private var ignoreMessages=false
  
  lazy val printServices=PrintServiceLookup.lookupPrintServices( null,null)
  
  var pras:PrintRequestAttributeSet  = new HashPrintRequestAttributeSet()

  val mediaModel=new DefaultComboBoxModel[MediaSizeWrapper]
  val trayModel=new DefaultComboBoxModel[MediaTrayWrapper]
	
  
  lazy val printServiceNames:IndexedSeq[String]={    
    if(printServices==null) IndexedSeq.empty
    else printServices.map(_.getName)
  }
  
  def borderLeft=printableArea.fold(0f)(_.getX(MediaPrintableArea.MM))
  def borderTop=printableArea.fold(0f)(_.getY(MediaPrintableArea.MM))
  def printableWidth=printableArea.fold(pageWidth)(_.getWidth(MediaPrintableArea.MM))
  def printableHeight=printableArea.fold(pageHeight)(_.getHeight(MediaPrintableArea.MM))
  
  def load(ref:Reference,doneListener:()=>Unit) = {
    //var firstLoad=true
    odefRef=None
    thisSubsID=ClientQueryManager.createSubscription(ref,-1){(command,data) => listLock.synchronized {Swing.onEDT{
       command match {
         case NotificationType.sendData|NotificationType.fieldChanged |NotificationType.updateUndo=>
           for(inst <-data.headOption)
             controller.headerPanel.setName(inst.fieldValue.head.toInt,inst.fieldValue(1).toString)
           if(command==NotificationType.sendData) {
             odefSubsID=ClientQueryManager.createSubscription(ref,0) {(command,data)=> listLock.synchronized {Swing.onEDT{
					    command match {
					      case NotificationType.sendData|NotificationType.updateUndo =>
                  if(data.size>0) loadOdef(data)
                  if(command==NotificationType.sendData){
					          if(data.size==0) createOdef(ref)
					          doneListener()
					        }
                case NotificationType.fieldChanged =>if(ignoreChangesAfterCreating==0) loadOdef(data) else
					      		{ignoreChangesAfterCreating -=1;/*println("ignore:"+ignoreChangesAfterCreating)*/}
					      case NotificationType.childAdded => if(ignoreChangesAfterCreating==0) loadOdef(data)
                case _=>
					    }
					  }}
					  }
           }
       }
    }}
    }      
  }
  
  private def createOdef(thisRef:Reference)= {
    //println("createOdef "+thisRef)
    ignoreChangesAfterCreating=3
    val oInst=ClientQueryManager.createInstance(OutputDefinition.odefType,Array(new OwnerReference(0.toByte,thisRef)))
    val oref=new Reference(OutputDefinition.odefType,oInst)
    odefRef=Some(oref)
    val printerIx=getPrintServiceIndex("")
    setPrinter(printerIx) 
    controller.headerPanel.selectPrinter(printerIx)
    //println("ix:"+printerIx+" default"+defaultPrinter)
    PrintFormInfo.plotDesignForms.headOption match {
      case Some(defaultForm)=> ClientQueryManager.writeInstanceField(oref,0,new IntConstant(defaultForm.id))//;println("write Form:"+defaultForm)
      case None =>util.Log.w("no Form found")
    }
    //println("selectedMedia:"+selectedMedia)
    if(selectedMedia!=null) setMediaWrapper(selectedMedia)
   //storeMedia()
    //isCreating=false
    //controller.updatePage()
    //println("Create done")
  }
  
  private def loadOdef(data:Seq[InstanceData])= {
    //println("load odef "+data.mkString(",")+" "+Thread.currentThread().getName()+"\n"+Thread.currentThread.getStackTrace().drop(2).take(3).mkString("\n")+"\n")
  	for(inst <-data.headOption) {             
  		ignoreMessages=true  
  		odefRef=Some(inst.ref)
  		controller.headerPanel.selectForm(inst.fieldValue.head.toInt)
  		val serviceIx=getPrintServiceIndex(inst.fieldValue(1).toString)
  		val service=printServices(serviceIx)
  		currentService=Some(service)
  		controller.headerPanel.selectPrinter(serviceIx)
  		updateMediaLists(service)
  		val paperS=inst.fieldValue(2).toString.split('|')
  		//System.out.println("Loading paperS:"+paperS.mkString(",")+" "+paperS.size)
  		val paged=paperS(0)
  		if(paged.length==0) {
  			val defMed=service.getDefaultAttributeValue(classOf[Media]).asInstanceOf[MediaSizeName]
  			val newMap=MediaMap.getMediaSize(defMed)
  			internSetMediaWrapper(newMap)
  			//println("set Media:"+defMed+" "+newMap)
  			controller.headerPanel.selectSize(mediaModel.getIndexOf(newMap))
  		} else {
  			service.getSupportedAttributeValues(classOf[Media],null,emptyAttributeSet).asInstanceOf[Array[Media]].
  			find(_.toString==paged) match {
  				case Some(media)=>
            val wrapper=MediaMap.getMediaSize(media.asInstanceOf[MediaSizeName])
            val ix=mediaModel.getIndexOf(wrapper)
            //println("Found Media: "+media+ " "+wrapper+" ix:"+ix)
            if(ix> -1)  controller.headerPanel.selectSize(ix)
            internSetMediaWrapper(wrapper)
          case None => util.Log.w("Cant find Media :"+paged)
  			}
  		}	
  		selectedTray=None
  		if(paperS.length>1) { // tray
  			val trayName=paperS(1)
  			service.getSupportedAttributeValues(classOf[Media],null,emptyAttributeSet).asInstanceOf[Array[Media]].
  			find(_.toString==trayName) match {
  				case Some(tray)=>
            val wrapper=TrayMap.getMediaTray(tray.asInstanceOf[MediaTray])
            val ix=trayModel.getIndexOf(wrapper)
            //println("Found tray "+tray+ " ix:"+ix)
            if(ix> -1) controller.headerPanel.selectTray(ix)
            internSetMediaTray(wrapper)
          case None =>util.Log.w("Cant find Tray :"+trayName)
  			}
  		}
  		portrait=inst.fieldValue(3).toBoolean
  		internSetOrientation(portrait)
  		controller.headerPanel.selectOrientation(portrait)   
  		controller.updatePage()
  		ignoreMessages=false
  	}         
  }
  
    
  def shutDown():Unit = listLock.synchronized {
    if(thisSubsID> -1){
      ClientQueryManager.removeSubscription(thisSubsID)
      thisSubsID= -1
    }
    if(odefSubsID> -1){
      ClientQueryManager.removeSubscription(odefSubsID)
      odefSubsID= -1
    }
    
  }
  
  def getPrintServiceWithName(name:String)={
    printServices find(_.getName==name ) match {
      case Some(service)=>service
      case None => defaultPrinter
    }
  }
  
  def getPrintServiceIndex(name:String)= {
    //println("get printerservice index:"+ name+" default:"+defaultPrinter.getName()+" "+printServices.map(_.getName).mkString(" "))
    val ix=  printServices.indexWhere(_.getName==name )
    if(ix<0) printServices.indexWhere(_ ==defaultPrinter)
    else ix  
  }
  
  
  def setPrinter( ix:Int)= if(ix>=0 && !ignoreMessages) listLock.synchronized{   
    val service=printServices(ix)
    currentService=Some(service)
    //println("set printer :"+ix+" "+service.getName())
    for(ref<-odefRef)
    	ClientQueryManager.writeInstanceField(ref,1,new StringConstant(service.getName))				
    adaptLastSelected(service,updateMediaLists(service))
  }
  
  def setForm(inst:Int)= listLock.synchronized {
    for(ref<-odefRef){
     // println("Set form inst:"+inst+" oref:"+ref)
      ClientQueryManager.writeInstanceField(ref,0,new IntConstant(inst))
    }
      
  }
  
  private def updateMediaLists(service:PrintService):Int = {
    var defaultTrayIx= -1
    mediaModel.removeAllElements()
		trayModel.removeAllElements()
		controller.headerPanel.combosAdjusting=true
    service.getSupportedAttributeValues(classOf[Media], null, emptyAttributeSet).asInstanceOf[Array[_]].
      foreach {
      case n: MediaSizeName => if (n.getValue < 40 && !(n.toString.length > 4 && n.toString.substring(0, 3).equalsIgnoreCase("JIS")))
        mediaModel.addElement(MediaMap.getMediaSize(n))
      case t: MediaTray =>
        if (t.toString == "Automatic-Feeder") defaultTrayIx = trayModel.getSize
        trayModel.addElement(TrayMap.getMediaTray(t))
      case p: MediaName => //println("*** MediaName:" + p)
      case e => util.Log.w("*** Unknown element:" + e)
    }
		controller.headerPanel.combosAdjusting=false
		defaultTrayIx
  }
  
  private def adaptLastSelected(service:PrintService,defaultTrayIx:Int) = {         
		if(selectedMedia!=null && service.isAttributeValueSupported(selectedMedia.mn, null, emptyAttributeSet))
			 controller.headerPanel.selectSize(mediaModel.getIndexOf(selectedMedia))
		 else {
      util.Log.e("lastSelMedia not supported "+selectedMedia)
			val defMed=service.getDefaultAttributeValue(classOf[Media]).asInstanceOf[MediaSizeName]
			val newMap=MediaMap.getMediaSize(defMed)
			internSetMediaWrapper(newMap)
			controller.headerPanel.selectSize(mediaModel.getIndexOf(selectedMedia))
		}
		
		if(trayModel.getSize>0&&defaultTrayIx> -1) {
		  controller.headerPanel.selectTray(defaultTrayIx)
			pras.add(trayModel.getElementAt(defaultTrayIx).altValue)
		}
  }
  
  def setMediaWrapper(newWrapper:MediaSizeWrapper) = if(!ignoreMessages){
    //println("Set Media Wrapper "+newWrapper+" "+Thread.currentThread().getStackTrace().drop(1).mkString("\n"))
    internSetMediaWrapper(newWrapper)    
    storeMedia()
  }
  
  private def storeMedia()= for(ref<-odefRef){
    val paperSettings=selectedMedia.mn.toString+(selectedTray match {case Some(tray)=>"|"+tray.mt.toString;case None =>"" })
    //println("Store Media:"+paperSettings)
    ClientQueryManager.writeInstanceField(ref,2,new StringConstant(paperSettings))	
  }
  
  
  private def internSetMediaWrapper(newWrapper:MediaSizeWrapper)= listLock.synchronized{
		pras.remove(classOf[MediaPrintableArea])
		pras.remove(classOf[MediaSizeName])
		pras.add(newWrapper.mn)		
		selectedMedia=newWrapper
		//println("internSetMediaWrapper "+newWrapper)
		for(service<-currentService){
		  val prList=service.getSupportedAttributeValues(classOf[MediaPrintableArea],null,pras).asInstanceOf[Array[MediaPrintableArea]]	
		  //println("prList:"+prList.mkString(";"))
		  printableArea = prList.headOption		  
		}
		calcPageBounds()
		calcClipBounds()
		controller.layerRefList.calcBounds()
		controller.calcAllBounds()
  }
  
  def setMediaTray(newWrapper:MediaTrayWrapper) =  if(!ignoreMessages){
    internSetMediaTray(newWrapper)    
    storeMedia()
  }
  
  private def internSetMediaTray(newWrapper:MediaTrayWrapper) = {    
  	if(newWrapper==AutoTray) {
  		pras.remove(classOf[MediaTray])
  		pras.remove(TrayMap.altClass)
  	}
  	else pras.add(newWrapper.altValue) 
  	selectedTray=Some(newWrapper)
  }
  
  def setOrientation(portrait:Boolean) =  if(!ignoreMessages){
    internSetOrientation(portrait)   
    for(ref<-odefRef)
      ClientQueryManager.writeInstanceField(ref,3,new BoolConstant(portrait))    
  }
  
  def internSetOrientation(portrait:Boolean ) = {
    pras.remove(classOf[OrientationRequested])
    pras.add(if(portrait) OrientationRequested.PORTRAIT else OrientationRequested.LANDSCAPE )
    internSetMediaWrapper(selectedMedia)
  }
  
  
  private def calcClipBounds()= {
    val rightBorder=pageWidth-borderLeft-printableWidth
    val bottomBorder=pageWidth-borderTop-printableHeight
    //println("PW:"+pageWidth+" PH:"+pageHeight+" borderLeft:"+borderLeft+" borderTop:"+borderTop)
    //println("ClipRect rightBorder:"+rightBorder+" bottomBorder:" + bottomBorder+" printableWidth:"+printableWidth+" printableHeight:"+printableHeight)
    if(rightBorder<0 || bottomBorder <0) {
      clipBounds.x=if(portrait)borderTop/1000f else borderLeft/1000f
      clipBounds.y=if(portrait) borderLeft/1000f else borderTop/1000f
    }
    else {
      clipBounds.x=if(portrait)borderLeft/1000f else borderTop/1000f
      clipBounds.y=if(portrait) borderTop/1000f else borderLeft/1000f
    }
    clipBounds.width= if(portrait)printableWidth/1000f else printableHeight/1000f
    clipBounds.height=if(portrait)printableHeight/1000f else printableWidth/1000f    
  }
  
  def calcPageBounds()= {
    bounds.x=0f
    bounds.y=0f
    bounds.width=pageWidth/1000f
    bounds.height=pageHeight/1000f    
  }
}

class FormInfo(val name:String,val id:Int) {
  def this(data:InstanceData)= this(data.fieldValue.head.toString,data.ref.instance)
  override def toString=name
}

object PrintFormInfo{
  lazy val folderType=SystemSettings().systemTypes("Folder")
	lazy val formType=SystemSettings().systemTypes("PrintForm")
	lazy val formFolders= SystemSettings().getCustomSettings("PrintForms").filter(_.ref.typ==folderType)	
  lazy val plotDesignType=SystemSettings().systemTypes("PlotDesign")	
	lazy val plotDesignForms=getFormsForType(plotDesignType)
	  
	def getFormsForType(atype:Int):Seq[FormInfo]= {
    formFolders.find(_.fieldValue.head.toInt==atype) match {
      case Some(folder)=> ClientQueryManager.queryInstance(folder.ref,1).filter(_.ref.typ==formType).map(new FormInfo(_))      
      case _ => throw new IllegalArgumentException("Cant find form for Type "+atype)
    }
  }
  
}
