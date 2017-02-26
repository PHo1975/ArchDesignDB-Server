/**
 * Author: Peter Started:05.09.2010
 */
package client.ui

import java.awt.{Color, Font}
import java.net._
import javafx.application.Platform
import javax.swing._

import client.comm._
import client.dataviewer._
import client.dialog._
import client.graphicsView._
import client.layout._
import client.model._
import com.sun.jna.{Native, NativeLong, WString}
import definition.comm.{PropertyGroup, PropertyValue}
import definition.typ.DataType

import scala.swing._
import scala.swing.event._

/** tests the instance dataviewer
 * 
 */



object ClientApp extends App {
  //val start=System.currentTimeMillis()
  
  val defFont=new Font("Arial", 0, 16)
   UIManager.getLookAndFeelDefaults()
        .put("defaultFont", defFont)

	var sock:ClientSocket=null
	
	def createHLine= {
	  val ret=Swing.VStrut(1)
	  ret.border=BorderFactory.createLineBorder(Color.GRAY)
	  ret
	}
	
	lazy val undoLabel=new Label()
	undoLabel.font=new Font("Arial",0,25)
	lazy val undoPanel=new BorderPanel{
	  add(undoLabel,BorderPanel.Position.Center)
	}
	
	val fieldEditPan=new FieldEditorsPanel	
	val undoBut = new IconableButton("Undo","MainWindow","letzte Aktion(en) rückgängig machen")
  val infoBut= new IconableButton("Info","MainWindow","Object-Infos anzeigen")
	undoBut.text="Rückgängig..."
	undoBut.font=defFont
	val hideBut=new ToggleButton("Hide")	
	val strokeEditBut=new Button("Tast")
	val consoleBut=new Button("Console")
	val normBack=strokeEditBut.background
	KeyStrokeManager.bindigsEnabledListener=Some((b:Boolean)=>strokeEditBut.background= if(b)normBack else Color.green )
	strokeEditBut.focusable=false
	consoleBut.focusable=false
	undoBut.focusable=false	
	val workplaceBut=new IconableButton("Workplaces","MainWindow","Fensteranordnung speichern / laden")
	workplaceBut.text="Arbeitsflächen ..."
	workplaceBut.font=defFont
	lazy val undoDialog=new UndoDialog(top)
	lazy val workplaceDialog=new WorkplaceDialog(top)	
	val mainBox=new MainBox
	val tableViewboxType=new ViewboxContentType(1,"table","T","Neues Tabellenfenster",()=>{
		new TableViewbox
	})
	val graphicsViewboxType=new ViewboxContentType(2,"graphics","G","Neues Grafikfenster",()=>{
		new GraphicsViewbox
	})
	ViewboxContentTypeList.addType(tableViewboxType)
	ViewboxContentTypeList.addType(graphicsViewboxType)
	
	
	lazy val strokeDialog=new StrokeEditDialog(top)
	lazy val middleBox= new Panel with SequentialContainer.Wrapper {
		override lazy val peer = {
			val p = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {
			  def getScrollableTracksViewportWidth()=true 
			 	def getScrollableTracksViewportHeight()=true
			 	def getPreferredScrollableViewportSize=getPreferredSize
			 	def getScrollableBlockIncrement(rect:Rectangle,o:Int,d:Int)=if(o==Orientation.Vertical.id) rect.height else rect.width
			 	def getScrollableUnitIncrement(rect:Rectangle,o:Int,d:Int)=30
			}
			val l = new javax.swing.BoxLayout(p, Orientation.Vertical.id)			
			p.setLayout(l)
			p
		}		
		contents+=createHLine+=fieldEditPan+=createHLine+=DialogManager.dialogPanel+=ActionPanel+=createHLine+=Swing.VStrut(20)
		opaque=true		
		background=DialogManager.leftPanelColor		
	}	
	
	private lazy  val middleScroller=new ScrollPane{
	    opaque=false
	    preferredSize=new Dimension(DialogManager.sidePanelWidth+20,50)	 
	    maximumSize=new Dimension(DialogManager.sidePanelWidth+20,Short.MaxValue)
		  viewportView= middleBox	
		  border=null
		  
		}  
	
	lazy val mainPanel= new BorderPanel(){
		add (mainBox,BorderPanel.Position.Center)
		add (new BoxPanel(scala.swing.Orientation.Vertical){	
		  border=BorderFactory.createEmptyBorder(5,5,5,10)	 
		  background=DialogManager.leftPanelColor
			workplaceBut.maximumSize=new java.awt.Dimension(Short.MaxValue,30)
			workplaceBut.xLayoutAlignment=0.5d		
			undoBut.maximumSize=workplaceBut.maximumSize
			undoBut.xLayoutAlignment=0.5d
			contents+=Swing.VStrut(10)+=DialogManager.selectLabScroller+=
			  middleScroller+=DialogManager.errorScroller+=createHLine+=undoBut+=Swing.VStrut(10)+=workplaceBut+=Swing.VStrut(15)+=new BoxPanel(Orientation.Horizontal) {	
		     opaque=false
					contents+=hideBut+=strokeEditBut+=consoleBut//poolBut
			}			
		},BorderPanel.Position.West)
		listenTo(undoBut,hideBut,strokeEditBut,workplaceBut,consoleBut,infoBut)
			reactions += {					
				case ButtonClicked(`undoBut`) => requestUndoData()
				case ButtonClicked(`hideBut`) => setHide(hideBut.selected)
				case ButtonClicked(`strokeEditBut`) => showStrokeDialog()
				case ButtonClicked(`workplaceBut`) => showWorkplaceDialog()
				case ButtonClicked(`consoleBut`) => showConsole()
        case ButtonClicked(`infoBut`) => showInfo()
			}
	}

	def updateSidePanel()= {
	  middleBox.revalidate() 
	}



	
	class TopMainFrame extends MainFrame {
		val logWindow=new LogWindow(this)
		override def closeOperation() =	{
      util.Log.e("Close " )
			shutDown()							
		}
		title="Datenbank"
		client.icons.IconManager.getIcon("MainWindow","Icon") match {
		  case Some(icon)=> iconImage=icon.getImage()
		  case _ =>
		} 		
		contents = mainPanel				
		KeyStrokeManager.topComponent=Some(mainPanel)
	}

	lazy val top =new TopMainFrame
	
	def showInfo()={
    for(gr<-DialogManager.selectedInstances.headOption;inst<-gr.children)
      println("inst:"+inst+" ref:"+inst.ref)
  }
	
	def saveWindowPositions() = {
	  val defaultGroup=new PropertyGroup("Default",new collection.mutable.HashMap[String,PropertyValue]())
	  mainBox.storeSettings(defaultGroup)
	  val oldList=UserSettings.getListProperty[PropertyGroup]("WindowSettings","Boxes")
	  val newList=oldList map (gr=> if(gr.name=="Default") defaultGroup else gr  )
	  val storeList=if(newList.isEmpty) List(defaultGroup) else newList
	  val bounds=top.bounds
	  UserSettings.setListProperty[PropertyGroup]("WindowSettings","Boxes",storeList)
	  UserSettings.setIntProperty("WindowSettings","Width",bounds.width)
	  UserSettings.setIntProperty("WindowSettings","Height",bounds.height)
	  UserSettings.setIntProperty("WindowSettings","XPos",bounds.x)
	  UserSettings.setIntProperty("WindowSettings","YPos",bounds.y)
	  UserSettings.setIntProperty("WindowSettings","Maximized",if(top.peer.getExtendedState==java.awt.Frame.MAXIMIZED_BOTH)1 else 0)
	}
	
	@native def SetCurrentProcessExplicitAppUserModelID(appID:WString):NativeLong
	
	startup(args)
	
	def startup(args: Array[String]):Unit = {	
	  Platform.setImplicitExit(false)
    if(args.length<3 ) { System.out.println("Usage: ViewTest host portnr name password [hide=1]"); shutDown() }
		val serverName=	args(0)
		
	  if(System.getProperty("os.name").contains("Windows 7") || System.getProperty("os.name").contains("Windows 8")) {
	    val appID="DBClient"
		  Native.register("shell32")	  
		  if (SetCurrentProcessExplicitAppUserModelID(new WString(appID)).longValue() != 0)
		      throw new RuntimeException("unable to set current process explicit AppUserModelID to: " + appID)    
	  } else util.Log.e("no windows")
		
		try {
			sock=new ClientSocket(InetAddress.getByName(serverName),args(1).toInt,args(2),args(3),"Table")
	  } catch { case c:ConnectException =>
			JOptionPane.showMessageDialog(null,"Kann Server an Adresse "+serverName+" nicht finden.","Datenbank",JOptionPane.ERROR_MESSAGE)
			System.exit(0)
		}
    top.title= "Datenbank ["+args(2)+"]"
    sock.connectionBrokenListener=()=>{top.close()}
	  ClientQueryManager.undoLockListener =Some((isLocked,lockUser) => {
	    if(isLocked) {undoLabel.text="Benutzer "+lockUser+" will rückgängig machen"
	    //println("Lock orig")  
	    undoPanel.preferredSize=mainPanel.size
	    top.contents=undoPanel
	    top.repaint()}
	    else {
	      //println("Released orig")
	      mainPanel.preferredSize=undoPanel.preferredSize
	      top.contents=mainPanel
	      top.repaint()}
	  })
	  sock.startupFinishListener+=( ()=>Swing.onEDT{
	    val storeList=UserSettings.getListProperty[PropertyGroup]("WindowSettings","Boxes")
				storeList.find(_.name=="Default") match {
				  case Some(group)=> mainBox.restoreSettings(Some(group), loadDone _)
				  case None => mainBox.restoreSettings(None,loadDone _)
				}
	  })
	  
		sock.classesReadListener=()=>Swing.onEDT{
      //println("Classes Read "+(System.currentTimeMillis()-start))
		  ClientQueryManager.setClientSocket(sock)  
		  ClientQueryManager.registerSetupListener (() => {			    
				val windowWidth=UserSettings.getIntProperty("WindowSettings","Width")
				val windowHeight=UserSettings.getIntProperty("WindowSettings","Height")
				val windowX=UserSettings.getIntProperty("WindowSettings","XPos")
				val windowY=UserSettings.getIntProperty("WindowSettings","YPos")
				if(UserSettings.getIntProperty("WindowSettings","Maximized")==1) {
				  top.bounds=new java.awt.Rectangle(10,10,800,600)
				  top.peer.setExtendedState(java.awt.Frame.MAXIMIZED_BOTH)
				}
				else 
				if(windowWidth>0 && windowHeight>0)
				  top.bounds=new java.awt.Rectangle(windowX,windowY,windowWidth,windowHeight)
				top.visible=true
				
			}	)		
			
			SelectEventDispatcher.registerSelectListener(ActionPanel)		
			SelectEventDispatcher.registerSelectListener(DialogManager)		
			SelectEventDispatcher.registerSelectListener(fieldEditPan)
			
			DialogManager.answerArea.registerCustomPanel[PointAnswerPanel](DataType.VectorTyp)
			DialogManager.answerArea.registerCustomPanel[ReferenceAnswerPanel](DataType.ObjectRefTyp)
			DialogManager.answerArea.registerCustomPanel[BlobAnswerPanel](DataType.BlobTyp)
			ClientQueryManager.registerStepListReader(undoDialog)	  		
			
	  	if(args.length>4){
	  		val sel:Boolean=args(4).toInt>0
	  		if(sel)hideBut.selected=true
	  		setHide(sel)
	  	} else {hideBut.selected=true;setHide(true)}	
			//super.startup(args)
	  	top.visible = true		
		}
		sock.start()
	}		
		

  def loadDone():Unit = {
    //System.out.println("Init Loading Done at "+(System.currentTimeMillis()-start))    
  }
	
	def shutDown() = {
    util.Log.e("shutdown")
		top.visible=false
		  
    if(sock!=null) {
      saveWindowPositions()
      ClientQueryManager.setClientSocket(sock)
      sock.quitApplication()
    } 
	}
	
	def showStrokeDialog()= {
	  val pos=strokeEditBut.peer.getLocationOnScreen()
	  strokeDialog.bounds=new java.awt.Rectangle(pos.x,pos.y-500,560,500)
	  strokeDialog.showDialog()
	}
	
	def requestUndoData() = {
		undoDialog.setLocationRelativeTo(DialogManager.errorScroller)
		val pos=undoBut.peer.getLocationOnScreen()
    undoDialog.bounds=new java.awt.Rectangle(pos.x,pos.y-310,600,300)		
		undoDialog.prepare()
		ClientQueryManager.runInPool{ClientQueryManager.requestUndoData()}
		
	}
	
	def setHide(value:Boolean) = {
		if(value){
			hideBut.text="Show"				
		}
		else hideBut.text="Hide"
		DataViewController.hideProperties=value
	}
	
	def showWorkplaceDialog() = {
	  val pos=workplaceBut.peer.getLocationOnScreen()
    workplaceDialog.bounds=new java.awt.Rectangle(pos.x,pos.y-400,400,400)
	  workplaceDialog.showWorkplaces(openWorkplace)
	}
	
	def openWorkplace(pg:PropertyGroup) = Swing.onEDT{
	  mainBox.shutDown()
	  mainBox.restoreSettings(Some(pg),()=>{
	    //System.out.println("Loading "+pg.name+" done ")
	    mainBox.revalidate()
	    mainBox.repaint()})
	}
	
	def showConsole()= ClientQueryManager.runInPool{
	  top.logWindow.visible=true
	}

}