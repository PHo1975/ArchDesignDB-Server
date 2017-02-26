/** Author: Peter Started:26.07.2010
 */
package management.databrowser

import java.awt.{SystemTray, Toolkit, TrayIcon}
import java.awt.event.{MouseAdapter, WindowAdapter}
import javax.swing.{BorderFactory, JTable, RowSorter, SortOrder}
import javax.swing.border.TitledBorder
import javax.swing.table.{DefaultTableModel, TableRowSorter}

import client.dialog.LogOutputStream
import com.sun.jna.{Native, NativeLong, WString}
import definition.typ.AllClasses
import management.databrowser.stat.StatPanel
import server.comm.MainServerSocket
import server.storage.{ServerObjectClass, StorageManager}
import transaction.handling.SessionManager
import util.Log

import scala.Array.fallbackCanBuildFrom
import scala.collection.JavaConversions.seqAsJavaList
import scala.swing.{BorderPanel, BoxPanel, Button, Component, Dimension, GridPanel, Label, MainFrame, Orientation, Rectangle, ScrollPane, SimpleSwingApplication, Swing, Table}
import scala.swing.event.ButtonClicked
import scala.util.control.NonFatal

//import java.swing.JTable

/**
 */

trait ClassListListener {
  def classListChanged(classList: Seq[(Int, String)]): Unit
}

object MainWindow extends SimpleSwingApplication {
  var trayIcon: TrayIcon = _
  var hidden: Boolean = false
  var firstopen = true
  var theModel: DefaultTableModel = null
  var dataList: Array[Array[java.lang.Object]] = Array()
  var shortClassList: Seq[(Int, String)] = Seq.empty
  val usedIDs = collection.mutable.HashSet[Int]()
  val classListListener = collection.mutable.HashSet[ClassListListener]()
  var newClass: ServerObjectClass = null
  var showWindowMinimized=false
  
  val reorgPanel= new ReorgPanel
  val manButSize=new Dimension(160,35)
  val buttonSize=new Dimension(Short.MaxValue,30)
  lazy val backupDialog=new BackupDialog(top)
  var debug=false
  lazy val consolePanel = new ConsolePanel(debug)
  
  val accordion=new AccordionComponent
  val statPanel=new StatPanel
  
  @native def SetCurrentProcessExplicitAppUserModelID(appID:WString):NativeLong
  if(System.getProperty("os.name").contains("Windows")) {
    val versID=System.getProperty("os.version").toFloat
    if(versID>=6.1) {
	    val appID="DBServer"
		  Native.register("shell32")	  
		  if (SetCurrentProcessExplicitAppUserModelID(new WString(appID)).longValue() != 0)
		      throw new RuntimeException("unable to set current process explicit AppUserModelID to: " + appID)    
    } else println("Windows version "+versID)
  } 
  
  val classTable = new Table {
    override lazy val peer: JTable = new JTable with SuperMixin
    peer.setAutoResizeMode(Table.AutoResizeMode.LastColumn.id)
    peer.setSelectionMode(Table.IntervalMode.Single.id)
  }
  

  def dataInit() =
    {    	
      generateDataList
      theModel = new DefaultTableModel(dataList, Array[Object]("ID", "Name", "Description")) {
        override def getColumnClass(col: Int) = {
          col match {
            case 0 => classOf[java.lang.Integer]
            case _ => classOf[String]
          }
        }
      }
      val sorter = new TableRowSorter[DefaultTableModel](theModel)
      val sortKeys = List(new RowSorter.SortKey(0, SortOrder.ASCENDING))
      sorter.setSortKeys(sortKeys)
      classTable.peer.setModel(theModel)
      classTable.peer.setRowSorter(sorter)
      val col = classTable.peer.getColumnModel.getColumn(0)
      col.setMaxWidth(40)
      col.setResizable(false)
      usedIDs.clear
      usedIDs ++= dataList.map(_(0).asInstanceOf[java.lang.Integer].intValue)
    }

  def generateDataList() = {
    def classListIterator = AllClasses.get.getClassList.valuesIterator
    dataList = classListIterator.map(
      x => Array[Object](new java.lang.Integer(x.id), x.name, x.description)).toSeq.
      sortWith(_(0).asInstanceOf[java.lang.Integer].intValue < _(0).asInstanceOf[java.lang.Integer].intValue).toArray
    shortClassList = classListIterator.map(x => (x.id, x.name)).toSeq.sortWith(_._2 < _._2)
    classListListener.foreach(_.classListChanged(shortClassList))
  }

  

  class MyBorderPanel extends BorderPanel {
    def addIt(c: Component, l: Constraints): Unit = {
      super.add(c, l)
    }
  }
  
  val rightPanel = new MyBorderPanel 
  
  
  def addRightPanelComponent(c:Component)= rightPanel.addIt(c,BorderPanel.Position.Center)

  override def startup(args: Array[String]) = {
    Log.w("startup "+args.toSeq.mkString(","))
    if(args.length>0){
      if(args(0).equalsIgnoreCase("debug")) debug=true
      if(args(0).equalsIgnoreCase("noBulk")) MainServerSocket.noBulkAction=true
      if (args.exists(_.equalsIgnoreCase("minimized")))showWindowMinimized=true
    }
    super.startup(args)
  }
  
  def createBut(name:String)= {
    val newBut=new Button(name)
    newBut.maximumSize=manButSize
    newBut.preferredSize=manButSize
    newBut.xLayoutAlignment=0.5f
    newBut
  }

  val managementPanel:BoxPanel = new BoxPanel(Orientation.Vertical) {
    val manButPanel=new BoxPanel(Orientation.Vertical) {
      val belowTopBorder = BorderFactory.createTitledBorder("Management")
      belowTopBorder.setTitlePosition(TitledBorder.BELOW_TOP)
      border = belowTopBorder
      this.xLayoutAlignment=0.5f		
      val backupBut=createBut("Backup")
      val shutDownBut=createBut("ShutDown")
      val showTransBut = createBut("TransLog")
      val showCachesBut = createBut("Caches")
      val showConsoleBut = createBut("Console")
      val reorgBut = createBut("Reorganize")
      val refreshBut= createBut("Refresh Undo")
      
      contents +=backupBut+=shutDownBut+= showTransBut += showCachesBut += showConsoleBut+= reorgBut+=refreshBut
      listenTo(backupBut,shutDownBut,showTransBut, showCachesBut, showConsoleBut,reorgBut,refreshBut)
      reactions += {
        case ButtonClicked(`backupBut`)   => showBackupDialog()
      	case ButtonClicked(`showTransBut`)   => showTransData()
      	case ButtonClicked(`showCachesBut`)  => StorageManager.printCacheReport
      	case ButtonClicked(`showConsoleBut`) => showConsole()
      	case ButtonClicked(`refreshBut`)=>
          server.comm.CommonSubscriptionHandler.refreshAfterUndo()
          println("refresh done")
        case ButtonClicked(`reorgBut`) => reorg()
      }
    }
    
    val userPanel=new UserPanel
    contents+=manButPanel+=Swing.VStrut(20)+=userPanel
  }

  val classPanel = new BorderPanel() {
    add(new Label("Class-List"), BorderPanel.Position.North)

    add(new ScrollPane() {
      viewportView = classTable
      preferredSize = new Dimension(380, 200)
    }, BorderPanel.Position.Center)

    add(new GridPanel(1, 3) {
      val newClassBut = new Button("Create Class")
      val changeClassBut = new Button("Change Class")
      val showDataBut = new Button("Show Data")

      contents += newClassBut += changeClassBut += showDataBut

      listenTo(newClassBut, changeClassBut, showDataBut)
      reactions += {
        case ButtonClicked(`newClassBut`)    => createClass
        case ButtonClicked(`changeClassBut`) => editClassDef
        case ButtonClicked(`showDataBut`)    => showData
      }
    }, BorderPanel.Position.South)
  }
  
  val basicSettingsPanel=new BasicSettingsPanel
  lazy val theIconImage=Toolkit.getDefaultToolkit().getImage(getClass.getResource("Server128.png"))

  val mainPanel = new BorderPanel() // main panel
  {
    add(accordion,BorderPanel.Position.West)
    accordion.preferredSize=new Dimension(380,10)
    accordion.addPanel("DB Management",managementPanel)
    accordion.addPanel("Class Management",classPanel)
    accordion.addPanel("Basic Settings",basicSettingsPanel) 
    accordion.addPanel("Statistics",statPanel)
    accordion.activatePanel(0)
    accordion.border=BorderFactory.createEtchedBorder()

    add(rightPanel, BorderPanel.Position.Center) // Center	

  } // main Panel

  lazy val top: MainFrame = new MainFrame {
    SessionManager.init(consolePanel)
    SessionManager.registerSetupListener(() => {
      dataInit
    }) 
    
    title = "Datenbank Management"
    contents = mainPanel
    iconImage= theIconImage
    bounds = new Rectangle(10, 200, 1250, 900)
    peer.addWindowListener(new WindowAdapter() {
      override def windowClosing(e: java.awt.event.WindowEvent): Unit = {
        if (hidden) removeTray()

      }
      override def windowIconified(e: java.awt.event.WindowEvent): Unit = {
        //System.out.println("Iconified")
        hideFenster()
      }
      override def windowOpened(e: java.awt.event.WindowEvent): Unit =
        if (firstopen) {
          firstopen = false
          if(showWindowMinimized) hideFenster()
          //hideFenster();
        }
    })

    initTray
    if(!debug) Swing.onEDT{
      LogOutputStream.registerListener(consolePanel)
      showConsole
    }
    //java.lang.System.setOut(LogOutputStream.ps)

  }

  def getSelectedType = {
    val ix: Int = classTable.peer.convertRowIndexToModel(classTable.selection.rows.head)
    dataList(ix)(0).asInstanceOf[java.lang.Integer].intValue
  }

  def showData(): Unit =   {
    //System.out.println("showData" +classTable.selection.rows +" "+classTable.selection.rows.empty)
    if (classTable.selection.rows.nonEmpty) {
      val typ = getSelectedType
      //System.out.println("showData "+typ)
      IndexTableModel.setTypeHandler(StorageManager.ixHandler(typ))
      rightPanel.addIt(DataViewPanel, BorderPanel.Position.Center)
      rightPanel.peer.invalidate()
      rightPanel.peer.revalidate()
      rightPanel.repaint
    }
  }

  def editClassDef(): Unit = {
    if (classTable.selection.rows.nonEmpty) {
      val typ = getSelectedType
      //System.out.println("editClass "+typ+" "+SessionManager.scl)
      TypeDefPanel.setClass(SessionManager.scl.getClassByID(typ), false)
      rightPanel.addIt(TypeDefPanel, BorderPanel.Position.Center)
      mainPanel.peer.invalidate()
      rightPanel.peer.invalidate()
      rightPanel.peer.revalidate()
      rightPanel.repaint
    }
  }

  def createClass(): Unit = {
    newClass = new ServerObjectClass("", 0)
    TypeDefPanel.setClass(newClass, true)
    rightPanel.addIt(TypeDefPanel, BorderPanel.Position.Center)
    mainPanel.peer.invalidate()
    rightPanel.peer.invalidate()
    rightPanel.peer.revalidate()
    rightPanel.repaint
  }

  def showTransData(): Unit = {
    rightPanel.addIt(TransLogPanel, BorderPanel.Position.Center)
    rightPanel.peer.invalidate()
    rightPanel.peer.revalidate()

    rightPanel.repaint
  }

  def showConsole(): Unit = {
    rightPanel.addIt(consolePanel, BorderPanel.Position.Center)
    rightPanel.peer.invalidate()
    rightPanel.peer.revalidate()
    rightPanel.repaint    
  }
  
  def showBackupDialog():Unit = {
    backupDialog.setLocationRelativeTo(managementPanel)
    backupDialog.showDialog()
  }
  

  def registerClassListListener(li: ClassListListener) = {
    classListListener += li
    if (shortClassList.nonEmpty) li.classListChanged(shortClassList)
  }

  def updateSeq[T](seq: Seq[T], index: Int, newValue: T) =
    if (index >= seq.size) seq
    else seq.indices.map(i => if (i == index) newValue else seq(i))


  def initTray(): Unit = {
    if (SystemTray.isSupported()) {
      // load an image
      
      trayIcon = new TrayIcon(theIconImage, "Neue DB")
      trayIcon.setImageAutoSize(true)
      //System.out.println("tray "+trayIcon)
      trayIcon.addMouseListener(new MouseAdapter() {
        override def mouseClicked(e1: java.awt.event.MouseEvent): Unit = {
          //System.out.println("tray click")					
          if (hidden) showFenster()
        }
      })
      // add the tray image                  
    }
  }

  def addTray() = {
    val tray = SystemTray.getSystemTray()
    try {
      tray.add(trayIcon)
    } catch {
      case NonFatal(e) =>
        Log.e("add tray "+tray,e)
    }
  }

  def hideFenster() = {
    //System.out.println("hide")
    top.visible = false
    addTray()
    hidden = true
  }

  def showFenster() = {
    top.visible = true
    top.peer.setExtendedState(java.awt.Frame.NORMAL)
    removeTray()
    hidden = false
  }

  def removeTray() = {
    SystemTray.getSystemTray().remove(trayIcon)
  }
  
  def reorg() = {
    rightPanel.addIt(reorgPanel, BorderPanel.Position.Center)
    rightPanel.peer.invalidate()
    rightPanel.peer.revalidate()
    rightPanel.repaint
    
  }
  
  def runSw (func: =>Unit) = 
		Swing.onEDT{ func }
	
	def runAsThread( a: => Unit) = {
		val tr= new Thread {override def run() = a}
		tr.start()
	}

}