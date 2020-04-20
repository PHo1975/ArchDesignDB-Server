/** Author: Peter Started:26.07.2010
  */
package management.databrowser

import java.awt.event.{MouseAdapter, WindowAdapter}
import java.awt.{Image, SystemTray, Toolkit, TrayIcon}

import client.ui.ViewConstants
import com.sun.jna.{Native, NativeLong, WString}
import definition.typ.AllClasses
import javax.swing.table.{DefaultTableModel, TableRowSorter}
import javax.swing.{BorderFactory, JTable, RowSorter, SortOrder}
import management.databrowser.stat.StatPanel
import server.comm.MainServerSocket
import server.storage.{ServerObjectClass, StorageManager, TransLogHandler}
import transaction.handling.SessionManager
import util.Log

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, BoxPanel, Button, Component, Dimension, GridPanel, MainFrame, Orientation, Rectangle, ScrollPane, SimpleSwingApplication, Swing, Table}
import scala.util.control.NonFatal

//import java.swing.JTable

/**
  */

trait ClassListListener {
  def classListChanged(classList: Seq[(Int, String)]): Unit
}

class SimpleTable extends Table {
  override lazy val peer: JTable = new JTable with SuperMixin
  peer.setAutoResizeMode(Table.AutoResizeMode.LastColumn.id)
  peer.setSelectionMode(Table.IntervalMode.Single.id)
}

object MainWindow extends SimpleSwingApplication {
  println("MainWindow start")
  var trayIcon: TrayIcon = _
  var hidden: Boolean = false
  var firstopen = true
  var theModel: DefaultTableModel = _
  var dataList: Array[Array[java.lang.Object]] = Array()
  var shortClassList: Seq[(Int, String)] = Seq.empty
  var shortBlockClassList: Seq[(Int, String)] = Seq.empty
  val usedIDs: mutable.HashSet[Int] = collection.mutable.HashSet[Int]()
  val classListListeners: mutable.HashSet[ClassListListener] = collection.mutable.HashSet[ClassListListener]()
  val blockClassListListeners: mutable.HashSet[ClassListListener] = collection.mutable.HashSet[ClassListListener]()
  var newClass: ServerObjectClass = _
  var showWindowMinimized=false
  var webSocketSSL = false
  var testWS= false

  lazy val reorgPanel = new ReorgPanel
  val manButSize=new Dimension(240,35)
  val buttonSize=new Dimension(Short.MaxValue,30)
  lazy val backupDialog=new BackupDialog(top)
  var debug=false
  lazy val consolePanel = new ConsolePanel(debug)
  lazy val borderPanel = new BorderPanel
  lazy val certPanel = new CertPanel
  lazy val blockPanel= new BlockClassPanel

  val accordion=new AccordionComponent
  val statPanel=new StatPanel
  @native def SetCurrentProcessExplicitAppUserModelID(appID:WString):NativeLong
  if(System.getProperty("os.name").contains("Windows")) {
    val versID=System.getProperty("os.version").toFloat
    if (versID >= 6.1f) {
      val appID="DBServer"
      Native.register("shell32")
      if (SetCurrentProcessExplicitAppUserModelID(new WString(appID)).longValue() != 0)
        throw new RuntimeException("unable to set current process explicit AppUserModelID to: " + appID)
    } else Log.w("Windows version " + versID)
  }


  val classTable: SimpleTable = new SimpleTable


  def dataInit(): Unit = {
   println("Datainit")
    generateDataList()
    theModel = new DefaultTableModel(dataList, Array[Object]("ID", "Name", "Description")) {
      override def getColumnClass(col: Int): Class[_] = {
        col match {
          case 0 => classOf[java.lang.Integer]
          case _ => classOf[String]
        }
      }
    }
    val sorter = new TableRowSorter[DefaultTableModel](theModel)
    sorter.setSortKeys(List(new RowSorter.SortKey(0, SortOrder.ASCENDING)).asJava)
    classTable.peer.setModel(theModel)
    classTable.peer.setRowSorter(sorter)
    val col = classTable.peer.getColumnModel.getColumn(0)
    col.setMaxWidth(40)
    col.setResizable(false)
    usedIDs.clear
    usedIDs ++= dataList.map(_(0).asInstanceOf[java.lang.Integer].intValue)
    blockPanel.dataInit()
    //println("Datainit done ")

  }

  def generateDataList(): Unit = {
    def classListIterator = AllClasses.get.getClassList.valuesIterator
    dataList = classListIterator.map(
      x => Array[Object](Integer.valueOf(x.id), x.name, x.description)).toSeq.
      sortWith(_(0).asInstanceOf[java.lang.Integer].intValue < _(0).asInstanceOf[java.lang.Integer].intValue).toArray
    shortClassList = classListIterator.map(x => (x.id, x.name)).toSeq.sortWith(_._2 < _._2)
    classListListeners.foreach(_.classListChanged(shortClassList))
  }

  def updateBlockClassListeners(newList:Seq[(Int,String)]):Unit={
    shortBlockClassList=newList
    for (li<-blockClassListListeners) li.classListChanged(shortBlockClassList)
  }



  class MyBorderPanel extends BorderPanel {
    def addIt(c: Component, l: Constraints): Unit = {
      super.add(c, l)
    }
  }

  val rightPanel = new MyBorderPanel


  def addRightPanelComponent(c: Component): Unit = rightPanel.addIt(c, BorderPanel.Position.Center)

  override def startup(args: Array[String]): Unit = {
    Log.w("startup "+args.toSeq.mkString(","))
    if(args.length>0){
      if(args(0).equalsIgnoreCase("debug")) debug=true
      if(args(0).equalsIgnoreCase("noBulk")) MainServerSocket.noBulkAction=true
      if (args.exists(_.equalsIgnoreCase("minimized")))showWindowMinimized=true
      if (args.exists(_.equalsIgnoreCase("ssl"))) webSocketSSL = true
      if (args.exists(_.equalsIgnoreCase("testws"))) testWS = true
    }
    super.startup(args)
  }

  def createBut(name: String): Button = {
    val newBut=new Button(name)
    newBut.maximumSize=manButSize
    newBut.preferredSize=manButSize
    newBut.xLayoutAlignment=0.5f
    newBut
  }

  val managementPanel:BoxPanel = new BoxPanel(Orientation.Vertical) {
    val manButPanel: BoxPanel =new BoxPanel(Orientation.Vertical) {
      this.xLayoutAlignment=0.5f

      val backupBut: Button = createBut("Backup")
      val shutDownBut: Button = createBut("ShutDown")
      val showTransBut: Button = createBut("TransLog")
      val showConsoleBut: Button = createBut("Console")
      val reorgBut: Button = createBut("Reorganize")
      val refreshBut: Button = createBut("Refresh Undo")
      val setTransLogBut:Button= createBut("Set TransLog to End")
      val createCertBut: Button = createBut("Create Certificate")

      contents += backupBut += shutDownBut += showTransBut += showConsoleBut += reorgBut += refreshBut+=setTransLogBut += createCertBut
      listenTo(backupBut, shutDownBut, showTransBut, showConsoleBut, reorgBut, refreshBut, createCertBut,setTransLogBut)
      reactions += {
        case ButtonClicked(`backupBut`)   => showBackupDialog()
        case ButtonClicked(`showTransBut`)   => showTransData()
        case ButtonClicked(`showConsoleBut`) => showConsole()
        case ButtonClicked(`refreshBut`)=>
          server.comm.CommonSubscriptionHandler.refreshAfterUndo()
          Log.w("refresh done")
        case ButtonClicked(`setTransLogBut` )=> setTransLogToEnd()
        case ButtonClicked(`reorgBut`) => reorg()
        case ButtonClicked(`createCertBut`) => createCert()
      }
    }
    contents += manButPanel
  }

  val classPanel: BorderPanel = new BorderPanel() {
    add(ViewConstants.label("Class-List"), BorderPanel.Position.North)
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
        case ButtonClicked(`newClassBut`) => createClass()
        case ButtonClicked(`changeClassBut`) => editClassDef()
        case ButtonClicked(`showDataBut`) => showData()
      }
    }, BorderPanel.Position.South)
  }

  val basicSettingsPanel = new BasicSettingsPanel()
  val userPanel = new UserPanel()
  lazy val theIconImage: Image = Toolkit.getDefaultToolkit.getImage(getClass.getResource("Server128.png"))

  val mainPanel: BorderPanel = new BorderPanel() { // main panel
    add(accordion,BorderPanel.Position.West)
    accordion.preferredSize=new Dimension(380,10)
    accordion.addPanel("DB Management",managementPanel)
    accordion.addPanel("User Management", userPanel)
    accordion.addPanel("Class Management",classPanel)
    accordion.addPanel("Block Class Management",blockPanel)
    accordion.addPanel("Setup", basicSettingsPanel)
    accordion.addPanel("Statistics",statPanel)
    accordion.activatePanel(0)
    accordion.border=BorderFactory.createEtchedBorder()
    add(rightPanel, BorderPanel.Position.Center) // Center
  } // main Panel

  lazy val top: MainFrame = new MainFrame {
    try {
      SessionManager.init(consolePanel)
      SessionManager.registerSetupListener(() => {
        dataInit()
      })

      title = "Datenbank Management"
      contents = mainPanel
      iconImage = theIconImage
      bounds = new Rectangle(10, 200, 1250, 900)

      peer.addWindowListener(new WindowAdapter() {
        override def windowClosing(e: java.awt.event.WindowEvent): Unit = if (hidden) removeTray()
        override def windowIconified(e: java.awt.event.WindowEvent): Unit =            hideFenster()
        override def windowOpened(e: java.awt.event.WindowEvent): Unit =
          if (firstopen) {
            firstopen = false
            if (showWindowMinimized) hideFenster()
          }
      })

      initTray()
      showConsole()
    } catch { case NonFatal(e)=> println("top init:"+e);println(e.getStackTrace.mkString("\n  "))}

  }

  def getSelectedType: Int = {
    val ix: Int = classTable.peer.convertRowIndexToModel(classTable.selection.rows.head)
    dataList(ix)(0).asInstanceOf[java.lang.Integer].intValue
  }

  def showData(): Unit =
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


  def editClassDef(): Unit = {
    if (classTable.selection.rows.nonEmpty) {
      val typ = getSelectedType
      //System.out.println("editClass "+typ+" "+SessionManager.scl)
      TypeDefPanel.setClass(SessionManager.scl.getClassByID(typ), create = false)
      rightPanel.addIt(TypeDefPanel, BorderPanel.Position.Center)
      mainPanel.peer.invalidate()
      rightPanel.peer.invalidate()
      rightPanel.peer.revalidate()
      rightPanel.repaint
    }
  }

  def createClass(): Unit = {
    newClass = new ServerObjectClass("", 0)
    TypeDefPanel.setClass(newClass, create = true)
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


  def registerClassListListener(li: ClassListListener): Unit = {
    classListListeners += li
    if (shortClassList.nonEmpty) li.classListChanged(shortClassList)
  }

  def registerBlockClassListListener(li: ClassListListener): Unit = {
    blockClassListListeners += li
    if (shortBlockClassList.nonEmpty) li.classListChanged(shortBlockClassList)
  }

  /*def updateSeq[T](seq: Seq[T], index: Int, newValue: T): Seq[T] =
    if (index >= seq.size) seq
    else seq.indices.map(i => if (i == index) newValue else seq(i))*/


  def initTray(): Unit = {
    //println("init tray")
    if (SystemTray.isSupported) {
      trayIcon = new TrayIcon(theIconImage, "Neue DB")
      trayIcon.setImageAutoSize(true)
      //System.out.println("tray "+trayIcon)
      trayIcon.addMouseListener(new MouseAdapter() {
        override def mouseClicked(e1: java.awt.event.MouseEvent): Unit =  if (hidden) showFenster()
      })
      // add the tray image                  
    }
  }

  def addTray(): Unit = {
    val tray = SystemTray.getSystemTray
    try {
      tray.add(trayIcon)
    } catch {
      case NonFatal(e) =>
        Log.e("add tray "+tray,e)
    }
  }

  def hideFenster(): Unit = {
    top.visible = false
    addTray()
    hidden = true
  }

  def showFenster(): Unit = {
    top.visible = true
    top.peer.setExtendedState(java.awt.Frame.NORMAL)
    removeTray()
    hidden = false
  }

  def removeTray(): Unit = {
    SystemTray.getSystemTray.remove(trayIcon)
  }

  def reorg(): Unit = {
    rightPanel.addIt(reorgPanel, BorderPanel.Position.Center)
    rightPanel.peer.invalidate()
    rightPanel.peer.revalidate()
    rightPanel.repaint()
  }

  def setTransLogToEnd():Unit= {
    TransLogHandler.setInsertPosToEnd()
  }

  def createCert(): Unit = Swing.onEDT {
    rightPanel.addIt(certPanel, BorderPanel.Position.Center)
    certPanel.peer.invalidate()
    rightPanel.peer.invalidate()
    rightPanel.peer.revalidate()
    rightPanel.repaint()
  }

  def runSw(func: => Unit): Unit =
    Swing.onEDT{ func }

  def runAsThread(a: => Unit): Unit = {
    val tr = new Thread {override def run(): Unit = a}
    tr.start()
  }

}