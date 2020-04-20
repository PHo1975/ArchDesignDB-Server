/**
 * Author: Peter Started:05.09.2010
 */
package client.ui

import java.awt.{Color, Font}
import java.net._

import client.comm._
import client.dataviewer._
import client.dialog.DialogManager.reset
import client.dialog._
import client.graphicsView._
import client.layout._
import client.model._
import client.search.SearchViewbox
import com.sun.jna.{Native, NativeLong, WString}
import definition.comm.{PropertyGroup, PropertyValue}
import definition.typ.{AllClasses, DataType}
import javafx.application.Platform
import javax.swing.{Scrollable, _}

import scala.swing._
import scala.swing.event._
import scala.util.control.NonFatal

/** tests the instance dataviewer
 * 
 */

object ClientApp extends App {
  //val start=System.currentTimeMillis()


  var sock: ClientSocket = _

  def createHLine: Component = {
    val ret = Swing.VStrut(1)
    ret.border = BorderFactory.createLineBorder(Color.GRAY)
    ret
  }

  lazy val undoLabel = new Label()
  undoLabel.font = new Font("Arial", 0, 25)
  lazy val undoPanel = new BorderPanel {
    add(undoLabel, BorderPanel.Position.Center)
  }

  val fieldEditPan = new FieldEditorsPanel
  val undoBut = new IconableButton("Undo", "MainWindow", "letzte Aktion(en) rückgängig machen")
  val infoBut = new Button("I")
  undoBut.text = "Rückgängig..."
  val hideBut = new ToggleButton("Hide")
  val strokeEditBut = new Button("Ta")
  val fontEditBut = new Button("F")
  val consoleBut = new Button("Co")
  val normBack = strokeEditBut.background
  KeyStrokeManager.bindigsEnabledListener = Some((b: Boolean) => strokeEditBut.background = if (b) normBack else Color.green)
  hideBut.focusable = false
  hideBut.tooltip = "Versteckte Daten anzeigen"
  strokeEditBut.focusable = false
  strokeEditBut.tooltip = "Tastenkürzel zuweisen"
  fontEditBut.focusable = false
  fontEditBut.tooltip = "Schriftgrößen einstellen"
  consoleBut.focusable = false
  consoleBut.tooltip = "Fehler-Konsole"
  undoBut.focusable = false
  val workplaceBut = new IconableButton("Workplaces", "MainWindow", "Fensteranordnung speichern / laden")
  workplaceBut.text = "Arbeitsflächen ..."

  lazy val undoDialog = new UndoDialog(top)
  lazy val workplaceDialog = new WorkplaceDialog(top)
  val mainBox = new MainBox
  val tableViewboxType = ViewboxContentType(1, "table", "T", "Neues Tabellenfenster", () => new TableViewbox)
  val graphicsViewboxType = ViewboxContentType(2, "graphics", "G", "Neues Grafikfenster", () => new GraphicsViewbox)
  val searchViewboxType = ViewboxContentType(3, "search", "S", "Neues Suchfenster", () => new SearchViewbox)
  ViewboxContentTypeList.addType(tableViewboxType)
  ViewboxContentTypeList.addType(graphicsViewboxType)
  ViewboxContentTypeList.addType(searchViewboxType)

  lazy val strokeDialog = new StrokeEditDialog(top)
  lazy val fontDialog = new ViewConstantsDialog(top)

  lazy val middleBox = new Panel with SequentialContainer.Wrapper {
    override lazy val peer: JPanel with SuperMixin with Scrollable = {
      val p = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {
        def getScrollableTracksViewportWidth() = true

        def getScrollableTracksViewportHeight() = false

        def getPreferredScrollableViewportSize: Dimension = {
          val s = getPreferredSize
          if (s != null && s.width > ViewConstants.sidePanelWidth + 20 * ViewConstants.fontScale / 100) {
            new Dimension(ViewConstants.sidePanelWidth + 20 * ViewConstants.fontScale / 100, s.height)
          }
          else s
        }

        def getScrollableBlockIncrement(rect: Rectangle, o: Int, d: Int): Int = if (o == Orientation.Vertical.id) rect.height else rect.width

        def getScrollableUnitIncrement(rect: Rectangle, o: Int, d: Int) = 30
      }
      val l = new javax.swing.BoxLayout(p, Orientation.Vertical.id)
      p.setLayout(l)
      p
    }
    contents += createHLine += fieldEditPan += createHLine += DialogManager.dialogPanel += ActionPanel //+=createHLine+=Swing.VStrut(20)
    opaque = true
    background = ViewConstants.leftPanelColor
  }

  private lazy val middleScroller = new ScrollPane {
    maximumSize = new Dimension(ViewConstants.sidePanelWidth + 20 * ViewConstants.fontScale / 100, Short.MaxValue)
    viewportView = middleBox
    peer.getViewport.setOpaque(true)
    peer.getViewport.setBackground(ViewConstants.leftPanelColor)

    peer.getViewport.setBorder(null)
    peer.setViewportBorder(null)
    peer.setBorder(null)
  }

  lazy val mainPanel = new BorderPanel() {
    add(mainBox, BorderPanel.Position.Center)
    add(new BoxPanel(scala.swing.Orientation.Vertical) {
      border = BorderFactory.createEmptyBorder(5, 5, 5, 10)
      workplaceBut.xLayoutAlignment = 0.5d
      undoBut.xLayoutAlignment = 0.5d
      opaque = true
      background = ViewConstants.leftPanelColor
      contents += Swing.VStrut(10) += DialogManager.selectLabScroller +=
        middleScroller += DialogManager.errorScroller += undoBut += Swing.VStrut(10) += workplaceBut += Swing.VStrut(15) += new BoxPanel(Orientation.Horizontal) {
        contents += hideBut += strokeEditBut += fontEditBut += consoleBut += infoBut
      }
    }, BorderPanel.Position.West)
    listenTo(undoBut, hideBut, strokeEditBut, fontEditBut, workplaceBut, consoleBut, infoBut)
    reactions += {
      case ButtonClicked(`undoBut`) => requestUndoData()
      case ButtonClicked(`hideBut`) => setHide(hideBut.selected)
      case ButtonClicked(`strokeEditBut`) => showStrokeDialog()
      case ButtonClicked(`workplaceBut`) => showWorkplaceDialog()
      case ButtonClicked(`fontEditBut`) => showFontDialog()
      case ButtonClicked(`consoleBut`) => showConsole()
      case ButtonClicked(`infoBut`) => showInfo()
    }
  }

  def updateSidePanel(): Unit = {
    middleBox.peer.invalidate()
    middleBox.revalidate()
  }


  class TopMainFrame extends MainFrame {
    val logWindow = new LogWindow(this)
    var isDecorating = false

    override def closeOperation(): Unit = {
      //util.Log.e("Close " )
      if (!isDecorating) shutDown()
    }

    title = "Datenbank"
    client.icons.IconManager.getIcon("MainWindow", "Icon") match {
      case Some(icon) => iconImage = icon.getImage()
      case _ =>
    }
    contents = mainPanel
    KeyStrokeManager.topComponent = Some(mainPanel)

    def undecorate(): Unit = if (!peer.isUndecorated) try {
      isDecorating = true
      peer.dispose()
      peer.setUndecorated(true)
      peer.pack()
      isDecorating = false
      peer.setVisible(true)
    }
    catch {
      case NonFatal(e) => println("Fehler: " + e)
    }

    def decorate(): Unit = if (peer.isUndecorated) {
      isDecorating = true
      peer.dispose()
      peer.setUndecorated(false)
      peer.pack()
      isDecorating = false
      peer.setVisible(true)
    }
  }

  lazy val top: TopMainFrame = new TopMainFrame

  def showInfo(): Unit =
    for (gr <- DialogManager.selectedInstances.headOption)
      ClientQueryManager.printErrorMessage((for (inst <- gr.children) yield inst.ref.sToString).mkString(", "))


  def saveWindowPositions(): Unit = {
    val defaultGroup = PropertyGroup("Default", new collection.mutable.HashMap[String, PropertyValue]())
    mainBox.storeSettings(defaultGroup)
    val oldList = UserSettings.getListProperty[PropertyGroup]("WindowSettings", "Boxes")
    val newList = oldList map (gr => if (gr.name == "Default") defaultGroup else gr)
    val storeList = if (newList.isEmpty) List(defaultGroup) else newList
    val bounds = top.bounds
    UserSettings.setIntProperty("Fonts", "RowHeight", ViewConstants.defaultRowHeight)
    UserSettings.setIntProperty("Fonts", "TableFont", ViewConstants.tableFont.getSize)
    UserSettings.setIntProperty("Fonts", "LabelFont", ViewConstants.labelFont.getSize)
    UserSettings.setIntProperty("Fonts", "SmallFont", ViewConstants.smallFont.getSize)
    UserSettings.setIntProperty("Fonts", "TinyFont", ViewConstants.tinyFont.getSize)
    UserSettings.setIntProperty("Fonts", "TableTypeFont", ViewConstants.tableTypeFont.getSize)
    UserSettings.setIntProperty("Fonts", "QuestionFont", ViewConstants.questionFont.getSize)
    UserSettings.setIntProperty("Fonts", "DefaultFont", ViewConstants.defFont.getSize)
    UserSettings.setIntProperty("Fonts", "FontScale", ViewConstants.fontScale)
    UserSettings.setIntProperty("Draw", "LineCatch", ViewConstants.lineCatchDistance)
    UserSettings.setIntProperty("Draw", "PointCatch", ViewConstants.pointCatchDistance)
    UserSettings.setIntProperty("Draw", "PolyLineTo", ViewConstants.polyLineTo)
    UserSettings.setIntProperty("Draw", "DragTreshold", ViewConstants.dragTreshold)
    UserSettings.setIntProperty("Draw", "antialias",ViewConstants.antialias)
    UserSettings.setIntProperty("Draw", "showToast",ViewConstants.showToast)
    UserSettings.setIntProperty("Draw", "showHitPoints",ViewConstants.showToast)
    UserSettings.setIntProperty("Draw","selectBorderWidth",ViewConstants.selectBorderWidth)
    UserSettings.setIntProperty("Draw","backgroundLayerTrans",ViewConstants.backgroundLayerTrans)
    //UserSettings.setIntProperty("Draw","hatchLineWidth",ViewConstants.hatchLineWidth)
      UserSettings.setStringProperty("WindowSettings", "imagePath",ViewConstants.imagePath)
    //UserSettings.setIntProperty("Draw", "stopFX", ViewConstants.stopFX)

    UserSettings.setListProperty[PropertyGroup]("WindowSettings", "Boxes", storeList)
    UserSettings.setIntProperty("WindowSettings", "Width", bounds.width)
    UserSettings.setIntProperty("WindowSettings", "Height", bounds.height)
    UserSettings.setIntProperty("WindowSettings", "XPos", bounds.x)
    UserSettings.setIntProperty("WindowSettings", "YPos", bounds.y)
    UserSettings.setIntProperty("WindowSettings", "Maximized", if (top.peer.getExtendedState == java.awt.Frame.MAXIMIZED_BOTH) 1 else 0)
  }

  @native def SetCurrentProcessExplicitAppUserModelID(appID: WString): NativeLong

  startup(args)

  def startup(args: Array[String]): Unit = try{
    Platform.setImplicitExit(false)
    if(args.length<3 ) { System.out.println("Usage: ViewTest host portnr name password [hide=1]"); shutDown() }
    val serverName = args(0)

    if (System.getProperty("os.name").contains("Windows")) {
      val appID = "DBClient"
      Native.register("shell32")
      if (SetCurrentProcessExplicitAppUserModelID(new WString(appID)).longValue() != 0)
        throw new RuntimeException("unable to set current process explicit AppUserModelID to: " + appID)
    } else util.Log.e("no windows")

    try {
      sock = new ClientSocket(InetAddress.getByName(serverName), args(1).toInt, args(2), args(3), "Table")
    } catch {
      case c: ConnectException =>
        JOptionPane.showMessageDialog(null, "Kann Server an Adresse " + serverName + " nicht finden.", "Datenbank", JOptionPane.ERROR_MESSAGE)
        System.exit(0)
    }

    sock.connectionBrokenListener=()=>{top.close()}

    ClientQueryManager.undoLockListener = Some((isLocked, lockUser) => {
      if (isLocked) {
        undoLabel.text = "Benutzer " + lockUser + " will rückgängig machen"
        //println("Lock orig")
        undoPanel.preferredSize = mainPanel.size
        top.contents = undoPanel
        top.repaint()
      }
      else {
        //println("Released orig")
        mainPanel.preferredSize = undoPanel.preferredSize
        top.contents = mainPanel
        top.repaint()
      }
    })

    sock.classesReadListener = () => Swing.onEDT {
      //println("Classes Read ")
      ClientQueryManager.setClientSocket(sock)

      ViewConstants.defaultRowHeight = UserSettings.getIntProperty("Fonts", "RowHeight", 25)
      ViewConstants.tableFont = new Font("Arial", 0, UserSettings.getIntProperty("Fonts", "TableFont", 14))
      ViewConstants.labelFont = new Font("Arial", 0, UserSettings.getIntProperty("Fonts", "LabelFont", 14))
      ViewConstants.smallFont = new Font("Arial", 0, UserSettings.getIntProperty("Fonts", "SmallFont", 12))
      ViewConstants.tinyFont = new Font("Arial", 0, UserSettings.getIntProperty("Fonts", "TinyFont", 10))
      ViewConstants.tableTypeFont = new Font("Arial", Font.ITALIC, UserSettings.getIntProperty("Fonts", "TableTypeFont", 11))
      ViewConstants.questionFont = new Font("Arial", 0, UserSettings.getIntProperty("Fonts", "QuestionFont", 15))
      ViewConstants.defFont = new Font("Arial", 0, UserSettings.getIntProperty("Fonts", "DefaultFont", 16))
      ViewConstants.lineCatchDistance = UserSettings.getIntProperty("Draw", "LineCatch", 6)
      ViewConstants.pointCatchDistance = UserSettings.getIntProperty("Draw", "PointCatch", 4)
      ViewConstants.polyLineTo = UserSettings.getIntProperty("Draw", "PolyLineTo", 1)
      ViewConstants.dragTreshold = UserSettings.getIntProperty("Draw", "DragTreshold", 8)
      ViewConstants.antialias= UserSettings.getIntProperty("Draw", "antialias", 1)
      ViewConstants.showToast= UserSettings.getIntProperty("Draw", "showToast", 1)
      ViewConstants.showHitPoints= UserSettings.getIntProperty("Draw", "showHitPoints", 1)
      ViewConstants.imagePath= UserSettings.getStringProperty("WindowSettings", "imagePath", "")
      ViewConstants.selectBorderWidth=UserSettings.getIntProperty("Draw", "selectBorderWidth", 1)
      ViewConstants.backgroundLayerTrans=UserSettings.getIntProperty("Draw","backgroundLayerTrans",40)
//      ViewConstants.hatchLineWidth=UserSettings.getIntProperty("Draw", "hatchLineWidth", 3)
      import javax.swing.{UIDefaults, UIManager}
      UIManager.setLookAndFeel(new javax.swing.plaf.nimbus.NimbusLookAndFeel() {
        override def getDefaults: UIDefaults = {
          val defaults = super.getDefaults
          defaults.put("defaultFont", ViewConstants.defFont)
          defaults.put("Button.font", ViewConstants.defFont)
          defaults.put("Label.font", ViewConstants.defFont)
          defaults.put("TextField.font", ViewConstants.tableFont)
          defaults.put("TextArea.font", ViewConstants.tableFont)
          defaults
        }
      })
      //val defaults = UIManager.getLookAndFeelDefaults()

      ViewConstants.fontScale = UserSettings.getIntProperty("Fonts", "FontScale", 100)
      //ViewConstants.stopFX=UserSettings.getIntProperty("Draw", "stopFX",1)
      top.title = "Datenbank [" + args(2) + "]"
      import javax.swing.SwingUtilities
      SwingUtilities.updateComponentTreeUI(top.peer)
      AllClasses.get.resolveFields()

      workplaceBut.font = ViewConstants.defFont
      undoBut.font = ViewConstants.defFont

      ClientQueryManager.registerSetupListener(() => {
        //println("clientapp setup")
        val windowWidth = UserSettings.getIntProperty("WindowSettings", "Width")
        val windowHeight = UserSettings.getIntProperty("WindowSettings", "Height")
        val windowX = UserSettings.getIntProperty("WindowSettings", "XPos")
        val windowY = UserSettings.getIntProperty("WindowSettings", "YPos")
        if (UserSettings.getIntProperty("WindowSettings", "Maximized") == 1) {
          top.bounds = new java.awt.Rectangle(10, 10, 800, 600)
          top.peer.setExtendedState(java.awt.Frame.MAXIMIZED_BOTH)
        }
        else if (windowWidth > 0 && windowHeight > 0)
          top.bounds = new java.awt.Rectangle(windowX, windowY, windowWidth, windowHeight)
        top.visible = true
        //println("Client app setup done")
      })

      SelectEventDispatcher.registerSelectListener(ActionPanel)
      SelectEventDispatcher.registerSelectListener(DialogManager)
      SelectEventDispatcher.registerSelectListener(fieldEditPan)

      DialogManager.answerArea.registerCustomPanel[PointAnswerPanel](DataType.VectorTyp)
      DialogManager.answerArea.registerCustomPanel[ReferenceAnswerPanel](DataType.ObjectRefTyp)
      DialogManager.answerArea.registerCustomPanel[BlobAnswerPanel](DataType.BlobTyp)
      ClientQueryManager.registerStepListReader(undoDialog)

      if (args.length > 4) {
        val sel: Boolean = args(4).toInt > 0
        if (sel) hideBut.selected = true
        setHide(sel)
      } else {hideBut.selected = true; setHide(true)}
      //super.startup(args)
      top.visible = true
      KeyStrokeManager.registerReceiver(new KeyStrokeReceiver {
        override def commandName: String = "Cancel"
        override def setStroke(stroke: KeyStroke): Unit = {}
        override def groupName: String = "Dialog"
        override def strokeHit(): Unit = reset()
      })
    }

    sock.addStartupFinishListener(() => Swing.onEDT {
      //println("startup finished")
      val storeList = UserSettings.getListProperty[PropertyGroup]("WindowSettings", "Boxes")
      //println("storeList loaded "+ storeList.size)
      storeList.find(_.name == "Default") match {
        case Some(group) => mainBox.restoreSettings(Some(group), loadDone _)
        case None => mainBox.restoreSettings(None, loadDone _)
      }
      //println("restore done")
    })
    sock.start()
    //println("Sock gestartet")
  } catch {
    case NonFatal(e) => println("Fataler Fehler "+e)
  }


  def loadDone():Unit = {
    //System.out.println("Init Loading Done ")
  }

  def shutDown(): Unit = {
    //util.Log.e("shutdown")
    top.visible = false

    if(sock!=null) {
      saveWindowPositions()
      ClientQueryManager.setClientSocket(sock)
      sock.quitApplication()
    }
  }

  def showStrokeDialog(): Unit = {
    val pos = strokeEditBut.peer.getLocationOnScreen()
    strokeDialog.bounds = new java.awt.Rectangle(pos.x, pos.y - 500, 560, 500)
    strokeDialog.showDialog()
  }

  def showFontDialog(): Unit = {
    val pos = strokeEditBut.peer.getLocationOnScreen()
    fontDialog.bounds = new java.awt.Rectangle(pos.x, pos.y - 700, 760, 700)
    fontDialog.visible = true
    UIManager.getLookAndFeelDefaults().put("defaultFont", ViewConstants.defFont)
  }

  def requestUndoData(): Unit = {
    undoDialog.setLocationRelativeTo(DialogManager.errorScroller)
    val pos = undoBut.peer.getLocationOnScreen()
    undoDialog.bounds = new java.awt.Rectangle(pos.x, pos.y - 310, 600, 300)
    undoDialog.prepare()
    ClientQueryManager.runInPool {ClientQueryManager.requestUndoData()}

  }

  def setHide(value: Boolean): Unit = {
    if (value) {
      hideBut.text = "Show"
    }
    else hideBut.text = "Hide"
    DataViewController.hideProperties = value
  }

  def showWorkplaceDialog(): Unit = {
    val pos = workplaceBut.peer.getLocationOnScreen()
    workplaceDialog.bounds=new java.awt.Rectangle(pos.x,pos.y-400,400,400)
    workplaceDialog.showWorkplaces(openWorkplace)
  }

  def openWorkplace(pg: PropertyGroup): Unit = Swing.onEDT {
    mainBox.shutDown()
    mainBox.restoreSettings(Some(pg), () => {
      //System.out.println("Loading "+pg.name+" done ")
      mainBox.revalidate()
      mainBox.repaint()
    })
  }

  def showConsole(): Unit = ClientQueryManager.runInPool {
    top.logWindow.visible = true
  }

}