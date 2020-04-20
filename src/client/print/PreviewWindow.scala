/**
  * Author: Peter Started:22.04.2011
  */
package client.print

import java.awt.Toolkit
import java.awt.event.{ActionEvent, KeyEvent, MouseAdapter, MouseWheelEvent}

import client.dialog.DialogManager
import client.ui.{ClientApp, ViewConstants}
import definition.data.{FontStyleList, Reference, RenderContext}
import javax.print.attribute.standard.PageRanges
import javax.swing.{AbstractAction, BorderFactory, JComponent, KeyStroke}

import scala.swing.event._
import scala.swing.{ListView, _}
import scala.util.control.NonFatal

trait PrintReceiver {
  def printOut(pageable:APageable,copies:Int,pages:PageRanges):Unit
  def printerName:String
  def mediaName:String
}

/**
  *
  */
class PreviewWindow(w:Window/*,preDialog:NewOutdefDialog*/)  extends Dialog(w){
  title="Druckvorschau"

  var thePageable:APageable=_

  var archiveListModel=new ArchiveListModel()
  preferredSize=new Dimension(1000,800)
  maximize()
  var dotPitch:Double=0.25

  var currentPage:Int=_
  var currentPrintReceiver:Option[PrintReceiver]=None

  val closeBut=new Button("Abbruch")
  val pageHeightBut=new Button("S-Höhe")
  val pageWidthBut=new Button("S-Breite")
  val twoPagesBut=new Button("2 Seiten")
  val zoomInBut=new Button("+")
  val zoomOutBut=new Button("-")
  val zoomEdit=new TextField("100%")
  zoomEdit.maximumSize=new Dimension(70,30)
  val formSettingsBut=new Button("Formular...")
  val pageSettingsBut=new Button("Papier...")
  val printBut=new Button("Drucken...")
  //val dateBut=new Button("Datum...")

  val firstPageBut=new Button("<<")
  val prevPageBut=new Button("<")
  val nextPageBut=new Button(">")
  val lastPageBut=new Button(">>")
  val pageEdit=new TextField()
  pageEdit.preferredSize=new Dimension(50,40)
  val numPageLab=new Label("/ 0")
  numPageLab.font = ViewConstants.labelFont
  pageEdit.maximumSize=new Dimension(50,30)

  val previewContext=new  AbstractContext {
    var masterContext:RenderContext= MyContext
    var scale:Float=1
    var changedFontStyleList:FontStyleList= _

    def setMasterContext(nm: RenderContext): Unit = masterContext = nm
    def getScale:Double=scale

    def setScale(ns: Double): Unit = {
      scale =ns.toFloat
      val fontScale=(scale*25.4f/72.0f/dotPitch).toFloat
      changedFontStyleList=new FontStyleList(masterContext.fontStyleList.list .map(a=> a.changeSize(fontScale) ))
    }

    def fontStyleList: FontStyleList = changedFontStyleList
    override def toUnit(mm:Double):Float=(mm*scale/dotPitch).toFloat
  }



  var pageViewer:PageViewer=new PageViewer(previewContext)


  val pageScroller:ScrollPane=new ScrollPane (){
    viewportView=pageViewer
    //peer.setWheelScrollingEnabled(false)
    peer.addMouseWheelListener(new MouseAdapter(){
      override def mouseWheelMoved(e:MouseWheelEvent):Unit = {
        //println("Wheel:"+e)
        if(peer.getVerticalScrollBar.isVisible||peer.getHorizontalScrollBar.isVisible)return
        if(e.getWheelRotation>0) incPage()
        else decPage()
      }
    })
  }


  class GroupPanel(labelText:String,elements:Component*) extends BorderPanel {
    val topLab=new Label(labelText)
    topLab.font = ViewConstants.labelFont
    topLab.horizontalAlignment=Alignment.Center
    topLab.peer.putClientProperty("JComponent.sizeVariant", "small")
    topLab.peer.updateUI()
    val buttonPane:Component=new BoxPanel(Orientation.Horizontal){
      contents++=elements
    }
    add(topLab,BorderPanel.Position.North )
    add(buttonPane,BorderPanel.Position.Center)
    border=BorderFactory.createEtchedBorder()//createBevelBorder(BevelBorder.RAISED)
    maximumSize=new Dimension(80,100)
  }

  val chosePagePan = new GroupPanel("Seite(n) anzeigen", firstPageBut, prevPageBut, pageEdit, numPageLab, nextPageBut, lastPageBut)
  val prozLab = new Label("%")
  prozLab.font = ViewConstants.labelFont
  val zoomPanel = new GroupPanel("Vergrößerung", pageHeightBut, pageWidthBut, twoPagesBut, Swing.HStrut(10), zoomInBut, zoomOutBut, zoomEdit, prozLab)
  val outputPanel=new GroupPanel("Ausgabe",/*dateBut,*/formSettingsBut,pageSettingsBut,/*dateBut,*/printBut,closeBut)

  val topPane:Component=new BoxPanel(Orientation.Horizontal ) {
    contents+=chosePagePan+=Swing.HGlue+=zoomPanel+=Swing.HGlue+=outputPanel
    //background=Color.white
  }



  lazy val archiveList:ListView[ArchivePageable]=new ListView[ArchivePageable] {
    peer.setModel(archiveListModel)
    selection.intervalMode=ListView.IntervalMode.Single
    listenTo(selection)
    reactions += {
      case e:ListSelectionChanged[_] =>
        if (!e.live&& selection.indices.nonEmpty&&archiveScroller.visible) {
          val ix= selection.indices.head
          setCurrentData(archiveListModel.archiveList(ix))
        }
    }
    renderer=new ListView.AbstractRenderer[ArchivePageable,ArchiveRenderer](new ArchiveRenderer){
      def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: ArchivePageable, index: Int): Unit = {
        component.config(list,isSelected,focused,a,index)
      }
    }
  }

  lazy val archiveScroller:ScrollPane=new ScrollPane{
    viewportView=archiveList
    preferredSize=new Dimension(200,40)
  }


  val mainPanel:Component=new BorderPanel(){
    add(topPane,BorderPanel.Position.North )
    add(pageScroller,BorderPanel.Position.Center )
    add(archiveScroller,BorderPanel.Position.West)
  }

  def initKeyStrokes():Unit = {
    val pagedownAction=new AbstractAction() {
      override def actionPerformed(actionEvent: ActionEvent): Unit = incPage()
    }
    val pageupAction=new AbstractAction() {
      override def actionPerformed(actionEvent: ActionEvent): Unit = decPage()
    }
    val amap=mainPanel.peer.getActionMap
    val imap=mainPanel.peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
    amap.put("pagedown",pagedownAction)
    amap.put("pageup",pageupAction)
    imap.put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN,0),"pagedown")
    imap.put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP,0),"pageup")
  }

  listenTo(closeBut,pageHeightBut,pageWidthBut,twoPagesBut,zoomInBut,zoomOutBut,zoomEdit,pageSettingsBut,printBut,
    nextPageBut,prevPageBut,firstPageBut,lastPageBut,pageEdit,formSettingsBut/*,dateBut*/)

  def decPage(): Unit = if (currentPage > 1) setCurrentPage(currentPage - 1)

  def incPage(): Unit = if (currentPage < thePageable.pagesList.size) setCurrentPage(currentPage + 1)

  reactions += {
    case ButtonClicked(`closeBut`)=> close();DialogManager.reset()
    case ButtonClicked(`printBut`)=> printToOutput()
    case ButtonClicked(`zoomInBut`)=> setScale(previewContext.scale*1.25)
    case ButtonClicked(`zoomOutBut`)=>setScale(previewContext.scale/1.25)
    case ButtonClicked(`pageSettingsBut`)=>
      close()
      PrintQuestionHandler.outdefDialog.changeOutdef()
    case ButtonClicked(`formSettingsBut`)=>
      close()
      PrintQuestionHandler.outdefDialog.visible=true
    case ButtonClicked(`nextPageBut`)=> incPage()
    case ButtonClicked(`prevPageBut`)=> decPage()
    case ButtonClicked(`firstPageBut`)=> setCurrentPage(1)
    case ButtonClicked(`lastPageBut`)=> setCurrentPage(thePageable.pagesList.size)

    case EditDone(`pageEdit`)=>
      try {
        val pn=pageEdit.text.toInt
        if(pn>0&&pn<=thePageable.pagesList.size)setCurrentPage(pn)
      }catch {case NonFatal(e)=> util.Log.e(e)
      case other:Throwable =>println(other);System.exit(0)}
    case ButtonClicked(`pageHeightBut`)=> setToPageHeight()
    case ButtonClicked(`pageWidthBut`)=>  setToPageWidth()

  }

  protected def setToPageHeight(): Unit = {
    val vAmount= if (pageScroller.peer.getHorizontalScrollBar.isVisible) pageScroller.peer.getHorizontalScrollBar.
      getSize().height + 3
    else 10
    val siz=pageScroller.peer.getViewport.getSize()
    setScale((siz.height-vAmount).toFloat*dotPitch/thePageable.pageHeight)
  }

  protected def setToPageWidth(): Unit = {
    val hAmount= if (pageScroller.peer.getVerticalScrollBar.isVisible) pageScroller.peer.getVerticalScrollBar.
      getSize().width + 10
    else 16
    val siz=pageScroller.peer.getViewport.getSize()
    setScale((siz.width-hAmount).toFloat*dotPitch/thePageable.pageWidth)
  }

  def printToOutput(): Unit = {
    //thePageable.context =MyContext
    for(r<-currentPrintReceiver) {
      val (pagesList,copies,receiverList)= PrintOutDialog.dialog.showPrintOutDialog(mainPanel,thePageable,currentPage,r.printerName,r.mediaName)
      if(pagesList!=null){
        r.printOut(thePageable,copies,pagesList)
        close()
      }
    }
  }

  contents=mainPanel
  initKeyStrokes()

  protected def setScale(nscale: Double): Unit = {
    previewContext.setScale(nscale)
    zoomEdit.text=f"${nscale * 100}%,.2f"
    pageViewer.updateSize()
  }

  def showPreview(ntitle: String, nPageable: APageable, receiver: PrintReceiver): Unit = {
    archiveScroller.visible=false
    currentPrintReceiver=Some(receiver)
    archiveListModel.clear()
    title="Druckvorschau "+ntitle
    setCurrentData(nPageable)
    setCurrentPage(1)
    visible=true

  }

  protected def showAll(): Unit = {
    if (thePageable.pageWidth<thePageable.pageHeight) setToPageHeight() else setToPageWidth()
  }

  def showArchive(ntitle: String, outRef: Reference, receiver: PrintReceiver): Unit = {
    archiveListModel.load(outRef)
    currentPrintReceiver=Some(receiver)
    archiveScroller.visible=true
    title=ntitle
    setCurrentPage(1)
    if(archiveListModel.getSize>0)
      archiveList.selectIndices(0)
    //if(thePageable!=null) numPageLab.text="/ "+thePageable.pagesList.size
    //println("Show Archive "+this.bounds)
    visible=true
  }

  protected def setCurrentData(nd:APageable):Unit = {
    thePageable=nd
    thePageable.tempContext=null
    previewContext.setMasterContext(thePageable.context)
    thePageable.tempContext=previewContext
    pageViewer.setData(nd)
    numPageLab.text="/ "+thePageable.pagesList.size
    setCurrentPage(1)
    setScale(1)
    showAll()
  }

  protected def maximize(): Unit = {
    val config = ClientApp.top.peer.getGraphicsConfiguration
    val insets =Toolkit.getDefaultToolkit.getScreenInsets(config)
    val screenSize = Toolkit.getDefaultToolkit.getScreenSize
    val w = screenSize.width/2// - insets.left - insets.right;
    val h = screenSize.height - insets.top - insets.bottom
    //println("max: "+w+" "+h)
    preferredSize=new Dimension(w,h)
    peer.setBounds(insets.left+w/2,insets.top,w,h)
  }

  protected def setCurrentPage(nr: Int): Unit = {
    currentPage=nr
    pageEdit.text=nr.toString
    pageViewer.pageNr =nr
    pageViewer.repaint()
  }

  override def closeOperation(): Unit = {
    DialogManager.reset()
  }

}

