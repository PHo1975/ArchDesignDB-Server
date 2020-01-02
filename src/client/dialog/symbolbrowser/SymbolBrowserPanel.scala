package client.dialog.symbolbrowser

import java.awt.Dimension

import client.comm.{ClientQueryManager, InstanceDataListModel, ListDataModel}
import client.dataviewer.ViewConstants
import client.dialog.DialogManager
import client.graphicsView.symbol.SymbolStamp
import client.model._
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.{ObjectReference, StringConstant}
import definition.typ.CustomPanel
//import scala.swing.ListView

import scala.swing.event.{ButtonClicked, ListSelectionChanged}
import scala.swing.{BoxPanel, Button, Label, ListView, Orientation, ScrollPane, Swing, TextField}


class SymbolBrowserPanel extends BoxPanel(Orientation.Vertical) with CustomPanel with PathControllable {

  val folderLab: Label = ViewConstants.label("Symbol-Ordner:")
  
  override var name="Neues Symbol absetzen"
  val symbolLab: Label = ViewConstants.label("Symbole:")
  val pathModel=new PathModel
  val pathList=new ListView[InstanceData]()
  val folderList=new ListView[InstanceData]()
  val folderModel=new InstanceDataListModel(List(SymbolBrowserController.folderType))  
  val pathLineLabel=new PathLineLabel
  val pathController:PathController=new PathController(pathModel,pathList,this)
  val pathScroller=new AdaptedScroller(pathList)
  val folderScroller=new AdaptedScroller(folderList)
    
  val symbolModel=new ListDataModel[SymbolStamp](List(SymbolBrowserController.symbolType),el=>new SymbolStamp(el))
  val symbolList=new ListView[SymbolStamp]()
  val symbolScroller=new ScrollPane{
    viewportView=symbolList
    preferredSize = new Dimension(ViewConstants.sidePanelWidth, 800)
  }
  var folderSelfSelected=false
  var symbolSelfSelected=false
  var loaded=false
  val theRender=new SymbolRenderer
  var createStampMode=false
  opaque=false
  val folderEdit=new TextField
  folderEdit.maximumSize=new Dimension(Short.MaxValue,30)
  var currentFolder:Option[Reference]=None
  var lastIndent=0
  var selectedStamp:Option[SymbolStamp]=None
  
  val createStampPanel=new BoxPanel(Orientation.Vertical) {
    val createFolderButton=new Button("Unterordner anlegen:")
    val chooseFolderButton=new Button("Ordner wählen")
    maximumSize=SymbolBrowserController.maximumSize    
    contents+=createFolderButton+=folderEdit+=Swing.VStrut(10)+=chooseFolderButton+=Swing.VGlue
    listenTo(createFolderButton,chooseFolderButton)
    reactions+= {
      case ButtonClicked(`createFolderButton`)=>
        if(folderEdit.text.trim.length>0)
          for (cfolder<-currentFolder){
            val inst=ClientQueryManager.createInstance(SymbolBrowserController.folderType, Array(new OwnerReference(1,cfolder)))
            val crRef=new Reference(SymbolBrowserController.folderType,inst)
            ClientQueryManager.writeInstanceField(crRef, 0,StringConstant( folderEdit.text.trim))
          }
      case ButtonClicked(`chooseFolderButton`)=>
        for(cfolder<-currentFolder)
        DialogManager.addAnswer(SymbolBrowserController.answer, new ObjectReference(cfolder))
        DialogManager.processResults()
    }
  }
  
  folderList.selection.intervalMode=ListView.IntervalMode.Single
  folderList.peer.setModel(folderModel)
  pathList.peer.setModel(pathModel)
  pathList.selection.intervalMode=ListView.IntervalMode.Single
  symbolList.peer.setModel(symbolModel)
  symbolList.selection.intervalMode=ListView.IntervalMode.Single
  //symbolList.peer.setLayoutOrientation(JList.HORIZONTAL_WRAP)
  symbolList.peer.setVisibleRowCount(-1)
  symbolList.maximumSize=SymbolBrowserController.maximumSize
  maximumSize=SymbolBrowserController.maximumSize
  symbolList.renderer=new ListView.AbstractRenderer[SymbolStamp,SymbolRenderer](theRender){
      def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: SymbolStamp, index: Int): Unit = {
        if(a!=null) {theRender.setStyle(a);} else theRender.setEmpty()    }
  }
  contents+=Swing.VStrut(10)+=folderLab+=pathScroller+=pathLineLabel+=Swing.VStrut(2)+=folderScroller +=Swing.VStrut(10)+=
    symbolLab += symbolScroller += createStampPanel += Swing.VGlue
  for(c<-contents) {c.xLayoutAlignment=0.5d}
  listenTo(folderList.selection)
  symbolList.listenTo(symbolList.selection)
  symbolList.reactions += {
    case ListSelectionChanged(view,range,live)=> if(!live && !range.isEmpty&& symbolList.selection.items.nonEmpty){
      selectedStamp=Some(symbolList.selection.items(0))
      DialogManager.answerGiven(SymbolBrowserController.answer, new ObjectReference(symbolModel.theList(symbolList.selection.indices.head).ref))
    }
  }
  
  reactions += {      
    case ListSelectionChanged(view,range,live)=> //println("Select "+view+"  range:"+range+" live:"+live+" self:"+folderSelfSelected);
      if(!live&& !range.isEmpty&& !folderSelfSelected && folderList.selection.indices.nonEmpty)
        pathController.addPathElement(folderModel.theList(folderList.selection.indices.head).ref)       
  }

  def open(): Unit = {
    println("open " + Thread.currentThread() + " loaded:" + loaded + " pr:" + currentFolder)
    //println("open createStamp:"+createStampMode+" loaded:"+loaded)
    symbolScroller.visible= !createStampMode
    symbolLab.visible= !createStampMode
    createStampPanel.visible=createStampMode
    symbolList.peer.clearSelection()  
    name=if(createStampMode)"Symbol erstellen" else "Neues Symbol absetzen"
    peer.invalidate()
    revalidate()
    if(loaded){
      for(pr<-currentFolder) 
        folderOpen(pr)
    } else for(sr<-SymbolBrowserController.symbolRootFolder) {     
      pathController.loadPath(Seq(sr.ref),()=> {  loaded=true })
    }
  }

  def setFocus(): Unit = {
    
  }

  def shutDown(): Unit = {
    //pathLineLabel.shutDown()
    //pathModel.shutDown()
    //folderModel.shutDown()
    folderSelfSelected=false
    symbolSelfSelected=false
    folderEdit.text=""
  }

  def openData(parentRef: Reference, selectRef: Option[Reference], indent: Int, doneListener: Option[() => Unit],withCustomEditor:Boolean): Unit = {
    println("folderList open Data " + parentRef + " indent:" + indent + "sel:" + selectRef)
    pathLineLabel.shutDown()
    currentFolder=Some(parentRef)
    lastIndent=indent
    folderSelfSelected=true
    symbolSelfSelected=true
    folderList.peer.clearSelection()    
    pathLineLabel.load(parentRef, indent)        
    folderModel.load(parentRef, 1,()=>{      
      folderSelfSelected=false
      revalidate()  
      for(d<-doneListener)d()      
    })    
    folderOpen(parentRef)
    revalidate()
  }

  def folderOpen(parentRef: Reference): Unit = {
    if(createStampMode) {
      
    } else symbolModel.load(parentRef,1,()=>{
      symbolSelfSelected=false
      revalidate()
    })
  }
}