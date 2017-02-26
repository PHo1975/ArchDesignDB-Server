package client.dialog.symbolbrowser

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Label
import scala.swing.ScrollPane
import scala.swing.Swing
import scala.swing.Button
import scala.swing.event.ButtonClicked
import client.dialog.DialogManager
import definition.expression.StringConstant
import definition.typ.{CustomPanel, AnswerDefinition, DataType}
import client.model.PathModel
import util.MyListView
import definition.data.InstanceData
import client.model.PathControllable
import definition.data.Reference
import client.model.PathController
import client.comm.ListDataModel
import scala.swing.event.ListSelectionChanged
import client.model.PathLineLabel
import client.model.AdaptedScroller
import javax.swing.JList
import definition.expression.ObjectReference
import client.comm.InstanceDataListModel
import client.graphicsView.symbol.SymbolStamp
import java.awt.Dimension
import scala.swing.TextField
import scala.swing.event.ButtonClicked
import client.comm.ClientQueryManager
import definition.data.OwnerReference


class SymbolBrowserPanel extends BoxPanel(Orientation.Vertical) with CustomPanel with PathControllable {
  
  val folderLab=new Label("Symbol-Ordner:")  
  
  override var name="Neues Symbol absetzen"
  val symbolLab=new Label("Symbole:") 
  val pathModel=new PathModel
  val pathList=new MyListView[InstanceData]()
  val folderList=new MyListView[InstanceData]()
  val folderModel=new InstanceDataListModel(List(SymbolBrowserController.folderType))  
  val pathLineLabel=new PathLineLabel
  val pathController:PathController=new PathController(pathModel,pathList,this)
  val pathScroller=new AdaptedScroller(pathList)
  val folderScroller=new AdaptedScroller(folderList)
    
  val symbolModel=new ListDataModel[SymbolStamp](List(SymbolBrowserController.symbolType),el=>new SymbolStamp(el))
  val symbolList=new MyListView[SymbolStamp]()  
  val symbolScroller=new ScrollPane{
    viewportView=symbolList
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
  
  folderList.selection.intervalMode=MyListView.IntervalMode.Single
  folderList.peer.setModel(folderModel)
  pathList.peer.setModel(pathModel)
  pathList.selection.intervalMode=MyListView.IntervalMode.Single
  symbolList.peer.setModel(symbolModel)
  symbolList.selection.intervalMode=MyListView.IntervalMode.Single
  //symbolList.peer.setLayoutOrientation(JList.HORIZONTAL_WRAP)
  symbolList.peer.setVisibleRowCount(-1)
  symbolList.maximumSize=SymbolBrowserController.maximumSize
  maximumSize=SymbolBrowserController.maximumSize
  symbolList.renderer=new MyListView.AbstractRenderer[SymbolStamp,SymbolRenderer](theRender){
      def configure(list: MyListView[SymbolStamp], isSelected: Boolean, focused: Boolean, a: SymbolStamp, index: Int): Unit = {
        if(a!=null) {component.setStyle(a);} else component.setEmpty()    }
  }
  contents+=Swing.VStrut(10)+=folderLab+=pathScroller+=pathLineLabel+=Swing.VStrut(2)+=folderScroller +=Swing.VStrut(10)+=
    symbolLab+=symbolScroller+=createStampPanel
  for(c<-contents) {c.xLayoutAlignment=0.5d}
  listenTo(folderList.selection)
  symbolList.listenTo(symbolList.selection)
  symbolList.reactions += {
    case ListSelectionChanged(view,range,live)=> if(!live && !range.isEmpty&& symbolList.selection.items.size>0){      
      selectedStamp=Some(symbolList.selection.items.get(0))
      DialogManager.answerGiven(SymbolBrowserController.answer, new ObjectReference(symbolModel.theList(symbolList.selection.indices.head).ref))
    }
  }
  
  reactions += {      
    case ListSelectionChanged(view,range,live)=> //println("Select "+view+"  range:"+range+" live:"+live+" self:"+folderSelfSelected);
      if(!live&& !range.isEmpty&& !folderSelfSelected && folderList.selection.indices.nonEmpty)
        pathController.addPathElement(folderModel.theList(folderList.selection.indices.head).ref)       
  }
  
  def open()={
    //println("open createStamp:"+createStampMode+" loaded:"+loaded)
    symbolScroller.visible= !createStampMode
    symbolLab.visible= !createStampMode
    createStampPanel.visible=createStampMode
    symbolList.peer.clearSelection()  
    name=if(createStampMode)"Symbol erstellen" else "Neues Symbol absetzen"
    revalidate()
    if(loaded){
      for(pr<-currentFolder) 
        folderOpen(pr)
    } else for(sr<-SymbolBrowserController.symbolRootFolder) {     
      pathController.loadPath(Seq(sr.ref),()=> {  loaded=true })
    }
  }
  def setFocus() = {    
    
  }
  
  def shutDown() = {
    //pathLineLabel.shutDown()
    //pathModel.shutDown()
    //folderModel.shutDown()
    folderSelfSelected=false
    symbolSelfSelected=false
    folderEdit.text=""
  }
  
  def openData(parentRef:Reference,selectRef:Option[Reference],indent:Int,doneListener:Option[()=>Unit])= {
    //println("folderList open Data "+parentRef+" indent:"+indent+ "sel:"+selectRef)
    pathLineLabel.shutDown()
    currentFolder=Some(parentRef)
    lastIndent=indent
    folderSelfSelected=true
    symbolSelfSelected=true
    folderList.peer.clearSelection()    
    pathLineLabel.load(parentRef, indent)        
    folderModel.load(parentRef, 1,()=>{      
      folderSelfSelected=false
      /*selectRef match {
        case Some(sr)=>
        folderModel.theList.indexWhere(_.ref==sr) match {
          case -1 =>
          case ix =>{
            folderSelfSelected=true
            folderList.selectIndices(ix)
            folderSelfSelected=false
          }
        }
        case None=>*/ folderList.peer.clearSelection()
      //}
      //loaded=true
      revalidate()  
      for(d<-doneListener)d()      
    })    
    folderOpen(parentRef)
    revalidate()
  }
  
  def folderOpen(parentRef:Reference)= {
    if(createStampMode) {
      
    } else symbolModel.load(parentRef,1,()=>{
      symbolSelfSelected=false
      revalidate()
    })
  }
}