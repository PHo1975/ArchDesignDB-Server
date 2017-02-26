package client.graphicsView

import util.StrToInt

import scala.swing._
import scala.swing.event.{EditDone, ButtonClicked, MouseClicked}
import javax.swing.BorderFactory
import java.awt.Color
import definition.data.LineStyle

class ListPanel[T](caption:String,data:Seq[T],doubleClickAction:(Int)=>Unit) extends BorderPanel(){
  val label=new Label(caption)
  val listView=new ListView(data)
  val scroller=new ScrollPane{
     viewportView=listView
  }
  val checkBox=new CheckBox("Alle auswählen")
  add(label,BorderPanel.Position.North)
  add(scroller,BorderPanel.Position.Center)
  add(checkBox,BorderPanel.Position.South)
  border=BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5),
      BorderFactory.createLineBorder(Color.BLACK))
  checkBox.selected=true
  listView.selection.intervalMode=ListView.IntervalMode.MultiInterval
  listView.peer.setSelectionInterval(0,data.size-1)
  listenTo(checkBox,listView.mouse.clicks)
  reactions+={
    case ButtonClicked(`checkBox`)=>if(checkBox.selected) listView.peer.setSelectionInterval(0,data.size-1)
    else listView.selection.indices.clear()
    case e:MouseClicked if e.clicks == 2 => if(listView.selection.indices.nonEmpty) doubleClickAction(listView.selection.indices.head)
  }
  def getInfo= if(listView.selection.indices.size==data.size) Seq.empty 
  else listView.selection.indices.toSeq
}

class SelectionFilterDialog(w:Window,controller:GraphViewController) extends Dialog(w) {
  val prefSize=new Dimension(700,500)
  val okBut=new Button("Filtern")
  val cancelBut=new Button("Abbruch")
  val colorEdit=new TextField("")
  val elementPanel=new ListPanel("Zeichen-Elemente",SelectionFilterInfo.elemFilters,(ix)=>{
   controller.setSelectionFilter(new SelectionFilterInfo(mapElemTypes(Seq(ix)),Seq.empty,Seq.empty,Seq.empty,None))
   visible=false
  })
  val widthPanel=new ListPanel("Linienstärke",LineStyleEditor.widthList,(ix)=>{
   controller.setSelectionFilter(new SelectionFilterInfo(Seq.empty,mapLineWidth(Seq(ix)),Seq.empty,Seq.empty,None))
   visible=false
  })
  val stylePanel=new ListPanel("Strichart",LineStyleHandler.styles,(ix)=>{
   controller.setSelectionFilter(new SelectionFilterInfo(Seq.empty,Seq.empty,Seq(ix),Seq.empty,None))
   visible=false
  })
  val hatchPanel=new ListPanel("Schraffur",HatchHandler.hatchList,(ix)=>{
   controller.setSelectionFilter(new SelectionFilterInfo(Seq(0),// hack: Polygon-Type is first type in List,
       Seq.empty,Seq.empty,Seq(ix),None))
   visible=false
  })
		
	val mainPanel=new BorderPanel(){
	  add(new BoxPanel(Orientation.Horizontal){
	    contents+=elementPanel+=Swing.HStrut(10)+=widthPanel+=Swing.HStrut(10)+=stylePanel+=Swing.HStrut(10)+=hatchPanel
	  },BorderPanel.Position.Center)
	  add(new BorderPanel(){
	    add(Swing.VStrut(15),BorderPanel.Position.North)
		  add(new BoxPanel(Orientation.Horizontal){
		    contents+=okBut+=Swing.HGlue+=new Label(" Color:" )+=colorEdit+=Swing.HGlue+=cancelBut
		  },BorderPanel.Position.Center)	    
	  },BorderPanel.Position.South)
	  
	}
	preferredSize=prefSize
  modal=true
  title="Elemente filtern"  
	contents=mainPanel
	listenTo(okBut,cancelBut,colorEdit)
	stylePanel.listView.renderer=new ListView.AbstractRenderer[LineStyle,StylePreviewPan](new StylePreviewPan){
  	  def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: LineStyle, index: Int): Unit = {
  		  if(a!=null) {component.setStyle(a.ix);component.peer.setToolTipText(a.name)} else component.setEmpty()  	}
	}
	widthPanel.listView.renderer=new ListView.AbstractRenderer[Int,LineWidthRenderer](new LineWidthRenderer){
  	  def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: Int, index: Int): Unit = {
  		  if(a>=0) component.setStyle(a) else component.setEmpty()  	}
	}
	
	hatchPanel.listView.renderer=new ListView.AbstractRenderer[HatchStyle,HatchFieldPreview](new HatchFieldPreview(()=>true)){
  	  def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: HatchStyle, index: Int): Unit = {
  		  if(a.ix>=0) component.setStyle(a) else component.setEmpty()  	}
	}
	
	
	reactions+= {
	  case ButtonClicked(`okBut`)=>
      controller.setSelectionFilter(getInfo)
      colorEdit.text=""
      visible=false
    case ButtonClicked(`cancelBut`)=>
      visible=false
      controller.cancelSelectionFilter()
    case EditDone(`colorEdit`)=>
      visible=false
      controller.setSelectionFilter(new SelectionFilterInfo(Seq.empty,Seq.empty,Seq.empty,Seq.empty,getColor))
  }
	
	def mapElemTypes(ixList:Seq[Int])=
	  ixList.map(ix=>SelectionFilterInfo.elemFilters(ix).typ)
	
	 def mapLineWidth(ixList:Seq[Int])=
	   ixList.map(ix=>LineStyleEditor.widthList(ix))	   


  def getColor=colorEdit.text.trim match {
    case "" => None
    case StrToInt(color)=>Some(color)
    case _=> None
  }
	
	def getInfo:SelectionFilterInfo={
	  new SelectionFilterInfo(mapElemTypes(elementPanel.getInfo),mapLineWidth(widthPanel.getInfo),stylePanel.getInfo,
      hatchPanel.getInfo, getColor)
	}
	
}