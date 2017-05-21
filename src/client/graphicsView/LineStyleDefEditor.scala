package client.graphicsView

import java.awt.{BasicStroke, Dimension, Graphics2D}

import client.comm.ClientQueryManager
import client.dataviewer.ViewConstants
import definition.comm.NotificationType
import definition.data.{InstanceData, Reference}
import definition.expression.StringConstant
import definition.typ.CustomInstanceEditor

import scala.swing.event._
import scala.swing.{Alignment, BorderPanel, BoxPanel, Button, Component, Orientation, Swing, TextField}


class TitlePanel(title:String,nameEdit:TextField) extends BorderPanel {
  val nameLab = ViewConstants.label("Name:")
  val titleLab = ViewConstants.label(title)
    titleLab.preferredSize=new Dimension(100,35)        
    titleLab.horizontalAlignment=Alignment.Left
  nameLab.preferredSize = new Dimension(60 * ViewConstants.fontScale / 100, 35)
    add(titleLab,BorderPanel.Position.North)
    add(nameLab,BorderPanel.Position.West)
    add(nameEdit,BorderPanel.Position.Center)    
    maximumSize=new Dimension(Short.MaxValue,70)
    minimumSize=new Dimension(100,70)
  }

class LineStyleDefEditor extends BoxPanel(Orientation.Vertical) /*Panel with SequentialContainer.Wrapper*/ with CustomInstanceEditor[Component] {
  
  
  def getComponent: Component = this  
  var groupList=collection.mutable.ArrayBuffer[LineElemGroup]()
  var currentRef:Option[Reference]=None  
  //var oldName:String=""  
  var subsID:Int = -1  
  var sizeChangeListener:Option[() => Unit]=None
  var preview=new DirectStylePreviewPan  
  val nameEdit=new TextField()
  val addBut=new Button(" + Segment hinzufügen")
  val nameGroup=new TitlePanel(" LinienStil bearbeiten   (Breiten in mm)",nameEdit)
  
  /*override def setSizeChangeListener(listener:() => Unit)= {
    sizeChangeListener=Some(listener)
  }*/
  
  val previewGroup=new BorderPanel{
    val prevLab = ViewConstants.label("Vorschau:")
    prevLab.preferredSize=new Dimension(60,50)
    maximumSize=new Dimension(Short.MaxValue,50)
    minimumSize=new Dimension(100,50)
    add(prevLab,BorderPanel.Position.West)
    add(preview,BorderPanel.Position.Center)
  }
  
  val endGroup=new BorderPanel{ 
    //border=BorderFactory.createLineBorder(Color.red)
    addBut.focusable=false
    add(addBut,BorderPanel.Position.West)
    add(Swing.HGlue,BorderPanel.Position.Center)
    maximumSize=new Dimension(Short.MaxValue,30)
    preferredSize=new Dimension(100,30)
  }
  
  listenTo(nameEdit,addBut)
  reactions+= {
    case EditDone(`nameEdit`)=>for(r<-currentRef) ClientQueryManager.writeInstanceField(r,0,StringConstant(nameEdit.text))
    case ButtonClicked(`addBut`)=>
      val newGroup=new LineElemGroup(groupList.size,"","")
      groupList+=newGroup
      Swing.onEDT{
      contents.insert(contents.size-1,newGroup)
revalidate()
for(c<-sizeChangeListener)c()
repaint()
      }
  }
  
  class LineElemGroup(tix:Int,lineText:String,distText:String) extends BoxPanel(Orientation.Horizontal) {
    var ix:Int=_
    minimumSize=new Dimension(100,33)  
    maximumSize=new Dimension(Short.MaxValue,33)
    val lineLab = ViewConstants.label()
    setIndex(tix)
    val distLab = ViewConstants.label("Breite Abstand:")
    val lineEdit=new TextField
    lineEdit.preferredSize=new Dimension(80,33)
    lineEdit.maximumSize=lineEdit.preferredSize
    lineEdit.text=lineText
    val distEdit=new TextField
    distEdit.maximumSize=lineEdit.preferredSize
    distEdit.preferredSize=lineEdit.preferredSize
    distEdit.text=distText
    val closeBut=new Button("X")
    closeBut.focusable=false
    contents+=lineLab+=lineEdit+=Swing.HStrut(10)+=distLab+=distEdit+=Swing.HStrut(10)+=closeBut+=Swing.HGlue
    listenTo(lineEdit,distEdit,closeBut)
    reactions+={
      case e:ButtonClicked=>  groupClosed(ix)
      case EditDone(`lineEdit`)=>if(lineEdit.text!=lineText) textToFloat(lineEdit.text) match {case Some(d)=>contentsChanged()
        case _=>}
      case EditDone(`distEdit`)=>if(distEdit.text!=distText) textToFloat(distEdit.text) match {case Some(d)=>contentsChanged()        
        case _=>}
      
    }
    def getValues:Seq[Float]=Seq(textToFloat(lineEdit.text) match {case Some(d)=>d;case _=>0},
        textToFloat(distEdit.text) match {case Some(d)=>d;case _=>0})

    def setIndex(nix: Int): Unit = {
      ix=nix
      lineLab.text="Breite "+(ix+1)+". Linie:"
    }
  }
  
  def textToFloat(text:String):Option[Float]= try{
    val tt=text.trim.replace(',','.')
    if(tt.length==0)Some(0f)
    else {      
    	Some(tt.toFloat)
    }
  } catch {case e:Exception=> println(e); None}


  def contentsChanged(): Unit = for (r <- currentRef) {
    val outputString=groupList.flatMap(_.getValues).mkString(";")
    ClientQueryManager.writeInstanceField(r,1,new StringConstant(outputString))
  }

  def groupClosed(groupIx: Int): Unit = if (groupList.size > 1) Swing.onEDT {
     groupList.remove(groupIx)
     contentsChanged()     
  }
  
  
  //contents+=nameGroup+=previewGroup

  def load(ref: Reference,doneListener:()=>Unit): Unit = {    
    subsID=ClientQueryManager.createSubscription(ref,-1){
				(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> Swing.onEDT{				  
					this.synchronized {					  
						typ match {
							case NotificationType.sendData| NotificationType.fieldChanged|NotificationType.updateUndo =>
                currentRef=Some(data.head.ref)
                nameEdit.text=data(0).fieldValue.head.toString
                val doubles=data.head.fieldValue(1).toString.trim.split(';')
                var ix=0
                groupList.clear()
                contents.clear()
                contents+=nameGroup+=previewGroup
                if(doubles.length>1 && doubles.length % 2 == 0) {
                  val doublesPairs=doubles.map(_.trim.toDouble).grouped(2)
                  for(Array(gr1,gr2)<-doublesPairs) {
                    val newGroup=new LineElemGroup(ix,gr1.toString,gr2.toString)
                    groupList+=newGroup
                    contents+=newGroup
                    ix+=1
                  }
                }
                val newGroup=new LineElemGroup(ix,"","")
                if (groupList.isEmpty) {
                  groupList+=newGroup
                  contents+=newGroup
                }
                contents+=endGroup
                preview.setStyle(groupList.flatMap(_.getValues))
                revalidate()
                for(c<-sizeChangeListener)c()
                repaint()
                if(typ==NotificationType.sendData) doneListener()
            }
					}
				}
    }	
  }

  def shutDown(): Unit = {  
    if(subsID>0) ClientQueryManager.removeSubscription(subsID)
    subsID= -1
    groupList.clear()
  }

  def editorName: String = "Linienstil-Editor"    
}


class DirectStylePreviewPan extends Component {
  var stroke=new BasicStroke(2f)
  opaque=true  
  preferredSize=new Dimension(40,25)

  def setStyle(dotList: Seq[Float]): Unit = {
    if (dotList.isEmpty || !dotList.exists(_ != 0)) stroke = new BasicStroke(1f)
    else stroke=new BasicStroke(1f,BasicStroke.CAP_BUTT ,BasicStroke.JOIN_BEVEL,10f,dotList.map(z=> z / client.graphicsView.ScaleModel._dotPitch.toFloat).toArray,0f)
    repaint()
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)    
    g.setColor(background)
    g.fill(g.getClipBounds)    
    g.setStroke(stroke)    
    g.setColor(foreground)    
    val siz=this.size
    g.drawLine(5,siz.height/2,siz.width-10,siz.height/2)    
  }
}