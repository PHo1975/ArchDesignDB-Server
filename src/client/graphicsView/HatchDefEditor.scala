package client.graphicsView


import java.awt.{Polygon => _, _}

import client.comm.ClientQueryManager
import client.ui.ViewConstants
import definition.comm.NotificationType
import definition.data.{InstanceData, LineStyle, Reference}
import definition.expression._
import definition.typ.CustomInstanceEditor
import javax.swing.BorderFactory
import util.StringUtils

import scala.swing.event.{ButtonClicked, EditDone, SelectionChanged, UIElementResized}
import scala.swing.{Alignment, BorderPanel, BoxPanel, CheckBox, ComboBox, Component, Label, ListView, Orientation, Swing, TextField}

class HatchDefEditor extends BoxPanel(Orientation.Horizontal)with CustomInstanceEditor[Component] {
  
  val nameEdit=new TextField
  val thickEdit=new TextField
  val nameBox=new TitlePanel("Schraffur bearbeiten",nameEdit)
  var subsID:Int= -1  
  var sizeChangeListener:Option[() => Unit]=None
  val previewPrototype=new StylePreviewPan
  
  var currentRef:Option[Reference]=None
  private var oldName:String=""
  private var oldThick:Double=0
  
  val firstBox=new LineBox(0)
  val secondBox=new LineBox(1)
  
  class MyChooseBox extends BoxPanel(Orientation.Horizontal) {
    //maximumSize=new Dimension(200,25)
    //preferredSize=new Dimension(100,25)
    val checkBox=new CheckBox("2. Liniengruppe darstellen ")
    checkBox.horizontalAlignment=Alignment.Left
    contents+=checkBox+=Swing.HGlue
    listenTo(checkBox)
    reactions+= {
      case ButtonClicked(`checkBox`)=>
        setField(5, IntConstant(if (secondBox.oldStyle > -1) -1 else 0)) // invert number to switch visibility
    }
  }
  val chooseSecondBox= new MyChooseBox 
  
  class LineBox(lineNr:Int) extends BoxPanel(Orientation.Horizontal)
   {
     val angleLab: Label = ViewConstants.label("Linie " + (lineNr + 1) + ",   Winkel:")
     val distLab: Label = ViewConstants.label(" Abstand:")
     val styleLab: Label = ViewConstants.label(" Stil:")
    val angleEdit=new TextField
    val distEdit=new TextField
    val styleCombo=new ComboBox(LineStyleHandler.styles)
    var selfStyleSelected=false
    private var oldAngle:Double=0f
    private var oldDist:Double=0f
    var oldStyle:Int=0
    styleCombo.renderer=new ListView.AbstractRenderer[LineStyle,StylePreviewPan](previewPrototype){
    	def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: LineStyle, index: Int): Unit = {
    		component.setStyle(a,index)
    	}		
    }
    styleCombo.preferredSize=new Dimension(100,25)
    contents+=angleLab+=angleEdit+=distLab+=distEdit+=styleLab+=styleCombo+=Swing.HGlue
    listenTo(angleEdit,distEdit,styleCombo.selection)
    reactions+= {
      case EditDone(`angleEdit`)=>
        val angle=StringUtils.stringToDouble(angleEdit.text)
        if(angle!=oldAngle) setField(1+lineNr*3,new DoubleConstant(angle))
      case EditDone(`distEdit`)=>
        val dist=StringUtils.stringToDouble(distEdit.text)
        if(dist!=oldDist) setField(3+lineNr*3,new DoubleConstant(dist))
      case SelectionChanged(`styleCombo`)=> if(selfStyleSelected) selfStyleSelected=false
			else if(styleCombo.selection.index> -1){
				val ix=styleCombo.selection.index
        if (ix != oldStyle) setField(2 + lineNr * 3, IntConstant(ix))
			}
    }

     def setValues(data: InstanceData): Unit = {
      oldAngle=data.fieldValue(1+lineNr*3).toDouble
      angleEdit.text=oldAngle.toString
      oldDist=data.fieldValue(3+lineNr*3).toDouble
      distEdit.text=oldDist.toString
      oldStyle=data.fieldValue(2+lineNr*3).toInt
      if(oldStyle>0&& oldStyle<LineStyleHandler.styles.size) {
        selfStyleSelected=true
      	styleCombo.selection.index=oldStyle  
      }      
    }
  }
  
  val endBox=new BoxPanel(Orientation.Horizontal) {
    val thickLabel: Label = ViewConstants.label("Linien-Dicke:")
    thickEdit.maximumSize=new Dimension(80,30)
    thickEdit.preferredSize=thickEdit.maximumSize
    
    contents+=thickLabel+=thickEdit+=Swing.HGlue
  }
  
  val leftBox=new BoxPanel(Orientation.Vertical) {
    contents+=nameBox+=firstBox+=chooseSecondBox+=secondBox+=endBox  
  }
  
  val previewComp=new HatchPreview 
  previewComp.background=Color.white
  
  
  val previewBox=new BorderPanel {
    val previewLabel: Label = ViewConstants.label("Vorschau:")
    add(previewLabel,BorderPanel.Position.North)
    add(previewComp,BorderPanel.Position.Center)
  }
  
  contents+=leftBox+=previewBox
  
  listenTo(nameEdit,thickEdit)
  
  reactions+= {
    case EditDone(`nameEdit`)=>
      if (nameEdit.text != oldName) setField(0, StringConstant(nameEdit.text))
    case EditDone(`thickEdit`)=>
      val thick=StringUtils.stringToDouble(thickEdit.text)
      if(thick!=oldThick) setField(7,new DoubleConstant(thick))
  }
  
  
  def setField(field:Int,value:Constant):Unit= for(r<-currentRef){
    ClientQueryManager.writeInstanceField(r,field.toByte,value)    
  }
  
 /* override def setSizeChangeListener(listener:() => Unit)= {
    sizeChangeListener=Some(listener)
  }*/

  def updatePreview(dat: InstanceData): Unit = {
    val hs=new HatchStyle(1,dat)
    previewComp.setHatch(hs,true,true)
  }

  def getComponent: HatchDefEditor = this
  

  def load(ref: Reference ,doneListener:()=>Unit):Unit= {
    var firstTime=true
    subsID=ClientQueryManager.createSubscription(ref,-1){
				(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> Swing.onEDT{				  
					this.synchronized {					  
						typ match {
							case NotificationType.sendData| NotificationType.fieldChanged |NotificationType.updateUndo=>
                currentRef=Some(data.head.ref)
                oldName=data(0).fieldValue.head.toString
                nameEdit.text=oldName
                oldThick=data.head.fieldValue(7).toDouble
                thickEdit.text=oldThick.toString
                firstBox.setValues(data.head)
                if(data.head.fieldValue(5).toInt<0) { // dont use 2nd lines
                  secondBox.visible=false
                  chooseSecondBox.checkBox.selected=false
                  secondBox.oldStyle= -1
                } else {
                  secondBox.visible=true
                  chooseSecondBox.checkBox.selected=true
                  secondBox.setValues(data.head)
                }
                updatePreview(data.head)
                if(firstTime){
                  firstTime=false
                  doneListener()
                }
            }
						for(c<-sizeChangeListener)c()
					}					
				}
    }    
  }

  def shutDown():Unit= {    
  }

  def editorName  = "Schraffur-Editor"
}



class HatchPreview extends Component {
   protected var _hatch:HatchStyle=HatchHandler.undefinedHatch
   protected var scale=100d   
   //println("Create preview ")
   preferredSize=new Dimension(55,30)
   val defaultStroke=new BasicStroke(0.5f)
   border=BorderFactory.createLineBorder(Color.gray)   
   var whiteBackground=false
   var p:Polygon=new Polygon(Seq.empty,Seq.empty)
   var lastSize:Dimension=new Dimension(0,0)
   var paperStyle:Boolean=false
   
   private val sm=new ScaleModel
   sm.vpBorder=1
   sm.viewSize=preferredSize
   sm.setScaleRatio(1,100)

  def setHatch(newHatch: HatchStyle, paperStyle: Boolean, update: Boolean): Unit = {
     _hatch=if(newHatch==null) HatchHandler.undefinedHatch else newHatch    
     if(update)Swing.onEDT{repaint()}
   }

  override def paintComponent(g: Graphics2D): Unit = {
     super.paintComponent(g)
     if(size!=lastSize) updateSize()
     g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
     g.setColor(if(whiteBackground)Color.white else background)
     g.fill(g.getClip)     
     if(_hatch.ix > 0) HatchHandler.drawHatch(p,_hatch,sm,paperStyle,g,if(whiteBackground)Color.black else foreground,NULLVECTOR,0,NULLVECTOR)          
     g.setStroke(defaultStroke)     
   }
   
   listenTo(this)
   reactions+= {
     case e:UIElementResized =>
       updateSize()
       Swing.onEDT{repaint()}
   }

  def updateSize(): Unit = {
  	 sm.viewSize=size
  	 sm.setScaleRatio(1,100)
  	 sm.viewSize=size
  	 val m=peer.getInsets
  	 val xp=sm.xToWorld(m.left)
  	 val yp=sm.yToWorld(m.top)
  	 val w= sm.xToWorld(size.width-m.left-m.right)
  	 val h=sm.yToWorld( size.height-m.top-m.bottom)
  	 lastSize=size
    p = new Polygon(Seq.empty, Seq(PointList(Seq(new VectorConstant(xp, yp, 0), new VectorConstant(w, yp, 0), new VectorConstant(w, h, 0),
      new VectorConstant(xp, h, 0)))))
   }
   
}

