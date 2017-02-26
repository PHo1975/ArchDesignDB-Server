package client.graphicsView

import java.awt.Dimension
import scala.collection.Seq
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.CheckBox
import scala.swing.Label
import scala.swing.Panel
import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged
import definition.expression.Constant
import definition.expression.IntConstant
import client.dialog.FieldEditor
import client.dialog.RenderedComboBox
import client.dialog.PanelPart
import client.dialog.RenderComponent
import client.dialog.SidePanelComponent
import client.dialog.SidePanelDoubleTextField
import scala.swing.Alignment


class HatchFieldEditor extends FieldEditor {  
  
  val tcn="PolyElem"
  val apn="AreaPolygon"
  var allowedClassNames=Seq(tcn,apn)	
	val angleEdit=new SidePanelDoubleTextField(Map((tcn,7),(apn,8)),this)  
  angleEdit.addSearchLookup({
    case c:PolyElement => c.hatchAngle      
  })
	  
  lazy val hatchPanel=new BorderPanel with SidePanelComponent[Int] {
    opaque=false
    xLayoutAlignment=0d
    var selfStyleSelected=false
    val paperScaleBut= new CheckBox("Papiermasstab")
    paperScaleBut.focusable=false
    val styleCombo=new RenderedComboBox(HatchHandler.hatchList,new HatchFieldPreview(()=>paperScaleBut.selected))
    
    val defaultValue=0
    val allowedFields=Map((tcn,5.toByte),(apn,6.toByte))
    
    add(styleCombo,BorderPanel.Position.Center)
    add( paperScaleBut,BorderPanel.Position.South)
    listenTo(styleCombo.selection,paperScaleBut)
    reactions+={			
			case SelectionChanged(`styleCombo`)=> if(selfStyleSelected) selfStyleSelected=false
				else intSetValues() 
			case ButtonClicked(`paperScaleBut`) =>				
				intSetValues()						
		}
    
    addSearchLookup({
      case c:PolyElement =>
        c.hatchStyle match {
          case Some(hatch) => hatch.ix*(if(c.paperScale) -1 else 1)
          case None=> 0
        }
    })
    
    
    def intSetValues ():Unit=  if(styleCombo.selection.index> -1){      
    	val ix=styleCombo.selection.index    	
    	val hValue=ix*(if (paperScaleBut.selected) -1 else 1)
    	storeValue(hValue,this)    	
    }    
    
    def getConstant(value:Int)=new IntConstant(value)  
    def valueFromConstant(c:Constant)=c.toInt
    
    override def setValue(newHatch:Option[Int])= {	
		  super.setValue(newHatch)
		  setToolTip(newHatch)
			newHatch match {
				case Some(style)=>
          selfStyleSelected=true
          paperScaleBut.selected=style<0
          styleCombo.selection.index=math.abs(style)
        case None=>
          styleCombo.selection.index= -1
          paperScaleBut.selected=false
      }
		}
    def setToolTip(newHatch:Option[Int])= {		
		styleCombo.peer.setToolTipText(newHatch match {
		  case Some(hatch) =>  HatchHandler.quickGetHatch(math.abs(hatch)).name	  		   
		  case None => "Undefiniert"		    
		  }) 
		}    
  }
  
  lazy val fieldComponents=Seq(hatchPanel,angleEdit)
  
	lazy val panel= new BoxPanel(scala.swing.Orientation.Vertical) {
    opaque=false
    val topPan=new PanelPart("Schraff:",hatchPanel)
    topPan.maximumSize=new Dimension(FieldEditor.panelSize.width,FieldEditor.panelSize.height*2)
		contents +=topPan
		contents += new PanelPart("Winkel:",angleEdit)		
		preferredSize=new Dimension(70,100)
		maximumSize=new Dimension(Short.MaxValue,100)		
	}  

  def getPanel: Panel = panel
}

class HatchFieldPreview (getPaperScaleCallBack: ()=> Boolean) extends BorderPanel with RenderComponent[HatchStyle]{
   opaque=false
   val hatchPreview=new HatchPreview
   val label=new Label
   label.horizontalAlignment=Alignment.Left 
   label.horizontalTextPosition=Alignment.Left
   var currentMaterial:MaterialDef=MaterialHandler.undefinedMaterial
   
   preferredSize=new Dimension(100,50)
   add(hatchPreview,BorderPanel.Position.Center)
   add(label,BorderPanel.Position.North)   
   
   def setStyle(hatch:HatchStyle)={
     hatchPreview.setHatch(HatchHandler.quickGetHatch(hatch.ix),getPaperScaleCallBack(),true)
     label.text=f"${hatch.ix}%2d"+" "+hatch.name
     label.background=background
     label.foreground=foreground
     peer.setToolTipText(hatch.name)
   }
   
   def setEmpty()= {
     hatchPreview.setHatch(null,false,true)
     label.text=""
     label.background=background
     peer.setToolTipText("")
   }
}
