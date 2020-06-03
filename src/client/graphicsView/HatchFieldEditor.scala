package client.graphicsView

import java.awt.Dimension

import client.dialog._
import client.graphicsView.Handlers._
import client.ui.ViewConstants
import definition.data.Material
import definition.expression.{Constant, IntConstant}

import scala.swing.event.{ButtonClicked, SelectionChanged}
import scala.swing.{Alignment, BorderPanel, BoxPanel, CheckBox, Label, Panel}


class HatchFieldEditor extends FieldEditor {  
  
  val tcn="PolyElem"
  val apn="AreaPolygon"
  val wof = "Wohnfläche"
  var allowedClassNames = Seq(tcn, apn, wof)
  val angleEdit = new SidePanelDoubleTextField(Map((tcn, 7), (apn, 8), (wof, 8)), this)
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
    styleCombo.maximumRowCount=22
    val defaultValue=0
    val allowedFields = Map((tcn, 5.toByte), (apn, 6.toByte), (wof, 6.toByte))
    
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

    def getConstant(value: Int) = IntConstant(value)

    def valueFromConstant(c: Constant): Int = c.toInt

    override def setValue(newHatch: Option[Int]): Unit = {
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

    def setToolTip(newHatch: Option[Int]): Unit = {
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
    preferredSize = new Dimension(70, 100 * ViewConstants.fontScale / 100)
    maximumSize = new Dimension(Short.MaxValue, 100 * ViewConstants.fontScale / 100)
	}  

  def getPanel: Panel = panel
}

class HatchFieldPreview (getPaperScaleCallBack: ()=> Boolean) extends BorderPanel with RenderComponent[HatchStyle]{
   opaque=false
   val hatchPreview=new HatchPreview
  val label: Label = ViewConstants.label()
   label.horizontalAlignment=Alignment.Left 
   label.horizontalTextPosition=Alignment.Left
   var currentMaterial:Material=MaterialHandler.undefinedMaterial

  preferredSize = new Dimension(100 * ViewConstants.fontScale / 100, 50 * ViewConstants.fontScale / 100)
   add(hatchPreview,BorderPanel.Position.Center)
   add(label,BorderPanel.Position.North)

  def setStyle(hatch: HatchStyle): Unit = {
     hatchPreview.setHatch(HatchHandler.quickGetHatch(hatch.ix),getPaperScaleCallBack(),true)
     label.text=f"${hatch.ix}%2d"+" "+hatch.name
     label.background=background
     label.foreground=foreground
     peer.setToolTipText(hatch.name)
   }

  def setEmpty(): Unit = {
     hatchPreview.setHatch(null,false,true)
     label.text=""
     label.background=background
     peer.setToolTipText("")
   }
}
