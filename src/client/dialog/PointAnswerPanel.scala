/**
 * Author: Peter Started:28.10.2010
 */
package client.dialog

import java.awt.Dimension

import client.comm.{ClientQueryManager, KeyStrokeManager}
import client.graphicsView.{ArcElement, GraphElemConst, LineElement, ViewportState}
import client.ui.ViewConstants
import definition.expression.{ObjectReference, VectorConstant}
import definition.typ.AnswerDefinition
import util.{Log, SemicolonSplit, StrToDouble}

import scala.swing._
import scala.swing.event._

trait BracketListener{
  def bracketModeStarted():Unit
}

trait PointClickListener extends BracketListener {
  def pointClicked(point:VectorConstant): Unit

  def forcePrecision: Boolean
}

object EmptyPointClickListener extends PointClickListener{
  def pointClicked(point: VectorConstant): Unit = {}

  def bracketModeStarted(): Unit = {}

  def forcePrecision: Boolean = false
}


/** answer panel to give answers to point questions 
 * 
 */
class PointAnswerPanel extends AnswerPanel with PointClickListener {
  val bracketBut=new IconableToggleButton("sumMode","PointPanel","Summenfunktion")
  val forcePrecBut = new IconableToggleButton("präz", "PointPanel", "Präzise Fangfunktion")
  
	val globalBut = new IconableButton("global","PointPanel","Globalpunkt eingeben")  
  val dxBut = new IconableButton("dx","PointPanel","Delta X eingeben")  
  val dyBut = new IconableButton("dy","PointPanel","Delta y eingeben")
  val dzBut = new IconableButton("dz","PointPanel","Delta z eingeben")
  val midBut =new IconableButton("mid","PointPanel","Mittelpunkt konstruieren")  
  val divBut = new IconableButton("part","PointPanel","Teilungspunkt konstruieren")  
  val interBut = new IconableButton("intersect","PointPanel","Schnittpunkt konstruieren")
  val textLabel: Label = ViewConstants.label()
  val textEdit=new TextField()
  var active=false
  var editActive=false
  var forcePrecision = true
  var defaultPrecision = true
  forcePrecBut.selected = true
  //var preventCancelOnReset=false
  
  var internPointClickListener:Option[VectorConstant =>Unit]=None
  var externPointClickListener:Option[VectorConstant =>Unit]=None

  var internEditListener:Option[String =>Unit]=None
  val buttons: Seq[AbstractPanelButton] = Seq(globalBut, dxBut, dyBut, dzBut, midBut, divBut, interBut, bracketBut, forcePrecBut)
  
  textEdit.maximumSize=new Dimension(Short.MaxValue,30)
  textEdit.preferredSize=new Dimension(100,30)
  textLabel.xLayoutAlignment = 0.5
  textEdit.xLayoutAlignment = 0.5
  minimumSize = new Dimension(ViewConstants.sidePanelWidth, 140)
  //preferredSize=minimumSize
  //opaque=true
  //background=Color.green


  contents+=infoLabel += Swing.RigidBox(new Dimension(0,10))+= new BoxPanel(Orientation.Horizontal) {
    xLayoutAlignment = 0.5
    opaque=false
    //println("Create panel "+ Thread.currentThread()+" "+ViewConstants.defaultRowHeight)
    contents+= bracketBut  +=  dxBut += dyBut+=dzBut 
    } += new BoxPanel(Orientation.Horizontal) {
    xLayoutAlignment = 0.5
    //opaque=true
    //background=Color.blue
      contents+=globalBut+= midBut +=divBut+=interBut
    minimumSize = new Dimension(ViewConstants.sidePanelWidth, 40)
  } += new BoxPanel(Orientation.Horizontal) {
    contents += forcePrecBut
    } += new BoxPanel(Orientation.Vertical) {
    xLayoutAlignment = 0.5
      opaque=false
      contents +=textLabel+=Swing.RigidBox(new Dimension(8,0))+=textEdit
    minimumSize = new Dimension(ViewConstants.sidePanelWidth, 40)
  } += Swing.VGlue
  listenTo(buttons:_*)  
  listenTo(textEdit)
  removeLastFocusListener(textEdit)

  def textDialog(caption:String,listener: String =>Unit):Unit= this.synchronized{
    initTextEdit(caption)
    internEditListener=Some(listener)
  }
  
  def pointDialog(caption:String,listener: VectorConstant =>Unit):Unit= {
    textEdit.visible=false
    AnswerPanelsData.currentViewController.requestFocus()
    showTextLabel(caption)
    internPointClickListener=Some(listener)
  }

  reactions += {
  	case ButtonClicked(`bracketBut`) =>
			if(bracketBut.selected) { // start
        bracketBut.tooltip="Summenfunktion abschliessen"
        AnswerPanelsData.currentViewController.startBracketMode()
      }
      else { // stop
          bracketBut.tooltip="Summenfunktion einschalten"
        AnswerPanelsData.currentViewController.stopBracketMode()
      }
		case ButtonClicked(`dxBut`)=> textDialog("dx-Wert:",_ => {
  		  AnswerPanelsData.currentViewController.addDelta(getTextEditDouble,0,0)
  		})  		
  	
  	case ButtonClicked(`dyBut`)=> textDialog("dy-Wert:",_ => {
  		  AnswerPanelsData.currentViewController.addDelta(0,getTextEditDouble,0)
  		})
  	
  	case ButtonClicked(`dzBut`)=>	textDialog("dz-Wert:",_ => {
  		  AnswerPanelsData.currentViewController.addDelta(0,0,getTextEditDouble)
  		})
  	
  	case ButtonClicked(`globalBut`)=> textDialog("Koordinate: x ; y ",{
      case SemicolonSplit(StrToDouble(xv), StrToDouble(yv)) => AnswerPanelsData.currentViewController.setCoordinate(xv, yv, 0)
      case _ =>
    })
  	
  	case ButtonClicked(`midBut`)=> pointDialog("Mitte von Punkt",p1=> {
  	    pointDialog("Mitte bis Punkt",p2=> {  			
  				val mid=VectorConstant.midPoint(p1,p2)
  				//internPointClickListener=None
  				AnswerPanelsData.currentViewController.setCoordinate(mid.x, mid.y, mid.z)
  				//func(ansParm,mid)
  			})
  		})  	
  	
  	case ButtonClicked(`divBut`)=>
			pointDialog("Teilung von Punkt",p1=> {
         pointDialog("Teilung bis Punkt",p2 => {
           textDialog("Anzahl Teilungen",text=> {
             val parts=parse(text).toInt
             if(parts>1) textDialog("Teilungspunkt Nr:",text2=> {
               val nr=parse(text2).toInt
               if(nr>0 && nr < parts) {
                 val dist=p2-p1
                 val partVect =p1+dist*(nr.toDouble/parts.toDouble)
                 AnswerPanelsData.currentViewController.setCoordinate(partVect.x,partVect.y,partVect.z)
                 //func(ansParm,p1+partVect)
               } else util.Log.e("Falsche Nr: "+nr)
            }) else ClientQueryManager.printErrorMessage("Anzahl Teilungen <1 "+parts+" text:'"+text+"'")
          })
        })
      })

		case ButtonClicked(`interBut`) =>  intersection()

    case ButtonClicked(`forcePrecBut`) =>
      //println("force Button "+forcePrecBut.selected)
      forcePrecision = forcePrecBut.selected
      defaultPrecision = forcePrecision
  	
  	
  	case EditDone(`textEdit`) =>
			//println("edit done act:"+editActive+" v:"+textEdit.visible+" listener:"+internEditListener+" text:"+textEdit.text)
			if(editActive&&textEdit.visible&&textEdit.text.length>0&& internEditListener.isDefined) {
        editActive=false
        textLabel.visible=false
        textEdit.visible=false
        //println("Listener:"+internEditListener)
        internEditListener match {
          case Some(listener)=>
            internEditListener=None
            Swing.onEDT{
             listener(textEdit.text)
            }
          case _ => util.Log.e("PointPanel Edit without listener !")
        }

      } // else AnswerPanelsData.currentViewController.requestFocus
			AnswerPanelsData.currentViewController.requestFocus()
	}
  
  def initTextEdit(labelText:String):Unit= {
    //println("Init text :"+labelText)
  	textLabel.text=labelText
  	textLabel.visible=true
  	textEdit.visible=true 
  	editActive=true
  	textEdit.selectAll()
  	revalidate()
  	textEdit.requestFocusInWindow()
  }
  
  def bracketModeStarted():Unit= bracketBut.selected=true

  def showTextLabel(text: String): Unit = {
    //println("Show text :"+text)
    textLabel.text=text    
    textLabel.visible=true
    revalidate()
  }
  
  def initPanel():Unit= {  
    //println("init  point panel \n"+Thread.currentThread().getStackTrace().drop(1).take(5).mkString("\n"))
    bracketBut.tooltip="Summenfunktion einschalten"
    textEdit.visible=false
    textLabel.visible=false
    editActive=false
    revalidate()
    internPointClickListener=None
    internEditListener=None
    for(b<-buttons)
      KeyStrokeManager.registerReceiver(b)    
  }

  def getTextEditDouble: Double = parse(textEdit.text).toDouble

  def getTextEditInt: Int = parse(textEdit.text).toInt

  override def loadParamAnswer(answerDesc: AnswerDefinition): Unit = {
  	super.loadParamAnswer(answerDesc)
  	//preventCancelOnReset=false;
  	initPanel()
    active = true
    answerDesc.constraint match {
      case AnswerPanelsData.STRICT_HIT => forcePrecision = true
      case AnswerPanelsData.NOSTRICT_HIT => forcePrecision = false
      case _ => forcePrecision= defaultPrecision
    }
    forcePrecBut.selected = forcePrecision
  	if(AnswerPanelsData.currentViewController!=null) {
      if(ViewConstants.showToast==1)FollowMouseToast.showToast(DialogManager.questionField.puretext+" : "+answerDesc.name,AnswerPanelsData.currentViewController.canvas.peer)
  		if(answerDesc.constraint =="Create")
  			AnswerPanelsData.currentViewController.deselect()
  		AnswerPanelsData.currentViewController.askForPointClick(this) 		
  	}  		
  }

  override def reset(): Unit = {
    //FollowMouseToast.reset()
    editActive=false
  	//System.out.println("pointpanel reset "+active+" ca"+preventCancelOnReset)
  	super.reset()
  	bracketBut.selected=false
  	if(internEditListener.isDefined)internEditListener=None
  	if(active) {  		
  		active=false
  		if(AnswerPanelsData.currentViewController!=null)
        AnswerPanelsData.currentViewController.cancelModus()
  	}
  }

  def pointClicked(point: VectorConstant): Unit = {
  	 internPointClickListener match{
      case Some(listener)=> internPointClickListener=None; listener(point)
      case None=> externPointClickListener match {
        case Some(elistener)=> elistener(point)
        case None => DialogManager.answerGiven(ansParm,point)
      }
     }
  }

  override def setFocus(): Boolean = {revalidate(); false}


  def intersection(): Unit = {
    val isBracketMode=bracketBut.selected
    showTextLabel("Schnittpunkt von:")
    val viewController=AnswerPanelsData.currentViewController
    viewController.askForObjectSelection(new DefaultObjectSelectListener() {
      override def objectsSelectedWithPoint(obj: ObjectReference, point: VectorConstant, editable: Boolean): Unit = {
        //println("answer 1:"+obj+" "+point)
        viewController.getElementByRef(obj.toObjectReference) match {
          case Some(line1:LineElement)=>
            showTextLabel("Schnittpunkt bis:")
            viewController.askForObjectSelection(new DefaultObjectSelectListener() {
              override def objectsSelectedWithPoint(obj2: ObjectReference, point: VectorConstant, editable: Boolean): Unit = {
                //println("answer :"+obj2+" "+point)
                (viewController.getElementByRef(obj2.toObjectReference) match {
                  case Some(line2:LineElement)=> GraphElemConst.intersectLineLine(line1.toLine3D,line2.toLine3D)
                  case Some(arc:ArcElement)=> GraphElemConst.intersectLineArc(line1,arc,point)
                  case _=> Log.e("Falscher Elementtyp"+obj2.getType);None
                }) match {
                  case Some(res) =>
                    viewController.changeViewportState(ViewportState.AskPoint,withStop = false)
                    if(isBracketMode) AnswerPanelsData.currentViewController.startBracketMode()
                    viewController.setCoordinate(res.x, res.y, res.z)
                  case _ =>
                }

              }
            },"i"+GraphElemConst.lineClassID.toString+","+GraphElemConst.arcClassID.toString)
          case Some(arc:ArcElement)=>
            showTextLabel("Schnittpunkt bis:")
            viewController.askForObjectSelection(new DefaultObjectSelectListener() {
              override def objectsSelectedWithPoint(obj2: ObjectReference, point2: VectorConstant, editable: Boolean): Unit = {
                //println("answer arc :"+obj2+" "+point)
                (viewController.getElementByRef(obj2.toObjectReference) match {
                  case Some(line2:LineElement)=> GraphElemConst.intersectLineArc(line2,arc,point)
                  case Some(arc2:ArcElement)=> GraphElemConst.intersectArcArc(arc,point,arc2)
                  case _=> Log.e("Falscher Elementtyp"+obj2.getType);None
                }) match {
                  case Some(res)=>
                    viewController.changeViewportState(ViewportState.AskPoint,withStop = false)
                    if(isBracketMode) AnswerPanelsData.currentViewController.startBracketMode()
                    viewController.setCoordinate(res.x, res.y, res.z)
                  case _=>
                }
              }
            },"i"+GraphElemConst.lineClassID.toString+","+GraphElemConst.arcClassID.toString)
          case _=>
        }
      }
    }, "i"+GraphElemConst.lineClassID.toString+","+GraphElemConst.arcClassID.toString)
  }
}

object AnswerPanelsData {
	var currentViewController:AbstractViewController[_,_]=_
  val STRICT_HIT = "strict"
  val NOSTRICT_HIT = "nostrict"
}