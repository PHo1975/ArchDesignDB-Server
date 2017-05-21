/**
 * Author: Peter Started:06.10.2010
 */
package client.graphicsView

import java.awt.Color
import java.awt.geom.{Area, Rectangle2D}
import javax.swing.BorderFactory

import client.comm.KeyStrokeManager
import client.dataviewer.{TitlePopupMenu, ViewConstants}
import client.dialog.{AbstractPanelButton, DialogManager}
import definition.data.StyleService
import definition.expression.{PointList, Polygon, VectorConstant}
import definition.typ.{AnswerDefinition, DataType, DialogQuestion}

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scala.swing.event._

class ScalePanel(model:ScaleModel,controller:GraphViewController) extends BoxPanel(scala.swing.Orientation.Horizontal) {
  import client.graphicsView.ScalePanel._
  
  trait AbstractScalePanelButton extends AbstractPanelButton {
    listenTo(this.mouse.clicks)
    reactions+={
			case _: MousePressed => if (!controller.canvasPanel.peer.isFocusOwner()) {
        controller.canvasPanel.requestFocus()
        controller.focusGained()
      } 
    }
  }

  class PanelButton(alabel:String,val commandName:String,val groupName:String) extends Button(alabel) with AbstractScalePanelButton 
  
  class PanelToggleButton(alabel:String,val commandName:String,val groupName:String) extends ToggleButton(alabel) with AbstractScalePanelButton

	lazy val relScaleCombo: ComboBox[(Int, Double)] = {
		//println("create combo "+model.scales.size)
		val scc=new ComboBox[(Int,Double)](ScaleModel.scales.toSeq)
		scc.maximumSize = new Dimension(70 * ViewConstants.fontScale / 100, 30 * ViewConstants.fontScale / 100)
		scc.preferredSize=scc.maximumSize
		scc.font=ViewConstants.smallFont
		scc.renderer = new ListView.AbstractRenderer[(Int, Double), Label](ViewConstants.label()) {
			def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: (Int,Double), index: Int): Unit = {
				val rel=a._2
						component.text=if(rel<1)"1 : "+math.round(1d/rel) else math.round(rel)+" : 1"
			}		
		} 
		scc
	}  
		
	val zoomAllBut=new PanelButton("Alles","Zoom alles",theGroupName) 
	val zoomLassoBut=new PanelButton("[]","Zoom Lasso",theGroupName)
	val newElemBut=new PanelButton("Neu...","Neues Element anlegen",theGroupName)
	val zoomOutBut=new PanelButton("-","Zoom heraus",theGroupName)
	val zoomInBut=new PanelButton("+","Zoom herein",theGroupName)
	val scaleEdit=new TextField("")
	val measureBut=new PanelButton("","Messen",theGroupName)
	val filterBut=new PanelToggleButton("","Filter",theGroupName)
	val colorsFixedBut=new PanelToggleButton("F=S","Farbe=Stift Kopplung",theGroupName)
	
	newElemBut.tooltip="Neues Element anlegen"
	zoomAllBut.tooltip="Gesamte Zeichnung darstellen"	
	zoomLassoBut.tooltip="Rechteckigen Bereich vergrößern"
	zoomOutBut.tooltip="Zurück zur letzten Vergrößerung"
	zoomInBut.tooltip="Hereinzoomen"	
	filterBut.tooltip="Nach Elementtyp filtern"
	scaleEdit.maximumSize = new Dimension(70 * ViewConstants.fontScale / 100, 30 * ViewConstants.fontScale / 100)
	scaleEdit.preferredSize=scaleEdit.maximumSize
	scaleEdit.font=ViewConstants.smallFont		
	//measureBut.icon=measureIcon
	measureBut.tooltip="Messfunktionen"
	measureBut.focusable=false
	val measurePopup=new TitlePopupMenu("Messen")	
	val measureCoordsBut=new MenuItem("Koordinate")
	measureCoordsBut.focusable=false
	val measureDistBut=new MenuItem("Abstand")
	val measureAreaBut=new MenuItem("Fläche")
	measurePopup.add(measureCoordsBut,true)
	measurePopup.add(measureDistBut,true)
	measurePopup.add(measureAreaBut,false)	
	colorsFixedBut.tooltip="Farbe=Stift Kopplung"
	colorsFixedBut.selected=true
	
	var selfRelativeSelected=false
	var selfChanged=false
	
	listenTo(zoomAllBut,zoomLassoBut,zoomOutBut,zoomInBut,relScaleCombo.selection,scaleEdit,measureBut,
	    measureCoordsBut,measureDistBut,measureAreaBut,newElemBut,colorsFixedBut,filterBut)
	
	reactions += {
		case ButtonClicked(`zoomAllBut`)=> controller.zoomAll()
		case ButtonClicked(`zoomLassoBut`)=> controller.zoomInClicked()
		case ButtonClicked(`zoomOutBut`)=> model.zoomOut()
		case ButtonClicked(`zoomInBut`) => model.zoomPlus(0.5d, 0.5d)
		case ButtonClicked(`measureBut`)=> measurePopup.show(measureBut.peer)
		case ButtonClicked(`measureCoordsBut`)=>measureCoords()
		case ButtonClicked(`measureDistBut`)=>measureLength()
		case ButtonClicked(`measureAreaBut`)=>measureArea()
		case ButtonClicked(`newElemBut`)=>Swing.onEDT(controller.showCreatePopup())
		case ButtonClicked(`colorsFixedBut`)=> controller.setColorsFixed(colorsFixedBut.selected)
		case ButtonClicked(`filterBut`)=> controller.filterButClicked(filterBut)
		
		case SelectionChanged(`relScaleCombo`)=> if(selfRelativeSelected) selfRelativeSelected=false
			else if(relScaleCombo.selection.index> -1)				
				controller.setActiveLayerScale(relScaleCombo.selection.item._1)				
		
		case EditDone(`scaleEdit`)=> if(!selfChanged){
			val scaleValue=splitScaleText(scaleEdit.text)
			if(scaleValue!=null) model.setScaleRatio(scaleValue._1,scaleValue._2)		
		}		
	}
	
	model.registerScaleListener(()=>{
	  selfChanged=true
		val sc=model.getScaleRatio
		scaleEdit.text = scaleToText(sc._1) + " : " + scaleToText(sc._2)
		selfRelativeSelected=true
		val rm=1d/model.relativeScaleValue		
		val ix=ScaleModel.scales.valuesIterator.indexWhere(_==rm)
		relScaleCombo.selection.index=ix
		selfRelativeSelected=false
		selfChanged=false
	})
	contents += newElemBut += zoomAllBut += zoomLassoBut += zoomInBut += zoomOutBut += Swing.HGlue += filterBut += colorsFixedBut += measureBut += Swing.HGlue += ViewConstants.label("Anzeige: ") +=
		scaleEdit += Swing.HStrut(20) += ViewConstants.label("Bezug: ") += relScaleCombo

	def activateKeyStrokes(): Unit = {
	  KeyStrokeManager.registerReceiver(zoomAllBut)
	  KeyStrokeManager.registerReceiver(zoomInBut)
	  KeyStrokeManager.registerReceiver(zoomOutBut)
	  KeyStrokeManager.registerReceiver(zoomLassoBut)
	  KeyStrokeManager.registerReceiver(measureBut)
	  KeyStrokeManager.registerReceiver(newElemBut)
	  KeyStrokeManager.registerReceiver(filterBut)
	}

	def measureCoords(): Unit = {
		controller.requestFocus()
		val xLabel = ViewConstants.label()
		val yLabel = ViewConstants.label()
		//val sm=controller.scaleModel

	  
	  val panel=new BoxPanel(Orientation.Vertical){	     
	     contents+=xLabel+=yLabel
	     background=Color.white
	     border=BorderFactory.createCompoundBorder(
	         BorderFactory.createLineBorder(Color.LIGHT_GRAY,2),
	         BorderFactory.createEmptyBorder(5,5,5,5))
			preferredSize = new Dimension(200 * ViewConstants.fontScale / 100, 50 * ViewConstants.fontScale / 100)
    }
	  val toast=controller.createDraggerToast((ntoast,x,y,worldPos)=>{
	    xLabel.text="X: "+worldPos.x
	    yLabel.text="Y: "+worldPos.y
	    ntoast.updatePos(x+10, y+1)
	  })
	  
	  toast.setContent(panel)
		DialogManager.startInterQuestion(GraphCustomQuestionHandler.singlePointQuestion("Koordinaten messen", "Punkt berühren", Some(true)), (answerList) => {
      DialogManager.resetDraggerToast()  },false)
	}

	def measureLength(): Unit = {
		controller.requestFocus()
		val dxLabel = ViewConstants.label()
		val dyLabel = ViewConstants.label()
		val angleLabel = ViewConstants.label()
		val lengthLabel = ViewConstants.label()
	  val sm=controller.scaleModel
	  var lastPos: VectorConstant=null
	  dxLabel.font=ViewConstants.tableFont	     
	  dyLabel.font=ViewConstants.tableFont
    angleLabel.font=ViewConstants.tableFont
	  
	  val panel=new BoxPanel(Orientation.Vertical){	     
	     contents+=lengthLabel+=dxLabel+=dyLabel+=angleLabel
	     background=Color.white
	     border=BorderFactory.createCompoundBorder(
	         BorderFactory.createLineBorder(Color.LIGHT_GRAY,2),
	         BorderFactory.createEmptyBorder(5,5,5,5))
			preferredSize = new Dimension(200 * ViewConstants.fontScale / 100, 82 * ViewConstants.fontScale / 100)
    }

		val nextPointQuestion = DialogQuestion("Strecke messen", Seq(new AnswerDefinition("nächster Punkt", DataType.VectorTyp, None)), true)
		val question = DialogQuestion("Strecke messen", Seq(new AnswerDefinition("StartPunkt", DataType.VectorTyp, None)))
	  
	  def dragger(pos:VectorConstant,g:Graphics2D)= {
	    g.setColor(Color.blue)
	    GraphElemConst.drawLineFloat(g,sm.xToScreen(lastPos.x), sm.yToScreen(lastPos.y), sm.xToScreen(pos.x), sm.yToScreen(pos.y))
	  }
	  	  
    DialogManager.startInterQuestion(question,(answerList)=>{      
      lastPos=answerList.last.result.toVector      
      val toast=controller.createDraggerToast((ntoast,x,y,worldPos)=>{
		    lengthLabel.text=f"${(worldPos-lastPos).toDouble}%12.12f m"
		    dxLabel.text=f"DX: ${worldPos.x-lastPos.x}%12.12f"
		    dyLabel.text=f"DY: ${worldPos.y-lastPos.y}%12.12f"
        val angle=math.atan2(worldPos.y-lastPos.y, worldPos.x-lastPos.x)*180d/math.Pi
        angleLabel.text="Winkel:"+anglePattern.format(angle)+"("+anglePattern.format(
            if (angle < 0) angle + 180 else angle - 180) +")"
		    ntoast.updatePos(x+10, y+1)
		  })
      toast.setContent(panel)
      controller.setCustomDragger(dragger)
      DialogManager.startInterQuestion(nextPointQuestion,(answerlist)=>{        
        lastPos=answerList.last.result.toVector
        //println("LastPos :"+lastPos+" answerList.size:"+answerList.size)
        controller.setCustomDragger(dragger)
      },false)
    },false)
	}


	def measureArea(): Unit = {
		controller.requestFocus()
		val areaLabel = ViewConstants.label()
		val umfangLabel = ViewConstants.label()
    val sm=controller.scaleModel
    val transform=GraphElemConst.transform(sm) _
    val points=ArrayBuffer[VectorConstant]()
    val noSize=new Dimension(0,0)
    val panSize=new Dimension(200,50)
    var showPanel=false
    val dragColor=StyleService.getAlphaColor(Color.blue.brighter.brighter.getRGB)
    //var shape:Shape=null
    
    areaLabel.font=ViewConstants.tableFont       
    umfangLabel.font=ViewConstants.tableFont    
    
    val panel=new BoxPanel(Orientation.Vertical){      
       contents+=areaLabel+=umfangLabel
       background=Color.white
       border=BorderFactory.createCompoundBorder(
           BorderFactory.createLineBorder(Color.LIGHT_GRAY,2),
           BorderFactory.createEmptyBorder(5,5,5,5))
       preferredSize=panSize
    }

		val nextPointQuestion = DialogQuestion("Fläche messen", Seq(new AnswerDefinition("nächster Punkt", DataType.VectorTyp, None)), true)
		val question = DialogQuestion("Fläche messen", Seq(new AnswerDefinition("StartPunkt", DataType.VectorTyp, None)))
    
    def dragger(pos:VectorConstant,g:Graphics2D)= {
      val outShape= if(points.size==1) createRect(points.head,pos) else
        if(points.size>1) createPolyShape(points:+pos) else null
      if(outShape!=null){       
        g.setColor(Color.blue)
        g.draw(outShape)
      }
      val shape=if(points.size==2) createRect(points.head,points(1))
      else if(points.size>2) createPolyShape(points)
      else null
      if(shape!=null){
        g.setPaint(dragColor)
        g.fill(shape)        
      }       
      showPanel= shape!=null
    } 
    
    def createRect(p1:VectorConstant,p2:VectorConstant)= {
      val t1=transform(p1)
      val t2=transform(p2)
      new Rectangle2D.Double(t1.x,t1.y,t2.x-t1.x,t2.y-t1.y)
    }
    
    def createPolyShape(spoints:Seq[VectorConstant])= {
			val plist = PointList(spoints)
      new Area(new Polygon(Nil,List(plist)).toPathTransformed(transform))
    }
        
    DialogManager.startInterQuestion(question,(answerList)=>{
      points+=answerList.last.result.toVector                  
      val toast=controller.createDraggerToast((ntoast,x,y,worldPos)=>{                
        ntoast.updatePos(x+10, y+1)
        ntoast.visible=showPanel
      })
      toast.setContent(panel)
      controller.setCustomDragger(dragger)
      DialogManager.startInterQuestion(nextPointQuestion,(answerlist)=>{
        val newPoint=answerList.last.result.toVector
        points+=newPoint
        //println("click:"+answerList.last.result+"  points: "+points.mkString(","))
        val (area,umfang)=if(points.size==2) {
          val p1=points.head
          val delta= points(1) - p1 
          //println("delta:"+delta)
          //shape=createRect(p1,points(1))                    
          (Math.abs(delta.x*delta.y),(Math.abs(delta.x)+Math.abs(delta.y))*2)
        } else if(points.size>2){
					val plist = PointList(points)
          val areaPoly=new Polygon(Nil,List(plist))
          //shape=new Area(areaPoly.toPathTransformed ( transform ))                    
          (Math.abs(areaPoly.getAreaValue),plist.getUmfang)
        } else {
          //shape= null
          (0d,0d)}
        areaLabel.text=f"Fläche: $area%.8f m2"
        umfangLabel.text=f"Umfang: $umfang%.6f m"
        controller.setCustomDragger(dragger)  
      },false)
    },false)
  }

		
}



object ScalePanel{
  val buttonSizeVariant="JComponent.sizeVariant"
  val buttonSizeSmall="small"
  val anglePattern="%4.4f"
	
  val theGroupName="Zeichnung-Werkzeuge"
  //val measureIcon=LayerPanelController.createImageIcon("measure.png")
  
  def scaleToText(sc:Number):String= sc match {
		case a:Integer => a.toString		
		case b:java.lang.Double => if(b==1d) "1" else  f"$b%4.0f"
		case c=> "u "+c
	}
	
	def splitScaleText(text:String):(Double,Double) = {
		val nseq=text.split(":")
		if(nseq.size!=2) null
		else {
			(nseq(0).trim.replace(',','.').toDouble,nseq(1).trim.replace(',','.').toDouble)
		}
	}		
}