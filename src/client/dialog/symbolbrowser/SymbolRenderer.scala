package client.dialog.symbolbrowser

import java.awt.geom.Rectangle2D
import java.awt.{Color, RenderingHints}

import client.dialog.RenderComponent
import client.graphicsView.symbol.SymbolStamp
import client.graphicsView.{ElemContainer, GraphElem, GraphElemConst, ScaleModel}
import client.plotdesign.LayerRef
import client.ui.ViewConstants
import definition.expression.{NULLVECTOR, VectorConstant}
import javax.swing.BorderFactory

import scala.swing.{Alignment, BorderPanel, Component, Label}

class SymbolRenderer extends BorderPanel with RenderComponent[SymbolStamp] {
  
  val symbolPreview=new SymbolPreview
  val label=new Label  
  label.horizontalAlignment=Alignment.Left 
  label.horizontalTextPosition=Alignment.Left
  label.font=ViewConstants.smallFont
  label.opaque=true
  label.background=Color.white
  border=BorderFactory.createLineBorder(Color.darkGray)
  opaque=true
  preferredSize=SymbolBrowserController.previewSize
  //minimumSize=SymbolBrowserController.previewSize  
  
  add(symbolPreview,BorderPanel.Position.Center)
  add(label,BorderPanel.Position.North)   
   
   def setStyle(stamp:SymbolStamp)={
     symbolPreview.setStamp(Some(stamp))
     label.text=stamp.name     
     label.background=background
     label.foreground=foreground
     peer.setToolTipText(stamp.name)
   }
   
   def setEmpty()= {
     symbolPreview.setStamp(None)
     label.text=""
     label.background=background
     peer.setToolTipText("")
   }
   
   
  
}

class SymbolPreview extends Component with ElemContainer {
  var currentStamp:Option[SymbolStamp]=None
  var graphElems:Seq[GraphElem]=Nil
  val elBounds=new Rectangle2D.Double
  val scaleModel=new ScaleModel
  val refCrossSize=7
  var offset:VectorConstant=NULLVECTOR
  val refCrossColor=Color.red.darker
  
  def setStamp(stamp:Option[SymbolStamp])= {
    currentStamp=stamp
    stamp match {
      case Some(st)=>
        graphElems=st.generateElements(Map.empty,0)
        LayerRef.calcWorldBounds(graphElems, this, elBounds)
        scaleModel.setWorldBounds(elBounds.x, elBounds.y, elBounds.width, elBounds.height)
      case None=> graphElems=Nil
    }
    
  }
  opaque=true
  scaleModel.viewSize=SymbolBrowserController.thumbSize
  
  override def paintComponent(g: java.awt.Graphics2D): Unit = {
    super.paintComponent(g)
    g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
    g.setColor(background)
    g.fill(g.getClip)
    g.setColor(Color.black)    
    for(stamp<-currentStamp){
      for(el<-graphElems){
        el.drawWithOffset(g, scaleModel, Color.black,offset)
      }
    }
    g.setColor(refCrossColor)
    g.setStroke(SymbolBrowserController.refCrossStroke)
    val crossX=scaleModel.xToScreen(0)
    val crossY=scaleModel.yToScreen(0)
    GraphElemConst.drawLineFloat(g,crossX-refCrossSize, crossY,crossX+refCrossSize, crossY)
    GraphElemConst.drawLineFloat(g,crossX,crossY-refCrossSize,crossX,crossY+refCrossSize)
    //g.drawString(f"x:${elBounds.x}%2.2f y:${elBounds.y}%2.2f",0,20)
    //g.drawString(f"w:${elBounds.width}%2.2f h: ${elBounds.height}%2.2f",0,40)
  }
  
  def scaleRatio:Double=1d
}