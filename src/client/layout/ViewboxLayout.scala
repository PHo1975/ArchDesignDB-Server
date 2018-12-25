/**
 * Author: Peter Started:06.11.2010
 */
package client.layout

import java.awt.{Component, Container, Dimension, LayoutManager}


/**
 * 
 */
class ViewboxLayout extends LayoutManager {
  
	val nullSize=new Dimension(0,0)
	//val x:GridLayout
	val hgap=1
	val vgap=1
	
	var maximizedBox:Option[Viewbox]=None
	
  def addLayoutComponent(name: String, comp: Component): Unit = { }

  def removeLayoutComponent(comp: Component): Unit = {  }

  def preferredLayoutSize(parent: Container): Dimension = parent.getTreeLock().synchronized{
  	//System.out.println("get pref size "+parent)
  	calcSize(parent,_.preferredSize)
  }
  	  

  def minimumLayoutSize(parent: Container): Dimension = parent.getTreeLock().synchronized{ calcSize(parent,_.minimumSize)}

  def layoutContainer(parent: Container): Unit = 
  parent match {
    case peer: MainBox#MainboxPeer => parent.getTreeLock().synchronized {
      val mainBox = peer.mainBox
      val cBox = mainBox.centerBox
      if (cBox != null) {
        val psize = parent.getSize
        val wishSize = mainBox.peer.getPreferredSize
        val insets = parent.getInsets
        val w = psize.width - (insets.left + insets.right)
        val h = psize.height - (insets.top + insets.bottom)
        val xScale: Double = w / (if (wishSize.width == 0) 1d else wishSize.width.toDouble)
        val yScale: Double = h / (if (wishSize.height == 0) 1d else wishSize.height.toDouble)

        //System.out.println("Layoutcontainer  w:"+w+" h:"+h+" xs:"+xScale+" ys:"+yScale+" wishSize:"+wishSize)
        maximizedBox match {
          case Some(box) => box.peer.setBounds(insets.left, insets.top, w, h)
          case None => layoutBox(cBox, insets.left, insets.top, w, h)
        }


        def scaleDim(dim: Dimension) = new Dimension((dim.width.toDouble * xScale).toInt,
          (dim.height.toDouble * yScale).toInt)

        def layoutBox(box: Viewbox, xoff: Int, yoff: Int, fullWidth: Int, fullHeight: Int): Unit = {
          val bwishSize = box.preferredSize
          var mySize = scaleDim(bwishSize)
          var newSize = scaleDim(bwishSize)
          //System.out.println("layBox "+box.header.label.text+" xoff:"+xoff+" yoff:"+yoff+" fullWidth:"+fullWidth+" fullHeight:"+fullHeight)	  			
          //System.out.println("lay box: wish:"+bwishSize+" nw:"+newSize.width+" nh:"+newSize.height)
          var rightSize: Dimension = null
          if (box.rightEdge.isExpanded) {
            rightSize = scaleDim(box.rightEdge.connectorStripe.get.connectedBox.getFullPrefSize)
            if (!box.rightFirstExpanded && rightSize.height > newSize.height) newSize.height = rightSize.height
          }
          var botSize: Dimension = null
          if (box.bottomEdge.isExpanded) {
            botSize = scaleDim(box.bottomEdge.connectorStripe.get.connectedBox.getFullPrefSize)
            if (box.rightFirstExpanded && botSize.width > newSize.width) newSize.width = botSize.width
            //newSize.height+= botSize.height
          }
          //System.out.println("rightSize:"+rightSize+" botSize:"+botSize)

          // Reacting on scaleValue
          if (box.rightEdge.isExpanded) {
            val scale = if (box.rightEdge.connectorStripe.get.dragScale == -1) box.rightEdge.connectorStripe.get.scaleValue
            else box.rightEdge.connectorStripe.get.dragScale
            if (scale != -1)
              newSize.width = (fullWidth.toDouble * scale).toInt
            else box.rightEdge.connectorStripe.get.defaultScale = newSize.width.toDouble / fullWidth.toDouble
          }

          if (box.bottomEdge.isExpanded) {
            val scale = if (box.bottomEdge.connectorStripe.get.dragScale == -1) box.bottomEdge.connectorStripe.get.scaleValue
            else box.bottomEdge.connectorStripe.get.dragScale
            if (scale != -1)
              newSize.height = (fullHeight.toDouble * scale).toInt
            else box.bottomEdge.connectorStripe.get.defaultScale = newSize.height.toDouble / fullHeight.toDouble
          }


          // set up child boxes
          if (box.rightEdge.isExpanded) {
            if (box.rightFirstExpanded)
              layoutBox(box.rightEdge.connectorStripe.get.connectedBox, xoff + newSize.width + hgap, yoff, fullWidth - newSize.width - hgap, fullHeight)
            else
              layoutBox(box.rightEdge.connectorStripe.get.connectedBox, xoff + newSize.width + hgap, yoff, fullWidth - newSize.width - hgap,
                newSize.height)
          }
          if (box.bottomEdge.isExpanded) {
            if (box.rightFirstExpanded)
              layoutBox(box.bottomEdge.connectorStripe.get.connectedBox, xoff, yoff + newSize.height + vgap, newSize.width,
                fullHeight - newSize.height - vgap)
            else
              layoutBox(box.bottomEdge.connectorStripe.get.connectedBox, xoff, yoff + newSize.height + vgap, fullWidth,
                fullHeight - newSize.height - vgap)
          }
          // set up this box
          if (!box.rightEdge.isExpanded) newSize.width = fullWidth
          if (!box.bottomEdge.isExpanded) newSize.height = fullHeight
          //System.out.println("layBox "+box.header.label.text+" Width:"+newSize.width+" Height:"+newSize.height)
          box.peer.setBounds(xoff, yoff, newSize.width, newSize.height)
        }
      }


    }
    case _ =>
  }
  
  def calcSize(parent: Container,gsize:(scala.swing.Component)=>Dimension): Dimension = 
  parent match {
    case peer: MainBox#MainboxPeer => parent.getTreeLock().synchronized {
      val par = peer.mainBox
      val cBox = par.centerBox
      if (cBox != null) {
        /*val insets = parent.getInsets
        var w: Int = insets.left + insets.right
        var h: Int = insets.top + insets.bottom*/
        cBox.getFullPrefSize
      }
      else nullSize
    }
    case _ => nullSize
  }
  
  

}