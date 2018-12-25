/**
 * Author: Peter Started:23.04.2011
 */
package client.print

import java.awt._

import definition.data.RenderContext
import javax.swing.JComponent

import scala.swing.Component

/**
 * 
 */
class PageViewer(context:RenderContext) extends Component {
  private val nullSize=new Dimension(0,0)
	var pageNr:Int=_	
	var data:APageable=_	
	var prefSize:Dimension=getPrefSize  

	def setData(nd:APageable): Unit = {
  	data=nd
  	updateSize()
  }	
	
	def updateSize(): Unit = {
		prefSize=getPrefSize		
		revalidate()
		repaint()
	}
  
  override lazy val peer=new JComponent with SuperMixin with javax.swing.Scrollable {
  	override def getPreferredSize: Dimension =prefSize
  	override def getMaximumSize: Dimension =prefSize
  	override def getMinimumSize: Dimension =prefSize
  	def getPreferredScrollableViewportSize: Dimension=getPreferredSize
  	def getScrollableTracksViewportHeight: Boolean =false
  	def getScrollableTracksViewportWidth: Boolean=false
  	def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
  	def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10
  }
  
  override def paintComponent(g:Graphics2D): Unit = {
  	super.paintComponent(g)  	
  	g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON ))
  	g.setColor(Color.white)
  	g.fillRect(0,0,prefSize.width,prefSize.height)
  	g.setColor(Color.black)
  	g.drawRect(0,0,prefSize.width-1,prefSize.height-1)
  	g.setColor(Color.blue)
  	g.draw(data.clipRect())
  	g.setColor(Color.black)  	
  	data.print(g, null,pageNr-1)  	
  }
  
  
  
	def getPrefSize: Dimension = if(data==null)nullSize
	  else new Dimension(context.fromMM(data.pageWidth),context.fromMM(data.pageHeight))
}