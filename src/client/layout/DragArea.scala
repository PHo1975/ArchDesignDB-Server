/**
 * Author: Peter Started:10.11.2010
 */
package client.layout

import java.awt.{Cursor, Point}

import javax.swing.SwingUtilities

import scala.swing._
import scala.swing.event._
/**
 * 
 */
class DragArea(isHorizontal:Boolean,stripe:ConnectorStripe) extends Component {
	listenTo(mouse.clicks,mouse.moves)
	var dragStartPoint:Point=_

	reactions += {
		case e:MousePressed =>
			SwingUtilities.convertPointToScreen(e.point,peer)
			dragStartPoint= e.point

		case e:MouseReleased =>
			dragStartPoint=null
			stripe.dragStopped()

		case e: MouseDragged => if(dragStartPoint!=null) {
			SwingUtilities.convertPointToScreen(e.point,peer)
			val newPos=e.point
			val delta=if(isHorizontal) newPos.y-dragStartPoint.y else newPos.x-dragStartPoint.x
			stripe.dragTo(delta)			
		}
	}
	cursor=if(isHorizontal) Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR)
		else Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR)
	maximumSize=DragArea.prefSize	
	
}

object DragArea {
	val prefSize=new Dimension(Short.MaxValue,Short.MaxValue)
}