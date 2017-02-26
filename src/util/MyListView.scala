

package util

import scala.swing.event.ListSelectionChanged
import javax.swing._
import javax.swing.event._
import scala.swing.Component
import scala.swing.Publisher
import java.awt.Color

object MyListView {
  /**
   * The supported modes of user selections.
   */
  object IntervalMode extends Enumeration {
    val Single = Value(ListSelectionModel.SINGLE_SELECTION)
    val SingleInterval = Value(ListSelectionModel.SINGLE_INTERVAL_SELECTION)
    val MultiInterval = Value(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
  }

  def wrap[A](c: JList[A]) = new MyListView[A] {
    override lazy val peer = c
  }

  object Renderer {
    def wrap[A](r: ListCellRenderer[A]): Renderer[A] = new Wrapped[A](r)

    /**
     * Wrapper for <code>javax.swing.ListCellRenderer<code>s
     */
  	class Wrapped[A](override val peer: ListCellRenderer[A]) extends Renderer[A] {
  	  def componentFor(list: MyListView[A], isSelected: Boolean, focused: Boolean, a: A, index: Int) = {
        Component.wrap(peer.getListCellRendererComponent(list.peer, a, index, isSelected, focused).asInstanceOf[JComponent])
      }
  	}

    
  }

  /**
   * Item renderer for a list view. This is contravariant on the type of the
   * items, so a more general renderer can be used in place of a more specific
   * one. For instance, an <code>Any</code> renderer can be used for a list view
   * of strings.
   *
   * @see javax.swing.ListCellRenderer
   */
  abstract class Renderer[A] {
    class IntR extends ListCellRenderer[A] {
      def getListCellRendererComponent(list: JList[_ <: A], a: A, index: Int, isSelected: Boolean, focused: Boolean) =
        componentFor(MyListView.wrap[A](list.asInstanceOf[JList[A]]), isSelected, focused, a.asInstanceOf[A], index).peer
    }
    def peer:ListCellRenderer[A] = new IntR 
    def componentFor(list: MyListView[A], isSelected: Boolean, focused: Boolean, a: A, index: Int): Component
  }

  /**
   * A default renderer that maintains a single component for item rendering
   * and preconfigures it to sensible defaults. It is polymorphic on the
   * component's type so clients can easily use component specific attributes
   * during configuration.
   */
  abstract class AbstractRenderer[A, C<:Component](protected val component: C) extends Renderer[A] {
    // The renderer component is responsible for painting selection
    // backgrounds. Hence, make sure it is opaque to let it draw
    // the background.
    component.opaque = true

    /**
     * Standard preconfiguration that is commonly done for any component.
     * This includes foreground and background colors, as well as colors
     * of item selections.
     */
    def preConfigure(list: MyListView[A], isSelected: Boolean, focused: Boolean, a: A, index: Int): Unit = {
      if (isSelected) {
        component.background = list.selectionBackground
        component.foreground = list.selectionForeground
      } else {
        component.background = list.background
        component.foreground = list.foreground
      }
    }
    /**
     * Configuration that is specific to the component and this renderer.
     */
    def configure(list: MyListView[A], isSelected: Boolean, focused: Boolean, a: A, index: Int): Unit

    /**
     * Configures the component before returning it.
     */
    def componentFor(list: MyListView[A], isSelected: Boolean, focused: Boolean, a: A, index: Int): Component = {
      preConfigure(list, isSelected, focused, a, index)
      configure(list, isSelected, focused, a, index)
      component
    }
  }

  /**
   * A generic renderer that uses Swing's built-in renderers. If there is no
   * specific renderer for a type, this renderer falls back to a renderer
   * that renders the string returned from an item's <code>toString</code>.
   */
  implicit object GenericRenderer extends Renderer[Object] {
    override lazy val peer: ListCellRenderer[Object] = new DefaultListCellRenderer
    def componentFor(list: MyListView[Object], isSelected: Boolean, focused: Boolean, a: Object, index: Int): Component = {
      val c = peer.getListCellRendererComponent(list.peer, a, index, isSelected, focused).asInstanceOf[JComponent]
      Component.wrap(c)
    }
  }
}

/**
 * A component that displays a number of elements in a list. A list view does
 * not support inline editing of items. If you need it, use a table view instead.
 *
 * Named <code>ListView</code> to avoid a clash with the frequently used
 * <code>scala.List</code>
 *
 * @see javax.swing.JList
 */
class MyListView[A] extends Component {
  import util.MyListView._
  override lazy val peer: JList[A] = new JList[A] with SuperMixin

  def this(items: Seq[A]) = {
    this()
    listData = items
  }

  protected class ModelWrapper(val items: Seq[A]) extends AbstractListModel[A] {
    def getElementAt(n: Int) = items(n)//.asInstanceOf[AnyRef]
    def getSize = items.size
  }

  def listData: Seq[A] = peer.getModel match {
    case model: ModelWrapper => model.items
    case model @ _ => new Seq[A] { selfSeq =>
     def length = model.getSize
     def iterator = new Iterator[A] {
       var idx = 0
       def next() = { idx += 1; apply(idx-1) }
       def hasNext = idx < selfSeq.length
     }
     def apply(n: Int) = model.getElementAt(n).asInstanceOf[A]
    }
  }

  def listData_=(items: Seq[A]): Unit = {
    peer.setModel(new AbstractListModel[A] {
      def getElementAt(n: Int) = items(n)//.asInstanceOf[AnyRef]
      def getSize = items.size
    })
  }

  /**
   * The current item selection.
   */
  object selection extends Publisher {
    protected abstract class Indices[B](a: =>Seq[B]) extends scala.collection.mutable.Set[B] {
      def -=(n: B): this.type
      def +=(n: B): this.type
      def contains(n: B) = a.contains(n)
      override def size = a.length
      def iterator = a.iterator
    }

    def leadIndex: Int = peer.getSelectionModel.getLeadSelectionIndex
    def anchorIndex: Int = peer.getSelectionModel.getAnchorSelectionIndex

    /**
     * The indices of the currently selected items.
     */
    object indices extends Indices(peer.getSelectedIndices.toSeq) {
      def -=(n: Int): this.type = { peer.removeSelectionInterval(n,n); this }
      def +=(n: Int): this.type = { peer.addSelectionInterval(n,n); this }
    }

    /**
     * The currently selected items.
     */
    //object items extends scala.collection.SeqProxy[A] {
      def items = peer.getSelectedValuesList
    //}

    def intervalMode: IntervalMode.Value = IntervalMode(peer.getSelectionModel.getSelectionMode)
    def intervalMode_=(m: IntervalMode.Value): Unit = { peer.getSelectionModel.setSelectionMode(m.id) }

    peer.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: javax.swing.event.ListSelectionEvent): Unit = {
        publish(new ListSelectionChanged(null, e.getFirstIndex to e.getLastIndex, e.getValueIsAdjusting))
      }
    })

    def adjusting = peer.getSelectionModel.getValueIsAdjusting
  }

  def renderer = MyListView.Renderer.wrap(peer.getCellRenderer)
  def renderer_=(r: MyListView.Renderer[A]): Unit = { peer.setCellRenderer(r.peer) }

  def fixedCellWidth = peer.getFixedCellWidth
  def fixedCellWidth_=(x: Int) = peer.setFixedCellWidth(x)

  def fixedCellHeight = peer.getFixedCellHeight
  def fixedCellHeight_=(x: Int) = peer.setFixedCellHeight(x)

  def prototypeCellValue: A = peer.getPrototypeCellValue

  def prototypeCellValue_=(a: A): Unit = { peer.setPrototypeCellValue(a) }

  def visibleRowCount = peer.getVisibleRowCount
  def visibleRowCount_=(n: Int) = peer.setVisibleRowCount(n)

  def ensureIndexIsVisible(idx: Int) = peer.ensureIndexIsVisible(idx)

  def selectionForeground: Color = peer.getSelectionForeground
  def selectionForeground_=(c: Color) = peer.setSelectionForeground(c)
  def selectionBackground: Color = peer.getSelectionBackground
  def selectionBackground_=(c: Color) = peer.setSelectionBackground(c)

  def selectIndices(ind: Int*) = peer.setSelectedIndices(ind.toArray)

  
}
