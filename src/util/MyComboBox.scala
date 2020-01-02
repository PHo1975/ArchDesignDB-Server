package util

import javax.swing.{AbstractListModel, ComboBoxModel, JComboBox, JComponent}

import scala.swing._
class MyComboBox[A](items: Seq[A]) extends Component with Publisher {
  def nitems:ComboBoxModel[A]=newConstantModel(items)
  override lazy val peer: JComboBox[A] = new JComboBox(nitems) with SuperMixin

  object selection extends Publisher {
    def index: Int = peer.getSelectedIndex
    def index_=(n: Int): Unit = { peer.setSelectedIndex(n) }
    def item: A = peer.getSelectedItem.asInstanceOf[A]
    def item_=(a: A): Unit = { peer.setSelectedItem(a) }

    peer.addActionListener(Swing.ActionListener { e =>
      publish(event.SelectionChanged(MyComboBox.this))
    })
  }
  
  def newConstantModel(items: Seq[A]): ComboBoxModel[A] = {
    new AbstractListModel[A] with ComboBoxModel[A] {
      private var selected: A = if (items.isEmpty) null.asInstanceOf[A] else items.head
      def getSelectedItem: AnyRef = selected.asInstanceOf[AnyRef]
      def setSelectedItem(a: Any): Unit = {
        if ((selected != null && selected != a) ||
            selected == null && a != null) {
          selected = a.asInstanceOf[A]
          fireContentsChanged(this, -1, -1)
        }
      }
      def getElementAt(n: Int) = items(n)
      def getSize = items.size
    }
  }
  
  
  
  def renderer: ListView.Renderer[A] = new ListView.Renderer[A] {
  	  def componentFor(list: ListView[_ <: A], isSelected: Boolean, focused: Boolean, a: A, index: Int) = {
        Component.wrap(peer.getListCellRendererComponent(list.peer, a, index, isSelected, focused).asInstanceOf[JComponent])
      }
  	}
  def renderer_=(r: ListView.Renderer[A]): Unit = { peer.setRenderer(r.peer) }

  
  def editable: Boolean = peer.isEditable

  
  def makeEditable()(implicit editor: MyComboBox[A] => ComboBox.Editor[A]): Unit = {
    peer.setEditable(true)
    peer.setEditor(editor(this).comboBoxPeer)
  }

  def prototypeDisplayValue: Option[A] = toOption(peer.getPrototypeDisplayValue())
  
  
  def toOption(o: A): Option[A] = if(o == null) None else Some(o.asInstanceOf[A])
  def toAnyRef(x: Any): AnyRef = x.asInstanceOf[AnyRef]
}