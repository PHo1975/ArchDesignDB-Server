package client.search

import javax.swing.AbstractListModel

import scala.collection.mutable.ArrayBuffer

/**
 *  Created by Peter Holzer on 26.03.2017 .
 */
class SearchResultListModel extends AbstractListModel[AbstractSearchResult] {
  val list = new ArrayBuffer[AbstractSearchResult]()

  override def getSize: Int = list.size

  override def getElementAt(index: Int): AbstractSearchResult = list(index)

  def reset(): Unit = {
    list.clear()
    notifyChange()
  }

  def notifyChange(): Unit =
    fireContentsChanged(this, 0, list.size - 1)


  def addSearchResult(res: AbstractSearchResult): Unit = {
    list += res
    notifyChange()
  }
}
