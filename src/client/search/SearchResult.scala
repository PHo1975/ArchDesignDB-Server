package client.search

import definition.data.InstanceData

/**
 * Created by Peter Holzer on 26.03.2017 .
 */
sealed trait AbstractSearchResult

object SearchFinished extends AbstractSearchResult {
  override def toString = "-- Suche beendet --"
}

case class SearchResult(tree: Seq[InstanceData], item: InstanceData) extends AbstractSearchResult {
  override def toString: String = tree.mkString(" / ") + " / " + item
}
