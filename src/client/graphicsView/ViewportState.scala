/**
 * Author: Peter Started:08.10.2010
 */
package client.graphicsView

/** possible states of the viewport
 * 
 */
object ViewportState extends Enumeration {
  val SelectState=Value("Select")
  val AskPoint=Value("AskPoint")  
  val ChoseObject=Value("Chose")  
  val AskPointOrObject=Value("PointOrObject")
  val InPlaceEdit=Value("InPlaceEdit")
  val SelectPoints=Value("SelectPoints")
  val DragDrop=Value("DragDrop")
}

object ObjectSelectMode extends Enumeration {
  val SingleObject=Value("Single")
  val EdgeAndPoint=Value("EdgePoint")
  val SegmentPart=Value("SegmentPart")
  val TempObject=Value("TempObject")
  val SingleObjectNotSelected=Value("SingleNotSelected")
}

/*object MeasureMode extends Enumeration {
  val NoMeasure=Value("None")
  val Coords=Value("Coords")
  val Distance=Value("Distance")
  val Area=Value("Area")
}*/