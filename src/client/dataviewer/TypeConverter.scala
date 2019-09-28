package client.dataviewer

import scala.collection.immutable.HashMap

object TypeConverter {
  val convertRules: HashMap[(Int,Int),ConvertRule]=HashMap(
    (162,133)->new ConvertRule("Rechnungspos zu LVpos",Array((2,1),(3,2),(4,3),(7,5),(8,6))),
    (163,135)->new ConvertRule("Rechnungsgewerk zu LV-Gewerk",Array((2,1),(3,2))))

}

class ConvertRule (val name:String,val fieldList:Array[(Int,Int)])
