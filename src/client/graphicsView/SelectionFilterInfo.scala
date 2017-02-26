package client.graphicsView

import definition.typ.AllClasses

case class ElementFilter(name:String,typ:Int){
  override def toString=name
}

class SelectionFilterInfo(val filterElementTypes:Seq[Int],filterWidths:Seq[Int],filterLineStyles:Seq[Int],filterHatches:Seq[Int],val color:Option[Int]) {
  def elementMatch(elem:GraphElem):Boolean={
    color match {
      case Some(c1)=> elem.color==c1
      case None => if(filterElementTypes.nonEmpty&& !filterElementTypes.contains(elem.ref.typ)) false
      else elem match {
        case linElem:LinearElement=>
          if((filterWidths.nonEmpty&& !filterWidths.contains(linElem.lineWidth))||
            (filterLineStyles.nonEmpty&& !filterLineStyles.contains(linElem.lineStyle))) false
          else linElem match {
            case poly:PolyElement if filterHatches.nonEmpty&&
              (! poly.hatchStyle.isDefined || !filterHatches.contains(Math.abs(poly.hatchStyle.get.ix) ))=> false
            case _=> true
          }
        case _=> true
      }
    }

  }
  override def toString="Filter types:"+filterElementTypes.mkString(",")+" widths:"+filterWidths.mkString(",")+" styles:"+filterLineStyles.mkString(",")+" hatches:"+filterHatches.mkString(",")+" color:"+color
}

object SelectionFilterInfo {
   lazy val elemFilters:IndexedSeq[ElementFilter]=getElemFilters
   
   protected def getElemFilters={
     val clInfo=AllClasses.get
     GraphElemFactory.typeMap.keys.map(typeID=>new ElementFilter(clInfo.getClassByID(typeID).description,typeID)).toIndexedSeq
   }
}