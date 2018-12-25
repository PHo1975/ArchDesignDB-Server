package server.print

import java.awt.Color
import java.awt.font.{LineBreakMeasurer, TextAttribute}
import java.awt.geom.Rectangle2D
import java.text.AttributedString

import definition.data._
import definition.typ.AllClasses
import server.storage.StorageManager


case class ColHeader(data:InstanceData) {
  import server.print.PreisspiegelGenerator._
  def ref: Reference =data.ref
  def summe: Double =data.fieldValue.head.toDouble

  val props: Option[InstanceProperties] =StorageManager.getInstanceProperties(ref)
  val headerObj: Option[InstanceData] = props.flatMap(pdata=>
    pdata.propertyFields(1).propertyList.headOption.map(StorageManager.getInstanceData))
  val firstLine: String =headerObj match{
    case Some(obj)=>obj.toString
    case None=>"Kein Headerobjekt"
  }
  val secondLine: String =headerObj match {
    case Some(inst) if inst.ref.typ == adressType =>inst.fieldValue(5).toString
    case _=> ""
  }
  protected val headerRefs=new Array[Reference](3)

  def matchRef(searchRef:Reference,level:Int):Boolean = if(ref==searchRef)true else{
    var result=false
    for(r<-headerRefs;if r!=null&& r==searchRef) result =true
    result
  }

  def setTitleRef(tref:Reference,level:Int): Unit =headerRefs(level)=tref
}



class PreisspiegelGenerator extends CustomGenerator {
  import server.print.PreisspiegelGenerator._

  val moneyForm="%,.2f€"
  val lightGray=new Color(245,245,245)
  val showDiffPrice=500d
  val printHeaderAdresses:Boolean=true

  def kurzeText(text:String):String = if(text.length>150)text.substring(0,75)+" [..] "+text.substring(text.length-75,text.length) else text

  def fillPlotPage(dataParent:InstanceData,dataEater:DataEater,generator:SinglePageGenerator): Unit = {
    generator.decoratePage(dataParent, dataEater, dataEater.currentContext)

    StorageManager.getInstanceProperties(dataParent.ref) match {
      case Some(propList)=>
        //val lvTitle=dataParent.fieldValue(2).toString
        val mwst=dataParent.fieldValue(3).toDouble
        val colHeaders=propList.propertyFields(1).propertyList.map(r=>ColHeader(StorageManager.getInstanceData(r)))
        val standardFont=generator.form.fonts.getStyle("Standard")
        val smallFont=generator.form.fonts.getStyle("Klein")
        val microFont=new FontStyle("Micro",smallFont.fontName,smallFont.height*0.8f,false,false,false,Color.gray)
        val uberFont=generator.form.fonts.getStyle("Überschrift")
        val fettFont=generator.form.fonts.getStyle("Fett")
        var currYPos=dataEater.form.top+topMargin
        val cx=dataEater.currentXPos

        def printText(text:String,x:Float,y:Float,width:Float,fontStyle:FontStyle,textStyle:Int=0)=
          dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(x, y, width, fontStyle.height), text.trim, fontStyle.fontName,
            fontStyle.graphStyle + textStyle, 0, 0, Color.black, 0))

        def printLine(x:Float,y:Float,width:Float,height:Float,thick:Float): Seq[PrintElement] =
          dataEater.addPrintElement(LinePrintElement(new Rectangle2D.Float(x, y, width, height), thick, 0, Color.black))

        def fillRect(x:Float,y:Float,width:Float,height:Float,col:Color)=
          dataEater.addPrintElement(FillPrintElement(new Rectangle2D.Float(x, y, width, height), Color.WHITE, 0, col))

        def printHeader(): Unit = {
          printText("Pos.",cx,currYPos,posWidth,standardFont)
          printText("Langtext",cx+posWidth+3,currYPos,langTextWidth,standardFont)
          printText("Menge",cx+posWidth+langTextWidth+3,currYPos,mengeWidth,standardFont)
          printLine(cx,currYPos+1,dataEater.pageWidth,0,0.5f)
          val lineHeight=dataEater.pageHeight-currYPos-dataEater.form.bottom+5f
          printLine(cx+posWidth-1,currYPos-11,0,lineHeight,0.5f)
          printLine(cx+posWidth+langTextWidth,currYPos-11,0,lineHeight,0.5f)
          printLine(cx+leftWidth,currYPos-11,0,lineHeight,1f)
          for(ix<-colHeaders.indices;header=colHeaders(ix)){
            val colX=cx+leftWidth+ix*colWidth
            if(printHeaderAdresses) {
              printText(header.firstLine, colX + 1, currYPos - 8f, colWidth - 2, standardFont)
              printText(header.secondLine, colX + 1, currYPos - 4f, colWidth - 2, smallFont)
            }
            printText("EP",colX+6,currYPos,epWidth,standardFont)
            printText("GP",colX+8+epWidth,currYPos,gpWidth,standardFont)
            printLine(colX+colWidth,currYPos-11,0,lineHeight,0.5f)
            printLine(colX+epWidth,currYPos-3,0,4,0.5f)
          }
          currYPos+=6
        }

        def findHeaderIx(searchRef:Reference,level:Int):Option[Int]={
          for(ix<-colHeaders.indices;ch=colHeaders(ix)) if(ch.matchRef(searchRef,level))return Some(ix)
          None
        }

        def printBlock(wishHeight:Float)(block: =>Unit): Unit = {
          val restHeight=dataEater.pageHeight-currYPos-dataEater.form.bottom-8f
          if(restHeight<wishHeight) {
            dataEater.addPage()
            generator.decoratePage(dataParent, dataEater, dataEater.currentContext)
            currYPos=dataEater.form.top+topMargin
            printHeader()
          }
          block
          currYPos+=wishHeight
        }

        def substNull(value:Double)=if(value==0d) Short.MaxValue.toDouble else value

        def splitText(text:String,bwidth:Float):Seq[String]= {
          val lineBuffer=new collection.mutable.ArrayBuffer[String]()
          for(part<-text.split("\n"))
            if(part.length==0)lineBuffer+="" else {
              val as=new AttributedString(part,smallFont.font.getAttributes)
              as.addAttribute(TextAttribute.FONT, smallFont.font)
              val measurer=new LineBreakMeasurer(as.getIterator,FontStyle.fontRenderCtx)
              var lastPos=0
              while(measurer.getPosition<part.length){
                measurer.nextLayout(bwidth)
                lineBuffer+=part.substring(lastPos,measurer.getPosition)
                lastPos=measurer.getPosition
              }
            }
          lineBuffer
        }


        def printPos(data:InstanceData,level:Int): Seq[PrintElement] ={
          val pos=data.fieldValue(1).toString
          val menge=f"${data.fieldValue(3).toDouble}%,.3f "+data.fieldData(3).getValue.toUnitNumber.unitFraction.toString
          val langtext=kurzeText(data.fieldValue(5).toString)
          val langtexts=splitText(langtext,FormDescription.fromMM(langTextWidth-6f))
          val t=data.fieldValue(2).toInt
          val preisList: IndexedSeq[InstanceData] =StorageManager.getInstanceProperties(data.ref).map(_.propertyFields(0).propertyList.map(StorageManager.getInstanceData)) match {
            case Some(l)=>l
            case None => IndexedSeq.empty
          }
          val lowestPrice = if(preisList.isEmpty||t>0)-1 else preisList.map(pe=>substNull(pe.fieldValue.head.toDouble)).min
          val commentWidth=FormDescription.fromMM(gpWidth+epWidth -6f)
          var notesInfo:Seq[(Int,Seq[String])]=Nil


          printBlock(4f){
            printText(pos,cx+posWidth-3,currYPos+2,0f,standardFont,PrintElement.orient_Right)
            if(langtexts.nonEmpty)printText(langtexts.head,cx+posWidth,currYPos+2,langTextWidth,smallFont)
            printText(menge,cx+posWidth+langTextWidth+mengeWidth-2,currYPos+2,0f,smallFont,PrintElement.orient_Right)
            notesInfo=(for(pdata<-preisList) yield {
              (for(ix<-findHeaderIx(pdata.owners(1).ownerRef,level)) yield{
                val ep=moneyForm.format(pdata.fieldValue(1).toDouble)
                val gpPreis=pdata.fieldValue.head.toDouble
                //print(" "+gpPreis+" "+(gpPreis==lowestPrice)+" ")
                val gp=if(t>0) "nur EP" else moneyForm.format(gpPreis)
                var note=pdata.fieldValue(2).toString.trim//.replace("\n","").trim
                val colX=cx+leftWidth+ix*colWidth
                if(gpPreis==lowestPrice) fillRect(colX+epWidth+5,currYPos-1.5f,gpWidth-5.5f,4f,lightGray)
                else if ((gpPreis-lowestPrice)>showDiffPrice) note="(+"+moneyForm.format(gpPreis-lowestPrice)+")\n"+note
                printText(ep,colX+epWidth-1,currYPos+2,0,standardFont,PrintElement.orient_Right)
                printText(gp,colX+colWidth-2,currYPos+2,0,standardFont,PrintElement.orient_Right)
                if(note.isEmpty)None
                else Some((ix,splitText(note,commentWidth)))
              }).flatten
            }).flatten

          }
          val numLines=math.max(langtexts.size-1,if(notesInfo.isEmpty)0 else notesInfo.maxBy { case (_, list) => list.size }._2.size)
          for(line<-0 until numLines)
            printBlock(3f){
              if(line<langtexts.length-1)
                printText(langtexts(line+1),cx+posWidth,currYPos+1,langTextWidth,smallFont)
              for((ix,noteLines)<-notesInfo;if line < noteLines.size){
                val colX=cx+leftWidth+ix*colWidth
                val ntext=noteLines(line)
                if(ntext.startsWith("(+"))printText(ntext,colX+colWidth-2,currYPos+2,0,microFont,PrintElement.orient_Right)
                else printText(ntext,colX+1,currYPos+2,colWidth-2,smallFont)
              }
            }
          printBlock(1.5f){}
          printLine(cx,currYPos-2,dataEater.pageWidth-cx-dataEater.form.right,0f,0.2f)
        }


        def printTitel(data:InstanceData,level:Int,superOZ:String): Unit = {
          val oz=data.fieldValue(1).toString
          val name=data.fieldValue(2).toString
          val summenRefs=StorageManager.getInstanceProperties(data.ref).map(
            _.propertyFields(0).propertyList.map(StorageManager.getInstanceData)) match {
            case Some(list)=>list;
            case None=>IndexedSeq.empty
          }
          for(su<-summenRefs;ix<-findHeaderIx(su.owners(1).ownerRef,level);ch=colHeaders(ix))
            ch.setTitleRef(su.ref,level+1)

          printBlock(8f){
            printText(if(superOZ.isEmpty) oz else superOZ+"."+oz,cx,currYPos+2,posWidth,uberFont)
            printText(name,cx+posWidth,currYPos+2,langTextWidth-1,uberFont)
          }
          StorageManager.getInstanceProperties(data.ref) match {
            case Some (tpropData)=> for(pref<-tpropData.propertyFields(1).propertyList){
              pref.typ match {
                case `posType`=> printPos(StorageManager.getInstanceData(pref),level+1)
                case `titelType`=> printTitel(StorageManager.getInstanceData(pref),level+1,oz)
              }
            }
            case None =>
          }

          printBlock(11f){
            printText("Summe "+name+":",cx+posWidth,currYPos+3,langTextWidth-1,standardFont)
            for(su<-summenRefs;ix<-findHeaderIx(su.owners(1).ownerRef,level)){
              val colX=cx+leftWidth+ix*colWidth
              printLine(colX+epWidth+1,currYPos-2.5f,gpWidth-2f,0,0.5f)
              printText(moneyForm.format(su.fieldValue.head.toDouble),colX-2+colWidth,currYPos+3,0f,standardFont,PrintElement.orient_Right)
            }
          }

        }


        printHeader()
        for (ref<-propList.propertyFields(2).propertyList) {
          val data=StorageManager.getInstanceData(ref)
          if(ref.typ==posType) printPos(data,0)
          if(ref.typ==titelType) printTitel(data,0,"")
        }
        printBlock(3f){}
        printBlock(30f){
          printText("Summe LV Netto:",cx+posWidth,currYPos,langTextWidth-1,standardFont)
          printText(f"zuzügl.$mwst%.1f %% MWSt.:",cx+posWidth,currYPos+4,langTextWidth-1,standardFont)
          printText("Summe LV Brutto:",cx+posWidth,currYPos+10f,langTextWidth-1,standardFont)
          printText("Vergleich:",cx+posWidth,currYPos+15f,langTextWidth-1,standardFont)


          val lowestPrice=colHeaders.map(el=>substNull(el.summe)).min
          for(cix<-colHeaders.indices;col=colHeaders(cix)) {
            val colX=cx+leftWidth+cix*colWidth+colWidth-1
            printText(moneyForm.format(col.summe),colX,currYPos,0f,standardFont,PrintElement.orient_Right)
            printText(moneyForm.format(col.summe*0.19d),colX,currYPos+4,0f,standardFont,PrintElement.orient_Right)
            printLine(colX-gpWidth+1,currYPos+5,gpWidth-1,0,0.5f)
            if(col.summe==lowestPrice) fillRect(colX+epWidth+6-colWidth,currYPos+5.5f,gpWidth-6,3,lightGray)
            printText(moneyForm.format(col.summe*1.19d),colX,currYPos+10f,0f,fettFont,PrintElement.orient_Right)
            val text=if(lowestPrice==0d) "" else if(col.summe==lowestPrice)"Günstigstes Angebot" else "+"+moneyForm.format(col.summe-lowestPrice)+" = "+f"${(col.summe - lowestPrice) / lowestPrice * 100}%.1f"+"%"
            printText(text,colX+3-colWidth,currYPos+15f,colWidth,standardFont)
          }
        }
      case None => util.Log.e ("Keine Preisspiegeldaten")
    }
  }
}

class PreisspiegelGeneratorWOHeaders extends PreisspiegelGenerator {
  override val printHeaderAdresses: Boolean = false
}

object PreisspiegelGenerator {
  val posWidth=8f
  val langTextWidth=41f
  val mengeWidth=18f
  val leftWidth: Float =posWidth+langTextWidth+mengeWidth
  val topMargin=23f
  val epWidth=18f
  val gpWidth=22f
  val colWidth: Float =epWidth+gpWidth

  lazy val posType: Int =AllClasses.get.getClassIDByName("NPosition")
  lazy val titelType: Int =AllClasses.get.getClassIDByName("NGewerk")
  lazy val adressType: Int =AllClasses.get.getClassIDByName("Adresse")
  lazy val colHeaderType: Int =AllClasses.get.getClassIDByName("PSColHeader")
  lazy val gewerkSummeType: Int =AllClasses.get.getClassIDByName("PSGewerkSumme")

}