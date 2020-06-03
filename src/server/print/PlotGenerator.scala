package server.print

import java.awt.Color
import java.awt.geom.{Point2D, Rectangle2D}
import java.io.File

import client.graphicsView.PolyLineElement
import definition.data._
import definition.expression.{BlobConstant, PointList, Polygon, VectorConstant}
import definition.typ.{AllClasses, EnumFieldDefinition, SystemSettings}
import javax.imageio.{ImageIO, ImageReader}
import server.config.FSPaths
import server.storage.{ServerObjectClass, StorageManager}
import util.{CollUtils, Log}

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

class PlotGenerator extends CustomGenerator {
	//val context=new PrintContext
  val pointNull=new Point2D.Float()
	val layerType: Int =SystemSettings().systemTypes("Layer")
	val measureLayerType: Int =SystemSettings().systemTypes("MeasureLayer")
	val layerRefType: Int =SystemSettings().systemTypes("LayerPlotRef")
	val cl: ServerObjectClass =AllClasses.get.getClassByID(layerType) .asInstanceOf[ServerObjectClass]
	val lineType: Int =AllClasses.get.getClassIDByName("LineElem")
	val arcType: Int =AllClasses.get.getClassIDByName("ArcElem")
	val polyType: Int =AllClasses.get.getClassIDByName("PolyElem")
  val polyLineType:Int=AllClasses.get.getClassIDByName("PolyLineElem")
	val ellType: Int =AllClasses.get.getClassIDByName("EllipseElem")
	val textType: Int =AllClasses.get.getClassIDByName("TextElem")
	val dimLineTyp: Int =AllClasses.get.getClassIDByName("DimLineElem")
	val areaPolyTyp: Int =AllClasses.get.getClassIDByName("AreaPolygon")
  val wohnFLTyp:Int =AllClasses.get.getClassIDByName("Wohnfläche")
  val measureLineType: Int = AllClasses.get.getClassIDByName("MeasurePolyLine")
  val symbolType: Int =AllClasses.get.getClassIDByName("SymbolElem")
  val symbolFillerType: Int =AllClasses.get.getClassIDByName("SymbolFiller")
  val bitmapType: Int =AllClasses.get.getClassIDByName("BitmapElem")
	val scales: Map[Int, Double] = SystemSettings().enumByID(cl.fields(2).asInstanceOf[EnumFieldDefinition].enumID).
	enumValues.map(v=>(v._2,stringToScale(v._1))).toMap

  val numberFormat="%,.2f"

  val filterMap: Map[Int, Int] =Map(lineType->1, arcType->2, ellType->4, polyType->8, polyLineType->16,
    bitmapType->32, textType->64, dimLineTyp->128, symbolType->256, symbolFillerType->512,
    areaPolyTyp->1024, wohnFLTyp->2048, measureLineType->4096)


	def fillPlotPage(dataParent:InstanceData,dataEater:DataEater,generator:SinglePageGenerator):Unit= this.synchronized{
	 val ph=dataEater.pageHeight	
   StorageManager.getInstanceProperties(dataParent.ref) match {
     case Some(props) =>
       val (withoutClip: Seq[InstanceData],withClip: Seq[InstanceData])=(
         for (layRef <- props.propertyFields(1).propertyList; if StorageManager.instanceExists(layRef.typ, layRef.instance) )
           yield StorageManager.getInstanceData(layRef)).partition(_.fieldValue(5).toDouble==0d) // split in layers without clip and layers with clip
       for(layRefInst<- withoutClip++withClip) {
         //val layRefInst = StorageManager.getInstanceData(layRef)
         val refScale = layRefInst.fieldValue(1).toInt
         val plotAngle:Double=layRefInst.fieldValue(2).toDouble*Math.PI/180d
         val textScale = if (layRefInst.fieldData(10).isNullConstant) 1d else layRefInst.fieldValue(10).toDouble
         def bx: Double = layRefInst.fieldValue(3).toDouble
         def by: Double = layRefInst.fieldValue(4).toDouble
         def bw: Double = layRefInst.fieldValue(5).toDouble
         def bh: Double = layRefInst.fieldValue(6).toDouble
         val px: Float = layRefInst.fieldValue(7).toFloat * 1000f
         val py: Float = layRefInst.fieldValue(8).toFloat * 1000f
         val filter=new PlotFilter(layRefInst.fieldValue(9).toInt)
         val theLayerRef = layRefInst.fieldValue.head.toObjectReference
         if (StorageManager.instanceExists(theLayerRef.typ, theLayerRef.instance)) {
           val theLayerInst = StorageManager.getInstanceData(theLayerRef)
           //val layerID=theLayerInst.fieldValue(2).toString
           val fieldOffset = if (theLayerRef.typ == measureLayerType) 1 else 0
           val layerScale = scales(if (refScale > 0) refScale else theLayerInst.fieldValue(2 + fieldOffset).toInt)

           def vectorToPoint(p: VectorConstant): Point2D.Float =
             new Point2D.Float((px + p.x * layerScale.toFloat).toFloat, (ph - py - p.y * layerScale.toFloat).toFloat)

           def vectorToVector(p: VectorConstant) =  new VectorConstant(px + p.x * layerScale.toFloat, ph - py - p.y * layerScale.toFloat, 0d)

           def createLine(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val start = vectorToPoint(data.fieldValue(3).toVector)
             val end = vectorToPoint(data.fieldValue(4).toVector)
             LinePrintElement(new Rectangle2D.Float(start.x, start.y, end.x - start.x, end.y - start.y),
               lineWidth, lineStyle.toByte, color)
           }
           def createArc(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val centerPoint = vectorToPoint(data.fieldValue(3).getValue.toVector)
             val diameter = (data.fieldValue(4).getValue.toDouble * layerScale).toFloat
             val startA = data.fieldValue(5).getValue.toFloat
             val endA = data.fieldValue(6).getValue.toFloat
             val bx = centerPoint.x - diameter
             val by = centerPoint.y - diameter
             ArcPrintElement(new Rectangle2D.Float(bx, by, centerPoint.x + diameter - bx, centerPoint.y + diameter - by),
               lineWidth, lineStyle.toByte, color, startA, endA)
           }
           def createEll(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val centerPoint = vectorToPoint(data.fieldValue(3).getValue.toVector)
             val r1 = (data.fieldValue(4).getValue.toDouble * layerScale).toFloat
             val r2 = (data.fieldValue(5).getValue.toDouble * layerScale).toFloat
             val mainA = data.fieldValue(6).getValue.toFloat
             val startA = data.fieldValue(7).getValue.toFloat
             val endA = data.fieldValue(8).getValue.toFloat
             val bx = centerPoint.x - r1
             val by = centerPoint.y - r2
             EllipsePrintElement(new Rectangle2D.Float(bx, by, r1 * 2, r2 * 2),
               lineWidth, lineStyle.toByte, color, mainA, startA, endA)
           }
           def createPoly(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val points = data.fieldValue(3).toPolygon.transform(vectorToVector)
             val fillColor = new Color(data.fieldValue(4).toInt)
             val hatch = data.fieldValue(5).toInt
             val startPoint = data.fieldValue(6).toVector
             val angle = data.fieldValue(7).toDouble
             PolyPrintElement(lineWidth.toFloat, lineStyle.toByte, color, fillColor, points,
               if (hatch == 0) None else Some(math.abs(hatch)), hatch < 0, layerScale.toFloat, vectorToPoint(startPoint), angle)
           }
           def createAreaPoly(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val area=data.fieldValue(4).toPolygon
             val points = area.transform(vectorToVector)
             val fillColor = new Color(data.fieldValue(5).toInt)
             val hatch = data.fieldValue(6).toInt
             val startPoint = data.fieldValue(7).toVector
             val angle = data.fieldValue(8).toDouble
             val name = data.fieldValue(9).getValue.toString
             //System.out.println("create Area "+name)
             PolyPrintElement(textScale.toFloat, data.fieldValue(3).toInt.toByte, new Color(data.fieldValue(1).toInt), fillColor, points,
               if (hatch == 0) None else Some(math.abs(hatch)), hatch < 0, layerScale.toFloat, vectorToPoint(startPoint), angle,
               name+"\nF: "+area.getAreaValue.formatted(numberFormat)+" m2"+"\nU: "+area.getUmfangValue.formatted(numberFormat)+" m")
           }

           def createWohnflaeche (data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val area=data.fieldValue(4).toPolygon
             val points = area.transform(vectorToVector)
             val fillColor = new Color(data.fieldValue(5).toInt)
             val hatch = data.fieldValue(6).toInt
             val startPoint = data.fieldValue(7).toVector
             val angle = data.fieldValue(8).toDouble
             val name = data.fieldValue(9).getValue.toString
             val nr: Double =data.fieldValue(12).getValue.toDouble

             PolyPrintElement(textScale.toFloat, data.fieldValue(3).toInt.toByte, new Color(data.fieldValue(1).toInt), fillColor, points,
               if (hatch == 0) None else Some(math.abs(hatch)), hatch < 0, layerScale.toFloat, vectorToPoint(startPoint), angle,
               (if(nr==0d) name else (if(nr==Math.round(nr)) nr.formatted("%.0f") else nr.toString) + ". "+name)+
               "\nF: "+area.getAreaValue.formatted(numberFormat)+" m2"+"\nU: "+area.getUmfangValue.formatted(numberFormat)+" m")
           }

           def createPolyLine(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int): PolyPrintElement ={
             val points=data.fieldValue(3).toPolygon
             val width=data.fieldValue(4).toDouble
             val align=data.fieldValue(5).toDouble
             val opaquity=data.fieldValue(6).toDouble
             val hatch=data.fieldValue(7).toInt
             val hatchAngle=data.fieldValue(8).toDouble
             val polyPoints=new Polygon(Seq.empty,points.pathList.headOption match {
               case Some(pl)=>Seq(new PointList(PolyLineElement.createPoints(pl.points,vectorToVector,width,align).toSeq))
               case _ => Seq.empty
             })
             PolyPrintElement(textScale.toFloat, lineStyle.toByte, color, color, polyPoints,
               if (hatch == 0) None else Some(math.abs(hatch)), hatch < 0, layerScale.toFloat, pointNull, hatchAngle)
           }

           def createText(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val text = data.fieldValue(1).toString
             val pos = vectorToPoint(data.fieldValue(2).toVector)
             val font = data.fieldValue(3).toString
             val height = data.fieldValue(4).toDouble * textScale
             //val widthr = data.fieldValue(5).toDouble
             val style = data.fieldValue(6).toInt
             val textAngle = data.fieldValue(7).toDouble
             val obAngle = data.fieldValue(8).toDouble
             val lineSpace = data.fieldValue(9).toInt
             GraphTextElement(new Rectangle2D.Float(pos.x, pos.y, 0f, height.toFloat),
               text, font, style, textAngle.toFloat, obAngle.toFloat, color, lineSpace)
           }

           def createDimLine(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val pos = vectorToVector(data.fieldValue(1).toVector)
             val angle = data.fieldValue(3).toDouble
             val dPoints = data.fieldValue(4) match {
               case b: BlobConstant => DimensionPoint.createDimLineList(b)
               case _ => IndexedSeq.empty
             }
             val points = dPoints.map(p => new DimensionPoint(vectorToVector(p.refPoint), p.helpLineLength * layerScale, p.textPos.map(vectorToVector)))
             val mPoints = dPoints.map(p => p.refPoint)
             val refPoint = data.fieldValue(5).toVector
             val refDist = data.fieldValue(6).toDouble
             //val textScale=data.fieldValue(7).toDouble
             DimLinePrintElement(new Rectangle2D.Float(pos.x.toFloat, pos.y.toFloat, angle.toFloat, 0f), color, lineStyle, refPoint, refDist, textScale, points, mPoints)
           }

           def createSymbol(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val symbRef = data.fieldValue(1).toObjectReference
             val angle = data.fieldValue(2).toDouble
             val scale = data.fieldValue(3).toDouble
             val paramValues = data.fieldValue(4).toString
             val pos = vectorToPoint(data.fieldValue(5).toVector)
             SymbolPrintElement(new Rectangle2D.Float(pos.x, pos.y, layerScale.toFloat, 0f), symbRef, angle, scale, paramValues)
           }

           def createSymbolFiller(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val symbRef = data.fieldValue(1).toObjectReference
             val angle = data.fieldValue(2).toDouble
             val scale = data.fieldValue(3).toDouble
             val paramValues = data.fieldValue(4).toString
             val startPoint = vectorToPoint(data.fieldValue(5).toVector)
             val endPoint = vectorToVector(data.fieldValue(6).toVector)
             val code = data.fieldValue(7).toInt
             val val1 = data.fieldValue(8).toDouble
             val val2 = data.fieldValue(9).toDouble
             SymbolFillerPrintElement(new Rectangle2D.Float(startPoint.x, startPoint.y, layerScale.toFloat, 0f), symbRef, angle, scale, paramValues,
               endPoint, code, val1, val2)
           }



           def getImageSize(file:File):(Int,Int)= {
             var ret=(0,0)
             println("get Image Size File="+file)
             CollUtils.tryWith(ImageIO.createImageInputStream(file))(ins=>{
               for (reader: ImageReader <- ImageIO.getImageReaders(ins).asScala) {
                 try {
                   reader.setInput(ins)
                   ret = (reader.getWidth(0), reader.getHeight(0))
                 }
                 catch {case NonFatal(e)=> util.Log.e("loading file:"+file,e)
                 case other:Throwable =>println(other);System.exit(0);null}
                 finally {
                   reader.dispose()
                 }
               }
             })
             ret
           }

           def createBitmap(data:InstanceData,color: Color, lineWidth: Float, lineStyle: Int): GraphBitmapPrintElement = {
             val link=data.fieldValue(1).toString
             val file=new File(util.JavaUtils.restoreDelimiters(util.JavaUtils.resolveImagePath(FSPaths.imageDir,link)))
             val (width,height)=getImageSize(file)
             val dpi = data.fieldValue(2).toDouble
             val scale = data.fieldValue(3).toDouble
             val angle = data.fieldValue(4).toDouble/180*Math.PI
             val cscale= data.fieldValue(5).toDouble
             val dpos=data.fieldValue(6).toVector
             val pos = vectorToPoint(dpos)
             val w=width/dpi*25.4d/1000d*cscale*scale
             val h=height/dpi*25.4d/1000d*cscale*scale
             val wpos=vectorToPoint(new VectorConstant(dpos.x+w,dpos.y+h,0))
             val rect=new Rectangle2D.Float(pos.x,wpos.y,wpos.x,pos.y)
             GraphBitmapPrintElement(rect, link, angle)
           }


           val elemFactory = collection.immutable.Map[Int, (InstanceData, Color, Float, Int) => PrintElement](lineType -> createLine, arcType -> createArc,
             ellType -> createEll, polyType -> createPoly, textType -> createText, dimLineTyp -> createDimLine, areaPolyTyp -> createAreaPoly, wohnFLTyp->createWohnflaeche,
             symbolType -> createSymbol, symbolFillerType ->createSymbolFiller,bitmapType -> createBitmap,polyLineType->createPolyLine)

           def shouldPrint(typ:Int):Boolean=(filterMap(typ) & filter.filter)==0

           def createPrintElement(ref: Reference): PrintElement =  {
             val data = try {StorageManager.getInstanceData(ref)} catch {case NonFatal(e) => util.Log.e(e); return null;case other:Throwable =>println(other);System.exit(0);null}
             val color = new Color(data.fieldValue.head.toInt)
             val lineWidth = toUnit(data.fieldValue(1).toDouble / 100)
             val lineStyle = data.fieldValue(2).toInt

             if (elemFactory.contains(ref.typ))
               elemFactory(ref.typ)(data, color, lineWidth, lineStyle)
             else {
               Log.e("unknown Printelement type :"+ref.typ)
               null
             }
           }
           StorageManager.getInstanceProperties(theLayerRef) match {
             case Some(grProps) =>
               if(plotAngle!=0f) dataEater.addPrintElement(new RotationPrintElement(px,ph-py,plotAngle.toFloat))
               if (bw > 0d) {
                 val p1 = vectorToPoint(new VectorConstant(bx, by, 0))
                 val p2 = vectorToPoint(new VectorConstant(bx + bw, by + bh, 0))
                 val clipRect = new Rectangle2D.Float(p1.x, p2.y, p2.x - p1.x, p1.y - p2.y)
                 dataEater.addPrintElement(new ClipPrintElement(clipRect))
               }
               dataEater.addPrintElements(newElems = grProps.propertyFields(0).propertyList.filter(el => shouldPrint(el.typ)).map(createPrintElement).filter(_ != null))

               if (bw > 0d) dataEater.addPrintElement(new RestoreClipPrintElement)
               if( plotAngle!=0f) dataEater.addPrintElement(RotationEndPrintElement)

             case None =>
           }
         } else util.Log.e("cant find Layer :" + theLayerRef)
       }

       val smallFont = generator.form.fonts.getStyle("Klein")
       val standardFont = generator.form.fonts.standardStyle
       val tableWidth=102
       val startX=dataEater.pageWidth - tableWidth-generator.form.left
       val lineHeight=smallFont.height*1.7f
       var lastY=dataEater.pageHeight-60

       val notesString=dataEater.paramValues.find(_.paramName=="Bemerkungen") match {case Some(ResultElement(_,value))=>value.toString.trim;case _=>""}
       if(notesString.length>0) {
         val notes=notesString.split("\n")
         dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX, lastY-(notes.length+1f)*lineHeight+1f, 0f, standardFont.height),
           "Hinweise:", standardFont.fontName, 0, 0f, 0f, Color.black, 0f))
         for(ix<-notes.indices;note=notes(ix)){
           println("ix:"+ix+" N:"+note+" y:"+(lastY-(notes.length-ix)*lineHeight+1f))
           dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX, lastY-(notes.length-ix)*lineHeight+1f, 0f, smallFont.height),
             note, smallFont.fontName, 0, 0f, 0f, Color.black, 0f))

         }
         lastY-=(notes.length+2)*lineHeight
       }

       if(dataEater.paramValues.find(_.paramName=="Versionstabelle") match {case Some(ResultElement(_,value))=>value.toBoolean;case _=>false}) {
         // Print Versionstabelle
          val versions=props.propertyFields(3).propertyList.map(StorageManager.getInstanceData)
         if(versions.nonEmpty) {
           val numLines=versions.foldLeft(0)((v,el)=>v+el.fieldValue(2).toString.split('\n').length)
           val recty=lastY-(numLines+2)*lineHeight-1
           val rect=new Rectangle2D.Float(startX-1,recty,tableWidth+.1f,dataEater.pageHeight-60f-recty+1f)
           dataEater.addPrintElement(FillPrintElement(rect,Color.black,0,Color.white))
           dataEater.addPrintElement(RectPrintElement(rect,0.5f,0,Color.gray))
           val columns=Array(7,17,26)
           //val firstY=lastY-(versions.size+1)*lineHeight
           if(notesString.length>0)
             dataEater.addPrintElement(LinePrintElement(new Rectangle2D.Float(startX-1,lastY+1,tableWidth,0),1f,0, Color.black ))
           var currentY=lastY
           for(ix<-versions.indices.reverse;version=versions(ix)){
             val lines=version.fieldValue(2).toString.split("\n")
             val numLines=lines.size
             for(lix<-lines.indices;line=lines(lix)) {
               dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX+columns(2), currentY - (numLines-lix-1) * lineHeight, 0f, smallFont.height*0.9f),
                 line, smallFont.fontName, 0, 0f, 0f, Color.black, 0f))
             }
             currentY-= numLines*lineHeight
             dataEater.addPrintElement(LinePrintElement(new Rectangle2D.Float(startX-1,currentY+0.5f,tableWidth,0),if(ix==0)1f else 0.5f,0,if(ix==0) Color.black else Color.gray))
             dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX, currentY + lineHeight, 0f, smallFont.height*0.9f),
               version.fieldValue(0).toString, smallFont.fontName, if(ix==versions.size-1)FontStyle.boldStyle else 0, 0f, 0f, Color.black, 0f))
             dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX+columns(0), currentY + lineHeight, 0f, smallFont.height/**0.9f*/),
               version.fieldValue(1).toString, smallFont.fontName, 0, 0f, 0f, Color.black, 0f))
             dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX+columns(1), currentY + lineHeight, 0f, smallFont.height/**0.9f*/),
               version.fieldValue(3).toString, smallFont.fontName, 0, 0f, 0f, Color.black, 0f))
           }
           dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX, currentY-lineHeight-1f, 0f, standardFont.height),
             "Überarbeitungen", standardFont.fontName, 0, 0f, 0f, Color.black, 0f))
           /*dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX, currentY-.9f, 0f, smallFont.height*0.9f),
             "V.", smallFont.fontName, 0, 0f, 0f, Color.black, 0f))*/
           dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX+columns(0), currentY-1.5f, 0f, smallFont.height*0.9f),
             "Datum", smallFont.fontName, 0, 0f, 0f, Color.black, 0f))
           dataEater.addPrintElement(GraphTextElement(new Rectangle2D.Float(startX+columns(2), currentY-1.5f, 0f, smallFont.height*0.9f),
             "Änderungen", smallFont.fontName, 0, 0f, 0f, Color.black, 0f))

         }
       }
     case None =>
		}
    dataEater.addPrintElement(DecoreMarkerElement)
	  generator.decoratePage(dataParent, dataEater, dataEater.currentContext)
	}

	def stringToDouble(st:String):Double= try {
		val tt=st.trim
		if(tt.length==0)0d
		else tt.replace(',','.').toDouble
	} catch {  case e:Exception => 0d  }

	def stringToScale(st:String ):Double= {
			val parts=st.trim.split(':')
			if(parts.length!=2) 1d
			else {
				val f1=stringToDouble(parts(0).trim)
				val f2=stringToDouble(parts(1).trim)
				f1/f2*1000d
			}
	}

	def toUnit(mm:Double):Float = (mm*72.0/25.4).toFloat
}