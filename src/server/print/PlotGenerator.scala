package server.print

import java.awt.Color
import java.awt.geom.{Point2D, Rectangle2D}
import java.io.File
import javax.imageio.{ImageIO, ImageReader}

import definition.data._
import definition.expression.{BlobConstant, VectorConstant}
import definition.typ.{AllClasses, EnumFieldDefinition, SystemSettings}
import server.storage.{ServerObjectClass, StorageManager}
import util.CollUtils

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

class PlotGenerator extends CustomGenerator {
	//val context=new PrintContext

	val layerType: Int =SystemSettings().systemTypes("Layer")
	val measureLayerType: Int =SystemSettings().systemTypes("MeasureLayer")
	val layerRefType: Int =SystemSettings().systemTypes("LayerPlotRef")
	val cl: ServerObjectClass =AllClasses.get.getClassByID(layerType) .asInstanceOf[ServerObjectClass]
	val lineType: Int =AllClasses.get.getClassIDByName("LineElem")
	val arcType: Int =AllClasses.get.getClassIDByName("ArcElem")
	val polyType: Int =AllClasses.get.getClassIDByName("PolyElem")
	val ellType: Int =AllClasses.get.getClassIDByName("EllipseElem")
	val textType: Int =AllClasses.get.getClassIDByName("TextElem")
	val dimLineTyp: Int =AllClasses.get.getClassIDByName("DimLineElem")
	val areaPolyTyp: Int =AllClasses.get.getClassIDByName("AreaPolygon")
  val symbolType: Int =AllClasses.get.getClassIDByName("SymbolElem")
  val symbolFillerType: Int =AllClasses.get.getClassIDByName("SymbolFiller")
  val bitmapType: Int =AllClasses.get.getClassIDByName("BitmapElem")
	val scales: Map[Int, Double] = SystemSettings().enumByID(cl.fields(2).asInstanceOf[EnumFieldDefinition].enumID).
	enumValues.map((v)=>(v._2,stringToScale(v._1))).toMap
	
	
	def fillPlotPage(dataParent:InstanceData,dataEater:DataEater,generator:SinglePageGenerator):Unit= this.synchronized{	  
	 val ph=dataEater.pageHeight	
   StorageManager.getInstanceProperties(dataParent.ref) match {
     case Some(props) =>
       val (withoutClip,withClip)=(for (layRef <- props.propertyFields(1).propertyList; if StorageManager.instanceExists(layRef.typ, layRef.instance) )
         yield StorageManager.getInstanceData(layRef))partition(_.fieldValue(5).toDouble==0d) // split in layers without clip and layers with clip
       for(layRefInst<- withoutClip++withClip) {
         //val layRefInst = StorageManager.getInstanceData(layRef)
         val refScale = layRefInst.fieldValue(1).toInt
         val textScale = if (layRefInst.fieldData(10).isNullConstant) 1d else layRefInst.fieldValue(10).toDouble
         def bx: Double = layRefInst.fieldValue(3).toDouble
         def by: Double = layRefInst.fieldValue(4).toDouble
         def bw: Double = layRefInst.fieldValue(5).toDouble
         def bh: Double = layRefInst.fieldValue(6).toDouble
         val theLayerRef = layRefInst.fieldValue.head.toObjectReference
         if (StorageManager.instanceExists(theLayerRef.typ, theLayerRef.instance)) {
           val theLayerInst = StorageManager.getInstanceData(theLayerRef)
           //val layerID=theLayerInst.fieldValue(2).toString
           val fieldOffset = if (theLayerRef.typ == measureLayerType) 1 else 0
           val layerName = theLayerInst.fieldValue(1 + fieldOffset).toString
           val layerScale = scales(if (refScale > 0) refScale else theLayerInst.fieldValue(2 + fieldOffset).toInt)
           //System.out.println("Layer "+layerName+" scale:"+layerScale)
           val px = layRefInst.fieldValue(7).toFloat * 1000f
           val py = layRefInst.fieldValue(8).toFloat * 1000f
           //println("print Layer "+layerName+" scale:"+layerScale+" px:"+px+ " py:"+py+" pageHeight:"+ph)

           def vectorToPoint(p: VectorConstant): Point2D.Float =
             new Point2D.Float((px + p.x * layerScale.toFloat).toFloat, (ph - py - p.y * layerScale.toFloat).toFloat)

           def vectorToVector(p: VectorConstant) =  new VectorConstant(px + p.x * layerScale.toFloat, ph - py - p.y * layerScale.toFloat, 0d)

           def createLine(data: InstanceData, color: Color, lineWidth: Float, lineStyle: Int) = {
             val start = vectorToPoint(data.fieldValue(3).toVector)
             val end = vectorToPoint(data.fieldValue(4).toVector)
             //println("Create Line start:"+start+" end:"+end+" w:"+lineWidth+" c:"+color)
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
             val points = data.fieldValue(4).toPolygon.transform(vectorToVector)
             val fillColor = new Color(data.fieldValue(5).toInt)
             val hatch = data.fieldValue(6).toInt
             val startPoint = data.fieldValue(7).toVector
             val angle = data.fieldValue(8).toDouble
             val name = data.fieldValue(9).getValue().toString
             //System.out.println("create Area "+name)
             PolyPrintElement(textScale.toFloat, data.fieldValue(3).toInt.toByte, new Color(data.fieldValue(1).toInt), fillColor, points,
               if (hatch == 0) None else Some(math.abs(hatch)), hatch < 0, layerScale.toFloat, vectorToPoint(startPoint), angle, name)
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

           def createBitmap(data:InstanceData,color: Color, lineWidth: Float, lineStyle: Int)= {
             val link=data.fieldValue(1).toString
             val file=new File(link)
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
             ellType -> createEll, polyType -> createPoly, textType -> createText, dimLineTyp -> createDimLine, areaPolyTyp -> createAreaPoly,
             symbolType -> createSymbol, symbolFillerType ->createSymbolFiller,bitmapType -> createBitmap)

           def createPrintElement(ref: Reference): PrintElement = {
             val data = try {StorageManager.getInstanceData(ref)} catch {case NonFatal(e) => util.Log.e(e); return null;case other:Throwable =>println(other);System.exit(0);null}
             val color = new Color(data.fieldValue.head.toInt)
             val lineWidth = toUnit(data.fieldValue(1).toDouble / 100)
             val lineStyle = data.fieldValue(2).toInt

             if (elemFactory.contains(ref.typ))
               elemFactory(ref.typ)(data, color, lineWidth, lineStyle)
             else null
           }
           if (bw > 0d) {
             val p1 = vectorToPoint(new VectorConstant(bx, by, 0))
             val p2 = vectorToPoint(new VectorConstant(bx + bw, by + bh, 0))
             val clipRect = new Rectangle2D.Float(p1.x, p2.y, p2.x - p1.x, p1.y - p2.y)
             dataEater.addPrintElement(new ClipPrintElement(clipRect))
           }

           StorageManager.getInstanceProperties(theLayerRef) match {
             case Some(grProps) =>
               dataEater.addPrintElements(grProps.propertyFields(0).propertyList.map(createPrintElement).filter(_ != null))
             case None =>
           }
           if (bw > 0d) {
             dataEater.addPrintElement(new RestoreClipPrintElement)
           }
         } else util.Log.e("cant find Layer :" + theLayerRef)
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