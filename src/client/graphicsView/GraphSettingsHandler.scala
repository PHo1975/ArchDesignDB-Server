package client.graphicsView

import client.comm.ClientQueryManager
import definition.comm.NotificationType
import definition.data._
import definition.expression._
import definition.typ.SystemSettings

import java.awt.geom.Line2D
import java.awt.{BasicStroke, Color, Graphics2D}
import java.text.DecimalFormat
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex


object GraphSettingsHandler {
  var mainFolders:Seq[InstanceData]=Seq.empty
  val settingHandlers: ArrayBuffer[AbstractSettingHandler] =collection.mutable.ArrayBuffer[AbstractSettingHandler]()
  ClientQueryManager.registerSetupListener(setup _)
  
  def setup():Unit= {
      //println("setup "+settingHandlers.mkString("|"))
  		mainFolders=SystemSettings().getCustomSettings("GraphicSettings")
  		for(handler<-settingHandlers){
  			mainFolders.find(_.fieldValue.head.toString==handler.name) match {
  				case Some(inst)=> handler.loadSettings(inst.ref)
  				case _=>
  			}
  		}
      println("Graphsettings setup done")
  }  

  def registerHandler(handler:AbstractSettingHandler): Unit = {
  	settingHandlers+=handler   	
		getMainFolder(handler.name) match {
			case Some(inst)=> handler.loadSettings(inst.ref)
			case _=>
		}
  	
  }
  
  def getMainFolder(name:String): Option[InstanceData] = mainFolders.find(_.fieldValue.head.toString==name)
  
  def shutDown(): Unit =    settingHandlers foreach (_.shutDown())
}

trait AbstractSettingHandler {
  def init(): Unit = GraphSettingsHandler.registerHandler(this)
  def name:String
  def loadSettings(ref:Reference): Unit
  def shutDown(): Unit = {}
  
}



case class HatchStyle(ix:Int, name:String, angle1:Double, lineStyle1:Int, distance1:Double,
                      angle2:Double, lineStyle2:Int, distance2:Double, thickness:Int) {
  def this(nix:Int,data:InstanceData)= this(nix,data.fieldValue.head.toString,data.fieldValue(1).toDouble,data.fieldValue(2).toInt,data.fieldValue(3).toDouble,
      data.fieldValue(4).toDouble,data.fieldValue(5).toInt,data.fieldValue(6).toDouble,data.fieldValue(7).toInt)
  override def toString: String =name
  def toExpression: Array[Expression] =Array[Expression](StringConstant(name),new DoubleConstant(angle1),new DoubleConstant(lineStyle1),
      new DoubleConstant(distance1),new DoubleConstant(angle2),new DoubleConstant(lineStyle2),new DoubleConstant(distance2),
      new DoubleConstant(thickness))
}



object HatchHandler extends AbstractSettingHandler {
  val undefinedHatch: HatchStyle =HatchStyle(-1, " - ", 0, 0, 0, 0, 0, 0, 0)
  var folderRef:Option[Reference]=None
  var hatchList:Seq[HatchStyle]=Seq.empty
  val name="Hatches"
  val hatchLine= new Line2D.Double
  init()
  
  def loadSettings(ref:Reference): Unit =this.synchronized{
    var i:Int= -1
    folderRef=Some(ref)
    hatchList=ClientQueryManager.queryInstance(ref,1).map(hs=>{i+=1;new HatchStyle(i,hs)})
    //println("Load Settings hatchlist:\n"+hatchList.map(_.toExpression.mkString("|")).mkString("\n"))
  }
  
  def getHatch(ix:Int):Option[HatchStyle]=if(ix<0||ix>=hatchList.size) None
  else Some(hatchList(ix))

  def quickGetHatch(ix:Int):HatchStyle=if(ix<0||ix>=hatchList.size) undefinedHatch
  else hatchList(ix)
  
  def findHatchID(angle1:Double,style1:Int,distance1:Double,angle2:Double,style2:Int,distance2:Double):Int= {
    for(h<-hatchList)
      if(style2== -1 &&  // hatch with single line
        h.lineStyle2== -1 &&h.angle1==angle1&&h.lineStyle1==style1&&h.distance1==distance1) return h.ix
      else if(h.angle1==angle1&&h.lineStyle1==style1&&h.distance1==distance1 // 2 lines
          &&h.angle2==angle2&&h.lineStyle2==style2&&h.distance2==distance2) return h.ix
    -1
  }
  
  def drawHatch(poly:Polygon,hs:HatchStyle,sm:Scaler,paperScale:Boolean,g:Graphics2D,c:Color=Color.black,startPoint:VectorConstant,hatchAngle:Double,aOffset:VectorConstant): Unit = {
  		g.setColor(c)		  
  		drawHatches(hs.angle1+hatchAngle,hs.distance1,hs.lineStyle1,hs.thickness,offset = false)
  		if(hs.lineStyle2> -1) 
  			drawHatches(hs.angle2+hatchAngle,hs.distance2,hs.lineStyle2,hs.thickness,hs.angle2==hs.angle1)

  			def drawHatches(angle:Double,distance:Double,style:Int,thickness:Int,offset:Boolean): Unit = if(distance>0){
         // if (thickness >0) println("Draw Hatch thickness:"+thickness) else println("DrawHatch 0")
  				val line=hatchLine
          val stroke=sm.getStroke(if(thickness>0)thickness.toFloat else 1f,style)
  				g.setStroke(stroke)
  				val a1=angle*math.Pi/180d
  				val dir1=new VectorConstant(scala.math.cos(a1),scala.math.sin(a1),0)
  				val dist=if(paperScale) (distance/10)*sm.thicknessScale/sm.scale else distance
  				val n1=new VectorConstant(-dir1.y*dist,dir1.x*dist,0)		 
  				//println("dir :"+dir1+" n1:"+n1)
  				val (minh: Double,maxh: Double)=poly.findMinMaxHatchDistances(dir1,n1,startPoint)
  				val startIx=scala.math.ceil(minh).toInt+(if(offset) -1 else 0)
  				val endIx=math.floor(maxh).toInt		  
  				//println("Draw start:"+startIx+" end:"+endIx)
  				def drawLine(p1:VectorConstant,p2:VectorConstant): Unit = {
  					line.x1=sm.xToScreen(p1.x+aOffset.x);line.y1=sm.yToScreen(p1.y+aOffset.y)
  					line.x2=sm.xToScreen(p2.x+aOffset.x);line.y2=sm.yToScreen(p2.y+aOffset.y)		    
  					g.draw(line)
  				}    
  				for(i <-startIx to endIx) {
  					val sp1: VectorConstant =startPoint+n1*(i.toDouble +(if(offset)0.5 else 0))
  					val sp2: VectorConstant =sp1+dir1
  					val inters = poly.intersectionsWith(sp1,sp2).grouped(2)
  					//println("Intersections sp1:"+sp1+" sp2:"+sp2+" "+inters.mkString(" | "))
  					for(li<-inters;if li.size==2)
  					  	drawLine(li.head._2,li(1)._2)
  				}	
  		}
  	}
}





object Handlers{
  implicit object LineStyleHandler extends AbstractLineStyleHandler with AbstractSettingHandler {
    //println("Start LineStyleHandler")
    val undefinedStyle=new LineStyle(-1," - ",Array())
    val hoverStroke=new BasicStroke(3.5f)
    val name="LineStyles"
    var stylesList:Seq[LineStyle]=Seq.empty
    var standardStrokes:Seq[BasicStroke]=Seq.empty
    var folderRef:Option[Reference]=None
    init()
    def styles: Seq[LineStyle] =stylesList

    def loadSettings(ref:Reference): Unit = this.synchronized{//Swing.onEDT {
      folderRef=Some(ref)
      var i:Int= -1
      stylesList=ClientQueryManager.queryInstance(ref,1).map(ls=>{i+=1;new LineStyle(i,ls)})
    } // }

    def createStandardStrokes():Seq[BasicStroke]={
      if(standardStrokes.isEmpty) standardStrokes=stylesList.indices.map(createStroke(1.0/ScaleModel._dotPitch,1f,_))
      standardStrokes
    }

    def getStyle(id:Int): LineStyle = if(id >= stylesList.size) stylesList.head else stylesList(id)

    def createStroke(scale:Double,width:Float,styleIx:Int): BasicStroke ={
      val style=if(styleIx<0||styleIx>=stylesList.size)undefinedStyle else stylesList(styleIx)
      if(scale*width<0) util.Log.e(" scale:"+scale+" width:"+width+" style:"+styleIx+"\n",Thread.currentThread().getStackTrace)
      val strokeWidth=if(scale*width<0) 0.5f else (scale*width/100).toFloat
      if(style.dots.length==0 || style.dots.contains(0)) new BasicStroke(strokeWidth)
      else  new BasicStroke(strokeWidth,BasicStroke.CAP_BUTT ,BasicStroke.JOIN_BEVEL,10f,style.dots.map(z => (z * scale).toFloat),0f)
    }
    def findStyleID(dots:Seq[Float]):Int= if (dots.isEmpty) 0
    else  stylesList.indexWhere(_.equalDots(dots))
  }


  implicit object MaterialHandler extends AbstractSettingHandler with AbstractMaterialHandler  {
    val undefinedMaterial=new Material(0," - ",0,0)
    val name="Material"
    var materialMap=new collection.mutable.LinkedHashMap[Int,Material]()
    init()
    def loadSettings(ref:Reference): Unit =this.synchronized{
      var i:Int= -1
      materialMap++=(ClientQueryManager.queryInstance(ref,1).map(data=>{val n=new Material(data); n.inst -> n}) :+ (0->undefinedMaterial))
    }
    def getMaterial(ix:Int):Option[Material]=
      materialMap.get(ix)
    lazy val materialList: Seq[Material] =materialMap.valuesIterator.toSeq
  }
}

import client.graphicsView.Handlers._


object CompositionHandler extends AbstractSettingHandler {
   val matFormat=new DecimalFormat("00.0")
  val undefinedShellLayer: ShellLayer =ShellLayer(MaterialHandler.undefinedMaterial, 0, 0, 0, 0, LineStyleHandler.undefinedStyle, 0,0,0)
   val undefinedComposition: Composition =Composition(0, " - ", 0, Seq(undefinedShellLayer))

   val name="Composition"
   var compMap:Map[Int,Composition]=Map.empty
   var subsID:Int = -1
   type CompListener= Map[Int, Composition] => Unit
   val compListeners=new collection.mutable.HashSet[CompListener]()
   init()
   
   def compList: Seq[Composition] =compMap.valuesIterator.toSeq
   
   def loadSettings(ref:Reference): Unit =this.synchronized{
  	 var i:Int= -1
  	 subsID= ClientQueryManager.createSubscription(ref,1){
			(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> 			
			  this.synchronized {				  
					typ match {
						case NotificationType.sendData| NotificationType.updateUndo =>
              compMap= collection.immutable.SortedMap[Int,Composition]()++((data map (d=>
              {val n=new Composition(d,ClientQueryManager.queryInstance(d.ref,0). map(new ShellLayer(_))); n.ix -> n} )) :+ (0->undefinedComposition))
              notifyListeners()
            case NotificationType.fieldChanged =>
              val comp= new Composition(data.head,ClientQueryManager.queryInstance(data.head.ref,0). map(new ShellLayer(_)))
              compMap=compMap.updated(comp.ix,comp)
              notifyListeners()
            case NotificationType.instanceRemoved =>
              val comp=new Composition(data.head,Seq.empty)
              compMap=compMap.filterNot(a=>a._1==comp.ix)
              notifyListeners()
            case _ =>
					}
			  }
			}		 	 
   } 
   
   def getMaterial(ix:Int):Option[Composition]= 
    compMap.get(ix)
    
   def quickGetComposition(ix:Int): Composition =this.synchronized{
     if(compMap.contains(ix))compMap(ix) else undefinedComposition 
   }
    
   def registerListener(listener:CompListener): Unit = {
       compListeners += listener
       listener(compMap)
     }
   
   private def notifyListeners(): Unit = compListeners foreach (_(compMap))
   
   override def shutDown(): Unit = {
     if(subsID > -1) ClientQueryManager.removeSubscription(subsID)
   }
     
}
class FontInfo(val name:String, val ref:Reference) {
  override def toString: String =name
}

object FontHandler extends AbstractSettingHandler  {
  val name="Fonts"
  var fontList:IndexedSeq[FontInfo]=IndexedSeq.empty
  val fontAliasList: mutable.HashMap[String, String] = collection.mutable.HashMap[String,String]()
  var rootRef:Reference=_
  init()
  //println("dims:"+ DimLineStyleHandler.styleMap.map(_._2.symbolElems).mkString)
  
  def getFont(fontName:String):String={
   for(f<-fontList) if(fontName.equalsIgnoreCase(f.name)) return f.name
   val lc=fontName.toLowerCase
   if(fontAliasList.contains(lc)) fontAliasList(lc)
   else null
  }
  
   def getFontInfo(fontName:String):FontInfo= if(fontName.isEmpty) defaultFont
  else {
    for(f<-fontList)
      if(fontName.equalsIgnoreCase(f.name)) return f
    val lc=fontName.toLowerCase
    if(fontAliasList.contains(lc)) {
      val alias=fontAliasList(lc)
      if(alias.equalsIgnoreCase(lc) ) throw new IllegalArgumentException("Recursive Alias "+fontName)
      else getFontInfo(alias)
    } else throw new IllegalArgumentException("Cant find font '"+fontName+"'")
  }
  
  def loadSettings(ref:Reference): Unit = this.synchronized{
    rootRef=ref
    fontList= ClientQueryManager.queryInstance(ref,1).map(inst=>{
      val aliases=inst.fieldValue(1).toString.split(";")
      val fontName=inst.fieldValue.head.toString.trim
      for(st<-aliases;aName=st.trim;if aName.length > 0)
        fontAliasList(aName.toLowerCase())=fontName
      new FontInfo(fontName,inst.ref)  
    }).sortBy(_.name)    
  }
  
  def defaultFont: FontInfo =fontList.head
  
  def addAlias(fontName:String,aliasName:String): Unit = {
    val currentFontEntry=fontList.filter(_.name.equalsIgnoreCase(fontName))
    val dataRef= 
    if(currentFontEntry.isEmpty) {
      val instID=ClientQueryManager.createInstance(rootRef.typ,Array(new OwnerReference(1.toByte,rootRef)))
      new Reference(rootRef.typ,instID)
    }
    else  currentFontEntry.head.ref    
    val aliasesForFont=fontAliasList.filter(_._2 == fontName).keys.toSet +aliasName
    ClientQueryManager.writeInstanceField(dataRef,1,StringConstant(aliasesForFont.mkString(";")))
    fontAliasList(aliasName.toLowerCase)=fontName
  }
}

case class DimLineStyle(ref:Reference,id:Int,name:String,options:Int,textFont:String,textHeight:Double,
    roundMM:Int,textPosition:Double,numberFormat:String,lineWidth:Double,
    fixedHelpLineLength:Double,helpLineOffset:Double) {  
  def this(data:InstanceData) = this(data.ref,data.fieldValue.head.toInt,data.fieldValue(1).toString,data.fieldValue(2).toInt,
      data.fieldValue(3).toString,data.fieldValue(4).toDouble,data.fieldValue(5).toInt,data.fieldValue(6).toDouble,
      data.fieldValue(7).toString,data.fieldValue(8).toDouble,data.fieldValue(9).toDouble,data.fieldValue(10).toDouble)
  import client.graphicsView.DimLineStyleHandler._
  lazy val symbolElems: immutable.IndexedSeq[GraphElem] =ClientQueryManager.queryInstanceFact(ref,0,GraphElemFactory)
  
  def hideDimensionLine: Boolean =(options & HideDimensionLine) >0
  def hideHelpLine: Boolean =(options & HideHelpLine) > 0
  def isStationDimLine: Boolean =(options & IsStationLine) >0
  def hasFixedHelpLine: Boolean =(options & FixedHelpLine)>0
  def hasHighMM: Boolean =(options & HighMM)>0
  def unitStyle: Int =(options >> 5) & 7
  def isArrow:Boolean = (options & Arrow)>0

  
  def roundedMeasure(measure:Double): Double =if(roundMM==0) measure else (Math.round(measure/roundMM)*roundMM).toDouble
  
  def formatMeasure(measure:Double):Seq[String]= {    
    val ftext= (unitStyle match {
      case UnitMM => numberFormat.format(roundedMeasure(measure*1000))
      case UnitM => numberFormat.format(roundedMeasure(measure*1000)/1000)
      case UnitM_MM=>
        if(math.abs(measure) <1){
          if(isStationDimLine&& measure!=0d ) f"${roundedMeasure(measure * 1000) / 10}%+3.1f"
          else f"${roundedMeasure(measure * 1000) / 10}%3.1f"
        }
        else numberFormat.format(roundedMeasure(measure*1000)/1000)
      case _=>"Fehler"
    }).trim    
    if(hasHighMM)ftext match {
      case Match1(text,h)=>Seq(text,h)
      case Match2(text,h)=>Seq(text,h)
      case _=> Seq(ftext)
    }
    else Seq(ftext)
  }
  
  
  
  //println("DimStyle "+name+" Symbols:"+symbolElems.mkString(" | "))
}

object DimLineStyleHandler extends AbstractSettingHandler {  
  val name="DimLineStyles"
  final val HideDimensionLine=1 //1
  final val HideHelpLine=2      //2
  final val IsStationLine=4     //3
  final val FixedHelpLine=8     //4
  final val HighMM=16           //5
  final val UnitMM=2
  final val UnitM=0
  final val UnitM_MM=1   
  final val DimLineHTextScale=0.75f
  final val Arrow=256

  
  
  val Match1: Regex ="""(\d+[\.,]\d+)(\d)""".r
  val Match2: Regex ="""(\d+)[\.,](\d)""".r
  
  
  var styleMap:collection.Map[Int,DimLineStyle]=Map.empty  
  var defaultStyle:DimLineStyle=_
  init()
  
  def loadSettings(ref:Reference): Unit =  this.synchronized{
   styleMap=collection.immutable.SortedMap[Int,DimLineStyle]()++ ClientQueryManager.queryInstance(ref,1).map(e=> {
     val d= new DimLineStyle(e)
     (d.id,d)
     }).toMap 
   defaultStyle=styleMap.values.head
   //println("load dim styles "+ styleMap.mkString(" | "))
  }
  
  def getStyle(styleID:Int): DimLineStyle =if(styleMap.contains(styleID)) styleMap(styleID) else defaultStyle
  
  def getStyleSeq: Seq[DimLineStyle] =styleMap.values.toSeq
}

object LineColorsHandler extends AbstractSettingHandler {
  val name="LineColors"
  var colorMap:Map[Int,Int]=Map.empty  
  init()
  
  def loadSettings(ref:Reference): Unit =
    colorMap=ClientQueryManager.queryInstance(ref,1).map(e=>{(e.fieldValue.head.toInt,e.fieldValue(1).toInt)}).toMap
  
  def getLineColor(thickness:Int): Int =colorMap.getOrElse(thickness,0)
}
