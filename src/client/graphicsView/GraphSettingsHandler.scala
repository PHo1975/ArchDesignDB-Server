package client.graphicsView

import client.comm.ClientQueryManager
import definition.typ.SystemSettings
import definition.data.{Reference,InstanceData}
import java.awt.BasicStroke
import java.awt.Graphics2D
import java.awt.geom.Line2D
import definition.expression.VectorConstant
import definition.expression.Polygon
import java.awt.Color
import definition.comm.NotificationType
import java.text.DecimalFormat
import definition.data.AbstractLineStyleHandler
import definition.data.StyleService
import definition.data.LineStyle
import definition.expression.Expression
import definition.expression.StringConstant
import definition.expression.DoubleConstant
import definition.data.OwnerReference

import scala.annotation.tailrec


object GraphSettingsHandler {
  var mainFolders:Seq[InstanceData]=Seq.empty
  val settingHandlers=collection.mutable.ArrayBuffer[AbstractSettingHandler]()  
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
  }  

  def registerHandler(handler:AbstractSettingHandler)= {    
  	settingHandlers+=handler   	
		getMainFolder(handler.name) match {
			case Some(inst)=> handler.loadSettings(inst.ref)
			case _=>
		}
  	
  }
  
  def getMainFolder(name:String)= mainFolders.find(_.fieldValue.head.toString==name)
  
  def shutDown() =    settingHandlers foreach (_.shutDown())
}

trait AbstractSettingHandler {
  def init()= GraphSettingsHandler.registerHandler(this)
  def name:String
  def loadSettings(ref:Reference): Unit
  def shutDown()= {}
  
}


object LineStyleHandler extends AbstractLineStyleHandler with AbstractSettingHandler {  
  //println("Start LineStyleHandler")
  val undefinedStyle=new LineStyle(-1," - ",Array()) 
  val hoverStroke=new BasicStroke(3.5f)
  val name="LineStyles"
  var stylesList:Seq[LineStyle]=Seq.empty
  var standardStrokes:Seq[BasicStroke]=Seq.empty
  var folderRef:Option[Reference]=None  
  init()
  def styles=stylesList      
  
  def loadSettings(ref:Reference)= this.synchronized{//Swing.onEDT {
    folderRef=Some(ref)    
    var i:Int= -1
    stylesList=ClientQueryManager.queryInstance(ref,1).map(ls=>{i+=1;new LineStyle(i,ls)})        
  } // }  
  
  def createStandardStrokes():Seq[BasicStroke]={
    if(standardStrokes.isEmpty) standardStrokes=stylesList.indices.map(createStroke(1/ScaleModel._dotPitch,1,_))
    standardStrokes
  }
  
  def createStroke(scale:Double,width:Float,styleIx:Int)={
    val style=if(styleIx<0||styleIx>=stylesList.size)undefinedStyle else stylesList(styleIx)
    if(scale*width<0) util.Log.e(" scale:"+scale+" width:"+width+" style:"+styleIx+"\n",Thread.currentThread().getStackTrace())
    val strokeWidth=if(scale*width<0) 0.5f else (scale*width/100).toFloat
    if(style.dots.length==0 || style.dots.contains(0)) new BasicStroke(strokeWidth)
      	else  new BasicStroke(strokeWidth,BasicStroke.CAP_BUTT ,BasicStroke.JOIN_BEVEL,10f,style.dots.map(z => (z * scale).toFloat),0f)
  }
  def findStyleID(dots:Seq[Float]):Int= if (dots.length == 0) 0
    else  stylesList.indexWhere(_.equalDots(dots))
}


case class HatchStyle(ix:Int, name:String, angle1:Double, lineStyle1:Int, distance1:Double,
                      angle2:Double, lineStyle2:Int, distance2:Double, thickness:Int) {
  def this(nix:Int,data:InstanceData)= this(nix,data.fieldValue.head.toString,data.fieldValue(1).toDouble,data.fieldValue(2).toInt,data.fieldValue(3).toDouble,
      data.fieldValue(4).toDouble,data.fieldValue(5).toInt,data.fieldValue(6).toDouble,data.fieldValue(7).toInt)
  override def toString=name
  def toExpression=Array[Expression](new StringConstant(name),new DoubleConstant(angle1),new DoubleConstant(lineStyle1),
      new DoubleConstant(distance1),new DoubleConstant(angle2),new DoubleConstant(lineStyle2),new DoubleConstant(distance2),
      new DoubleConstant(thickness))
}



object HatchHandler extends AbstractSettingHandler {
  val undefinedHatch=new HatchStyle(-1," - ",0,0,0,0,0,0,0)
  var folderRef:Option[Reference]=None
  var hatchList:Seq[HatchStyle]=Seq.empty
  val name="Hatches"
  init()
  
  def loadSettings(ref:Reference)=this.synchronized{    
    var i:Int= -1
    folderRef=Some(ref)
    hatchList=ClientQueryManager.queryInstance(ref,1).map(hs=>{i+=1;new HatchStyle(i,hs)})    
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
  
  def drawHatch(poly:Polygon,hs:HatchStyle,sm:Scaler,paperScale:Boolean,g:Graphics2D,c:Color=Color.black,startPoint:VectorConstant,hatchAngle:Double,aOffset:VectorConstant) = {
  		g.setColor(c)		  
  		drawHatches(hs.angle1+hatchAngle,hs.distance1,hs.lineStyle1,hs.thickness,false)
  		if(hs.lineStyle2> -1) 
  			drawHatches(hs.angle2+hatchAngle,hs.distance2,hs.lineStyle2,hs.thickness,hs.angle2==hs.angle1)

  			def drawHatches(angle:Double,distance:Double,style:Int,thickness:Int,offset:Boolean)= if(distance>0){
  				val line=new Line2D.Double
  				g.setStroke(sm.getStroke(if(thickness>0)thickness else 1,style))
  				val a1=angle*math.Pi/180d
  				val dir1=new VectorConstant(scala.math.cos(a1),scala.math.sin(a1),0)
  				val dist=if(paperScale) (distance/10)*sm.thicknessScale/sm.scale else distance
  				val n1=new VectorConstant(-dir1.y*dist,dir1.x*dist,0)		 
  				//println("dir :"+dir1+" n1:"+n1)
  				val (minh,maxh)=poly.findMinMaxHatchDistances(dir1,n1,startPoint)
  				val startIx=scala.math.ceil(minh).toInt+(if(offset) -1 else 0)
  				val endIx=math.floor(maxh).toInt		  
  				//println("Draw start:"+startIx+" end:"+endIx)
  				def drawLine(p1:VectorConstant,p2:VectorConstant)= {
  					line.x1=sm.xToScreen(p1.x+aOffset.x);line.y1=sm.yToScreen(p1.y+aOffset.y)
  					line.x2=sm.xToScreen(p2.x+aOffset.x);line.y2=sm.yToScreen(p2.y+aOffset.y)		    
  					g.draw(line)
  				}    
  				for(i <-startIx to endIx) {
  					val sp1=startPoint+n1*(i.toDouble +(if(offset)0.5 else 0))
  					val sp2=sp1+dir1  					
  					val inters= poly.intersectionsWith(sp1,sp2).grouped(2) .toSeq
  					//println("Intersections sp1:"+sp1+" sp2:"+sp2+" "+inters.mkString(" | "))
  					for(li<-inters) 
  					  if(li.size==2)
  					  	drawLine(li.head._2,li(1)._2)
  				}	
  		}
  	}
}


class MaterialDef(val ix:Int,val name:String,val hatch:Int,val priority:Int) {
  def this(data:InstanceData) = this(data.fieldValue.head.toInt,data.fieldValue(1).toString,data.fieldValue(2).toInt,data.fieldValue(6).toInt)
  override def toString=name
}

object MaterialHandler extends AbstractSettingHandler {
   val undefinedMaterial=new MaterialDef(0," - ",0,0)
   val name="Material"
   var materialMap=new collection.mutable.LinkedHashMap[Int,MaterialDef]()
   init()
   def loadSettings(ref:Reference)=this.synchronized{
  	 var i:Int= -1
  	 materialMap++=(ClientQueryManager.queryInstance(ref,1).map(data=>{val n=new MaterialDef(data); n.ix -> n}) :+ (0->undefinedMaterial))
   } 
   def getMaterial(ix:Int):Option[MaterialDef]= 
    materialMap.get(ix)
   lazy val materialList=materialMap.valuesIterator.toSeq  
}

case class TierDef(material:MaterialDef, thickness:Double, lineFrom:Int, hatchFrom:Int,lineThick:Int,lineStyle:LineStyle,role:Int) {
  def this(data:InstanceData)= this(MaterialHandler.getMaterial(data.fieldValue.head.toInt).getOrElse(throw new IllegalArgumentException("cant find Material "+data.fieldValue.head.toInt)),
      data.fieldValue(1).toDouble,data.fieldValue(2).toInt,data.fieldValue(3).toInt,
      data.fieldValue(4).toInt,LineStyleHandler.styles(data.fieldValue(5).toInt),data.fieldValue(6).toInt)
  def rolePriority=4-role
  override def toString=(if(thickness==0)"" else CompositionHandler.matFormat.format(thickness*100)+" cm ") +material.name
}

case class Composition(ix:Int, name:String, typ:Int, tiers:Seq[TierDef])  {
  def this(data:InstanceData )= {    
    this(data.fieldValue.head.toInt,data.fieldValue(1).toString,data.fieldValue(2).toInt,ClientQueryManager.queryInstance(data.ref,0). map(new TierDef(_)))
  }
  override def toString=ix.toString+" "+name
}

object CompositionHandler extends AbstractSettingHandler {
   val matFormat=new DecimalFormat("00.0")
   val undefinedComposition=new Composition(0," - ",0,Seq(new TierDef(MaterialHandler.undefinedMaterial,0,0,0,10,LineStyleHandler.undefinedStyle,0)))
   val undefinedTier=new TierDef(MaterialHandler.undefinedMaterial,0,0,0,0,LineStyleHandler.undefinedStyle,0)
   val name="Composition"
   var compMap:Map[Int,Composition]=Map.empty
   var subsID:Int = -1
   type CompListener=(Map[Int, Composition]) => Unit
   val compListeners=new collection.mutable.HashSet[CompListener]()
   init()
   
   def compList=compMap.valuesIterator.toSeq
   
   def loadSettings(ref:Reference)=this.synchronized{
  	 var i:Int= -1
  	 subsID= ClientQueryManager.createSubscription(ref,1){
			(typ:NotificationType.Value,data:IndexedSeq[InstanceData])=> 			
			  this.synchronized {				  
					typ match {
						case NotificationType.sendData| NotificationType.updateUndo =>
              compMap= collection.immutable.SortedMap[Int,Composition]()++((data map (d=> {val n=new Composition(d); n.ix -> n} )) :+ (0->undefinedComposition))
              notifyListeners()
            case NotificationType.fieldChanged =>
              val comp= new Composition(data.head)
              compMap=compMap.updated(comp.ix,comp)
              notifyListeners()
            case NotificationType.instanceRemoved =>
              val comp=new Composition(data.head)
              compMap=compMap.filterNot(a=>a._1==comp.ix)
              notifyListeners()
            case _ =>
					}
			  }
			}		 	 
   } 
   
   def getMaterial(ix:Int):Option[Composition]= 
    compMap.get(ix)
    
   def quickGetComposition(ix:Int)=this.synchronized{     
     if(compMap.contains(ix))compMap(ix) else undefinedComposition 
   }
    
   def registerListener(listener:CompListener) = { 
       compListeners += listener
       listener(compMap)
     }
   
   private def notifyListeners() = compListeners foreach (_(compMap))
   
   override def shutDown() = {
     if(subsID > -1) ClientQueryManager.removeSubscription(subsID)
   }
     
}
class FontInfo(val name:String, val ref:Reference) {
  override def toString=name
}

object FontHandler extends AbstractSettingHandler  {
  val name="Fonts"
  var fontList:IndexedSeq[FontInfo]=IndexedSeq.empty
  val fontAliasList= collection.mutable.HashMap[String,String]()
  var rootRef:Reference=null
  init()
  //println("dims:"+ DimLineStyleHandler.styleMap.map(_._2.symbolElems).mkString)
  
  def getFont(fontName:String):String={
   for(f<-fontList) if(fontName.equalsIgnoreCase(f.name)) return f.name
   val lc=fontName.toLowerCase
   if(fontAliasList.contains(lc)) fontAliasList(lc)
   else null
  }
  

  def getFontInfo(fontName:String):FontInfo= if(fontName.length==0) defaultFont
  else {
    for(f<-fontList)
      if(fontName.equalsIgnoreCase(f.name)) return f
    val lc=fontName.toLowerCase
    if(fontAliasList.contains(lc)) {
      val alias=fontAliasList(lc)
      if(alias.equalsIgnoreCase(lc) ) throw new IllegalArgumentException("Recursive Alias "+fontName)
      getFontInfo(alias)
    } else throw new IllegalArgumentException("Cant find font '"+fontName+"'")
  }
  
  def loadSettings(ref:Reference)= this.synchronized{
    rootRef=ref
    fontList= ClientQueryManager.queryInstance(ref,1).map(inst=>{
      val aliases=inst.fieldValue(1).toString.split(";")
      val fontName=inst.fieldValue.head.toString.trim
      for(st<-aliases;aName=st.trim;if aName.length > 0)
        fontAliasList(aName.toLowerCase())=fontName
      new FontInfo(fontName,inst.ref)  
    }).sortBy(_.name)    
  }
  
  def defaultFont=fontList.head
  
  def addAlias(fontName:String,aliasName:String) = {
    val currentFontEntry=fontList.filter(_.name.equalsIgnoreCase(fontName))
    val dataRef= 
    if(currentFontEntry.isEmpty) {
      val instID=ClientQueryManager.createInstance(rootRef.typ,Array(new OwnerReference(1.toByte,rootRef)))
      new Reference(rootRef.typ,instID)
    }
    else  currentFontEntry.head.ref    
    val aliasesForFont=fontAliasList.filter( _._2==fontName).map(_._1).toSet +aliasName
    ClientQueryManager.writeInstanceField(dataRef,1,new StringConstant(aliasesForFont.mkString(";")))
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
  lazy val symbolElems=ClientQueryManager.queryInstanceFact(ref,0,GraphElemFactory) 
  
  def hideDimensionLine=(options & HideDimensionLine) >0
  def hideHelpLine=(options & HideHelpLine) > 0 
  def isStationDimLine=(options & IsStationLine) >0
  def hasFixedHelpLine=(options & FixedHelpLine)>0
  def hasHighMM=(options & HighMM)>0
  def unitStyle=(options >> 5) & 7
  
  def roundedMeasure(measure:Double)=if(roundMM==0) measure else (Math.round(measure/roundMM)*roundMM).toDouble
  
  def formatMeasure(measure:Double):Seq[String]= {    
    val ftext= (unitStyle match {
      case UnitMM => numberFormat.format(roundedMeasure(measure*1000))
      case UnitM => numberFormat.format(roundedMeasure(measure*1000)/1000)
      case UnitM_MM=>
        if(math.abs(measure) <1) f"${roundedMeasure(measure * 1000) / 10}%3.1f"
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
  
  
  val Match1="""(\d+[\.,]\d+)(\d)""".r
  val Match2="""(\d+)[\.,](\d)""".r
  
  
  var styleMap:collection.Map[Int,DimLineStyle]=Map.empty  
  var defaultStyle:DimLineStyle=null
  init()
  
  def loadSettings(ref:Reference)=  this.synchronized{    
   styleMap=collection.immutable.SortedMap[Int,DimLineStyle]()++ ClientQueryManager.queryInstance(ref,1).map(e=> {
     val d= new DimLineStyle(e)
     (d.id,d)
     }).toMap 
   defaultStyle=styleMap(1)
   //println("load dim styles "+ styleMap.mkString(" | "))
  }
  
  def getStyle(styleID:Int)=if(styleMap.contains(styleID)) styleMap(styleID) else defaultStyle
  
  def getStyleSeq=styleMap.values.toSeq
}

object LineColorsHandler extends AbstractSettingHandler {
  val name="LineColors"
  var colorMap:Map[Int,Int]=Map.empty  
  init()
  
  def loadSettings(ref:Reference)= 
    colorMap=ClientQueryManager.queryInstance(ref,1).map(e=>{(e.fieldValue.head.toInt,e.fieldValue(1).toInt)}).toMap
  
  def getLineColor(thickness:Int)=colorMap.getOrElse(thickness,0)
}
