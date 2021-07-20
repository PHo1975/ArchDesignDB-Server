package client.importer

import client.graphicsView.Handlers._
import client.graphicsView.{FontHandler, HatchHandler, HatchStyle}
import definition.data.LineStyle

import java.io.{BufferedReader, File, FileReader}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

class UnknownLineStyle(val name:String,val dots:Seq[Float],setting:DXFSettings) {
  override def toString: String ="("+name+"|"+dots.mkString(",")+")"+
    (setting.lineStyleMapping.get(name) match {
    	case Some(ix)=>" => " +LineStyleHandler.stylesList(ix).name
    	case None =>""
    }
    )  
}

class HatchStylewithUnknownLS (n:String,a1:Double,ls1:Int,d1:Double,a2:Double,ls2:Int,d2:Double,
    val lsName1:Option[String],val lsName2:Option[String]) extends HatchStyle(0,n,a1,ls1,d1,a2,ls2,d2,10) {
  override def toString: String = "UKHatch "+n+ " ls1:"+ls1+ " ls2:"+ls2+" lsName1:"+lsName1+ " lsName2:"+lsName2
 
}
   


class DXFSettings {
  var drawingScale:Double  =1
  var textYAdjust=1d
  var drawingScaleID:Int=0
  var fontScale:Double=1
  val drawingLineStyles: ArrayBuffer[LineStyle] =ArrayBuffer[LineStyle]()
  val ignoreLabels=Seq("ByLayer","ByBlock")
  
  val lineStyleMapping: mutable.HashMap[String, Int] =collection.mutable.HashMap[String,Int]()
  val unknownLineStyles: ArrayBuffer[UnknownLineStyle] =ArrayBuffer[UnknownLineStyle]()
  val hatchStyleMapping: mutable.HashMap[String, Int] =collection.mutable.HashMap[String,Int]()
  val unknownHatchStyles: ArrayBuffer[HatchStyle] =ArrayBuffer[HatchStyle]()
  val unknownFonts: ArrayBuffer[String] =ArrayBuffer[String]()
  val layers: ArrayBuffer[String] =ArrayBuffer[String]()
  var selectedLayers:Seq[Int]=Seq.empty
  var allTextsBlack=true
  var dx:Double=0d
  var dy:Double=0d
  var explodeBlocks:Boolean=false
  var globalScale:Double=1.0
  
  def analyzeFile(file:File):Unit = {
     val reader=new BufferedReader(new FileReader(file))
     try {
       readLineStyles(reader)       
     }
     catch {
       case NonFatal(e)=> util.Log.e(e)
       case other:Throwable =>println(other);System.exit(0)
     }
     finally {
       reader.close()
     }
  }  
  
  /** scans a subclass in a dxf file
   * @return (shouldFinish,resultSet)
   * 
   */
  protected def scanSubClass(reader:BufferedReader,acClassName:String,endLabel:String):(Boolean,collection.Map[Int,Seq[String]])= {
    var cn:String=""
    var result=mutable.HashMap[Int,Seq[String]]()
    do {
    	while("100" !=cn.trim) {
        cn=reader.readLine
        if (cn == null) return (true, result)
      }
      cn=reader.readLine.trim
    } while (cn!=acClassName)
    var code:Int =0
    //println("Found ")
    do {
      code=Integer.parseInt(reader.readLine.trim)
      //println("Code:"+code)
      if(code==0) {
        return(endLabel==reader.readLine ,result)
      }  
      val value=reader.readLine
      result(code)= if(result.contains(code)) {
        result(code) :+ value          
      } else Seq(value)
    } while (code != 0)
    (false,result)
  }
  
  def loopSubClass(reader:BufferedReader,className:String,endLabel:String,func:(collection.Map[Int,Seq[String]])=>Unit):Unit= {
    var (shouldFinish,resMap)=scanSubClass(reader,className,endLabel)    
    while (resMap.nonEmpty){
      func(resMap)           
      if(shouldFinish) return
      val a=scanSubClass(reader,className,endLabel)
      shouldFinish=a._1
      resMap=a._2
    }
  }
  
  def partion[T](partLens:Seq[Int],all: Seq[T]):Seq[Seq[T]]= {
    var rest=all
    var result:Seq[Seq[T]]=Seq.empty
    for(i<-partLens) {
      result= result :+ rest.take(i) 
      rest=rest.drop(i)
    }
    result
  }
  
  def sToDouble(s:String): Double =java.lang.Double.parseDouble(s.trim)
  def sToInt(s:String): Int =Integer.parseInt(s.trim)
  
  def findUnknownLineStyleName(dots:Seq[Float]):Option[String]=
    unknownLineStyles.find(_.dots==dots).map(_.name)
  
  protected def readLineStyles(reader:BufferedReader):Unit={
    lineStyleMapping.clear()
    unknownLineStyles.clear()
    hatchStyleMapping.clear()
    unknownHatchStyles.clear()
    unknownFonts.clear()
    layers.clear()
    val dotScale= 1000d / drawingScale
    loopSubClass(reader,"AcDbLinetypeTableRecord","ENDTAB",(resMap)=>{
      val name=resMap(2).head.trim
      val descName=resMap(3).headOption
      val numElems=sToInt(resMap(73).head)
      if(numElems== 0) {
        lineStyleMapping(name)=0 // straight line
      } else {
        //println("Dots:"+resMap(49))
        val dots=resMap(49).map(a=>math.abs(sToDouble(a)*dotScale).toFloat)
        val styleIx=LineStyleHandler.findStyleID(dots)
        if(styleIx> -1) lineStyleMapping(name)=styleIx
        else unknownLineStyles +=new UnknownLineStyle(name,dots,this)
      }
    })
    
    loopSubClass(reader,"AcDbLayerTableRecord","ENDTAB",(resMap)=>{
      val name=resMap(2).head
      val flag=resMap(70).head
      //println("Layer found "+name)
      layers+=name
    })
    
    loopSubClass(reader,"AcDbTextStyleTableRecord","ENDTAB",(resMap)=>{
      val name=resMap(2).head
      val height=resMap(40).head
      val widthFactor=resMap(41).head
      //println("Font :"+name+" height:"+height+" widthFactor:"+widthFactor)
      if(FontHandler.getFont(name)==null)
        unknownFonts+=name
    })
    loopSubClass(reader,"AcDbHatch","ENDTAB",(resMap)=>{
      if(sToInt(resMap(76).head)==2){// custom pattern type
        val name=resMap(2).head.trim
        if(!hatchStyleMapping.contains(name) && !unknownHatchStyles.exists(_.name==name)) {
        	val numPatternLines=sToInt(resMap(78).head)
        	if(numPatternLines>0) { // 2 pattern lines 
        		val dashNums=resMap(79).map(a=>sToInt(a))
        		if(!resMap.contains(49)) util.Log.e(" hatch "+name+" dashNums "+dashNums+" no dots")
        		val allDots=if(!resMap.contains(49)) Seq.empty else resMap(49).map(a=>math.abs(sToDouble(a)*dotScale).toFloat)
        		val dotList=partion(dashNums,allDots)
        		//println("Hatch "+name+" dotList:"+dotList)
        		var allLineStylesFit=true
        		var newHatchIx=1
        		val lineStyleIDs=dotList.map(d=> {
        			val styleIx=LineStyleHandler.findStyleID(d)
        			if(styleIx== -1) { // style not found
        				allLineStylesFit=false
        				if(!unknownLineStyles.exists(_.dots==d)) {
        					unknownLineStyles += new UnknownLineStyle(name+newHatchIx,d,this)
        					//println("Hatchline "+(name+newHatchIx)+" added to unknownLineStyles")
        					newHatchIx+=1
        				} //else println("HatchLine found in unknownLineStyles")
        			} //else println("Hatchline found as "+LineStyleHandler.stylesList(styleIx).name)
        			styleIx
        		})
        		val angle1=sToDouble(resMap(53).head)
        		val ox= sToDouble(resMap(45).head)
        		val oy= sToDouble(resMap(46).head)
        		val distance1=math.sqrt(ox*ox+oy*oy)
        		if (numPatternLines==1) {
        			if(allLineStylesFit) {
        				val hatchStyleID=HatchHandler.findHatchID(angle1,lineStyleIDs.head,distance1,0,-1,0)
        				if(hatchStyleID> -1) hatchStyleMapping(name)=hatchStyleID
        				else unknownHatchStyles += HatchStyle(-1, name, angle1, lineStyleIDs.head, distance1, 0, -1, 0, 10)
        			} 
        			else unknownHatchStyles += new HatchStylewithUnknownLS(name,angle1,-1,distance1,0,-1,0,
        					findUnknownLineStyleName(dotList.head),None)
        		} else { // numpatternlines>1
        		  val angle2=sToDouble(resMap(53)(1))
        		  val ox1=sToDouble(resMap(45)(1))
        		  val ox2=sToDouble(resMap(46)(1))
        		  val distance2=math.sqrt(ox*ox+oy*oy)
              if(allLineStylesFit) {
        				val hatchStyleID=HatchHandler.findHatchID(angle1,lineStyleIDs.head,distance1,angle2,lineStyleIDs(1),distance2)
        				//println("AllLinestylesFit " +hatchStyleID)
        				//println("Angle1:"+angle1+" lineStyle1:"+lineStyleIDs(0)+" d1:"+distance1+" a2:"+angle2+" ls2:"+lineStyleIDs(1)+
        				//    " d2:"+distance2)
        				//val h20=HatchHandler.hatchList(20)
        				//println("A1:"+(angle1==h20.angle1)+" ls1:"+(lineStyleIDs(0)== h20.lineStyle1)+" d1:"+(distance1==h20.distance1)
        				if(hatchStyleID> -1) hatchStyleMapping(name)=hatchStyleID
        				else unknownHatchStyles += new HatchStyle(-1,name,angle1,lineStyleIDs.head,distance1,angle2,lineStyleIDs(1),
        				    distance2,10)       	  	 
        			} 
        			else unknownHatchStyles += new HatchStylewithUnknownLS(name,angle1,lineStyleIDs.head,distance1,
        			    angle2,lineStyleIDs(1),distance2,if(lineStyleIDs.head== -1) findUnknownLineStyleName(dotList.head) else None,
        			    if(lineStyleIDs(1)== -1)findUnknownLineStyleName(dotList(1)) else None)
        		}        	
        	}  
        }
      }  
    })
  }
}