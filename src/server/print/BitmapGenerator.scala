package server.print

import java.awt.geom.Rectangle2D
import java.io.File

import definition.data._
import javax.imageio.ImageIO
import server.config.FSPaths

import scala.util.control.NonFatal

class BitmapGenerator extends CustomGenerator {
  
  class DataSet(dataParent:InstanceData){
    lazy val name=dataParent.fieldValue.head.toString
    val link=new File(util.JavaUtils.restoreDelimiters(util.JavaUtils.resolveImagePath(FSPaths.imageDir,dataParent.fieldValue(1).toString)))
    val dwidth=dataParent.fieldValue(2).toFloat
    val dheight=dataParent.fieldValue(3).toFloat
    val xMargin=dataParent.fieldValue(4).toFloat
    val aspect=if(link.exists) {      
      try {
      val image=ImageIO.read(link)
      //System.out.println("image :"+link+" w:"+image.getWidth+" h:"+image.getHeight+" "+(image.getWidth.toFloat/image.getHeight.toFloat))
      image.getWidth.toFloat/image.getHeight.toFloat
      } catch {case NonFatal(e) => util.Log.e("when reading image :'"+link.toString+ "' ",e );1f
      case other:Throwable =>println(other);System.exit(0);1f}
    } else {util.Log.e("image :"+link+" does not exist "); 1f}
     
  }
  
  def fillPlotPage(dataParent:InstanceData,dataEater:DataEater,generator:SinglePageGenerator):Unit= {
    
  }
  override def fillInIterator(dataParent:InstanceData,dataEater:DataEater,context:PrintContext):Seq[PrintElement]={    
    var retList:List[PrintElement]=Nil
    val textHeight=5f
    
    val dataSet=new DataSet(dataParent)
    val (width,height)=if(dataSet.dwidth>0) (dataSet.dwidth,dataSet.dwidth/dataSet.aspect) 
       else if(dataSet.dheight>0) (dataSet.aspect*dataSet.dheight,dataSet.dheight) else
         (dataEater.restWidth -dataSet.xMargin,(dataEater.restWidth -dataSet.xMargin)/dataSet.aspect)
    if((height+textHeight)>dataEater.restHeight) {					  
		  retList=PageBreakMarker::retList
		  dataEater.currentYPos=dataEater.pageStartYPos				  
		}     
    retList=new BitmapPrintElement(new Rectangle2D.Float(dataEater.currentXPos+dataSet.xMargin,dataEater.currentYPos,width,height),dataSet.link.toString())::
    new TextPrintElement(new Rectangle2D.Float(dataEater.currentXPos+dataSet.xMargin,dataEater.currentYPos+height,0,textHeight),dataSet.name,"Standard")::retList
    dataEater.currentYPos+=height+textHeight     
    dataEater.intRestHeight=dataEater.usablePageHeight-dataEater.currentYPos
    retList.reverse
  }
}