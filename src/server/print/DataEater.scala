/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import java.io.DataOutput

import definition.data._

import scala.collection.mutable.ArrayBuffer
/**
 * 
 */

trait YPosHolder{
  var currentYPos:Float=0f
  var pageStartYPos:Float=0f
  var usablePageHeight:Float=0f
  var intRestHeight:Float=0f
  def copyFrom(other:YPosHolder): Unit ={
    currentYPos=other.currentYPos
    pageStartYPos=other.pageStartYPos
    usablePageHeight=other.usablePageHeight
    intRestHeight=other.intRestHeight
  }
}

class ProxyYHolder extends YPosHolder{}

class DataEater extends YPosHolder {
  val pagesList=collection.mutable.ArrayBuffer[PageData]()  
  
  var form:FormDescription = _	
  var paramValues:Iterable[ResultElement]=Nil
  
  var elementList=collection.mutable.ArrayBuffer[PrintElement]()
  
  var pageWidth:Float=_
  var pageHeight:Float=_
  
  var currentXPos:Float=0  
  val footerYPosHolder=new ProxyYHolder()
  
  var currentHeader:Option[Stamp]=_
  var currentFooter:Option[Stamp]=_
  
  var hasPrintElements:Boolean=false
  var currentContext:PrintContext=_
  
  def initPrinting(pWidth:Float,pHeight:Float,nform:FormDescription,ctx:PrintContext,nvalues:Iterable[ResultElement]): Unit = {
  	pageWidth=pWidth
  	pageHeight=pHeight
  	paramValues=nvalues
  	pagesList.clear
  	elementList.clear
  	form=nform
  	currentXPos=form.left
  	currentYPos=form.top
  	currentHeader=None
  	currentFooter=None
  	hasPrintElements=false
  	currentContext=ctx
  	ctx.setPageNr(1)
  	//initPage()
  }
  
  def addPage(withInit:Boolean=true): Unit = if(hasPrintElements){
    //println("Add Page "+(pagesList.size+1)+" withInit:"+withInit+" "+Thread.currentThread.getStackTrace().drop(2).take(10).mkString("\n")+"\n")
  	pagesList +=new PageData(pagesList.size+1,elementList)  	
  	if(withInit) initPage() 	
  	else {
  	  elementList=collection.mutable.ArrayBuffer[PrintElement]()
  	  hasPrintElements=false
  	}
  }
  
  def getCurrPageNum: Int =pagesList.size
  
  def initSection(): Unit = {
    //println("init section "+hasPrintElements)
  	if(hasPrintElements)addPage()
  	else initPage()
  }
  
  def initPage(): Unit = {
    //println("init page "+pagesList.size+" has:"+hasPrintElements)
  	currentXPos=form.left
  	currentYPos=form.top
  	elementList=collection.mutable.ArrayBuffer[PrintElement]()
  	currentContext.setPageNr(pagesList.size+1)
  	val tempInstance=currentContext.currentInstance
  	for(si<-currentContext.sectionInstance) currentContext.setCurrInstance(si)
  	for(c<-currentHeader) {
  		c.updateVariables(PrintEngine.context)
  		intRestHeight=restHeight
  		elementList++= c.getPrintElements(currentXPos,this,restWidth,c.minHeight,false)  
  		//currentYPos+=c.minHeight
  		//intRestHeight=restHeight-c.minHeight
  	}
  	for(c<-currentFooter) {
  		c.updateVariables(PrintEngine.context)
  		footerYPosHolder.currentYPos=pageHeight-form.bottom-c.minHeight
  		footerYPosHolder.intRestHeight=restHeight
  		elementList++= c.getPrintElements(currentXPos,footerYPosHolder,restWidth,c.minHeight,false)
  	}
  	intRestHeight=restHeight
  	pageStartYPos=currentYPos
  	usablePageHeight=restHeight
  	currentContext.setCurrInstance(tempInstance)
  	hasPrintElements=false
  }
  
  def restHeight: Float =pageHeight-currentYPos-form.bottom-(currentFooter match {
  	case Some(f)=>f.minHeight
  	case _=> 0})

  def restWidth: Float =pageWidth-currentXPos -form.right
  
	def addStamp(stamp:StampBox,horDirection:Boolean): Unit = {
  	if(!hasPrintElements)hasPrintElements=true
  	intRestHeight=restHeight
		if(horDirection) {
		  //println("Add Stamp hor:"+horDirection+" ")
		  val tempYPos=currentYPos		  
		  stamp.getPrintElements(currentXPos,this,restWidth,intRestHeight,measure = true)
		  currentYPos=tempYPos
			if(stamp.minWidth>restWidth) {addPage();hasPrintElements=true}
			elementList++= stamp.getPrintElements(currentXPos,this,restWidth,restHeight,measure = false)
			currentXPos+=stamp.minWidth
		}
		else {		  
		  val tempYPos=currentYPos // save YPos
		  val theRestWidth=restWidth
		  stamp.getPrintElements(currentXPos,this,theRestWidth,restHeight,measure = true) // measure !!!
		  currentYPos=tempYPos // restore YPos
		  //println("Add Stamp hor:"+horDirection+" y:"+currentYPos+" restHeight "+restHeight+" stamp.minHeight:"+stamp.minHeight)
		  val stampMinHeight=stamp.minHeight
		  val theRestHeight=restHeight
		  if(stampMinHeight>=theRestHeight){
		    //if(stamp.hasCutElement)println("Cuteelement "+stamp)
		    if(stamp.hasCutElement&&theRestHeight>PrintEngine.orphanTreshold&&
		        stampMinHeight>PrintEngine.widowTreshold*2) {
			    //println("cutelement "+stamp.inst)
		      val sendRestHeight=if(stampMinHeight-theRestHeight>PrintEngine.widowTreshold)theRestHeight 
		      		else theRestHeight-PrintEngine.widowTreshold 
			    val elList=stamp.getPrintElements(currentXPos,this,theRestWidth,sendRestHeight,measure = false)
			    val tempYPos=currentYPos
			    if(tempYPos>0) util.Log.e("Eater TempPos= falsch " +tempYPos)
			    for(el<-elList){
			      if(el.getElementType==PrintElType.PageBreak){
			        //println("Pagebreak")
			        addPage()
			        hasPrintElements=true
			      }
			      else elementList+=el
			    }			    
			    currentYPos= -tempYPos
		    } else {	      
		      //println("uncutted overrun "+stamp.inst)
		      addPage()
					hasPrintElements=true
		      elementList++= stamp.getPrintElements(currentXPos,this,theRestWidth,restHeight,measure = false)
		    } 
		  } else elementList++= stamp.getPrintElements(currentXPos,this,theRestWidth,theRestHeight,measure = false)
		  			
		}
	}
  
  
  def addCustomBlock(elList:Seq[PrintElement]): Unit = {
    for(el<-elList){      
      if(el.getElementType==PrintElType.PageBreak){
        val tempYPos= currentYPos
        addPage()
        currentYPos=tempYPos
        hasPrintElements=true
      }
      else elementList+=el
    }			    
  }
  
  def addPrintElements(newElems:Seq[PrintElement]):Unit = if(newElems.nonEmpty){
    if(!hasPrintElements)hasPrintElements=true    
    elementList++=newElems
  }
  
  def addPrintElement(newElem:PrintElement): Unit = {
    if(!hasPrintElements)hasPrintElements=true
    elementList += newElem
  }
  
  def getPagesData: ArrayBuffer[PageData] = {
    //System.out.println("getPagesData :"+hasPrintElements+" "+elementList.size)
    //if(!elementList.isEmpty) hasPrintElements=true
  	if (hasPrintElements ) {
  	  addPage(false)	  	  
  	}
  	pagesList
  }
  
  
  def write(out:DataOutput): Unit = DataEater.this.synchronized{
  	getPagesData  	
  	out.writeInt(pagesList.size)
  	pagesList.foreach(_.write(out))
  }
  //override def toString= pagesList.mkString("\n")
}