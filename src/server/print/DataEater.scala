/**
 * Author: Peter Started:28.12.2010
 */
package server.print

import java.io.DataOutput
import definition.data.FormDescription
import definition.data.PageData
import definition.data.PrintElement
import definition.expression.Constant
import definition.data.PrintElType
/**
 * 
 */

trait YPosHolder{
  var currentYPos:Float=0f
  var pageStartYPos:Float=0f
  var usablePageHeight:Float=0f
  var intRestHeight:Float=0f
  def copyFrom(other:YPosHolder)={
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
  var paramValues:Seq[(String,Constant)]=Nil
  
  var elementList=collection.mutable.ArrayBuffer[PrintElement]()
  
  var pageWidth:Float=_
  var pageHeight:Float=_
  
  var currentXPos:Float=0  
  val footerYPosHolder=new ProxyYHolder()
  
  var currentHeader:Option[Stamp]=_
  var currentFooter:Option[Stamp]=_
  
  var hasPrintElements:Boolean=false
  var currentContext:PrintContext=_
  
  def initPrinting(pWidth:Float,pHeight:Float,nform:FormDescription,ctx:PrintContext,nvalues:Seq[(String,Constant)])= {
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
  
  def addPage(withInit:Boolean=true) = if(hasPrintElements){
    //println("Add Page "+(pagesList.size+1)+" withInit:"+withInit+" "+Thread.currentThread.getStackTrace().drop(2).take(10).mkString("\n")+"\n")
  	pagesList +=new PageData(pagesList.size+1,elementList)  	
  	if(withInit) initPage() 	
  	else {
  	  elementList=collection.mutable.ArrayBuffer[PrintElement]()
  	  hasPrintElements=false
  	}
  }
  
  def getCurrPageNum=pagesList.size
  
  def initSection() = {
  	if(hasPrintElements)addPage()
  	else initPage()
  }
  
  def initPage() = {
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
  
  def restHeight=pageHeight-currentYPos-form.bottom-(currentFooter match {
  	case Some(f)=>f.minHeight
  	case _=> 0})
  def restWidth=pageWidth-currentXPos -form.right
  
	def addStamp(stamp:StampBox,horDirection:Boolean)= {    
  	if(!hasPrintElements)hasPrintElements=true
  	intRestHeight=restHeight
		if(horDirection) {
		  //println("Add Stamp hor:"+horDirection+" ")
		  val tempYPos=currentYPos		  
		  stamp.getPrintElements(currentXPos,this,restWidth,intRestHeight,true)
		  currentYPos=tempYPos
			if(stamp.minWidth>restWidth) {addPage(true);hasPrintElements=true}
			elementList++= stamp.getPrintElements(currentXPos,this,restWidth,restHeight,false)
			currentXPos+=stamp.minWidth
		}
		else {		  
		  val tempYPos=currentYPos // save YPos
		  val theRestWidth=restWidth
		  stamp.getPrintElements(currentXPos,this,theRestWidth,restHeight,true) // measure !!!
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
			    val elList=stamp.getPrintElements(currentXPos,this,theRestWidth,sendRestHeight,false)
			    val tempYPos=currentYPos
			    if(tempYPos>0) util.Log.e("Eater TempPos= falsch " +tempYPos)
			    for(el<-elList){
			      if(el.getElementType==PrintElType.PageBreak){
			        //println("Pagebreak")
			        addPage(true)
			        hasPrintElements=true
			      }
			      else elementList+=el
			    }			    
			    currentYPos= -tempYPos
		    } else {	      
		      //println("uncutted overrun "+stamp.inst)
		      addPage(true)
					hasPrintElements=true
		      elementList++= stamp.getPrintElements(currentXPos,this,theRestWidth,restHeight,false)
		    } 
		  } else elementList++= stamp.getPrintElements(currentXPos,this,theRestWidth,theRestHeight,false)  
		  			
		}
	}
  
  
  def addCustomBlock(elList:Seq[PrintElement])= {
    for(el<-elList){      
      if(el.getElementType==PrintElType.PageBreak){
        val tempYPos= currentYPos
        addPage(true)
        currentYPos=tempYPos
        hasPrintElements=true
      }
      else elementList+=el
    }			    
  }
  
  def addPrintElements(newElems:Seq[PrintElement])= if(newElems.nonEmpty){
    if(!hasPrintElements)hasPrintElements=true    
    elementList++=newElems
  }
  
  def addPrintElement(newElem:PrintElement)= {
    if(!hasPrintElements)hasPrintElements=true
    elementList += newElem
  }
  
  def getPagesData = {
    //System.out.println("getPagesData :"+hasPrintElements+" "+elementList.size)
    //if(!elementList.isEmpty) hasPrintElements=true
  	if (hasPrintElements ) {
  	  addPage(false)	  	  
  	}
  	pagesList
  }
  
  
  def write(out:DataOutput)= DataEater.this.synchronized{
  	getPagesData  	
  	out.writeInt(pagesList.size)
  	pagesList.foreach(_.write(out))
  }
  //override def toString= pagesList.mkString("\n")
}