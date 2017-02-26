package client.graphicsView
import definition.data.Reference
import definition.expression.VectorConstant
import java.awt.Color
import java.awt.Graphics2D
import definition.expression.{Polygon,PointList}
import definition.expression.NULLVECTOR



class TierLine(val p1:VectorConstant,val p2:VectorConstant,val tierDef:TierDef){
  var e1=p1 // endpoints
  var e2=p2
  lazy val dist=e2-e1
  var hatchPoly:Polygon=null
  def createHatch(lastLine:TierLine)= {
    hatchPoly=SectionLineElement.createHatchPoly(e1,e2,lastLine.e2,lastLine.e1)
  }
  def intersectTo(otherLine:TierLine,isP1:Boolean) = for(ip<-VectorConstant.intersection2D(p1,p2,otherLine.p1,otherLine.p2)){    
    if(isP1) e1=ip
    else e2=ip
  }
  
  override def toString="p1:"+p2+" p2:"+p2
}

class LineConnection(val otherLine:SectionLineElement,val isP1:Boolean){ 
  override def toString=" LC "+otherLine.ref+" "+(if(isP1)"p1" else "p2")
}

case class SectionLineElement(nref:Reference,nstartPoint:VectorConstant,nendPoint:VectorConstant,dir:VectorConstant,material:Int,offset:Double,connAreaRef:Reference) extends 
   AbstractLineElement(nref,0,0,0,nstartPoint,nendPoint) {
   var p1LeftConnection:Option[LineConnection]=None
   var p2LeftConnection:Option[LineConnection]=None
   var p1RightConnection:Option[LineConnection]=None
   var p2RightConnection:Option[LineConnection]=None
   
   def getP1RightConn=if(p1RightConnection.isDefined)p1RightConnection else if(p1LeftConnection.isDefined) p1LeftConnection else None
   def getP2RightConn=if(p2RightConnection.isDefined)p2RightConnection else if(p2LeftConnection.isDefined) p2LeftConnection else None
   
   lazy val p12vect=endPoint-startPoint
   lazy val p21vect=startPoint-endPoint
   
   lazy val dirPointsLeft=VectorConstant.pointLocation2D(startPoint,endPoint,startPoint+dir)>0
   val middle=(startPoint+endPoint)*0.5
   val dirEnd=middle+dir
   val dotStyle=6
   val startWidth=30
   val compos=CompositionHandler.quickGetComposition(material)
   
   val dist1=dir*offset
   val startLine=new TierLine(nstartPoint+dist1,endPoint+dist1,if(compos.tiers.isEmpty)CompositionHandler.undefinedTier else compos.tiers.head)
   val tierLines= compos.tiers.foldLeft((offset, List(startLine)))(createTierLine)._2
   //println("SL "+nref+" comp:"+compos+" Lines:"+tierLines.mkString(";"))
   
   val numTierLines=tierLines.size-1
   
   val firstConstrTierIx=tierLines.indexWhere(_.tierDef.role==SectionLineElement.constructionTierType)
   val lastConstrTierIx=tierLines.lastIndexWhere(_.tierDef.role==SectionLineElement.constructionTierType)+1
   
   val firstOutTiers=if(firstConstrTierIx== -1)tierLines.toSeq else (0 to firstConstrTierIx) map tierLines
   val lastOutTiers=if(lastConstrTierIx== -1)Seq.empty else (tierLines.size-1 to lastConstrTierIx by -1) map tierLines
   val firstCheckOutTiers=if(lastConstrTierIx== -1)Seq.empty else (0 to lastConstrTierIx) map tierLines
   val lastCheckOutTiers=if(firstConstrTierIx== -1)Seq.empty else (tierLines.size-1 to firstConstrTierIx+1 by -1) map (ix=>{val tl=tierLines(ix);new TierLine(tl.p1,tl.p2,tierLines(ix-1).tierDef)})
  
   override def draw(g:Graphics2D,sm:Scaler,selectColor:Color=null)={
    val col=if(selectColor==null) ColorMap.getColor(color)else selectColor    
    //val connStr=" "+(if(p1LeftConnection.isDefined)"1L ")+(if(p1RightConnection.isDefined)"1R ")+(if(p2LeftConnection.isDefined)"2L ")+(if(p2RightConnection.isDefined)"2R ")
		val connStr=nref.sToString+(if(dirPointsLeft)" L"else " R")+lastConstrTierIx
    g.setPaint(col)		
		g.setStroke(sm.getStroke(if(lineWidth>0)lineWidth else 1,dotStyle))
		intDrawLine(startPoint,endPoint)	
	  intDrawLine(middle,dirEnd)
	  g.drawString(connStr,sm.xToScreen(middle.x) ,sm.yToScreen(middle.y) )
	  g.setStroke(sm.getStroke(startWidth,0))	
	  
	  for(tierLine<-tierLines) {	    
	    g.setStroke(sm.getStroke(tierLine.tierDef.lineThick,tierLine.tierDef.lineStyle.ix))
	    intDrawLine(tierLine.e1,tierLine.e2)
	    if(tierLine.hatchPoly!=null&& tierLine.tierDef.material.hatch>0) {
	      HatchHandler.drawHatch(tierLine.hatchPoly,HatchHandler.quickGetHatch(math.abs(tierLine.tierDef.material.hatch)),sm,
	          tierLine.tierDef.material.hatch<0,g,col,NULLVECTOR,0,NULLVECTOR)
	    }
	    
	  }
    g.setColor(Color.yellow)
    intDrawLine(tierLines.head.e1,tierLines.head.e2)
    
    if(selectColor!=null) {
    	for(conn <-p1LeftConnection) {
    		g.setStroke(sm.getStroke(15,2))
    		g.setColor( Color.green)
    		val ndir = if(conn.isP1) (conn.otherLine.endPoint-conn.otherLine.startPoint)*.3
    		else (conn.otherLine.startPoint-conn.otherLine.endPoint)*.3
    		val epos=startPoint+ndir
    		intDrawLine(startPoint,epos)   
    		g.drawString(SectionLineElement.roundDouble(SectionLineElement.getAngle(this,conn,true)),sm.xToScreen(epos.x) ,sm.yToScreen(epos.y) )
    	}
    	for(conn <-p1RightConnection) {
    		g.setStroke(sm.getStroke(15,3))
    		g.setColor( Color.blue )
    		val ndir = if(conn.isP1) (conn.otherLine.endPoint-conn.otherLine.startPoint)*.3    		
    		else (conn.otherLine.startPoint-conn.otherLine.endPoint)*.3
    		val epos=startPoint+ndir
    		intDrawLine(startPoint,startPoint+ndir)
    		g.drawString(SectionLineElement.roundDouble(SectionLineElement.getAngle(this,conn,true)),sm.xToScreen(epos.x) ,sm.yToScreen(epos.y) )
    	}
    	for(conn <-p2LeftConnection) {
    		g.setStroke(sm.getStroke(15,4))
    		g.setColor( Color.cyan)
    		val ndir = if(conn.isP1) (conn.otherLine.endPoint-conn.otherLine.startPoint)*.3
    		else (conn.otherLine.startPoint-conn.otherLine.endPoint)*.3
    		val epos=endPoint+ndir
    		intDrawLine(endPoint,endPoint+ndir)
    		g.drawString(SectionLineElement.roundDouble(SectionLineElement.getAngle(this,conn,false)),sm.xToScreen(epos.x) ,sm.yToScreen(epos.y) )
    	}
    	for(conn <-p2RightConnection) {
    		g.setStroke(sm.getStroke(15,5))
    		g.setColor( Color.pink )
    		val ndir = if(conn.isP1) (conn.otherLine.endPoint-conn.otherLine.startPoint)*.3
    		else (conn.otherLine.startPoint-conn.otherLine.endPoint)*.3
    		val epos=endPoint+ndir
    		intDrawLine(endPoint,endPoint+ndir)      
    		g.drawString(SectionLineElement.roundDouble(SectionLineElement.getAngle(this,conn,false)),sm.xToScreen(epos.x) ,sm.yToScreen(epos.y) )
    	}
    }
    
	 
		def intDrawLine(n1:VectorConstant,n2:VectorConstant)= 
				GraphElemConst.drawLineFloat(g,sm.xToScreen(n1.x) ,sm.yToScreen(n1.y) ,sm.xToScreen(n2.x),sm.yToScreen(n2.y))
	}
   
  
   
  def createTierLine(curr:(Double,List[TierLine]),tierDef:TierDef):(Double,List[TierLine])= {    
    val distVect=dir*(curr._1+tierDef.thickness)
    val p1=startPoint+distVect
    val p2=endPoint+distVect
    val nl=new TierLine(p1,p2,tierDef)
    (curr._1+tierDef.thickness,nl::curr._2)
  }
  
  private def intersectTiers(ri:(LineConnection,Boolean),thisIterator:Iterator	[TierLine],isP1:Boolean)= {
  		 val otherIter=if(ri._2) ri._1.otherLine.firstCheckOutTiers.iterator else ri._1.otherLine.lastCheckOutTiers.iterator
  		 if(otherIter.hasNext&&thisIterator.hasNext) {
  			 var currentOther=otherIter.next()
  			 var currentTier=thisIterator.next()
  			 currentTier.intersectTo(currentOther,isP1)
  			 while(thisIterator.hasNext) {
  			   currentTier=thisIterator.next()
  				 while(otherIter.hasNext && (
  				     (currentOther.tierDef.rolePriority==currentTier.tierDef.rolePriority&&currentOther.tierDef.material.priority==currentTier.tierDef.material.priority&&
  				         ri._1.otherLine.ref.instance<ref.instance)||
  				     (currentOther.tierDef.rolePriority==currentTier.tierDef.rolePriority&&currentOther.tierDef.material.priority<currentTier.tierDef.material.priority)||
  						 currentOther.tierDef.rolePriority<currentTier.tierDef.rolePriority)) currentOther=otherIter.next()
  				currentTier.intersectTo(currentOther,isP1)
  			 }
  		 }   
  }
  
  
  
  def adaptLineLengths() = if(tierLines.size>1){   
    val p1RightInfo=for(rightConn1<-getP1RightConn) yield (rightConn1,rightConn1.otherLine.dirPointsLeft^rightConn1.isP1)     
    //val rightMostP1Tier=for(rightConn1<-getP1RightConn) yield if(rightConn1.otherLine.dirPointsLeft^rightConn1.isP1)rightConn1.otherLine.tierLines.first else rightConn1.otherLine.tierLines.last
    val p1LeftInfo=for(leftConn1<-p1LeftConnection) yield (leftConn1,!(leftConn1.otherLine.dirPointsLeft^leftConn1.isP1))
    //val leftMostP1Tier=for(leftConn1<-p1LeftConnection) yield if(leftConn1.otherLine.dirPointsLeft^leftConn1.isP1)leftConn1.otherLine.tierLines.last else leftConn1.otherLine.tierLines.first
    val p2RightInfo=for(rightConn2<-getP2RightConn) yield (rightConn2,rightConn2.otherLine.dirPointsLeft^rightConn2.isP1) 
    //val rightMostP2Tier=for(rightConn2<-getP2RightConn) yield if(rightConn2.otherLine.dirPointsLeft^rightConn2.isP1)rightConn2.otherLine.tierLines.first else rightConn2.otherLine.tierLines.last
    val p2LeftInfo=for(leftConn2<-p2LeftConnection) yield (leftConn2,!(leftConn2.otherLine.dirPointsLeft^leftConn2.isP1))
    //val leftMostP2Tier=for(leftConn2<-p2LeftConnection) yield if(leftConn2.otherLine.dirPointsLeft^leftConn2.isP1)leftConn2.otherLine.tierLines.last else leftConn2.otherLine.tierLines.first
    if(dirPointsLeft) { // start this by first      
      for(ri<-p1RightInfo) intersectTiers(ri,firstOutTiers.iterator,true)     
      for(ri<-p1LeftInfo) intersectTiers(ri,lastOutTiers.iterator,true)
      //for(lm<-leftMostP1Tier) tierLines.last.intersectTo(lm,true)
      for(ri<-p2RightInfo) intersectTiers(ri,lastOutTiers.iterator,false)
      //for(rm<-rightMostP2Tier) tierLines.last.intersectTo(rm,false)
      for(ri<-p2LeftInfo) intersectTiers(ri,firstOutTiers.iterator,false)
      //for(lm<-leftMostP2Tier) tierLines.first.intersectTo(lm,false)
    }
    else { // start this by last            
    	for(ri<-p1RightInfo) intersectTiers(ri,lastOutTiers.iterator,true)   	               
      for(ri<-p1LeftInfo) intersectTiers(ri,firstOutTiers.iterator,true)
    	//for(lm<-leftMostP1Tier ) tierLines.first.intersectTo(lm,true)
      for(ri<-p2RightInfo) intersectTiers(ri,firstOutTiers.iterator,false)
      //for(rm<-rightMostP2Tier) tierLines.first.intersectTo(rm,false)
      for(ri<-p2LeftInfo) intersectTiers(ri,lastOutTiers.iterator,false)
    	//for(lm<-leftMostP2Tier ) tierLines.last.intersectTo(lm,false)     	
    }
  }
  
  def updateHatches() = {
    for(i<- 0 until tierLines.size-1;next=tierLines(i+1);that=tierLines(i))
      that.createHatch(next)
  }
  
  def createClone=new SectionLineElement(nref,nstartPoint,nendPoint,dir,material,offset,connAreaRef)
   
  override def toString= "SLine "+nref.sToString+" ("+startPoint.shortToString+","+endPoint.shortToString+", Offset:"+offset+", mat:"+material+", connRef:"+connAreaRef.sToString+")"
  
  
}

object SectionLineElement {
  val constructionTierType=1
  def getAngle(a:SectionLineElement,b:LineConnection,isP1:Boolean):Double= {
    val otherPoint=if(b.isP1)b.otherLine.endPoint else  b.otherLine.startPoint
    val pointLoc = if(isP1)  VectorConstant.pointLocation2D(a.endPoint,a.startPoint,otherPoint)  else VectorConstant.pointLocation2D(a.startPoint,a.endPoint,otherPoint)       
    val avect=if(isP1)a.p21vect else a.p12vect    
    val bvect=if(b.isP1)b.otherLine.p12vect else b.otherLine.p21vect    
    avect.angleBetweenRad(bvect)*(if(pointLoc > VectorConstant.tolerance)-1 else 1)    
  }
  
  def roundDouble(d: Double)=(math.round(d*100d)/100d).toString
  
  def createHatchPoly(p1:VectorConstant,p2:VectorConstant,p3:VectorConstant,p4:VectorConstant) = {
		  new Polygon(Seq.empty,Seq(new PointList(Seq(p1,p2,p3,p4))))
		}
}