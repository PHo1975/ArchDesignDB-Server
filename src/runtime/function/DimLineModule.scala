package runtime.function
import server.storage.ActionModule
import server.storage.ActionIterator
import server.storage.ActionImpl
import server.storage.CreateActionImpl
import definition.data.InstanceData
import definition.expression.VectorConstant
import definition.expression.DoubleConstant
import definition.expression.StringConstant
import definition.data.OwnerReference
import definition.expression.Constant
import server.comm.{AbstractUserSocket, JavaClientSocket}
import definition.typ.{DataType,AnswerDefinition}
import definition.expression.Line3D
import definition.expression.Edge
import definition.expression.BlobConstant
import transaction.handling.TransactionManager
import definition.typ.DialogQuestion
import definition.typ.CommandQuestion
import definition.data.DimensionPoint
import definition.data.Reference
import definition.expression.ObjectReference
import server.storage.StorageManager

class DimLineModule extends ActionModule with GraphActionModule {
  import runtime.function.GraphElemModule._
  override val createActions=List(createDimLineAction)	
  val actions=List(moveMainLineAction,changeHelpLines,addDimPoint,delDimPoint,changeRefPoint,setMainRefPoint)
	
	lazy val moveMainLineAction=new ActionIterator("Linie verschieben",mvQuestion("Linie verschieben"),doMoveMainLine)
	
  lazy val changeHelpLines=new ActionIterator("Hilfslinien ändern",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"ChangeHelpLines")),doChangeHelpLines)
  
  lazy val changeRefPoint=new ActionImpl("Referenzpunkt ändern",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"ChangeRefPoint")),doChangeRefPoint)
  
  lazy val addDimPoint=new ActionIterator("Maßpunkt hinzufügen",Some( new DialogQuestion("Masspunkt<br>hinzufügen",Seq(
      new AnswerDefinition("Punkt angeben",DataType.VectorTyp,Some(new DialogQuestion("Hilfslinie bis",Seq(
          new AnswerDefinition("Punkt angeben",DataType.VectorTyp,None)))))))),doAddDimPoint,true)
  
  lazy val delDimPoint=new ActionIterator("Maßpunkt löschen",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"DelDimPoint")),doDelDimPoint)
  
	lazy val setMainRefPoint=new ActionIterator("HauptReferenz setzen",Some(new DialogQuestion("HauptReferenz setzen",
	    Seq(new AnswerDefinition("Nullpunkt auswählen",DataType.VectorTyp,Some(new DialogQuestion("Wert bei Nullpunkt",
	        Seq(new AnswerDefinition("Delta-Wert:",DataType.DoubleTyp,None)))))))),doSetMainRef) 
  
	def createDimLineAction=new CreateActionImpl("Maßlinie",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"CreateDimLine")),doCreateDimLine)
	
	def doCreateDimLine(u:AbstractUserSocket, parents:Seq[InstanceData], param:Seq[(String,Constant)], newTyp:Int, formFields:Seq[(Int,Constant)]):Boolean= {
    if(param.size<4) return false
		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
		//println("DimLine params:"+param.mkString("\n"))
		val position=param.head._2.toVector
		val angle:Double=param(1)._2 match {
			case d:DoubleConstant=> d.toDouble
			case v:VectorConstant=> math.atan2(v.y-position.y,v.x-position.x)*180d/math.Pi
			case s:StringConstant=> if(s.toString=="Vertikal") 90d else 0d
			case o:ObjectReference if o.toObjectReference.typ == TypeInfos.lineElemType =>
				val inst=StorageManager.getInstanceData(o.toObjectReference)
				val startPoint=inst.fieldValue(3).toVector
				val endPoint=inst.fieldValue(4).toVector
				(endPoint-startPoint).XYAngle *180d/math.Pi
		}
    val partIx=param.indexWhere(_._2.getType==DataType.StringTyp,2)
    val points=if(partIx<0) param.drop(2).map(_._2.toVector) 
      else for(i<-2 until partIx) yield param(i)._2.toVector
    val helpPolyPoints=if(partIx<0) Seq.empty else for(i<-partIx+1 until param.size) yield param(i)._2.toVector
    //println("points:"+points.mkString(" | "))
    //println("poly points:"+helpPolyPoints.mkString(" | "))
    if(points.size>1) {
      val helpLineLengths:Seq[Double]=if(helpPolyPoints.size<2) Seq.empty.padTo(points.length,0d)
      else {
        val mainLineVect=VectorConstant.fromAngle2D(angle*math.Pi/180d)
        val hdirVect=new VectorConstant(-mainLineVect.y,mainLineVect.x,0)
        val mline=new Line3D(position,mainLineVect)
        val intersectionEdges=points.map(p=>new Edge(p,mline.intersectionWith(new Line3D(p,hdirVect))))
        val polyEdges=(for(Seq(a,b)<-helpPolyPoints.sliding(2)) yield new Edge(a,b)).toSeq
        for(edge<-intersectionEdges) yield {
          //val len=edge.length
          var curLen=0d
          for(poly<-polyEdges) edge.getIntersectionWith(poly) match {
            case Some(v)=>
							val crossPos=(v-edge.p1).toDouble
							//println("Helpline Cross: edge:"+edge+" point:"+v+" crossPos:"+crossPos+" len:"+len)
							if(crossPos>curLen) curLen=crossPos
						case _=>
          }
          curLen
        }
      }
      //println("Help Line Lengths:"+helpLineLengths.mkString(", "))
      val blob=BlobConstant.fillData(out=>{
		    out.writeInt(points.size)
		    for(i<-0 until points.size){
		      points(i).write(out)
		      out.writeDouble(helpLineLengths(i))
		      out.writeBoolean(false)
		    }		    
		  })		
	    val inst=TransactionManager.tryCreateInstance(theTypeID,parentRef,false)
	    TransactionManager.tryWriteInstanceField(inst.ref,1,param.head._2)
	    TransactionManager.tryWriteInstanceField(inst.ref,3,new DoubleConstant(angle))
	    TransactionManager.tryWriteInstanceField(inst.ref,4,blob)
	    writeFormatParams(inst.ref,formFields)   
	    true
    }		  
    else false
  } 
  
	
  def doMoveMainLine(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
    if(param.size==2) {
			val delta = param.head._2 match {
			  case startPoint:VectorConstant=>
					val endPoint=param(1)._2.toVector
					endPoint-startPoint
				case deltaX:DoubleConstant if param(1)._2.getType == DataType.DoubleTyp =>
					new VectorConstant (deltaX.toDouble,param(1)._2.toDouble,0)				 
			  case _=> throw new IllegalArgumentException("Falscher Parametertyp verschieben "+param.head._2+", "+param(1)._2)
			}		
			for(d <-data;if d.ref.typ == theTypeID)
			  TransactionManager.tryWriteInstanceField(d.ref,1,d.fieldValue(1).toVector+delta)						  
			true	
		}
		else false
  }
  
  
  def doChangeHelpLines(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
    if(param.size<2) return false    
    val helpPolyPoints=param map (_._2.toVector)
    val polyEdges=(for(Seq(a,b)<-helpPolyPoints.sliding(2)) yield new Edge(a,b)).toSeq
    //println("Change Helplines "+polyEdges.mkString(" \n "))
    updateDimList(data,(ref,position,mainLineVect,hdirVect,dimList)=>{
      val mline=new Line3D(position,mainLineVect)
      val intersectionEdges=dimList.map(p=>new Edge(p.refPoint,mline.intersectionWith(new Line3D(p.refPoint,hdirVect))))
      for(i<-0 until dimList.size;edge=intersectionEdges(i)) yield {          
       	var curLen=0d
       	for(poly<-polyEdges) edge.getIntersectionWith(poly) match {
        	case Some(v)=>
						val crossPos=(v-edge.p1).toDouble
						//println("cross:"+crossPos+" i:"+i+" edge:"+edge+ " poly:"+poly)
						if(crossPos>curLen) curLen=crossPos
					case _=>
        }
        if(curLen>0) dimList(i).helpLineLength=curLen
       }
       //println("\n result list"+dimList.mkString("\n"))
       val newBlob=DimensionPoint.createBlob(dimList)
       TransactionManager.tryWriteInstanceField(ref,4,newBlob)
    })   
    true
  }
  
  def doChangeRefPoint(u:AbstractUserSocket,data:InstanceData,param:Seq[(String,Constant)]):Boolean =  {
    val changePoint=param.head._2.toInt
    val newPos=param(1)._2.toVector
    data.fieldValue(4) match {
    	case blob:BlobConstant =>
				val dimList=DimensionPoint.createDimLineList(blob)
				val d=dimList(changePoint)
				d.refPoint=newPos
				val newBlob=DimensionPoint.createBlob(dimList)
				TransactionManager.tryWriteInstanceField(data.ref,4,newBlob)
			case _ =>
    }   
    true
  }
   
  def moveElement(elem:InstanceData,delta:VectorConstant):Unit = {
		TransactionManager.tryWriteInstanceField(elem.ref,1,elem.fieldValue(1).toVector+delta)
		elem.fieldValue(4).getValue match {
		  case blob:BlobConstant =>
				val dimList=DimensionPoint.createDimLineList(blob)
				for(d<-dimList){
          d.refPoint=d.refPoint+delta
          d.textPos match {
            case Some(pos)=> d.textPos=Some(pos+delta)
            case None=>
          }
        }
				val newBlob=DimensionPoint.createBlob(dimList)
				TransactionManager.tryWriteInstanceField(elem.ref,4,newBlob)
			case _=>
		}		
	}
  
  def copyElement(elem:InstanceData,delta:VectorConstant) = {
		val retel=elem.setField(1,elem.fieldValue(1).toVector+delta)		
		elem.fieldValue(4).getValue match {
		  case blob:BlobConstant =>
				val dimList=DimensionPoint.createDimLineList(blob)
				for(d<-dimList){
          d.refPoint=d.refPoint+delta
          d.textPos match {
            case Some(pos)=> d.textPos=Some(pos+delta)
            case None=>
          }
        }
				val newBlob=DimensionPoint.createBlob(dimList)
				retel.setField(4,newBlob)
			case _=>retel
		}	
	}	
  
   def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  val result=elem.setField(1,mirror(elem.fieldValue(1).toVector))
	  elem.fieldValue(4).getValue match {
	    case blob:BlobConstant =>
				val dimList=DimensionPoint.createDimLineList(blob)
				for(d<-dimList){
          d.refPoint=mirror(d.refPoint)
          for(pos<-d.textPos) d.textPos=Some(mirror(pos))
        }
				result.setField(4,DimensionPoint.createBlob(dimList))
			case _=> result
	  }
	} 
  
  
  
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= {
	  TransactionManager.tryWriteInstanceField(elem.ref,1,rotator(elem.fieldValue(1).toVector))
	  GraphElemModule.rotateAngleField(elem,3,angle)
	  elem.fieldValue(4).getValue match {
		  case blob:BlobConstant =>
				val dimList=DimensionPoint.createDimLineList(blob)
				for(d<-dimList){
          d.refPoint=rotator(d.refPoint)
          for(pos<-d.textPos) d.textPos=Some(rotator(pos))
        }
				val newBlob=DimensionPoint.createBlob(dimList)
				TransactionManager.tryWriteInstanceField(elem.ref,4,newBlob)
			case _=>
		}
	}
	
	override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = {
	  val p1=elem.fieldValue(1).toVector
	  if(chPoints.contains(p1)) TransactionManager.tryWriteInstanceField(elem.ref,1,p1+delta)
	  var blobChanged=false
		elem.fieldValue(4).getValue match {
		  case blob:BlobConstant =>
				val dimList=DimensionPoint.createDimLineList(blob)
				for(d<-dimList){
          if(chPoints.contains(d.refPoint)) {
            d.refPoint=d.refPoint+delta
            if(!blobChanged) blobChanged=true
          }
          d.textPos match {
            case Some(pos)=> if(chPoints.contains(pos)) {
              d.textPos=Some(pos+delta)
              if(!blobChanged) blobChanged=true
            }
            case None=>
          }
        }
				if(blobChanged) {
          val newBlob=DimensionPoint.createBlob(dimList)
          TransactionManager.tryWriteInstanceField(elem.ref,4,newBlob)
        }
			case _=>
		}		
	  
	}
  
  
  /**
   * @param func  a function(ObjReference,StartPoint,MainDir,HelpDir,DimList)
   * 
   */
  private def updateDimList(data:Seq[InstanceData],func:(Reference,VectorConstant,VectorConstant,VectorConstant,Seq[DimensionPoint])=>Unit)= {
    for (d<-data;if d.ref.typ == theTypeID) {
      val position=d.fieldValue(1).toVector
      val angle=d.fieldValue(3).toDouble
      d.fieldValue(4) match {
        case blob:BlobConstant =>
					val dimList=DimensionPoint.createDimLineList(blob)
					val mainLineVect=VectorConstant.fromAngle2D(angle*math.Pi/180d)
					val hdirVect=new VectorConstant(-mainLineVect.y,mainLineVect.x,0)
					func(d.ref,position,mainLineVect,hdirVect,dimList)
				case _ =>
      }
    }
  }
  
  def doAddDimPoint(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
    if(param.size!=2) return false
    val newPoint=param.head._2.toVector
    val helpPoint=param(1)._2.toVector
    updateDimList(data,(ref,position,mainLineVect,hdirVect,dimList)=>{
      val mline=new Line3D(position,mainLineVect)
      val hline=new Line3D(newPoint,hdirVect)
      val iPoint=mline.intersectionWith(hline)
      val lpoint=hline.orthProjection(helpPoint)
      val dist=lpoint-newPoint
      val hdirUnit=(iPoint-newPoint).unit
      val sc=dist.getScaleTo(hdirUnit)
      println("mline:"+mline+" hline:"+hline+" ipoint:"+iPoint+" lpoint:"+lpoint+" dist:"+dist+" sc:"+sc)
      
      val helpLineLength=if(sc<0) (iPoint-newPoint).toDouble else sc
      val newBlob=DimensionPoint.createBlob(dimList :+ new DimensionPoint(newPoint,sc,None))
       TransactionManager.tryWriteInstanceField(ref,4,newBlob)
    })    
    true
  }
  
  def doDelDimPoint(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
    if(param.size!=2) return false
    val point=param.head._2.toVector
    val lcd=param(1)._2.toDouble
    updateDimList(data,(ref,position,mainLineVect,hdirVect,dimList)=>{
      val mline=new Line3D(position,mainLineVect)      
      val newDimList=if(dimList.size<3) dimList else dimList.filter(dimElem => {
        val ip=mline.intersectionWith(new Line3D(dimElem.refPoint,hdirVect))
        math.abs(ip.x - point.x) > lcd || math.abs(ip.y - point.y) > lcd
      })
      val newBlob=DimensionPoint.createBlob(newDimList)
      TransactionManager.tryWriteInstanceField(ref,4,newBlob)
    })
    true
  }
  
  def doSetMainRef(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
    if(param.size!=2) return false
    println("Set Main Ref "+param.mkString("| "))
    val point=param.head._2.toVector
    val delta=DoubleConstant(param.last._2.toDouble)
    for (d<-data;if d.ref.typ == theTypeID) {
      TransactionManager.tryWriteInstanceField(d.ref,5,point)
      TransactionManager.tryWriteInstanceField(d.ref,6,delta)
    }
    true
  }

   
}