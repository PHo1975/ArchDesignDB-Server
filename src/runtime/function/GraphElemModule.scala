/**
 * Author: Peter Started:04.10.2010
 */
package runtime.function

import java.io.{ByteArrayInputStream, DataInputStream}

import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.{BlobConstant, Constant, DoubleConstant, EMPTY_EX, Edge, Expression, Line3D, ObjectReference, PointList, Polygon, PolygonDivider, StringConstant, VectorConstant}
import definition.typ.{AnswerDefinition, CommandQuestion, DataType, DialogQuestion}
import server.comm.{AbstractUserSocket, JavaClientSocket}
import server.storage.{ActionIterator, ActionModule, CreateActionImpl, StorageManager}
import transaction.handling.{ActionList, TransactionManager}
import util.{GraphUtils, Log, StringUtils}

/**
 * 
 */

class Circle (val data:InstanceData) extends AnyVal {
  def centerPoint=data.fieldValue(3).toVector
	def diameter=data.fieldValue(4).toDouble
	def startAngle=data.fieldValue(5).toDouble
	def endAngle=data.fieldValue(6).toDouble
	override def toString="Circle center"+centerPoint+" diameter:"+diameter+" startAngle:"+startAngle+" endAngle:"+endAngle
}


class Line(val data:InstanceData) extends AnyVal {
  def p1= data.fieldValue(3).toVector
	def p2= data.fieldValue(4).toVector
	def delta=p2-p1
	def length={
		val d=delta
		math.sqrt(d.x*d.x+d.y*d.y)
	}
	override def toString="Line p1:"+p1+" p2:"+p2
}



trait GraphActionModule {
	def moveElement(elem:InstanceData,delta:VectorConstant):Unit
	def copyElement(elem:InstanceData,delta:VectorConstant):InstanceData
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit
	def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData
	def cutElementByLine(elem:InstanceData,cutLine:Edge):Unit= {}
	def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]):Unit = {}
	def scale(elem:InstanceData,refPoint:VectorConstant,sx:Double,sy:Double):Unit= {}
	
	var theTypeID:Int= -1
	
	def setObjectType(typeID:Int) = {
		//System.out.println("set object type ":+typeID+" "+this)
		theTypeID=typeID
		TypeInfos.moduleMap(theTypeID)=this
	}
	
	def writeFormatParams(ref:Reference,params:Seq[(Int,Constant)]):Unit = {
	  for(p <-params)
	  TransactionManager.tryWriteInstanceField(ref,p._1.toByte,p._2)
	}
	
	def scalePoint(oPoint:VectorConstant,refPoint:VectorConstant,sx:Double,sy:Double)={
	  //println("scalePoint "+oPoint+" sx:"+sx+" sy:"+sy)
	  new VectorConstant(refPoint.x+(oPoint.x-refPoint.x)*sx,refPoint.y+(oPoint.y-refPoint.y)*sy,0)
	}
  
  protected def pointModField(fieldNr:Int,elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = {
    val p1=elem.fieldValue(fieldNr).toVector    
    if (chPoints.contains(p1)) TransactionManager.tryWriteInstanceField(elem.ref,fieldNr.toByte,p1+delta)    
  }
}

object GraphElemModule{  
  val wrongExtendTreshold=10000d
  val nearNullTreshold=0.00001d
  def singlePointQuestion(qname:String,pname:String)=Some(new DialogQuestion(qname,Seq(new AnswerDefinition(pname,DataType.VectorTyp,None))))
  
  def mvQuestion(aName:String)=Some(new DialogQuestion(aName+"<br>Distanz angeben",
		Seq(new AnswerDefinition("'von Punkt' angeben",DataType.VectorTyp,
		    Some(new DialogQuestion(aName+"<br>Distanz",
		    		Seq(new AnswerDefinition("'nach Punkt' angeben",DataType.VectorTyp,None)))) ),
			  new AnswerDefinition("Delta X eingeben:",DataType.DoubleTyp,dyQuestion)
			)))
  
  
	lazy val dyQuestion= Some(new DialogQuestion("Eingabe Distanzwert",
		Seq(new AnswerDefinition("delta Y eingeben:",DataType.DoubleTyp,None))))
		
	def checkAngle(angle:Double)= if(angle==360) 360d else  angle % 360d
	
	def rotateAngleField(elem:InstanceData,fieldNr:Byte,angle:Double):Double= {
    val newAngle=checkAngle(elem.fieldValue(fieldNr).toDouble+angle*180d/math.Pi)
	  TransactionManager.tryWriteInstanceField(elem.ref,fieldNr,new DoubleConstant(newAngle))
    newAngle
  }  
	def pointFromAngle(centerPoint:VectorConstant,angle:Double,radius:Double) = 
		new VectorConstant(centerPoint.x+scala.math.cos(angle*scala.math.Pi/180d)*radius,
			centerPoint.y+scala.math.sin(angle*scala.math.Pi/180d)*radius,0)  
  
  def getAngle(p:VectorConstant,center:VectorConstant):Double= {
	    val v=math.atan2(p.y-center.y, p.x-center.x)*180d/math.Pi
	    if(v<0) v+360d else v
	  }

	def cutPointQuestion=Some(new DialogQuestion("Point1",Seq(new AnswerDefinition("hitpoint1",DataType.VectorTyp,
		Some(new DialogQuestion("Point2",Seq(	new AnswerDefinition("hitpoint2",DataType.VectorTyp,None)),false)))),false))
}

class GraphElemModule extends ActionModule {
  import runtime.function.GraphElemModule._
  var graphTypeID:Int= -1  
  
	def setObjectType(typeID:Int) =graphTypeID=typeID 
  
	
	val moveAction=new ActionIterator("Verschieben",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	    "Move")),doMove)
	
	val copyAction=new ActionIterator("Kopieren",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",	
	    "Copy")),doCopy,true)	
	
	val rotateAction=new ActionIterator("Drehen",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",	
	    "Rotate")),doRotate)

  val rotateMultAction=new ActionIterator("Mehrfach Drehen",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
    "RotateMulti")),doRotateMulti)

  val pointModAction=new ActionIterator("Punkt-Mod",Some(new DialogQuestion("Punkt-Mod",Seq(new AnswerDefinition("Punkte wählen",DataType.BlobTyp,mvQuestion("Punkt-Mod"),"SelectPoints")))),
      doPointMod)
 
  val scaleAction=new ActionIterator("Verzerren",Some(new DialogQuestion("Bezugspunkt",Seq(new AnswerDefinition("Bezugspunkt eingeben",DataType.VectorTyp ,
      	Some(new DialogQuestion("Faktor X",Seq(new AnswerDefinition("Faktor X eingeben",DataType.DoubleTyp ,
      	Some(new DialogQuestion("Faktor Y",Seq(new AnswerDefinition("Faktor Y eingeben",DataType.DoubleTyp ,None)))))))))))),
      doScale)
  
  val createSymbolAction=new ActionIterator("Symbol erzeugen",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
        "CreateSymbolStamp")),doCreateSymbol,false,900)
  
  val mirrorAction=new ActionIterator("Spiegeln",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
      "Mirror")),doMirror)
	
	lazy val actions=List(moveAction,copyAction,rotateAction,rotateMultAction,mirrorAction,pointModAction,scaleAction,createSymbolAction)
	
	
	def doMove(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {		
		if(param.size==2) {
			val delta = param.head._2 match {
			  case startPoint:VectorConstant=>
					val endPoint=param(1)._2.toVector
					endPoint-startPoint
				case deltaX:DoubleConstant if param(1)._2.getType == DataType.DoubleTyp =>
					new VectorConstant (deltaX.toDouble,param(1)._2.toDouble,0)				 
			  case _=> throw new IllegalArgumentException("Falscher Parametertyp verschieben "+param.head._2)
			}				
			//System.out.println("move delta:"+delta)
			for(d <-data) {
				TypeInfos.moduleMap(d.ref.typ).moveElement(d, delta)					
			}			  
			true	
		}
		else false
	}
	
	def doCopy(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {		
		if(param.size==3||param.size==2) {
		  val (offset,numCopy)=if(param.head._2.getType==DataType.IntTyp)(1,param.head._2.toInt) else (0,1)
			val delta = if(param(0+offset)._2.getType==DataType.VectorTyp )	{
					val startPoint=param(0+offset)._2.toVector
					val endPoint=param(1+offset)._2.toVector					
					endPoint-startPoint
				}
				else if(param(0+offset)._2.getType==DataType.DoubleTyp )
					new VectorConstant (param(0+offset)._2.toDouble,param(1+offset)._2.toDouble,0)
				else throw new IllegalArgumentException(" move wrong parametertype ")	
		  for(i<-1 to numCopy;theDelta=delta*i;d <-data) {
				val createInst=TransactionManager.tryCreateInstance(d.ref.typ,d.owners,false)
				var newInst=d.clone(createInst.ref,d.owners,Seq.empty)
				newInst=TypeInfos.moduleMap(d.ref.typ).copyElement(newInst,theDelta)					
				TransactionManager.tryWriteInstanceData(newInst)
			}			
			true	
		}
		else false
	}	
	
	
	def doRotate(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {
	  //println("Rotate params:\n"+param.mkString(" | " ))
	  val center=param.head._2.toVector
	  val angle=param(1)._2 match {
	    case d:DoubleConstant =>d.n*math.Pi/180d
	    case p1:VectorConstant =>
				val p2=param(2)._2.toVector
				val startAngle=math.atan2(p1.y-center.y,p1.x-center.x)
				val endAngle=math.atan2(p2.y-center.y,p2.x-center.x)
				endAngle-startAngle
			case r1:ObjectReference =>
				val angle=getLineAngle(param(2)._2.toObjectReference) - getLineAngle(r1.toObjectReference)
				if(angle>math.Pi) angle-math.Pi else if(angle<math.Pi) angle+math.Pi else angle
		}
	  val cosa=math.cos(angle)
	  val sina=math.sin(angle)
	  val rotator=GraphUtils.createRotator(center,angle)
	  for(d<-data) {
	    TypeInfos.moduleMap(d.ref.typ).rotateElement(d,angle,rotator)
	  }
	  true
	}

  def doRotateMulti(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {
    val num=param.head._2.toInt
    val center=param(1)._2.toVector
    val angle=param(2)._2 match {
      case d:DoubleConstant =>d.n*math.Pi/180d
      case p1:VectorConstant =>
				val p2=param(3)._2.toVector
				val startAngle=math.atan2(p1.y-center.y,p1.x-center.x)
				val endAngle=math.atan2(p2.y-center.y,p2.x-center.x)
				endAngle-startAngle
			case r1:ObjectReference =>
				val angle=getLineAngle(param(3)._2.toObjectReference) - getLineAngle(r1.toObjectReference)
				if(angle>math.Pi) angle-math.Pi else if(angle<math.Pi) angle+math.Pi else angle
		}
    for(i<-1 to num){
      val rAngle=i.toDouble*angle
      val cosa=math.cos(rAngle)
      val sina=math.sin(rAngle)
      val rotator=GraphUtils.createRotator(center,rAngle)
      for(d<-data) {
        val createInst=TransactionManager.tryCreateInstance(d.ref.typ,d.owners,false)
        var newInst=d.clone(createInst.ref,d.owners,Seq.empty)
        TransactionManager.tryWriteInstanceData(newInst)
        TypeInfos.moduleMap(d.ref.typ).rotateElement(newInst, rAngle, rotator)
      }
    }
    true
  }
	
	def doMirror(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {	
	  val withCopies=param.head._2.getType==DataType.StringTyp          
	  val (p1,p2)=param(if(withCopies)1 else 0)._2 match {
	    case oref:ObjectReference =>
				val lineInst=StorageManager.getInstanceData(oref.toObjectReference)
				(lineInst.fieldValue(3).toVector,lineInst.fieldValue(4).toVector)
			case p1:VectorConstant=> (p1,param(if(withCopies)2 else 1)._2.toVector)
	  }
	  val mirrorLine=new Line3D(p1,p2-p1)	 	  
	  for(d<-data){
	    val inst=if(withCopies) {
	      val createInst=TransactionManager.tryCreateInstance(d.ref.typ,d.owners,false)
				d.clone(createInst.ref,d.owners,Seq.empty)
	    } else d
	    val result=TypeInfos.moduleMap(d.ref.typ).mirrorElement(inst,mirrorLine.mirrorPoint)
	    TransactionManager.tryWriteInstanceData(result)
	  }
	  true
	}
	
	
	private def getLineAngle(ref:Reference):Double= {
	  val lineInst=StorageManager.getInstanceData(ref)
	  val p1=lineInst.fieldValue(3).toVector
	  val p2=lineInst.fieldValue(4).toVector
	  math.atan2(p2.y-p1.y,p2.x-p1.x)
	}
	
	def doPointMod(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {
	   val delta = if(param(1)._2.getType==DataType.VectorTyp ){
					val startPoint=param(1)._2.toVector
					val endPoint=param(2)._2.toVector					
					endPoint-startPoint
				}
				else if(param(1)._2.getType==DataType.DoubleTyp )
					new VectorConstant (param(1)._2.toDouble,param(2)._2.toDouble,0)
				else throw new IllegalArgumentException(" move wrong parametertype ")	  
	  param.head._2 match {
	    case b:BlobConstant =>
				val inStream=new DataInputStream(new ByteArrayInputStream(b.data))
				val numPoints=inStream.readInt()
				val pointList=new collection.immutable.HashSet ++(for(i<-0 until numPoints ) yield Expression.readConstant(inStream).toVector)
				for(d<-data)
          TypeInfos.moduleMap(d.ref.typ).pointMod(d,delta,pointList)
			case _ =>
	  }
	  true
	}	
	
	def doScale(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)])= {
	  val refPoint= param.head._2.toVector
	  val sx=param(1)._2.toDouble
	  val sy=param(2)._2.toDouble
	 //println("scale refPoint:"+refPoint+" sx:"+sx+" sy:"+sy+" "+data.mkString(","))
	  for(d<-data) 
	      	 TypeInfos.moduleMap(d.ref.typ).scale(d,refPoint,sx,sy)
	  true
	}
  
  def doCreateSymbol(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {
    val parentFolder=param(2)._2.toObjectReference
    println("parentFolder "+parentFolder)
    val owner=Array(new OwnerReference(1,parentFolder))
    val refPoint=param.head._2.toVector* -1d
    println("refPoint:"+refPoint)
    val name=param(1)._2
    val symbolInst=TransactionManager.tryCreateInstance(TypeInfos.symbolDefType,owner,false)
    TransactionManager.tryWriteInstanceField(symbolInst.ref, 0, name)
    val sowner=Array(new OwnerReference(0,symbolInst.ref))    
     for(d <-data) {
        val createInst=TransactionManager.tryCreateInstance(d.ref.typ,sowner,false)
        var newInst=d.clone(createInst.ref,sowner,Seq.empty)
        newInst=TypeInfos.moduleMap(d.ref.typ).copyElement(newInst,refPoint)          
        TransactionManager.tryWriteInstanceData(newInst)
      }         
      true
    }
}



class TextModule extends ActionModule with GraphActionModule {
	val horizontalText="Horizontal"
  override val createActions=List(createTextAction)
  val actions=List(replaceAction,alignHor,alignVert,distributeAction)
  
  def alignHor=new ActionIterator("Vert.ausrichten",None,doAlignHor)
  def alignVert=new ActionIterator("Hor.ausrichten",None,doAlignVert)
	def distributeAction=new ActionIterator("Verteilen",Some(new DialogQuestion("Verteilen",Seq(new AnswerDefinition("Ausrichtung",DataType.EnumTyp,None,horizontalText+",Vertikal")))),
		doDistribute)
	
	def moveElement(elem:InstanceData,delta:VectorConstant) = {
		TransactionManager.tryWriteInstanceField(elem.ref,2,elem.fieldValue(2).toVector+delta)		
	}
	def copyElement(elem:InstanceData,delta:VectorConstant) = {
		elem.setField(2,elem.fieldValue(2).toVector+delta)
	}
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= {
	   TransactionManager.tryWriteInstanceField(elem.ref,2,rotator(elem.fieldValue(2).toVector))
	  TransactionManager.tryWriteInstanceField(elem.ref,7,new DoubleConstant(elem.fieldValue(7).toDouble+angle*180d/math.Pi))	  
	}
	
	def createTextAction=new CreateActionImpl("Text", Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"CreateText")),doCreateText)
	
	def doCreateText(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
    if(parents.size>1) {Log.e("Multiple parents !");return false}
	  val (pos,text)=if(param.size==3) (param(1)._2,param(2)._2) else (param.head._2,param(1)._2)
	  if(text.toString().length>0) {
	    //println("create Text params:"+param.mkString("|")+"\nFormatparams:"+formFields.mkString("|"))
	    val inst=TransactionManager.tryCreateInstance(theTypeID,Array(new OwnerReference(0.toByte,parents.head.ref)),false)
	    TransactionManager.tryWriteInstanceField(inst.ref,1,text)
	  	TransactionManager.tryWriteInstanceField(inst.ref,2,pos)		  	
	  	writeFormatParams(inst.ref,formFields)	    	    
	  } else println("Wrong param.size:"+param.mkString("| "))
	  
	  true
	}
	override def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  elem.setField(2,mirror(elem.fieldValue(2).toVector))
	}
	
	override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = pointModField(2,elem,delta,chPoints)
  
	
	def replaceAction=new ActionIterator("Text ersetzen",Some(new DialogQuestion("Text ersetzen",
		Seq(new AnswerDefinition("Suche nach",DataType.StringTyp,
		    Some(new DialogQuestion("Text ersetzen",Seq(new AnswerDefinition("Ersetzen mit",DataType.StringTyp,None))))) ))),doReplace)
	
	def doReplace(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {
	  if(param.size!=2) {println("Ersetzen falsche parameter:"+param.mkString(","));return false}
	  val searchText=param.head._2.toString
	  val replaceText=param(1)._2.toString
	  for(d<-data;if d.ref.typ == theTypeID){
	    val text=d.fieldValue(1).toString
	    if(text.contains(searchText)){
	      TransactionManager.tryWriteInstanceField(d.ref,1,StringConstant(text.replace(searchText,replaceText)))
	    }
	  }
	  true
	}
	
	def doAlignHor (u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  if(data.size>1){
    var xValue=0d
    for(d<-data) 
      xValue+=d.fieldValue(2).toVector.x    
    val newValue=xValue/data.size.toDouble
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,2.toByte,new VectorConstant(newValue,d.fieldValue(2).toVector.y,0))
    true
	} else false
	
	def doAlignVert (u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  if(data.size>1){
    var yValue=0d
    for(d<-data) 
      yValue+=d.fieldValue(2).toVector.y    
    val newValue=yValue/data.size.toDouble
    for(d<-data)
      TransactionManager.tryWriteInstanceField(d.ref,2.toByte,new VectorConstant(d.fieldValue(2).toVector.x,newValue,0))
    true
	} else false

	def doDistribute (u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  if(data.size>2){
		//println("Verteilen "+param.mkString("|"))
		val horizontal= param.head._2.toString==horizontalText

		def getValue(d:InstanceData):Double={
			val vector=d.fieldValue(2).toVector
			if(horizontal) vector.x else vector.y
		}

		val sortedList=data.sortBy(getValue)		
		val min:Double=getValue(sortedList.head)
		val max:Double=getValue(sortedList.last)
		val step=(max-min)/(data.size-1)
    var current=min+step
    for(i<-1 until data.size-1;d=sortedList(i)){
			val vector=d.fieldValue(2).toVector
			val nv=new VectorConstant(if(horizontal)current else vector.x,if(horizontal)vector.y else current,vector.z)
			TransactionManager.tryWriteInstanceField(d.ref,2,nv)
			current+=step
		}

		true
	} else false
}



class BitmapModule extends ActionModule with GraphActionModule {
	override val createActions=List(createBitmapAction)
	val actions = Nil

	def moveElement(elem:InstanceData,delta:VectorConstant) = {
		TransactionManager.tryWriteInstanceField(elem.ref,6,elem.fieldValue(6).toVector+delta)
	}
	def copyElement(elem:InstanceData,delta:VectorConstant) = {
		elem.setField(6,elem.fieldValue(6).toVector+delta)
	}
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= {
		TransactionManager.tryWriteInstanceField(elem.ref,6,rotator(elem.fieldValue(6).toVector))
		TransactionManager.tryWriteInstanceField(elem.ref,4,new DoubleConstant(elem.fieldValue(4).toDouble+angle*180d/math.Pi))
	}

	def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
		elem.setField(6,mirror(elem.fieldValue(6).toVector))
	}

	def createBitmapAction=new CreateActionImpl("Bitmap", Some(new DialogQuestion("Bitmap erzeugen",
		Seq(new AnswerDefinition("Dateipfad",DataType.StringTyp,Some(
			new DialogQuestion("Bitmap erzeugen",Seq(new AnswerDefinition("Absetzposition",DataType.VectorTyp,None)))))))),doCreateBitmap)

	def doCreateBitmap(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
		if(parents.size>1) {Log.e("Multiple parents !");return false}
		val path=param(0)._2
		val pos=param(1)._2
		if(path.toString.length>0) {
			val inst=TransactionManager.tryCreateInstance(theTypeID,Array(new OwnerReference(0.toByte,parents.head.ref)),false)
			TransactionManager.tryWriteInstanceField(inst.ref,1,path)
			TransactionManager.tryWriteInstanceField(inst.ref,6,pos)
			writeFormatParams(inst.ref,formFields)
		}
		true
	}
}



class LineModule extends ActionModule with GraphActionModule {
	override val createActions=List(createLineAction,createRectAction,/*createCutLineAction,*/createOrthoLineAction,createTangentAction,createParPolyAction)
	val actions=List(extendAction,cutPartAction,parallelAction,cutElemsAction)	
	
	def moveElement(elem:InstanceData,delta:VectorConstant) = {
		TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toVector+delta)
		TransactionManager.tryWriteInstanceField(elem.ref,4,elem.fieldValue(4).toVector+delta)
	}
	def copyElement(elem:InstanceData,delta:VectorConstant) = {
		elem.setField(3,elem.fieldValue(3).toVector+delta).setField(4,
		elem.fieldValue(4).toVector+delta)
	}
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= {
	  TransactionManager.tryWriteInstanceField(elem.ref,3,rotator(elem.fieldValue(3).toVector))
	  TransactionManager.tryWriteInstanceField(elem.ref,4,rotator(elem.fieldValue(4).toVector))
	}
	override def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  elem.setField(3,mirror(elem.fieldValue(3).toVector)).setField(4,mirror(elem.fieldValue(4).toVector))
	}
	
	override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = {
    pointModField(3,elem,delta,chPoints)
    pointModField(4,elem,delta,chPoints)	  
	}
	
	override def scale(elem:InstanceData,refPoint:VectorConstant,sx:Double,sy:Double):Unit= {
	  val p1=elem.fieldValue(3).toVector
	  val p2=elem.fieldValue(4).toVector
	  TransactionManager.tryWriteInstanceField(elem.ref,3,scalePoint(p1,refPoint,sx,sy))
	  TransactionManager.tryWriteInstanceField(elem.ref,4,scalePoint(p2,refPoint,sx,sy))
	}
	
		
	def createLineAction=new CreateActionImpl("Linie",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"LineTo")),doCreateLine)
	
	def doCreateLine(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {		
  	//System.out.println("create line "+param.mkString)
  	//System.out.println("newTyp:"+newTyp+" theTyp:"+theTypeID)
  	makeLinesFromParams(parents,param,formFields)	    
		true
	}
	
	private def makeLinesFromParams(nparents:Seq[InstanceData],param:Seq[(String,Constant)],formFields:Seq[(Int,Constant)])= {
	  var lastPoint=param.head._2.toVector
	  var parents=Array(new OwnerReference(0.toByte,nparents.head.ref))
	  for(i<- 1 until param.size) {
	    val np=param(i)
	    val nextPoint=np match {
	      case (_,v:VectorConstant)=> v
	      case ("dx",d:DoubleConstant) =>  lastPoint + new VectorConstant(d.toDouble,0d,0d)
	      case ("dy",d:DoubleConstant) =>	 lastPoint + new VectorConstant(0d,d.toDouble,0d)
	      case o => throw new IllegalArgumentException("Wrong parameter "+o)
	    }
	    makeLine(parents,lastPoint,nextPoint,formFields)
	    lastPoint=nextPoint
	  }
	}
	
	private def makeLine(parents:Seq[InstanceData],p1:VectorConstant,p2:VectorConstant,formFields:Seq[(Int,Constant)]):InstanceData= 
	  makeLine(Array(new OwnerReference(0.toByte,parents.head.ref)),p1,p2,formFields)
	
	private def makeLine(parentRefs:Array[OwnerReference],p1:VectorConstant,p2:VectorConstant,formFields:Seq[(Int,Constant)]):InstanceData={	 
	  val inst=TransactionManager.tryCreateInstance(theTypeID,parentRefs,false)
	  TransactionManager.tryWriteInstanceField(inst.ref,3,p1)
	  TransactionManager.tryWriteInstanceField(inst.ref,4,p2)
	  writeFormatParams(inst.ref,formFields)
	  inst
	}
	
	
	def createTangentAction=new CreateActionImpl("Tangente",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"Tangent")),doCreateTangent)
	
	def doCreateTangent(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	 // System.out.println("create Tangent "+param.mkString)
	  createTangent(parents,param.head._2.toObjectReference,param(1)._2.toVector,param(2)._2.toInt==0,formFields)
	  true
	}
	
	private def createTangent(parents:Seq[InstanceData],circleRef:Reference,point:VectorConstant,first:Boolean,formFields:Seq[(Int,Constant)]):Unit= {
	  val cInst=StorageManager.getInstanceData(circleRef)
	  val center=cInst.fieldValue(3).toVector
	  val diameter=cInst.fieldValue(4).toDouble
	  val dist=(center-point).toDouble
    if(dist!=0&& diameter!=0){
    	val hyp=math.sqrt(dist*dist-diameter*diameter)
    	for (tp<-VectorConstant.triangulationPoint2D(point,center,hyp,diameter,first))
         makeLine(parents,point,tp,formFields)              
    }
	}
	
	def extendAction=new ActionIterator("Verschneiden",Some(new DialogQuestion("Linie(n) verschneiden",
		Seq(new AnswerDefinition("mit Linie",DataType.ObjectRefTyp,
		    Some(new DialogQuestion("isEdible",Seq(new AnswerDefinition("edible",DataType.BoolTyp,None)))),
		        "M"+TypeInfos.lineElemType.toString+","+TypeInfos.arcElemType.toString)))),doExtend)


	def intersectLineArc(line:Line,arc:Circle,hitPoint:VectorConstant):Option[VectorConstant]= {
		val p1=line.p1-arc.centerPoint
		val p2=line.p2-arc.centerPoint
		val det=p1.x*p2.y-p2.x*p1.y
		val r=line.length
		val dis=r*r*arc.diameter *arc.diameter-det*det
		if(dis<0) {println("no intersection between "+line+" and "+arc+" dis:"+dis);None}
		else {
			val delta = line.delta
			if (dis == 0)
				Some(new VectorConstant(arc.centerPoint.x + det * delta.y / (r * r), arc.centerPoint.y - det * delta.x / (r * r), 0))
			else {
				val s1 = new VectorConstant(arc.centerPoint.x + (det * delta.y + StringUtils.mySgn(delta.y) * delta.x * Math.sqrt(dis)) / (r * r),
					arc.centerPoint.y + (-det * delta.x + Math.abs(delta.y) * Math.sqrt(dis)) / (r * r), 0)
				val s2 = new VectorConstant(arc.centerPoint.x + (det * delta.y - StringUtils.mySgn(delta.y) * delta.x * Math.sqrt(dis)) / (r * r),
					arc.centerPoint.y + (-det * delta.x - Math.abs(delta.y) * Math.sqrt(dis)) / (r * r), 0)
				Some(if ((hitPoint - s1).toDouble < (hitPoint - s2).toDouble) s1 else s2)
			}
		}
	}

	def doExtend(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean =  {	
	  println("Do Extends data:"+data.mkString(" | ")+"\nparam:"+param.mkString(" | "))
	  //println("the Type:"+theTypeID)
		if(param.size>1&&param.size<4 && param.head._2.getType==DataType.ObjectRefTyp) {
			val otherRef=param.head._2.toObjectReference
			val oInst=StorageManager.getInstanceData(otherRef)
			if(otherRef.typ==theTypeID){
			  val op1=oInst.fieldValue(3).toVector
			  val op2=oInst.fieldValue(4).toVector
			  var hitPoints=List(op1,op2) // add extend line end points to hitpointslist
			  val ody=op2.y-op1.y
			  val odx=op2.x-op1.x
			  //println("op1:"+op1+" op2:"+op2)
			  for(linst<-data;if linst.ref.typ == theTypeID) {
			    val p1=linst.fieldValue(3).toVector
			    val p2=linst.fieldValue(4).toVector
			    //println("p1:"+p1+" p2:"+p2)
			    def dx=p2.x-p1.x
			    def dy=p2.y-p1.y
			    val d=ody*dx-odx*dy			    
			    if(d!=0) {
			    	val ua=(odx*(p1.y-op1.y)-ody*(p1.x-op1.x))/d
			    	//println("d:"+d+" ua:"+ua)
			    	val sp=new VectorConstant(p1.x+ua*dx,p1.y+ua*dy,0)
			    	if(Math.abs(sp.x)>GraphElemModule.wrongExtendTreshold||
			    	   Math.abs(sp.y)>GraphElemModule.wrongExtendTreshold ) {
			    	  Log.e("wrong extension Point "+sp+" when extending "+linst+" with "+otherRef)
			    	  return false
			    	}
			    	hitPoints=sp::hitPoints
			    	if(ua<0) TransactionManager.tryWriteInstanceField(linst.ref,3,sp)			    		
			    	else if(ua>1) TransactionManager.tryWriteInstanceField(linst.ref,4,sp)			    	  
			    }     	
			  }
			  if(param(2)._2.toBoolean){ // is editable
				  val (firstPoint,lastPoint)= if(odx==0) // extend line is vertical
				    (hitPoints.minBy(_.y),hitPoints.maxBy(_.y))	// find min/max vertical hitpoints		    
				   else  (hitPoints.minBy(_.x),hitPoints.maxBy(_.x)) // find min/max horizontal hitpoints
				  if((odx==0 && op1.y<op2.y)||(odx!=0 && op1.x<op2.x)) { // p1 < p2 ?
					  if(op1!=firstPoint) TransactionManager.tryWriteInstanceField(otherRef,3,firstPoint)
					  if(op2!=lastPoint) TransactionManager.tryWriteInstanceField(otherRef,4,lastPoint)
				  } else { // p2>=p1
				    if(op1!=lastPoint) TransactionManager.tryWriteInstanceField(otherRef,3,lastPoint)
					  if(op2!=firstPoint) TransactionManager.tryWriteInstanceField(otherRef,4,firstPoint)
			    }
			  }else throw new IllegalArgumentException("nicht editierbar "+param.mkString(","))

			} else if(otherRef.typ== TypeInfos.arcElemType){
			  val circle=new Circle(oInst)
				val hitPoint=param(1)._2.toVector
				println("check arc "+circle+" hp:"+hitPoint)
				for(linst<-data;if linst.ref.typ == theTypeID;line=new Line(linst);
						sp<-intersectLineArc(line,circle,hitPoint)) {
					val scale=line.delta.getScaleTo(sp-line.p1)
					println("scale "+scale+"dif:"+(sp-line.p1)+" delta:"+line.delta)
					if(scale<0) TransactionManager.tryWriteInstanceField(linst.ref,3,sp)
					else if(scale<1) TransactionManager.tryWriteInstanceField(linst.ref,4,sp)
				}
			}
		} else throw new IllegalArgumentException("Falsche Parameter "+param.mkString(","))
		true
	}
	

	
	def cutPartAction=new ActionIterator("Teillinie löschen",Some(new DialogQuestion("Teillinie löschen",
		Seq(new AnswerDefinition("Teillinie auswählen",DataType.ObjectRefTyp,GraphElemModule.cutPointQuestion,"S"+TypeInfos.lineElemType.toString)))),doCutPart,true)
	
	def doCutPart(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {
	  cutLine(param)
	  true
	}
	
	/*def createCutLineAction=new CreateActionImpl("TeilLinie löschen",Some(new DialogQuestion("Teillinie löschen",
		Seq(new AnswerDefinition("Teillinie auswählen",DataType.ObjectRefTyp,GraphElemModule.cutPointQuestion,"S"+TypeInfos.lineElemType.toString)))),doCreateCutLine,true)
	
	def doCreateCutLine(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  //System.out.println("Do Cut Part:"+param.mkString(" | "))
	  cutLine(param)
	  true
	}*/
	
	def cutLine(param:Seq[(String,Constant)]):Unit= if(param.size==3){
	  val lineRef=param.head._2.toObjectReference
	  val p1=param(1)._2.toVector
	  val p2=param(2)._2.toVector
	  if(lineRef.typ==TypeInfos.lineElemType) {
	    val lineInst=StorageManager.getInstanceData(lineRef)
	    val op1=lineInst.fieldValue(3).toVector
	    val op2=lineInst.fieldValue(4).toVector
	    if(p1==op1 ){ 
	      if(p2==op2) TransactionManager.tryDeleteInstance(lineRef,Some(lineInst.owners.head),None) 
	      else TransactionManager.tryWriteInstanceField(lineRef,3,p2)	      
	    }
	    else if(p2==op2) TransactionManager.tryWriteInstanceField(lineRef,4,p1)
	    else {
	      TransactionManager.tryWriteInstanceField(lineRef,4,p1)
	      val newInst=TransactionManager.tryCreateInstance(TypeInfos.lineElemType,lineInst.owners,false)
	      val outInst=new InstanceData(newInst.ref,for(i<-lineInst.fieldData.indices) 
	        yield if(i==3) p2 else if (i==4) op2 else lineInst.fieldData(i) , newInst.owners)
	      TransactionManager.tryWriteInstanceData(outInst)
	    }
	  } else Log.e("Line Ref not lineType :"+lineRef)
	}
	
			        
	        
	def parallelAction=new ActionIterator("Parallele Linie",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",	
	    "ParLine")),doParallel,true)
	
	def doParallel(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {	
	  val (offset,numCopy)=if(param.head._2.getType==DataType.IntTyp)(1,param.head._2.toInt) else (0,1)
	  if(data.size==1) {
	    val oldInst=data.head
	    val p1=oldInst.fieldValue(3).toVector
	    val p2=oldInst.fieldValue(4).toVector
	    val line=new Line3D(p1,p2-p1)
	    val dist= param(0+offset)._2 match {
	      case v:VectorConstant => line.orthogonalThrough(v) // parallel through point
	      case d:DoubleConstant => line.orthogonalThrough(param(1+offset)._2.toVector).unit*d.toDouble
	    }
	    for(i<-1 to numCopy){
	      val ndist=dist*i.toDouble
		    val newInst=TransactionManager.tryCreateInstance(TypeInfos.lineElemType,oldInst.owners,false)
		    val outInst=new InstanceData(newInst.ref,for(i<-oldInst.fieldData.indices) 
		        yield if(i==3) p1+ndist else if (i==4) p2+ndist else oldInst.fieldData(i) , newInst.owners)
		      TransactionManager.tryWriteInstanceData(outInst)
	    }
	  }
	  true
	}
	
	/*def createOrthoLineAction=new CreateActionImpl("LotLinie",Some(new DialogQuestion("Lotlinie zeichnen",
		Seq(new AnswerDefinition("Lot zu Linie",DataType.ObjectRefTyp,
		    Some(new DialogQuestion("Lot durch Punkt",Seq(	new AnswerDefinition("Zielpunkt wählen",DataType.VectorTyp,None)),false)),TypeInfos.lineElemType.toString)))),doCreateOrthoLine)*/

  def createOrthoLineAction=new CreateActionImpl("Lot-Linie",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
    "OrthoLine")),doCreateOrthoLine,false)
	
	def doCreateOrthoLine(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {	  
	  if(param.size==2) {	    
	    val lineInst=StorageManager.getInstanceData(param.head._2.toObjectReference)
	    val p1=lineInst.fieldValue(3).toVector
	    val p2=lineInst.fieldValue(4).toVector
	    val hitPoint=param(1)._2.toVector
	    val startPoint=new Line3D(p1,p2-p1).orthProjection(hitPoint)
	    makeLine(parents,startPoint,hitPoint,formFields)	    
	  }
	  
	  true
	}	
	
	def createRectAction=new CreateActionImpl("Rechteck",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"Rectangle")),doCreateRect)
	
	def doCreateRect(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  val oparents=Array(new OwnerReference(0.toByte,parents.head.ref))
	  //println("create rect params: "+param.mkString("\n   "))
	  if(param.size>=2) {
	    val startPoint=param.head._2.toVector
	    param(1)._2 match {
	      case v:VectorConstant => makeRectangle(oparents,Seq(startPoint,new VectorConstant(startPoint.x,v.y,0),v,new VectorConstant(v.x,startPoint.y,0)),formFields)
	      case DoubleConstant(b) =>
					val h= param(2)._2 match {
            case DoubleConstant(he)=>he
            case v2:VectorConstant=> v2.y-startPoint.y
          }
					makeRectangle(oparents,Seq(startPoint,new VectorConstant(startPoint.x+b,startPoint.y,0),
              new VectorConstant(startPoint.x+b,startPoint.y+h,0),new VectorConstant(startPoint.x,startPoint.y+h,0)),formFields)
				case StringConstant("über Achse") =>
					val endPoint=param(2)._2.toVector
					val deltaV= param(3)._2 match {
            case defPoint:VectorConstant => defPoint-new Line3D(startPoint,endPoint-startPoint).orthProjection(defPoint)
            case DoubleConstant(width)=> (endPoint-startPoint).unit.transposeXY * width
          }
					makeRectangle(oparents,Seq(startPoint+deltaV,endPoint+deltaV,endPoint-deltaV,startPoint-deltaV),formFields)
				case StringConstant("über Randkante")=>
					val endPoint=param(2)._2.toVector
					val deltaV= param(3)._2 match {
            case defPoint:VectorConstant => defPoint-new Line3D(startPoint,endPoint-startPoint).orthProjection(defPoint)
            case DoubleConstant(width)=> (endPoint-startPoint).unit.transposeXY * width
          }
					makeRectangle(oparents,Seq(startPoint,endPoint,endPoint+deltaV,startPoint+deltaV),formFields)
				case StringConstant(a)=> throw new IllegalArgumentException("wrong Option:'"+a+"'")
	    }
	    true
	  }
	  else false
	}
	
	private def makeRectangle(parentRefs:Array[OwnerReference],points:Seq[VectorConstant],formFields:Seq[(Int,Constant)])= {
	  makeLine(parentRefs,points.head,points(1),formFields)
	  makeLine(parentRefs,points(1),points(2),formFields)
	  makeLine(parentRefs,points(2),points(3),formFields)
	  makeLine(parentRefs,points(3),points.head,formFields)
	}
	
	def createParPolyAction=new CreateActionImpl("Paralleler Polygonzug",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"ParPoly")),doCreateParPoly)
	
	 
    
	
	def doCreateParPoly(u:AbstractUserSocket, parents:Seq[InstanceData], param:Seq[(String,Constant)], newTyp:Int, formFields:Seq[(Int,Constant)]):Boolean= {
	  println("ParPoly "+param.mkString(" |"))
	  val oparents=Array(new OwnerReference(0.toByte,parents.head.ref))
	  val firstPointIndex=param.indexWhere(_._2.getType==DataType.VectorTyp)
	  val distanceValues=param.take(firstPointIndex).map(_._2.toDouble)
	  var dres=0d
	  val distances= for(d<-distanceValues) yield {
	    dres+=d
	    dres
	  }
	  def getNormVectors(p1:VectorConstant,p2:VectorConstant)= {
      val norm=(p2-p1).unit.transposeXY
      distances map (norm* _)
    }
	  
	  val points=param.drop(firstPointIndex).map(_._2 .toVector)
	  val startNV=getNormVectors(points.head,points(1))
	  val endNV=getNormVectors(points(points.size-2),points.last)
	  val (firstPoints,lastPoints)=  if(points.head==points.last&& points.size>3) {
	    val result= for(i<-distances.indices;nv1=startNV(i);nv2=endNV(i)) 
	      yield {
	      val vvl=points(points.size-2)
	      new Line3D(points.head+nv1,points(1)-points.head).intersectionWith(new Line3D(vvl+nv2,points.last-vvl))
	    	} 
	    (result,result)    
	  } else 
	    (startNV map(_ + points.head),endNV map(_ + points.last))	  
	  
	  val nextPoints=for(i <- 1 until (points.size - 1)) yield{
	    val lp=points(i-1)
	    val tp=points(i)
	    val np=points(i+1)
	    val normVectors1=getNormVectors(lp,tp)
	    val normVectors2=getNormVectors(tp,np)
	    for(i<-distances.indices;nv1=normVectors1(i);nv2=normVectors2(i)) 
	      yield new Line3D(lp+nv1,tp-lp).intersectionWith(new Line3D(tp+nv2,np-tp))	    
	  }
	  val allPoints=firstPoints+:nextPoints:+lastPoints
	    
	  for(Seq(a,b)<-allPoints.sliding(2,1);i<-distances.indices) {
	    makeLine(oparents,a(i),b(i),formFields)
	  } 	  
	  true
	}
	
	def cutElemsAction=new ActionIterator("Elemente schneiden",None,doCutElems,false)
	
	def doCutElems(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {	
	  //println("Do Cut Part:"+data.mkString(" | ")+"\nparam:"+param.mkString(" | "))
	  for(lineData<-data) {	  
	  	val p1=lineData.fieldValue(3).toVector
	  	val p2=lineData.fieldValue(4).toVector
	  	val edge=new Edge(p1,p2)
	  	val layerRef=lineData.owners.head	  
	  	ActionList.getInstanceProperties(layerRef.ownerRef) match {
	  		case Some(prop)=>  for(elRef<-prop.propertyFields(layerRef.ownerField).propertyList;if TypeInfos.moduleMap.contains(elRef.typ))
	  			TypeInfos.moduleMap(elRef.typ).cutElementByLine(ActionList.getInstanceData(elRef),edge)
	  		case _=>
	  	}
	  }
	  true
	}
	
	override def cutElementByLine(elData:InstanceData,cutLine:Edge)= {	  
	  val p1=elData.fieldValue(3).toVector
	  val p2=elData.fieldValue(4).toVector
	  cutLine.getCutIntersectionWith(p1,p2) match {
	    case Seq((pos,cutPoint)) if pos > 0 && pos < 1 & cutPoint != p1 && cutPoint != p2 =>
				TransactionManager.tryWriteInstanceField(elData.ref,4,cutPoint)
				val newRef=TransactionManager.tryCreateInstance(theTypeID, elData.owners,false)
				TransactionManager.tryWriteInstanceField(newRef.ref,3,cutPoint)
				TransactionManager.tryWriteInstanceField(newRef.ref,4,p2)
				for(i<- 0 until 3)if(elData.fieldData(i)!=EMPTY_EX) {
          //println("write format field:"+elData.ref+"->"+newRef.ref+" data:"+elData.fieldData(i))
          TransactionManager.tryWriteInstanceField(newRef.ref,i.toByte,elData.fieldData(i))
        }
			case _ =>
	  }
	}
	
}

class EllipseModule extends ActionModule with GraphActionModule {  
  override val createActions=List(createEllipseCenterAction)  
	val actions=  Seq.empty
  
  def moveElement(elem:InstanceData,delta:VectorConstant):Unit= {
    TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toVector+delta)
  }
	def copyElement(elem:InstanceData,delta:VectorConstant):InstanceData = {
	  elem.setField(3,elem.fieldValue(3).toVector+delta)
	}
	override def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  import runtime.function.GraphElemModule._
	  val oldCenter=elem.fieldValue(3).toVector
	  val newCenter=mirror(oldCenter)
	  val r1=elem.fieldValue(4).toDouble	  
	  val oldMainAngle=elem.fieldValue(6).toDouble
	  val sa=elem.fieldValue(7).toDouble
	  val ea=elem.fieldValue(8).toDouble
	  val newMainAnglePoint=mirror(pointFromAngle(oldCenter,oldMainAngle,r1))
	  val newMainAngle=getAngle(newMainAnglePoint,newCenter)
	  val newSAPoint=mirror(pointFromAngle(oldCenter,oldMainAngle+sa,r1))
	  val newSA=getAngle(newSAPoint,newCenter)-newMainAngle
	  val newEAPoint=mirror(pointFromAngle(oldCenter,oldMainAngle+ea,r1))
	  val newEA=getAngle(newEAPoint,newCenter)-newMainAngle
	  elem.setField(3,newCenter).setField(6,new DoubleConstant(newMainAngle)).setField(7,new DoubleConstant(newEA)).
	     setField(8,new DoubleConstant(if(newSA<newEA)newSA+360 else newSA))
	}
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit = {
	  TransactionManager.tryWriteInstanceField(elem.ref,3,rotator(elem.fieldValue(3).toVector))	  
	  GraphElemModule.rotateAngleField(elem,6,angle)
	 // GraphElemModule.rotateAngleField(elem,7,angle)
	  //GraphElemModule.rotateAngleField(elem,8,angle)
	}
  
  override def scale(elem:InstanceData,refPoint:VectorConstant,sx:Double,sy:Double):Unit= {
    val p1=elem.fieldValue(3).toVector 
    val axis1=elem.fieldValue(4).toDouble
    val axis2=elem.fieldValue(5).toDouble
    TransactionManager.tryWriteInstanceField(elem.ref,3,scalePoint(p1,refPoint,sx,sy))
    TransactionManager.tryWriteInstanceField(elem.ref,4,new DoubleConstant(axis1*sx))
    TransactionManager.tryWriteInstanceField(elem.ref,5,new DoubleConstant(axis2*sx))
  }
	
	override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = pointModField(3,elem,delta,chPoints)
	
	def createEllipseCenterAction=new CreateActionImpl("Ellipse",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"EllipseCenter")),doCreateEllipseCenter)
	
	def doCreateEllipseCenter(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  //println("Create Ellipse param:" + param.mkString("  | ")+"\n Formfields:"+formFields.mkString(" | "))
	  var currParam:Int= -1
	  def nextParam()= {
	    currParam+=1
	    param(currParam)._2
	  }
	  
	  val center=nextParam().toVector	
	  
	  def getAngle(p:VectorConstant):Double= {
	    val v=math.atan2(p.y-center.y, p.x-center.x)*180d/math.Pi
	    if(v< -180d) v+360d else v
	  }
	  
	  val (axis1Len:Double,mainAngle:Double)= nextParam() match {
	    case v:VectorConstant => ((v-center).toDouble,math.atan2(v.y-center.y,v.x-center.x)*180d/math.Pi)
	    case d:DoubleConstant =>
				nextParam() match {
          case v:VectorConstant => (d,getAngle(v))
          case d2:DoubleConstant =>(d,d2)
        }
		}
	  if(axis1Len==0 ) return false
	  val axis2Len= nextParam() match {
	    case v:VectorConstant =>
				val mAngle=mainAngle*math.Pi/180d
				val axis1=new VectorConstant(math.cos(mAngle),math.sin(mAngle),0)
				val axis2=new VectorConstant(-math.sin(mAngle),math.cos(mAngle),0)
				val deltaP=v-center
				val rotx=deltaP.orthogonalThrough(axis2).toDouble
				val roty=deltaP.orthogonalThrough(axis1).toDouble
				val div=1d-(rotx*rotx)/(axis1Len*axis1Len)
				if(div>0) math.sqrt((roty*roty)/div)
        else return false
			case d:DoubleConstant=> d.toDouble
	  }
	  if(axis2Len==0) return false
	  
	  def createEllipse(sa:Double,ea:Double) = {
	    val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
	    val inst=TransactionManager.tryCreateInstance(newTyp,parentRef,false)  
	    TransactionManager.tryWriteInstanceField(inst.ref,3,center)
	    TransactionManager.tryWriteInstanceField(inst.ref,4,new DoubleConstant(axis1Len))
	    TransactionManager.tryWriteInstanceField(inst.ref,5,new DoubleConstant(axis2Len))
	    TransactionManager.tryWriteInstanceField(inst.ref,6,new DoubleConstant(mainAngle))
	    TransactionManager.tryWriteInstanceField(inst.ref,7,new DoubleConstant(sa ))
	    TransactionManager.tryWriteInstanceField(inst.ref,8,new DoubleConstant((if(ea<sa)360 else 0 )+ea))
	    writeFormatParams(inst.ref,formFields)	  
	  }
	  
	  val startAngle=nextParam() match {
	    case s:StringConstant =>
				createEllipse(0,360)
				return true
			case v:VectorConstant => getAngle(v)-mainAngle
	    case d:DoubleConstant => d.toDouble
	  } 
	  
	  val endAngle=nextParam() match {
	    case v:VectorConstant => getAngle(v)-mainAngle
	    case d:DoubleConstant => d.toDouble
	  }
	  
	  createEllipse((if (startAngle<0) startAngle+360 else startAngle)% 360,if(endAngle==360) 360 else (if(endAngle<0) endAngle+360 else endAngle) % 360)  
	  true
	}	
}


class ArcModule extends ActionModule with GraphActionModule {
  import runtime.function.GraphElemModule._
  lazy val parDistQuestion= Some(new DialogQuestion("Abstand",
		Seq(new AnswerDefinition("Abstand eingeben:",DataType.DoubleTyp,None))))
  
	override val createActions=List(createArcCenterAction,createArcGeneralAction)	
	val actions =  Seq(makeParallelArc,cutPartAction)
	
	def createArcCenterAction=new CreateActionImpl("Mittelpunktkreis",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"ArcCenter")),doCreateArcCenter)
	
	
	
	def doCreateArcCenter(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  println("Create Arc param:" + param.mkString("  | ")+"\n Formfields:"+formFields.mkString(" | "))
	  val center=param.head._2.toVector
	  val radius= param(1)._1 match {
				case "Randpunkt des Kreises"=> (param(1)._2.toVector - center).toDouble
				case "Umfang eingeben"=> param(1)._2.toDouble/(2*math.Pi)				
				case "Radius eingeben" => param(1)._2.toDouble
			}
	  
	  
	  
	  val (startAngle,endAngle)= param(2)._2 match {
	    case v:VectorConstant => val sa=getAngle(v,center)
				(sa,param(3)._2 match {
          case v2:VectorConstant=> getAngle(v2,center)
          case d:DoubleConstant=> sa+d.toDouble
        })
			case d1:DoubleConstant => (d1.toDouble, param(3)._2 match {
	      case v2:VectorConstant=> getAngle(v2,center)
	      case d2:DoubleConstant=> d1.toDouble+d2.toDouble
	    })
			case s:StringConstant => (0d,360d) // vollkreis
	  }
	  println("sa:"+startAngle+" ea:"+endAngle)
	  val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
	  val inst=TransactionManager.tryCreateInstance(newTyp,parentRef,false)  
	  TransactionManager.tryWriteInstanceField(inst.ref,3,center)
	  TransactionManager.tryWriteInstanceField(inst.ref,4,new DoubleConstant(radius))
	  TransactionManager.tryWriteInstanceField(inst.ref,5,new DoubleConstant(startAngle))
	  TransactionManager.tryWriteInstanceField(inst.ref,6,new DoubleConstant(endAngle))
	  writeFormatParams(inst.ref,formFields)	  
	  true
	}
	
	def createArcGeneralAction=new CreateActionImpl("Allgemeiner Kreis",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"ArcGeneral")),doCreateArcGeneral)
	
	def doCreateArcGeneral(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  false
	}
	
	def moveElement(elem:InstanceData,delta:VectorConstant) = {
		TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toVector+delta)
	}
	
	def copyElement(elem:InstanceData,delta:VectorConstant) = {
		elem.setField(3,elem.fieldValue(3).toVector+delta)
	}	
	
	
	
	override def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  val radius=elem.fieldValue(4).toDouble
	  val centerPoint=elem.fieldValue(3).toVector
	  val newCenter=mirror(centerPoint)
	  val result =elem.setField(3,newCenter)
	  val p1=mirror(GraphElemModule.pointFromAngle(centerPoint,elem.fieldValue(5).toDouble,radius))
	  val p2=mirror(GraphElemModule.pointFromAngle(centerPoint,elem.fieldValue(6).toDouble,radius))	
	  val (newA,newB)=fixAngles(getAngle(p2,newCenter),getAngle(p1,newCenter))
	  result.setField(5,new DoubleConstant(newA)).setField(6,new DoubleConstant(newB))    
	}
	
	def fixAngles(sa:Double,ea:Double)= {
	  val newA=if(sa<360d) sa+360d else if(sa>360d) sa-360d else sa
	  val newB=if(ea<360d) ea+360d else if(ea>360d) ea-360d else ea
	  (newA,if(newB<newA)newB+360d else newB)
	}
	
	
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= {
	  TransactionManager.tryWriteInstanceField(elem.ref,3,rotator(elem.fieldValue(3).toVector))	  
	  val startAngle=GraphElemModule.rotateAngleField(elem,5,angle)
    if((elem.fieldValue(6).toDouble-elem.fieldValue(5).toDouble)%360d ==0) 
      TransactionManager.tryWriteInstanceField(elem.ref,6,new DoubleConstant(startAngle+360d))
    else GraphElemModule.rotateAngleField(elem,6,angle)	  
	}
  
  override def scale(elem:InstanceData,refPoint:VectorConstant,sx:Double,sy:Double):Unit= {
    val p1=elem.fieldValue(3).toVector 
    val radius=elem.fieldValue(4).toDouble
    TransactionManager.tryWriteInstanceField(elem.ref,3,scalePoint(p1,refPoint,sx,sy))
    TransactionManager.tryWriteInstanceField(elem.ref,4,new DoubleConstant(radius*sx))
  }
	
	override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = pointModField(3,elem,delta,chPoints)
	
	def makeParallelArc=new ActionIterator("Paralleler Kreis",parDistQuestion,doMakeParallelArc) 
	
	def doMakeParallelArc(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean= {
	  for (elem<-data;if elem.ref.typ == theTypeID){
	    val oldRadius=elem.fieldValue(4).toDouble  
	    val dist=param.head._2.toDouble
	    val inst=TransactionManager.tryCreateInstance(theTypeID,elem.owners,false)
	    for(i<-0 to 3) TransactionManager.tryWriteInstanceField(inst.ref, i.toByte, elem.fieldValue(i))
	    for(i<-5 to 6) TransactionManager.tryWriteInstanceField(inst.ref, i.toByte, elem.fieldValue(i))
	     TransactionManager.tryWriteInstanceField(inst.ref, 4.toByte, new DoubleConstant(oldRadius+dist))
	  	
	  }
	  true
	}

	def cutPartAction=new ActionIterator("Teilkreis löschen",Some(new DialogQuestion("Teilkreis löschen",
		Seq(new AnswerDefinition("Teilkreis auswählen",DataType.ObjectRefTyp,GraphElemModule.cutPointQuestion,"S"+TypeInfos.arcElemType.toString)))),doCutPart,true)

	def doCutPart(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {
		val arcRef=param.head._2.toObjectReference
		val a1=param(1)._2.toVector.x
		val a2=param(2)._2.toVector.x
		val arcInst=new Circle(StorageManager.getInstanceData(arcRef))
		println("Cut Arc a1:"+a1+" a2:"+a2)
		val ownEndAngle=if(arcInst.endAngle<arcInst.startAngle) arcInst.endAngle+360d else arcInst.endAngle
		if(a1==arcInst.startAngle) {
			if(a2==ownEndAngle) TransactionManager.tryDeleteInstance(arcRef,None,None)
			else 	TransactionManager.tryWriteInstanceField(arcRef,5,new DoubleConstant(if(a2 > 360d)a2 % 360d else a2))
		} else {
			if(a2==ownEndAngle) TransactionManager.tryWriteInstanceField(arcRef,6,new DoubleConstant(if(a1>360d) a1 % 360d else a1))
			else { // in the middle
				TransactionManager.tryWriteInstanceField(arcRef,6,new DoubleConstant(if(a1 > 360d) a1 % 360d else a1))
				val newArc=TransactionManager.tryCreateInstance(TypeInfos.arcElemType,arcInst.data.owners,false)
				val newInst=arcInst.data.clone(newArc.ref,newArc.owners,Seq.empty).
					setField(5,DoubleConstant(if(a2>360d) a2 % 360d else a2)).
				  setField(6,arcInst.data.fieldValue(6))
				TransactionManager.tryWriteInstanceData(newInst)
			}
		}
		true
	}
}





class PolygonModule extends ActionModule with GraphActionModule {

  override val createActions=List(createPolyAction)
  val actions=List(intersectAction,cutAction,addAction,setStartPointAction)
  
	def moveElement(elem:InstanceData,delta:VectorConstant) = 
  	TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toPolygon.translate(delta))
  
  def copyElement(elem:InstanceData,delta:VectorConstant) = 
    elem.setField(3,elem.fieldValue(3).toPolygon.translate(delta)) 
    
  def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  elem.setField(3,elem.fieldValue(3).toPolygon.transform(mirror))
	}  
  
  def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= 
    TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toPolygon.transform(rotator))  
  
  override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) = 
	  TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toPolygon.translatePoints(chPoints,delta))  
	
	def createPolyAction=new CreateActionImpl("Füllfläche",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"PolyTo")),doCreatePoly)
	
  
  protected def polygonFromParams(inst:Reference,params:Seq[(String,Constant)])= {
    var lastPoint=params.head._2.toVector
    val plist=params.map{case(name,const)=> const match {
		    case v:VectorConstant=>lastPoint=v;v
		    case d:DoubleConstant=>lastPoint=if(name=="dx") lastPoint+new VectorConstant(d.toDouble,0d,0d)
		    	else lastPoint + new VectorConstant(0d,d.toDouble,0d);lastPoint
		    case o => throw new IllegalArgumentException("Wrong answer:"+o)
		  }
    }
    val pointList=if(plist.size==2){
      val p1=plist.head
      val p2=plist(1)
      List(p1,new VectorConstant(p1.x,p2.y,p1.z),p2,new VectorConstant(p2.x,p1.y,p2.z))
    } 
     else plist
   new Polygon(Seq(inst),Seq(  new PointList(pointList).clockWise))
  }
  
  
	def doCreatePoly(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
  	val inst=TransactionManager.tryCreateInstance(theTypeID,parentRef,true)  	  	
		TransactionManager.tryWriteInstanceField(inst.ref,3,polygonFromParams(inst.ref,param))  
		writeFormatParams(inst.ref,formFields)
		true
	}
  
  def intersectQuestion=new DialogQuestion("Schnittfläche mit Polygon",Seq(new AnswerDefinition("anderes Polygon wählen",DataType.ObjectRefTyp,
      None,"F"+TypeInfos.polyElemType.toString)) )
  
  def intersectAction=new ActionIterator("Schnittfläche",Some(intersectQuestion),doTrans(_.intersect(_)))
  
  def cutQuestion=new DialogQuestion("Polygonfläche abschneiden",Seq(new AnswerDefinition("anderes Polygon wählen",DataType.ObjectRefTyp,
      None,"F"+TypeInfos.polyElemType.toString)) )
  
  def cutAction=new ActionIterator("Abschneiden",Some(cutQuestion),doTrans(_.subtract(_)))
  
  def addQuestion=new DialogQuestion("Polygonfläche hinzufügen",Seq(new AnswerDefinition("anderes Polygon wählen",DataType.ObjectRefTyp,
      None,"F"+TypeInfos.polyElemType.toString)) )
  
  def addAction=new ActionIterator("Hinzufügen",Some(addQuestion),doTrans(_.add(_)))
  
  def doTrans(func:(Polygon,Polygon)=>Polygon)(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean= {   
    val otherPoly=StorageManager.getInstanceData(param.head._2.toObjectReference).fieldValue(3).toPolygon.createCopy().asInstanceOf[Polygon]
    for(inst <-data) {
      val oldPoly=inst.fieldValue(3).toPolygon.createCopy().asInstanceOf[Polygon]
      val newPoly=func(oldPoly,otherPoly)            
      TransactionManager.tryWriteInstanceField(inst.ref,3,newPoly)     
    }
    true
  }
  
  def setStartPointQuestion()=new DialogQuestion("Ausgangspunkt Schraffur",
		Seq(new AnswerDefinition("Punkt wählen",DataType.VectorTyp,None)))
	
  def setStartPointAction=new ActionIterator("Ausgangspunkt",Some(setStartPointQuestion),doSetStartPoint)
  
  def doSetStartPoint(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean= {
  	if(param.size==1 && param.head._2.getType==DataType.VectorTyp ) {
  		val startPoint=param.head._2.toVector
  		for(d <-data) 
  			TransactionManager.tryWriteInstanceField(d.ref,6,startPoint)						  
  			true	
  	}
  	else false
  }
}

/** Measure of Area
 * 
 */
class AreaPolygonModule extends PolygonModule {  
  override def moveElement(elem:InstanceData,delta:VectorConstant) = 
  	TransactionManager.tryWriteInstanceField(elem.ref,4,elem.fieldValue(4).toPolygon.translate(delta))
  
  override def copyElement(elem:InstanceData,delta:VectorConstant) = 
    elem.setField(4,elem.fieldValue(4).toPolygon.translate(delta))  
  
  override def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= 
    TransactionManager.tryWriteInstanceField(elem.ref,4,elem.fieldValue(4).toPolygon.transform(rotator)) 
    
	def updateAreaValue(instRef:Reference,p:Polygon)= {
    val poly=new Polygon(Seq(instRef),p.pathList.map(_.removeDoublePoints().removeStraightEdges()))
	  //TransactionManager.tryWriteInstanceField(instRef,0,new DoubleConstant(p.getAreaValue))	  
	  TransactionManager.tryWriteInstanceField(instRef,4,p)
	  val areaList=p.pathList.flatMap(path=>PolygonDivider.divideArea(path.removeDoublePoints().removeStraightEdges()))
	  if(areaList.size>0) {
	    val expression=PolygonDivider.combineExpression(areaList)	    
	    TransactionManager.tryWriteInstanceField(instRef,10,if(expression.isNullConstant)DoubleConstant(p.getAreaValue) else expression)
	  } else
	    TransactionManager.tryWriteInstanceField(instRef,10,DoubleConstant(p.getAreaValue))
  }
  
  override def pointMod(elem:InstanceData,delta:VectorConstant,chPoints:Set[VectorConstant]) =      
	  updateAreaValue(elem.ref,elem.fieldValue(4).toPolygon.translatePoints(chPoints,delta)	)
  
	
	override def createPolyAction=new CreateActionImpl("FlächenPolygon",Some(new CommandQuestion("client.graphicsView.GraphCustomQuestionHandler",
	"PolyTo")),doCreatePoly)	
	
	
	override def doCreatePoly(u:AbstractUserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
  	val inst=TransactionManager.tryCreateInstance(theTypeID,parentRef,true)  		
		updateAreaValue(inst.ref,polygonFromParams(inst.ref, param))
		writeFormatParams(inst.ref,formFields)
		true
	}  
	
	override def intersectQuestion=new DialogQuestion("Schnittfläche mit Polygon",Seq(new AnswerDefinition("anderes Polygon wählen",DataType.ObjectRefTyp,
      None,"F"+TypeInfos.areaPolyElemType.toString)) ) 
  
  override def cutQuestion=new DialogQuestion("Polygonfläche abschneiden",Seq(new AnswerDefinition("anderes Polygon wählen",DataType.ObjectRefTyp,
      None,"F"+TypeInfos.areaPolyElemType.toString)) )  
  
  override def addQuestion=new DialogQuestion("Polygonfläche hinzufügen",Seq(new AnswerDefinition("anderes Polygon wählen",DataType.ObjectRefTyp,
      None,"F"+TypeInfos.areaPolyElemType.toString)) )
  
  override def doTrans(func:(Polygon,Polygon)=>Polygon)(u:AbstractUserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean= {   
    val otherPoly=StorageManager.getInstanceData(param.head._2.toObjectReference).fieldValue(4).toPolygon.createCopy().asInstanceOf[Polygon]
    for(inst <-data) {
      val oldPoly=inst.fieldValue(4).toPolygon.createCopy().asInstanceOf[Polygon]
      updateAreaValue(inst.ref,func(oldPoly,otherPoly))
    }
    true
  }  
}


object TypeInfos {	
	val moduleMap= collection.mutable.HashMap[Int,GraphActionModule]()	
	val polyElemType=42 // WARNING ElementType is fixed because it can not be found at runtime
	val lineElemType=40
	val arcElemType=41
	val textElemType=44
	val areaPolyElemType=341
  val symbolElemType=45
  val symbolFillType=47
  val symbolDefType=411
}