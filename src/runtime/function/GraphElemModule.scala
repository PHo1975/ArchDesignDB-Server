/**
 * Author: Peter Started:04.10.2010
 */
package runtime.function

import java.io.{ByteArrayInputStream, DataInputStream}

import client.dialog.AnswerPanelsData
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression._
import definition.typ._
import server.comm.AbstractUserSocket
import server.storage.{ActionIterator, ActionModule, CreateActionImpl, StorageManager}
import transaction.handling.{ActionList, TransactionManager}
import util.{GraphUtils, Log, StringUtils}

import scala.collection.mutable

/**
 * 
 */

class Circle (val data:InstanceData) extends AnyVal {
  def centerPoint: VectorConstant = data.fieldValue(3).toVector

  def diameter: Double = data.fieldValue(4).toDouble

  def startAngle: Double = data.fieldValue(5).toDouble

  def endAngle: Double = data.fieldValue(6).toDouble

  override def toString: String = "Circle center" + centerPoint + " diameter:" + diameter + " startAngle:" + startAngle + " endAngle:" + endAngle
}


class Line(val data:InstanceData) extends AnyVal {
  def p1: VectorConstant = data.fieldValue(3).toVector

  def p2: VectorConstant = data.fieldValue(4).toVector

  def delta: VectorConstant = p2 - p1

  def length: Double = {
		val d=delta
		math.sqrt(d.x*d.x+d.y*d.y)
	}

  override def toString: String = "Line p1:" + p1 + " p2:" + p2
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

  def setObjectType(typeID: Int): Unit = {
		//System.out.println("set object type ":+typeID+" "+this)
		theTypeID=typeID
		TypeInfos.moduleMap(theTypeID)=this
	}
	
	def writeFormatParams(ref:Reference,params:Seq[(Int,Constant)]):Unit = {
	  for(p <-params)
	  TransactionManager.tryWriteInstanceField(ref,p._1.toByte,p._2)
	}

  def scalePoint(oPoint: VectorConstant, refPoint: VectorConstant, sx: Double, sy: Double): VectorConstant = {
	  //println("scalePoint "+oPoint+" sx:"+sx+" sy:"+sy)
	  new VectorConstant(refPoint.x+(oPoint.x-refPoint.x)*sx,refPoint.y+(oPoint.y-refPoint.y)*sy,0)
	}

  protected def pointModField(fieldNr: Int, elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): AnyVal = {
    val p1=elem.fieldValue(fieldNr).toVector    
    if (chPoints.contains(p1)) TransactionManager.tryWriteInstanceField(elem.ref,fieldNr.toByte,p1+delta)    
  }
}



object GraphElemModule{  
  val wrongExtendTreshold=10000d
  val nearNullTreshold=0.00001d

  def singlePointQuestion(qname: String, pname: String) = Some(DialogQuestion(qname, Seq(new AnswerDefinition(pname, DataType.VectorTyp, None))))

  def mvQuestion(aName: String,strict:Boolean=true) = Some(DialogQuestion(aName + "<br>Distanz angeben",
    Seq(new AnswerDefinition("'von Punkt' angeben", DataType.VectorTyp,
      Some(DialogQuestion(aName + "<br>Distanz",
        Seq(new AnswerDefinition("'nach Punkt' angeben", DataType.VectorTyp, None,if (strict) "" else AnswerPanelsData.NOSTRICT_HIT)))),
      if (strict) "" else AnswerPanelsData.NOSTRICT_HIT),
      new AnswerDefinition("Delta X eingeben:", DataType.DoubleTyp, dyQuestion)
    )))


  lazy val dyQuestion = Some(DialogQuestion("Eingabe Distanzwert",
    Seq(new AnswerDefinition("delta Y eingeben:", DataType.DoubleTyp, None))))

  def checkAngle(angle: Double): Double = if (angle == 360) 360d else angle % 360d
	
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

  def cutPointQuestion = Some(DialogQuestion("Point1", Seq(new AnswerDefinition("hitpoint1", DataType.VectorTyp,
    Some(DialogQuestion("Point2", Seq(new AnswerDefinition("hitpoint2", DataType.VectorTyp, None)), repeat = false)))), repeat = false))

  private def createPointList(params: Seq[(String, Constant)]): Seq[VectorConstant] = {
    if (params.lengthCompare(2) < 0) throw new IllegalArgumentException("Create Polygon wrong number of params:" + params.size + " " + params.mkString(","))
    val paramIterator = params.iterator
    val startPoint: VectorConstant = paramIterator.next()._2.toVector

    paramIterator.scanLeft(startPoint) { case (lastPoint, (name, const)) => const match {
      case v: VectorConstant => v
      case d: DoubleConstant => if (name == "dx") lastPoint + new VectorConstant(d.toDouble, 0d, 0d)
      else lastPoint + new VectorConstant(0d, d.toDouble, 0d)
      case o => throw new IllegalArgumentException("Wrong answer:" + o)
    }
    }.toSeq
  }

  def polygonFromParams(inst: Reference, params: Seq[(String, Constant)]): Polygon = {
    val plist=createPointList(params)
    val pointList = if (plist.lengthCompare(2) == 0) {
      val p1 = plist.head
      val p2 = plist(1)
      List(p1, new VectorConstant(p1.x, p2.y, p1.z), p2, new VectorConstant(p2.x, p1.y, p2.z))
    } else plist
    new Polygon(Seq(inst), Seq(PointList(pointList).clockWise))
  }

  def polygonFromParamsToLine(inst: Reference, params: Seq[(String, Constant)]): Polygon = {
    val plist=createPointList(params)
    Log.w("mline "+params.mkString+" "+plist.mkString)
    new Polygon(Seq(inst), Seq(PointList(plist).clockWise))
  }

  def groupByOrdered[A, K](t: Iterable[A], f: A => K): mutable.Map[K, List[A]] = {
    val map = mutable.LinkedHashMap[K, List[A]]().withDefault(_ => List[A]())
    for (i <- t) {
      val key = f(i)
      map(key) = i :: map(key)
    }
    map
  }

  def getUmfang(poly: Polygon, addStartPoint: Boolean): Expression = {
    val lengths: Seq[Double] = (poly.pathList.headOption match {
      case Some(pl) if pl.points.lengthCompare(1) > 0 =>
        val lastPointIterator: Iterator[Double] = new Iterator[Double] {
          var open = true

          def hasNext: Boolean = addStartPoint && open

          def next: Double = {open = false; PolygonDivider.roundValue((pl.points.head - pl.points.last).toDouble)}
        }
        pl.points.indices.iterator.drop(1).map(i =>
          PolygonDivider.roundValue((pl.points(i) - pl.points(i - 1)).toDouble)) ++ lastPointIterator
      case _ => Iterator.empty
    }).toSeq

    val r: Iterable[Expression] = if (lengths.size == 1) Seq(UnitNumber(lengths.head, PolygonDivider.meterFraction)) else
      groupByOrdered[Double, Double](lengths, identity).map((k) => {
        //println("f len key:"+k._1+" list:"+k._2.mkString)
        val num = k._2.size
        val theLength = k._1
        if (num == 1) new UnitNumber(theLength, PolygonDivider.meterFraction)
        else BinaryOperation(UnitNumber(theLength, PolygonDivider.meterFraction), BinOperator.getOp('*'), DoubleConstant(num))
      })
    //println("r:"+r.mkString(" | "))
    if (r.nonEmpty) r.reduceLeft((a, b) => {
      def combine = BinaryOperation(a, BinOperator.getOp('+'), b)

      a match {
        case (a1@BinaryOperation(_, _, DoubleConstant(num1))) => b match {
          case (b1@BinaryOperation(_, _, DoubleConstant(num2))) if num1 == num2 =>
            BinaryOperation(BinaryOperation(a1.left, BinOperator.getOp('+'), b1.left), BinOperator.getOp('*'), a1.right)
          case _ => combine
        }
        case _ => combine
      }
    }) else EMPTY_EX
  }

  def generateWohnExpression(inst: InstanceData, poly: Polygon): Expression = {
    val areaList: Seq[PartArea] = poly.pathList.flatMap(path => PolygonDivider.divideArea(path.removeDoublePoints().removeStraightEdges()))
    val putz = UnitNumber(inst.fieldValue(13).toDouble, PolygonDivider.meterFraction)
    val umfang = GraphElemModule.getUmfang(poly, addStartPoint = true)
    if (areaList.isEmpty) EMPTY_EX else if (putz.value == 0d || umfang.isNullConstant) PolygonDivider.combineExpression(areaList)
    else BinaryOperation(PolygonDivider.combineExpression(areaList), BinOperator.minusOp,
      BinaryOperation(putz, BinOperator.getOp('*'), umfang))
  }

  def generateAreaExpression(inst: InstanceData, poly: Polygon): Expression = {
    val areaList: Seq[PartArea] = poly.pathList.flatMap(path => PolygonDivider.divideArea(path.removeDoublePoints().removeStraightEdges()))
    if (areaList.isEmpty) EMPTY_EX else PolygonDivider.combineExpression(areaList)
  }
}




class LinearElemModule extends ActionModule {
  var graphTypeID:Int= -1

  lazy val allowedClasses: List[Int] =List(TypeInfos.lineElemType,TypeInfos.arcElemType,TypeInfos.ellipseElemType,TypeInfos.polyLineElemType,TypeInfos.measureLineElemType)

  lazy val actions=List(takeOverAction)

  def setObjectType(typeID: Int): Unit = graphTypeID = typeID


  val takeOverAction=new ActionIterator("Format Übern",Some(DialogQuestion("Format übernehmen von welchem Objekt",
    Seq(new AnswerDefinition("Objekt wählen",DataType.ObjectRefTyp,None,allowedClasses.map(_.toString).mkString(",") )))),doTakeOver,false,-1)

  def doTakeOver(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    if(param.size==1) {
      val otherRef=param.head._2.toObjectReference
      val oInst=StorageManager.getInstanceData(otherRef)
      for(elem<-data;if allowedClasses.contains(elem.ref.typ)){
        if(elem.ref.typ!=TypeInfos.measureLineElemType) TransactionManager.tryWriteInstanceField(elem.ref,0.toByte,oInst.fieldValue(0))
        TransactionManager.tryWriteInstanceField(elem.ref,1.toByte,oInst.fieldValue(1))
        TransactionManager.tryWriteInstanceField(elem.ref,2.toByte,oInst.fieldValue(2))
        if(elem.ref.typ==TypeInfos.polyLineElemType&& oInst.ref.typ==TypeInfos.polyLineElemType)
          for(i<-4 to 8)
            TransactionManager.tryWriteInstanceField(elem.ref,i.toByte,oInst.fieldValue(i))
        else if(elem.ref.typ==TypeInfos.measureLineElemType&& oInst.ref.typ==TypeInfos.measureLineElemType) {
          TransactionManager.tryWriteInstanceField(elem.ref,3.toByte,oInst.fieldValue(3))
          for(i<-5 to 9)
            TransactionManager.tryWriteInstanceField(elem.ref,i.toByte,oInst.fieldValue(i))
        }
      }
      true
    } else false
  }
}




class GraphElemModule extends ActionModule {
  import runtime.function.GraphElemModule._
  var graphTypeID:Int= -1

  def setObjectType(typeID: Int): Unit = graphTypeID = typeID


  val moveAction = new ActionIterator("Verschieben", Some(CommandQuestion(ModuleType.Graph, "Move")), doMove)

  val copyAction = new ActionIterator("Kopieren", Some(CommandQuestion(ModuleType.Graph, "Copy")), doCopy, true)

  val rotateAction = new ActionIterator("Drehen", Some(CommandQuestion(ModuleType.Graph, "Rotate")), doRotate)

  val rotateMultAction = new ActionIterator("Mehrfach Drehen", Some(CommandQuestion(ModuleType.Graph, "RotateMulti")),doRotateMulti)

  val pointModAction = new ActionIterator("Punkt-Mod", Some(DialogQuestion("Punkt-Mod", Seq(new AnswerDefinition("Punkte wählen", DataType.BlobTyp, mvQuestion("Punkt-Mod"), "SelectPoints")))),
      doPointMod)

  val scaleAction = new ActionIterator("Verzerren", Some(DialogQuestion("Verzerren", Seq(new AnswerDefinition("Bezugspunkt eingeben", DataType.VectorTyp,
    Some(DialogQuestion("Faktor X", Seq(new AnswerDefinition("Faktor X eingeben", DataType.DoubleTyp,
      Some(DialogQuestion("Faktor Y", Seq(new AnswerDefinition("Faktor Y eingeben", DataType.DoubleTyp, None)))))))))))),
      doScale)

  val createSymbolAction = new ActionIterator("Symbol erzeugen", Some(CommandQuestion(ModuleType.Graph, "CreateSymbolStamp")), doCreateSymbol, false, 900)

  val mirrorAction = new ActionIterator("Spiegeln", Some(CommandQuestion(ModuleType.Graph,"Mirror")), doMirror)
	
	lazy val actions=List(moveAction,copyAction,rotateAction,rotateMultAction,mirrorAction,pointModAction,scaleAction,createSymbolAction)


  def doMove(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
		if(param.size==2) {
			val delta = param.head._2 match {
			  case startPoint:VectorConstant=>
					val endPoint=param.last._2.toVector
					endPoint-startPoint
				case deltaX:DoubleConstant if param.last._2.getType == DataType.DoubleTyp =>
					new VectorConstant (deltaX.toDouble,param.last._2.toDouble,0)
			  case _=> throw new IllegalArgumentException("Falscher Parametertyp verschieben "+param.head._2)
			}				
			//System.out.println("move delta:"+delta)
      for (d <- data)
        TypeInfos.moduleMap(d.ref.typ).moveElement(d, delta)
			true	
		}
		else false
	}

  def doCopy(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
		if(para.size==3||para.size==2) {
      val param=para.toSeq
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
				val createInst=TransactionManager.tryCreateInstance(d.ref.typ,d.owners,notifyRefandColl = false)
				var newInst=d.clone(createInst.ref,d.owners,Array.empty)
				newInst=TypeInfos.moduleMap(d.ref.typ).copyElement(newInst,theDelta)					
				TransactionManager.tryWriteInstanceData(newInst)
			}			
			true	
		}
		else false
	}


  def doRotate(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
	  //println("Rotate params:\n"+param.mkString(" | " ))
    val param=para.toSeq
	  val center=param.head._2.toVector
	  val angle=param.last._2 match {
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
    // val cosa=math.cos(angle)
    //val sina=math.sin(angle)
	  val rotator=GraphUtils.createRotator(center,angle)
    for (d <- data)
	    TypeInfos.moduleMap(d.ref.typ).rotateElement(d,angle,rotator)
	  true
	}

  def doRotateMulti(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
    val param=para.toSeq
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
      //val cosa=math.cos(rAngle)
      //val sina=math.sin(rAngle)
      val rotator=GraphUtils.createRotator(center,rAngle)
      for(d<-data) {
        val createInst=TransactionManager.tryCreateInstance(d.ref.typ,d.owners,notifyRefandColl = false)
        val newInst = d.clone(createInst.ref, d.owners, Array.empty)
        TransactionManager.tryWriteInstanceData(newInst)
        TypeInfos.moduleMap(d.ref.typ).rotateElement(newInst, rAngle, rotator)
      }
    }
    true
  }

  def doMirror(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
    val param=para.toSeq
	  val withCopies=param.head._2.getType==DataType.StringTyp          
	  val (p1,p2)=param(if(withCopies)1 else 0)._2 match {
	    case oref:ObjectReference =>
				val lineInst=StorageManager.getInstanceData(oref.toObjectReference)
				(lineInst.fieldValue(3).toVector,lineInst.fieldValue(4).toVector)
			case p1:VectorConstant=> (p1,param(if(withCopies)2 else 1)._2.toVector)
	  }
    val mirrorLine = Line3D(p1, p2 - p1)
	  for(d<-data){
	    val inst=if(withCopies) {
	      val createInst=TransactionManager.tryCreateInstance(d.ref.typ,d.owners,notifyRefandColl = false)
				d.clone(createInst.ref,d.owners,Array.empty)
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

  def doPointMod(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
     val param=para.toSeq
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

  def doScale(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
    val param=para.toSeq
	  val refPoint= param.head._2.toVector
	  val sx=param(1)._2.toDouble
	  val sy=param(2)._2.toDouble
	 //println("scale refPoint:"+refPoint+" sx:"+sx+" sy:"+sy+" "+data.mkString(","))
	  for(d<-data) 
	      	 TypeInfos.moduleMap(d.ref.typ).scale(d,refPoint,sx,sy)
	  true
	}

  def doCreateSymbol(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
    val param=para.toSeq
    val parentFolder=param(2)._2.toObjectReference
    println("parentFolder "+parentFolder)
    val owner=Array(new OwnerReference(1,parentFolder))
    val refPoint=param.head._2.toVector* -1d
    println("refPoint:"+refPoint)
    val name=param(1)._2
    val symbolInst=TransactionManager.tryCreateInstance(TypeInfos.symbolDefType,owner,notifyRefandColl = false)
    TransactionManager.tryWriteInstanceField(symbolInst.ref, 0, name)
    val sowner=Array(new OwnerReference(0,symbolInst.ref))    
     for(d <-data) {
        val createInst=TransactionManager.tryCreateInstance(d.ref.typ,sowner,notifyRefandColl = false)
        var newInst=d.clone(createInst.ref,sowner,Array.empty)
        newInst=TypeInfos.moduleMap(d.ref.typ).copyElement(newInst,refPoint)          
        TransactionManager.tryWriteInstanceData(newInst)
      }         
      true
  }
}



class TextModule extends ActionModule with GraphActionModule {
	val horizontalText="Horizontal"
  override val createActions=List(createTextAction)
  val actions = List(takeOverAction,editTextAction, replaceAction, numAction, alignHor, alignVert, distributeAction)
  
  def alignHor=new ActionIterator("Vert ausrichten",None,doAlignHor)
  def alignVert=new ActionIterator("Hor ausrichten",None,doAlignVert)

  def distributeAction = new ActionIterator("Verteilen", Some(DialogQuestion("Verteilen", Seq(new AnswerDefinition("Ausrichtung", DataType.EnumTyp, None, horizontalText + ",Vertikal")))),
		doDistribute)

  def editTextAction = new ActionIterator("Text ändern", Some(DialogQuestion("Text(e) ändern", Seq(new AnswerDefinition("Neuer Text", DataType.StringTyp, None)))), doEditText)

  def moveElement(elem: InstanceData, delta: VectorConstant): Unit = {
		TransactionManager.tryWriteInstanceField(elem.ref,2,elem.fieldValue(2).toVector+delta)		
	}

  def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData = {
		elem.setField(2,elem.fieldValue(2).toVector+delta)
	}
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= {
	   TransactionManager.tryWriteInstanceField(elem.ref,2,rotator(elem.fieldValue(2).toVector))
	  TransactionManager.tryWriteInstanceField(elem.ref,7,new DoubleConstant(elem.fieldValue(7).toDouble+angle*180d/math.Pi))	  
	}

  def createTextAction = new CreateActionImpl("Text", Some(CommandQuestion(ModuleType.Graph,
      "CreateText")), doCreateText)
	
	def doCreateText(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean=
    if(parents.size>1) {Log.e("Multiple parents !"); false}
    else if(param.isEmpty) {Log.e("no param when Create Text");false}
    else{
	  val (pos,text)=if(param.size==3) (param(1)._2,param(2)._2) else (param.head._2,param(1)._2)
	  if(text.toString.length>0) {
	    //println("create Text params:"+param.mkString("|")+"\nFormatparams:"+formFields.mkString("|"))
	    val inst=TransactionManager.tryCreateInstance(theTypeID,Array(new OwnerReference(0.toByte,parents.head.ref)),notifyRefandColl = false)
	    TransactionManager.tryWriteInstanceField(inst.ref,1,text)
	  	TransactionManager.tryWriteInstanceField(inst.ref,2,pos)		  	
	  	writeFormatParams(inst.ref,formFields)	    	    
	  } else println("Wrong param.size:"+param.mkString("| "))
	  
	  true
	}
	override def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  elem.setField(2,mirror(elem.fieldValue(2).toVector))
	}

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit = pointModField(2, elem, delta, chPoints)


  def replaceAction = new ActionIterator("Text ersetzen", Some(DialogQuestion("Text ersetzen",
    Seq(new AnswerDefinition("Suche nach", DataType.StringTyp,
      Some(DialogQuestion("Text ersetzen", Seq(new AnswerDefinition("Ersetzen mit", DataType.StringTyp, None)))))))), doReplace)
	
	def doReplace(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean =
	  if(param.size!=2) {Log.e("Ersetzen falsche parameter:"+param.mkString(","));false} else {
	  val searchText: String =param.head._2.toString
	  val replaceText: String =param.last._2.toString
	  for(d<-data;if d.ref.typ == theTypeID){
	    val text=d.fieldValue(1).toString
	    if(text.contains(searchText)){
	      TransactionManager.tryWriteInstanceField(d.ref,1,StringConstant(text.replace(searchText,replaceText)))
	    }
	  }
	  true
	}

  def numAnswer=new AnswerDefinition("StartWert",DataType.IntTyp,Some(DialogQuestion("Nummerieren",Seq(new AnswerDefinition("Schrittweite",DataType.IntTyp,None)))))

  def numAction= new ActionIterator("Nummerieren", Some(DialogQuestion("Nummerieren",Seq(numAnswer,
      new AnswerDefinition("Muster ($)",DataType.StringTyp,Some(DialogQuestion("Nummerieren",Seq(numAnswer))))
  ))),doNum)

  def doNum(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],para:Iterable[(String,Constant)]):Boolean =  {
    val param=para.toSeq
    val (pattern,start,step)=param.head._2 match {
      case IntConstant(st) => ("",st,param(1)._2.toInt)
      case StringConstant(pa)=> (pa,param(1)._2.toInt,param(2)._2.toInt)
    }
    val placeHolderPos=pattern.indexOf('$')
    val (beforeText,afterText)=if(placeHolderPos== -1) ("","")
                               else (pattern.substring(0,placeHolderPos),pattern.substring(placeHolderPos+1,pattern.length))
    var value=start
    for(elem <- data; if elem.ref.typ==theTypeID) {
      TransactionManager.tryWriteInstanceField(elem.ref,1,StringConstant(beforeText+value+afterText))
      value+=step
    }

    true
  }


  def doAlignHor(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean =
    if (data.size > 1) {
      var xValue = 0d
      for (d <- data)
        xValue += d.fieldValue(2).toVector.x
      val newValue = xValue / data.size.toDouble
      for (d <- data)
        TransactionManager.tryWriteInstanceField(d.ref, 2.toByte, new VectorConstant(newValue, d.fieldValue(2).toVector.y, 0))
      true
    } else false

  def doAlignVert(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean =
    if (data.size > 1) {
      var yValue = 0d
      for (d <- data)
        yValue += d.fieldValue(2).toVector.y
      val newValue = yValue / data.size.toDouble
      for (d <- data)
        TransactionManager.tryWriteInstanceField(d.ref, 2.toByte, new VectorConstant(d.fieldValue(2).toVector.x, newValue, 0))
      true
    } else false

  def doDistribute(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean =
    if (data.size > 2) {
      //println("Verteilen "+param.mkString("|"))
      val horizontal = param.head._2.toString == horizontalText

      def getValue(d: InstanceData): Double = {
        val vector = d.fieldValue(2).toVector
        if (horizontal) vector.x else vector.y
      }

      val sortedList = data.toSeq.sortBy(getValue)(Ordering.Double.TotalOrdering)
      val min: Double = getValue(sortedList.head)
      val max: Double = getValue(sortedList.last)
      val step = (max - min) / (data.size - 1)
      var current = min + step
      for (i <- 1 until data.size - 1; d = sortedList(i)) {
        val vector = d.fieldValue(2).toVector
        val nv = new VectorConstant(if (horizontal) current else vector.x, if (horizontal) vector.y else current, vector.z)
        TransactionManager.tryWriteInstanceField(d.ref, 2, nv)
        current += step
      }

      true
    } else false

  def doEditText(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean =
    if (param.size == 1) {
      val text = StringConstant(param.head._2.toString)
      for (d <- data)
        TransactionManager.tryWriteInstanceField(d.ref, 1, text)
      true
    } else false

  def takeOverAction=new ActionIterator("Stil Übernehmen",Some(DialogQuestion("Textstil übernehmen von welchem Text",
    Seq(new AnswerDefinition("Objekt wählen",DataType.ObjectRefTyp,None,TypeInfos.textElemType.toString )))),doTakeOver,false,buttonID = -1)

  def doTakeOver(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    if(param.size==1) {
      val otherRef=param.head._2.toObjectReference
      val oInst=StorageManager.getInstanceData(otherRef)
      for(elem<-data;if elem.ref.typ == TypeInfos.textElemType){
        TransactionManager.tryWriteInstanceField(elem.ref,0.toByte,oInst.fieldValue(0))
        for(i<-3 to 9)
          TransactionManager.tryWriteInstanceField(elem.ref,i.toByte,oInst.fieldValue(i))
      }
      true
    } else false
  }
}



class BitmapModule extends ActionModule with GraphActionModule {
	override val createActions=List(createBitmapAction)
	val actions:Iterable[ActionTrait] = Nil

  def moveElement(elem: InstanceData, delta: VectorConstant): Unit = {
		TransactionManager.tryWriteInstanceField(elem.ref,6,elem.fieldValue(6).toVector+delta)
	}

  def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData = {
		elem.setField(6,elem.fieldValue(6).toVector+delta)
	}
	def rotateElement(elem:InstanceData,angle:Double,rotator:(VectorConstant)=>VectorConstant):Unit= {
		TransactionManager.tryWriteInstanceField(elem.ref,6,rotator(elem.fieldValue(6).toVector))
		TransactionManager.tryWriteInstanceField(elem.ref,4,new DoubleConstant(elem.fieldValue(4).toDouble+angle*180d/math.Pi))
	}

	def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
		elem.setField(6,mirror(elem.fieldValue(6).toVector))
	}

  def createBitmapAction = new CreateActionImpl("Bitmap", Some(DialogQuestion("Bitmap erzeugen",
      Seq(new AnswerDefinition("Dateipfad", DataType.StringTyp, Some(
        DialogQuestion("Bitmap erzeugen", Seq(new AnswerDefinition("Absetzposition", DataType.VectorTyp, None)))))))), doCreateBitmap)

	def doCreateBitmap(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean=
    if(parents.size>1) {Log.e("Multiple parents !");false} else {
    val path = param.head._2
		val pos=param(1)._2
		if(path.toString.length>0) {
			val inst=TransactionManager.tryCreateInstance(theTypeID,Array(new OwnerReference(0.toByte,parents.head.ref)),notifyRefandColl = false)
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

  def moveElement(elem: InstanceData, delta: VectorConstant): Unit = {
		TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toVector+delta)
		TransactionManager.tryWriteInstanceField(elem.ref,4,elem.fieldValue(4).toVector+delta)
	}

  def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData = {
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

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit = {
    pointModField(3,elem,delta,chPoints)
    pointModField(4,elem,delta,chPoints)	  
	}
	
	override def scale(elem:InstanceData,refPoint:VectorConstant,sx:Double,sy:Double):Unit= {
	  val p1=elem.fieldValue(3).toVector
	  val p2=elem.fieldValue(4).toVector
	  TransactionManager.tryWriteInstanceField(elem.ref,3,scalePoint(p1,refPoint,sx,sy))
	  TransactionManager.tryWriteInstanceField(elem.ref,4,scalePoint(p2,refPoint,sx,sy))
	}


  def createLineAction = new CreateActionImpl("Linie", Some(CommandQuestion(ModuleType.Graph,
      "LineTo")), doCreateLine)
	
	def doCreateLine(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
  	//System.out.println("create line "+param.mkString)
  	//System.out.println("newTyp:"+newTyp+" theTyp:"+theTypeID)
  	makeLinesFromParams(parents,param,formFields)	    
		true
	}
	
	private def makeLinesFromParams(nparents:Iterable[InstanceData],param:Seq[(String,Constant)],formFields:Seq[(Int,Constant)]): Unit = {
	  var lastPoint=param.head._2.toVector
    val parents = Array(new OwnerReference(0.toByte, nparents.head.ref))
    var i = 1
    while (i < param.size) {
	    val np: (String, Constant) =param(i)
	    val nextPoint: VectorConstant =np match {
	      case (_,v:VectorConstant)=> v
	      case ("dx",d:DoubleConstant) =>  lastPoint + new VectorConstant(d.toDouble,0d,0d)
        case ("dx",n:IntConstant)=> lastPoint+new VectorConstant(n.toDouble,0d,0d)
	      case ("dy",d:DoubleConstant) =>	 lastPoint + new VectorConstant(0d,d.toDouble,0d)
        case ("dy",n:IntConstant)=> lastPoint + new VectorConstant(0d,n.toDouble,0d)
        case ("Winkel", d: DoubleConstant) =>
          i += 1
          param(i)._2 match {
            case d2: DoubleConstant =>
              lastPoint + VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d) * d2.toDouble
            case v: VectorConstant =>
              val delta = v - lastPoint
              lastPoint + delta.orthoProjection(VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d))
          }

	      case o => throw new IllegalArgumentException("Wrong parameter "+o+" "+o.getClass.getName)
	    }
	    makeLine(parents,lastPoint,nextPoint,formFields)
	    lastPoint=nextPoint
      i += 1
	  }
	}
	
	private def makeLine(parents:Iterable[InstanceData],p1:VectorConstant,p2:VectorConstant,formFields:Seq[(Int,Constant)]):InstanceData=
	  makeLine(Array(new OwnerReference(0.toByte,parents.head.ref)),p1,p2,formFields)
	
	private def makeLine(parentRefs:Array[OwnerReference],p1:VectorConstant,p2:VectorConstant,formFields:Seq[(Int,Constant)]):InstanceData={	 
	  val inst=TransactionManager.tryCreateInstance(theTypeID,parentRefs,notifyRefandColl = false)
	  TransactionManager.tryWriteInstanceField(inst.ref,3,p1)
	  TransactionManager.tryWriteInstanceField(inst.ref,4,p2)
	  writeFormatParams(inst.ref,formFields)
	  inst
	}


  def createTangentAction = new CreateActionImpl("Tangente", Some(CommandQuestion(ModuleType.Graph,
      "Tangent")), doCreateTangent)
	
	def doCreateTangent(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	 // System.out.println("create Tangent "+param.mkString)
	  createTangent(parents,param.head._2.toObjectReference,param(1)._2.toVector,param(2)._2.toInt==0,formFields)
	  true
	}
	
	private def createTangent(parents:Iterable[InstanceData],circleRef:Reference,point:VectorConstant,first:Boolean,formFields:Seq[(Int,Constant)]):Unit= {
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

  def extendAction = new ActionIterator("Verschneiden", Some(DialogQuestion("Linie(n) verschneiden",
    Seq(new AnswerDefinition("mit Linie", DataType.ObjectRefTyp,
      Some(DialogQuestion("isEdible", Seq(new AnswerDefinition("edible", DataType.BoolTyp, None)))),
      "M" + TypeInfos.lineElemType.toString + "," + TypeInfos.arcElemType.toString)))), doExtend)


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

	def doExtend(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],para:Iterable[(String,Constant)]):Boolean =  {
    import Ordering.Double.TotalOrdering
	  println("Do Extends data:"+data.mkString(" | ")+"\nparam:"+para.mkString(" | "))
	  //println("the Type:"+theTypeID)
		if(para.size>1&&para.size<4 && para.head._2.getType==DataType.ObjectRefTyp) {
      val param=para.toSeq
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
			    def dx: Double =p2.x-p1.x
			    def dy: Double =p2.y-p1.y
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
          val odxIsNull = Math.abs(odx) < GraphElemModule.nearNullTreshold
          val (firstPoint, lastPoint) = if (odxIsNull) //extend line is vertical
				    (hitPoints.minBy(_.y),hitPoints.maxBy(_.y))	// find min/max vertical hitpoints
				   else  (hitPoints.minBy(_.x),hitPoints.maxBy(_.x)) // find min/max horizontal hitpoints
          if ((odxIsNull && op1.y < op2.y) || (!odxIsNull && op1.x < op2.x)) { // p1 < p2 ?
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
		} else throw new IllegalArgumentException("Falsche Parameter "+para.mkString(","))
		true
	}


  def cutPartAction = new ActionIterator("Teillinie löschen", Some(DialogQuestion("Teillinie löschen",
    Seq(new AnswerDefinition("Teillinie auswählen", DataType.ObjectRefTyp, GraphElemModule.cutPointQuestion, "S" + TypeInfos.lineElemType.toString)))),
    doCutPart, true)

  def doCutPart(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
	  cutLine(param.toSeq)
	  true
	}

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
	      val newInst=TransactionManager.tryCreateInstance(TypeInfos.lineElemType,lineInst.owners,notifyRefandColl = false)
	      val outInst=new InstanceData(newInst.ref,for(i<-lineInst.fieldData.indices) 
	        yield if(i==3) p2 else if (i==4) op2 else lineInst.fieldData(i) , newInst.owners)
	      TransactionManager.tryWriteInstanceData(outInst)
	    }
	  } else Log.e("Line Ref not lineType :"+lineRef)
	}


  def parallelAction = new ActionIterator("Parallele Linie", Some(CommandQuestion(ModuleType.Graph,
    "ParLine")), doParallel, true)

  def doParallel(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
    val param=para.toSeq
	  val (offset,numCopy)=if(param.head._2.getType==DataType.IntTyp)(1,param.head._2.toInt) else (0,1)
	  if(data.size==1) {
	    val oldInst=data.head
	    val p1=oldInst.fieldValue(3).toVector
	    val p2=oldInst.fieldValue(4).toVector
      val line = Line3D(p1, p2 - p1)
	    val dist= param(0+offset)._2 match {
	      case v:VectorConstant => line.orthogonalThrough(v) // parallel through point
	      case d:DoubleConstant => line.orthogonalThrough(param(1+offset)._2.toVector).unit*d.toDouble
	    }
	    for(i<-1 to numCopy){
	      val ndist=dist*i.toDouble
		    val newInst=TransactionManager.tryCreateInstance(TypeInfos.lineElemType,oldInst.owners,notifyRefandColl = false)
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

  def createOrthoLineAction = new CreateActionImpl("Lot-Linie", Some(CommandQuestion(ModuleType.Graph,
      "OrthoLine")), doCreateOrthoLine, false)
	
	def doCreateOrthoLine(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  if(param.size==2) {	    
	    val lineInst=StorageManager.getInstanceData(param.head._2.toObjectReference)
	    val p1=lineInst.fieldValue(3).toVector
	    val p2=lineInst.fieldValue(4).toVector
	    val hitPoint=param(1)._2.toVector
      val startPoint = Line3D(p1, p2 - p1).orthProjection(hitPoint)
	    makeLine(parents,startPoint,hitPoint,formFields)	    
	  }
	  
	  true
	}

  def createRectAction = new CreateActionImpl("Rechteck", Some(CommandQuestion(ModuleType.Graph,
      "Rectangle")), doCreateRect)
	
	def doCreateRect(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
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
            case defPoint: VectorConstant => defPoint - Line3D(startPoint, endPoint - startPoint).orthProjection(defPoint)
            case DoubleConstant(width)=> (endPoint-startPoint).unit.transposeXY * width
          }
					makeRectangle(oparents,Seq(startPoint+deltaV,endPoint+deltaV,endPoint-deltaV,startPoint-deltaV),formFields)
				case StringConstant("über Randkante")=>
					val endPoint=param(2)._2.toVector
					val deltaV= param(3)._2 match {
            case defPoint: VectorConstant => defPoint - Line3D(startPoint, endPoint - startPoint).orthProjection(defPoint)
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

  def createParPolyAction = new CreateActionImpl("Paralleler Polygonzug", Some(CommandQuestion(ModuleType.Graph,
      "ParPoly")), doCreateParPoly)
	
	 
    
	
	def doCreateParPoly(u:AbstractUserSocket, parents:Iterable[InstanceData], param:Seq[(String,Constant)], newTyp:Int, formFields:Seq[(Int,Constant)]):Boolean= {
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
          Line3D(points.head + nv1, points(1) - points.head).intersectionWith(Line3D(vvl + nv2, points.last - vvl))
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
        yield Line3D(lp + nv1, tp - lp).intersectionWith(Line3D(tp + nv2, np - tp))
	  }
	  val allPoints=firstPoints+:nextPoints:+lastPoints
	    
	  for(Seq(a,b)<-allPoints.sliding(2,1);i<-distances.indices) {
	    makeLine(oparents,a(i),b(i),formFields)
	  } 	  
	  true
	}
	
	def cutElemsAction=new ActionIterator("Elemente schneiden",None,doCutElems,false)

  def doCutElems(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
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

  override def cutElementByLine(elData: InstanceData, cutLine: Edge): Unit = {
	  val p1=elData.fieldValue(3).toVector
	  val p2=elData.fieldValue(4).toVector
	  cutLine.getCutIntersectionWith(p1,p2) match {
	    case Seq((pos,cutPoint)) if pos > 0 && pos < 1 & cutPoint != p1 && cutPoint != p2 =>
				TransactionManager.tryWriteInstanceField(elData.ref,4,cutPoint)
				val newRef=TransactionManager.tryCreateInstance(theTypeID, elData.owners,notifyRefandColl = false)
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
  val actions: Iterable[ActionTrait] = Seq.empty
  
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
	}
  
  override def scale(elem:InstanceData,refPoint:VectorConstant,sx:Double,sy:Double):Unit= {
    val p1=elem.fieldValue(3).toVector 
    val axis1=elem.fieldValue(4).toDouble
    val axis2=elem.fieldValue(5).toDouble
    TransactionManager.tryWriteInstanceField(elem.ref,3,scalePoint(p1,refPoint,sx,sy))
    TransactionManager.tryWriteInstanceField(elem.ref,4,new DoubleConstant(axis1*sx))
    TransactionManager.tryWriteInstanceField(elem.ref,5,new DoubleConstant(axis2*sx))
  }

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit = pointModField(3, elem, delta, chPoints)

  def createEllipseCenterAction = new CreateActionImpl("Ellipse", Some(CommandQuestion(ModuleType.Graph,
      "EllipseCenter")), doCreateEllipseCenter)
	
	def doCreateEllipseCenter(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  //println("Create Ellipse param:" + param.mkString("  | ")+"\n Formfields:"+formFields.mkString(" | "))
	  var currParam:Int= -1
	  def nextParam(): Constant = {
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
	    case DoubleConstant(d) =>
				nextParam() match {
          case v:VectorConstant => (d,getAngle(v))
          case DoubleConstant(d2) =>(d,d2)
          case i2:IntConstant => (d,i2.toDouble)
        }
      case i:IntConstant => nextParam() match {
        case v:VectorConstant => (i.toDouble,getAngle(v))
        case DoubleConstant(d2) =>(i.toDouble,d2)
        case i2:IntConstant => (i.toDouble,i2.toDouble)
      }
      case other => Log.e("unknown match:"+other+" "+other.getClass);(0.0,0.0)
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
			case DoubleConstant(d)=> d
      case i:IntConstant=> i.toDouble
	  }
	  if(axis2Len==0) return false
	  
	  def createEllipse(sa:Double,ea:Double): Unit = {
	    val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
	    val inst=TransactionManager.tryCreateInstance(newTyp,parentRef,notifyRefandColl = false)
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
	    case DoubleConstant(d) => d
      case i:IntConstant=> i.toDouble
	  } 
	  
	  val endAngle=nextParam() match {
	    case v:VectorConstant => getAngle(v)-mainAngle
	    case DoubleConstant(d) => d
      case i:IntConstant => i.toDouble
	  }
	  
	  createEllipse((if (startAngle<0) startAngle+360 else startAngle)% 360,if(endAngle==360) 360 else (if(endAngle<0) endAngle+360 else endAngle) % 360)  
	  true
	}	
}


class ArcModule extends ActionModule with GraphActionModule {
  import runtime.function.GraphElemModule._

  lazy val parDistQuestion = Some(DialogQuestion("Abstand",
    Seq(new AnswerDefinition("Abstand eingeben:", DataType.DoubleTyp, None))))
  
	override val createActions=List(createArcCenterAction,createArcGeneralAction)	
	val actions =  Seq(makeParallelArc,cutPartAction)

  def createArcCenterAction = new CreateActionImpl("Mittelpunktkreis", Some(CommandQuestion(ModuleType.Graph,
      "ArcCenter")), doCreateArcCenter)
	
	
	
	def doCreateArcCenter(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
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
	  val inst=TransactionManager.tryCreateInstance(newTyp,parentRef,notifyRefandColl = false)
	  TransactionManager.tryWriteInstanceField(inst.ref,3,center)
	  TransactionManager.tryWriteInstanceField(inst.ref,4,new DoubleConstant(radius))
	  TransactionManager.tryWriteInstanceField(inst.ref,5,new DoubleConstant(startAngle))
	  TransactionManager.tryWriteInstanceField(inst.ref,6,new DoubleConstant(endAngle))
	  writeFormatParams(inst.ref,formFields)	  
	  true
	}

  def createArcGeneralAction = new CreateActionImpl("Allgemeiner Kreis", Some(CommandQuestion(ModuleType.Graph,
      "ArcGeneral")), doCreateArcGeneral)
	
	def doCreateArcGeneral(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
	  false
	}

  def moveElement(elem: InstanceData, delta: VectorConstant): Unit = {
		TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toVector+delta)
	}

  def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData = {
		elem.setField(3,elem.fieldValue(3).toVector+delta)
	}	
	
	
	
	override def mirrorElement(elem:InstanceData,mirror: VectorConstant =>VectorConstant):InstanceData= {
	  val radius=elem.fieldValue(4).toDouble
	  val centerPoint=elem.fieldValue(3).toVector
	  val newCenter=mirror(centerPoint)
	  val result =elem.setField(3,newCenter)
	  val p1=mirror(GraphElemModule.pointFromAngle(centerPoint,elem.fieldValue(5).toDouble,radius))
	  val p2=mirror(GraphElemModule.pointFromAngle(centerPoint,elem.fieldValue(6).toDouble,radius))	
	  val (newA,newB)=fixAngles(getAngle(p2,newCenter),getAngle(p1,newCenter))
	  result.setField(5,new DoubleConstant(newA)).setField(6,new DoubleConstant(newB))    
	}

  def fixAngles(sa: Double, ea: Double): (Double, Double) = {
    val newA = /*if (sa < 360d && sa != 0) sa + 360d else*/ if (sa > 360d) sa - 360d else sa
	  val newB=/*if(ea<360d) ea+360d else*/ if(ea>360d) ea-360d else ea
	  (newA,if(newB<newA)newB+360d else newB)
	}
	
	
	def rotateElement(elem:InstanceData,angle:Double,rotator: VectorConstant =>VectorConstant):Unit= {
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

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit = pointModField(3, elem, delta, chPoints)
	
	def makeParallelArc=new ActionIterator("Paralleler Kreis",parDistQuestion,doMakeParallelArc) 
	
	def doMakeParallelArc(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean= {
	  for (elem<-data;if elem.ref.typ == theTypeID){
	    val oldRadius=elem.fieldValue(4).toDouble  
	    val dist=param.head._2.toDouble
	    val inst=TransactionManager.tryCreateInstance(theTypeID,elem.owners,notifyRefandColl = false)
	    for(i<-0 to 3) TransactionManager.tryWriteInstanceField(inst.ref, i.toByte, elem.fieldValue(i))
	    for(i<-5 to 6) TransactionManager.tryWriteInstanceField(inst.ref, i.toByte, elem.fieldValue(i))
	     TransactionManager.tryWriteInstanceField(inst.ref, 4.toByte, new DoubleConstant(oldRadius+dist))
	  	
	  }
	  true
	}

  def cutPartAction = new ActionIterator("Teilkreis löschen", Some(DialogQuestion("Teilkreis löschen",
    Seq(new AnswerDefinition("Teilkreis auswählen", DataType.ObjectRefTyp, GraphElemModule.cutPointQuestion, "S" + TypeInfos.arcElemType.toString)))), doCutPart, true)

  def doCutPart(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], para: Iterable[(String, Constant)]): Boolean = {
    val param=para.toSeq
		val arcRef=param.head._2.toObjectReference
		val a1: Double =param(1)._2.toVector.x
		val a2: Double =param(2)._2.toVector.x
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
				val newInst=arcInst.data.clone(newArc.ref,newArc.owners,Array.empty).
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
  val actions = List(intersectAction, cutAction, addAction(), setStartPointAction(),takeOverAction)
  lazy val allowedClasses: List[Int] =List(TypeInfos.polyElemType,TypeInfos.areaPolyElemType,TypeInfos.wohnflaechenElementType)

  def moveElement(elem: InstanceData, delta: VectorConstant): Unit =
  	TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toPolygon.translate(delta))

  def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData =
    elem.setField(3, elem.fieldValue(3).toPolygon.translate(delta))

  def mirrorElement(elem:InstanceData,mirror:(VectorConstant)=>VectorConstant):InstanceData= {
	  elem.setField(3,elem.fieldValue(3).toPolygon.transform(mirror))
  }

  def rotateElement(elem: InstanceData, angle: Double, rotator: (VectorConstant) => VectorConstant): Unit =
    TransactionManager.tryWriteInstanceField(elem.ref, 3, elem.fieldValue(3).toPolygon.transform(rotator))

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit =
    TransactionManager.tryWriteInstanceField(elem.ref, 3, elem.fieldValue(3).toPolygon.translatePoints(chPoints, delta))

  def createPolyAction = new CreateActionImpl("Füllfläche", Some(CommandQuestion(ModuleType.Graph,
      "PolyTo")), doCreatePoly)




	def doCreatePoly(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
    val inst = TransactionManager.tryCreateInstance(theTypeID, parentRef, notifyRefandColl = true)
    TransactionManager.tryWriteInstanceField(inst.ref, 3, GraphElemModule.polygonFromParams(inst.ref, param))
		writeFormatParams(inst.ref,formFields)
		true
	}

  def intersectQuestion: DialogQuestion = DialogQuestion("Schnittfläche mit Polygon", Seq(new AnswerDefinition("anderes Polygon wählen", DataType.ObjectRefTyp,
    None, "F" + TypeInfos.polyElemType.toString)))

  def intersectAction=new ActionIterator("Schnittfläche",Some(intersectQuestion),doTrans(_.intersect(_)))

  def cutQuestion: DialogQuestion = DialogQuestion("Polygonfläche abschneiden", Seq(new AnswerDefinition("anderes Polygon wählen", DataType.ObjectRefTyp,
    None, "F" + TypeInfos.polyElemType.toString)))

  def cutAction=new ActionIterator("Abschneiden",Some(cutQuestion),doTrans(_.subtract(_)))

  def addQuestion(): DialogQuestion = DialogQuestion("Polygonfläche hinzufügen", Seq(new AnswerDefinition("anderes Polygon wählen", DataType.ObjectRefTyp,
    None, "F" + TypeInfos.polyElemType.toString)))

  def addAction(): ActionIterator = new ActionIterator("Hinzufügen", Some(addQuestion()), doTrans(_.add(_)))

  def doTrans(func: (Polygon, Polygon) => Polygon)(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    val otherPoly=StorageManager.getInstanceData(param.head._2.toObjectReference).fieldValue(3).toPolygon.createCopy().asInstanceOf[Polygon]
    for(inst <-data) {
      val oldPoly=inst.fieldValue(3).toPolygon.createCopy().asInstanceOf[Polygon]
      val newPoly = func(oldPoly, otherPoly)
      TransactionManager.tryWriteInstanceField(inst.ref, 3, newPoly)
    }
    true
  }

  def setStartPointQuestion(): DialogQuestion = DialogQuestion("Ausgangspunkt Schraffur",
    Seq(new AnswerDefinition("Punkt wählen", DataType.VectorTyp, None)))

  def setStartPointAction(): ActionIterator = new ActionIterator("Ausgangspunkt", Some(setStartPointQuestion()), doSetStartPoint)

  def doSetStartPoint(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean= {
  	if(param.size==1 && param.head._2.getType==DataType.VectorTyp ) {
  		val startPoint=param.head._2.toVector
      for (d <- data)
        TransactionManager.tryWriteInstanceField(d.ref, 6, startPoint)
      true
  	}
  	else false
  }

  def takeOverAction=new ActionIterator("Format Übern",Some(DialogQuestion("Format übernehmen von welchem Objekt",
    Seq(new AnswerDefinition("Objekt wählen",DataType.ObjectRefTyp,None,allowedClasses.map(_.toString).mkString(",") )))),doTakeOver,false,-1)

  def doTakeOver(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    if(param.size==1) {
      val otherRef=param.head._2.toObjectReference
      val oInst=StorageManager.getInstanceData(otherRef)
      val fieldOffset=if(otherRef.typ==TypeInfos.polyElemType)0 else 1
      for(elem<-data;if allowedClasses.contains(elem.ref.typ)){
        for(i<-5 to 7)
          TransactionManager.tryWriteInstanceField(elem.ref,(i+(if(elem.ref.typ==TypeInfos.polyElemType)0 else 1)).toByte,oInst.fieldValue(i+fieldOffset))
        TransactionManager.tryWriteInstanceField(elem.ref, (if (elem.ref.typ == TypeInfos.polyElemType) 0 else 1).toByte,oInst.fieldValue(fieldOffset))
      }
      true
    } else false
  }
}


class PolygonLineModule extends ActionModule with GraphActionModule {
  override val createActions = List(createPolyLineAction)
  val actions:Iterable[ActionTrait] = Nil

  def moveElement(elem: InstanceData, delta: VectorConstant): Unit =
    TransactionManager.tryWriteInstanceField(elem.ref, 3, elem.fieldValue(3).toPolygon.translate(delta))

  def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData =
    elem.setField(3, elem.fieldValue(3).toPolygon.translate(delta))

  def mirrorElement(elem: InstanceData, mirror: (VectorConstant) => VectorConstant): InstanceData = {
    elem.setField(3, elem.fieldValue(3).toPolygon.transform(mirror))
  }

  def rotateElement(elem: InstanceData, angle: Double, rotator: (VectorConstant) => VectorConstant): Unit =
    TransactionManager.tryWriteInstanceField(elem.ref, 3, elem.fieldValue(3).toPolygon.transform(rotator))

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit =
    TransactionManager.tryWriteInstanceField(elem.ref, 3, elem.fieldValue(3).toPolygon.translatePoints(chPoints, delta))

  def createPolyLineAction = new CreateActionImpl("PolyLinie", Some(CommandQuestion(ModuleType.Graph,
      "PolyLineTo")), doCreatePolyLine)


  def doCreatePolyLine(u: AbstractUserSocket, parents: Iterable[InstanceData], param: Seq[(String, Constant)], newTyp: Int, formFields: Seq[(Int, Constant)]): Boolean = {
    val parentRef = Array(new OwnerReference(0.toByte, parents.head.ref))
    val inst = TransactionManager.tryCreateInstance(theTypeID, parentRef, notifyRefandColl = true)
    TransactionManager.tryWriteInstanceField(inst.ref, 3, GraphElemModule.polygonFromParamsToLine(inst.ref, param))
    writeFormatParams(inst.ref, formFields)
    true
  }

}


class MeasureLineModule extends PolygonLineModule {
  override val actions:Iterable[ActionTrait] = Seq(setFactorAction,verbindenAction)
  override def moveElement(elem: InstanceData, delta: VectorConstant): Unit =
    TransactionManager.tryWriteInstanceField(elem.ref, 4, elem.fieldValue(4).toPolygon.translate(delta))

  override def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData =
    elem.setField(4, elem.fieldValue(4).toPolygon.translate(delta))

  override def rotateElement(elem: InstanceData, angle: Double, rotator: (VectorConstant) => VectorConstant): Unit =
    TransactionManager.tryWriteInstanceField(elem.ref, 4, elem.fieldValue(4).toPolygon.transform(rotator))

  def removeDoublePoints(pl: PointList): PointList = {
    val doublePoints = (1 until pl.points.size).filter(i => VectorConstant.similar(pl.points(i - 1), pl.points(i)))
    //println("doublepoints:"+doublePoints.mkString(","))
    if (doublePoints.isEmpty) pl
    else PointList(pl.points.zipWithIndex.filterNot { case (_, pix) => doublePoints.contains(pix) }.map(_._1))
  }

  def updateLengthValue(instRef: Reference, p: Polygon): Unit = {
    val poly = new Polygon(Seq(instRef), p.pathList.map(list => {
      val res = removeDoublePoints(list)
      if (res.points.size < 3) res else res.removeStraightEdges()
    }))

    TransactionManager.tryWriteInstanceField(instRef, 11, GraphElemModule.getUmfang(poly, addStartPoint = false))
    TransactionManager.tryWriteInstanceField(instRef, 4, poly)
  }

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit =
    updateLengthValue(elem.ref, elem.fieldValue(4).toPolygon.translatePoints(chPoints, delta))

  override def createPolyLineAction = new CreateActionImpl("MessLinie", Some(CommandQuestion(ModuleType.Graph,
      "PolyLineTo")), doCreatePolyLine)

  override def doCreatePolyLine(u: AbstractUserSocket, parents: Iterable[InstanceData], param: Seq[(String, Constant)], newTyp: Int, formFields: Seq[(Int, Constant)]): Boolean = {
    val parentRef = Array(new OwnerReference(0.toByte, parents.head.ref))
    val inst = TransactionManager.tryCreateInstance(theTypeID, parentRef, notifyRefandColl = true)
    updateLengthValue(inst.ref, GraphElemModule.polygonFromParamsToLine(inst.ref, param))
    writeFormatParams(inst.ref, formFields)
    true
  }

  def factorQuestion: DialogQuestion = DialogQuestion("Faktor eingeben", Seq(new AnswerDefinition("Faktor", DataType.DoubleTyp,
    None)))

  def setFactorAction: ActionIterator =new ActionIterator("Faktor eingeben",Some(factorQuestion),doSetFactor)
  def doSetFactor(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    val factor=param.head._2
    for(elem<-data)
      TransactionManager.tryWriteInstanceField(elem.ref,12,factor)
    true
  }

  def doVerbinden(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    if (param.size == 1) {
      val otherRef = param.head._2.toObjectReference
      val oInst = StorageManager.getInstanceData(otherRef)
      val thisPath: PointList =data.head.fieldValue(4).toPolygon.pathList.head
      val otherPath: PointList =oInst.fieldValue(4).toPolygon.pathList.head
      val resultPoints: Seq[VectorConstant] =if(otherPath.points.head==thisPath.points.last) thisPath.points++ otherPath.points.drop(1)
      else if(thisPath.points.head==otherPath.points.last) otherPath.points ++ thisPath.points.drop(1)
      else if(otherPath.points.head==thisPath.points.head) otherPath.points.drop(1).reverse ++ thisPath.points
      else if(thisPath.points.last==otherPath.points.last) thisPath.points ++ otherPath.points.dropRight(1).reverse
      else throw new IllegalArgumentException("Andere Poly-Linie überlappt nicht")
      val resPointList=new PointList(resultPoints).removeStraightEdges()
      val poly=new Polygon(Seq(data.head.ref),Seq(resPointList))
      TransactionManager.tryWriteInstanceField(data.head.ref,4,poly)
      TransactionManager.tryWriteInstanceField(data.head.ref,11, GraphElemModule.getUmfang(poly, addStartPoint = false))
      TransactionManager.tryDeleteInstance(otherRef,None,None)
      true
    } else false
  }

  def verbindenAction=new ActionIterator("Verbinden",Some(DialogQuestion("Verbinden mit",
    Seq(new AnswerDefinition("PolyLinie wählen",DataType.ObjectRefTyp,None,"342")))),doVerbinden,false,-1)
}


/** Measure of Area
*
*/
class AreaPolygonModule extends PolygonModule {
  override val actions=List(intersectAction, cutAction, addAction(), setStartPointAction(),takeOverAction,
    convertWohnflaecheAction,convertLineAction,setFactorAction)
  override def moveElement(elem: InstanceData, delta: VectorConstant): Unit =
  	TransactionManager.tryWriteInstanceField(elem.ref,4,elem.fieldValue(4).toPolygon.translate(delta))

  override def copyElement(elem: InstanceData, delta: VectorConstant): InstanceData =
    elem.setField(4,elem.fieldValue(4).toPolygon.translate(delta))  
  
  override def rotateElement(elem:InstanceData,angle:Double,rotator: VectorConstant =>VectorConstant):Unit=
    TransactionManager.tryWriteInstanceField(elem.ref,4,elem.fieldValue(4).toPolygon.transform(rotator))

  protected def generateExpression(inst: InstanceData, poly: Polygon): Expression = GraphElemModule.generateAreaExpression(inst,poly)


  def updateAreaValue(inst: InstanceData, p: Polygon): Boolean = {
    val instRef = inst.ref
    val poly=new Polygon(Seq(instRef),p.pathList.map(_.removeDoublePoints().removeStraightEdges()))
    TransactionManager.tryWriteInstanceField(instRef, 4, poly)
    TransactionManager.tryWriteInstanceField(instRef, 10, generateExpression(inst, poly) match {
      case EMPTY_EX => DoubleConstant(poly.getAreaValue)
      case ex: Expression => ex
    })
  }

  override def pointMod(elem: InstanceData, delta: VectorConstant, chPoints: Set[VectorConstant]): Unit =
    updateAreaValue(elem, elem.fieldValue(4).toPolygon.translatePoints(chPoints, delta))

  override def createPolyAction = new CreateActionImpl("Messfläche", Some(CommandQuestion(ModuleType.Graph,
      "PolyTo")), doCreatePoly)

	override def doCreatePoly(u:AbstractUserSocket,parents:Iterable[InstanceData],param:Seq[(String,Constant)],newTyp:Int,formFields:Seq[(Int,Constant)]):Boolean= {
		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
  	val inst=TransactionManager.tryCreateInstance(theTypeID,parentRef,notifyRefandColl = true)
    updateAreaValue(inst, GraphElemModule.polygonFromParams(inst.ref, param))
		writeFormatParams(inst.ref,formFields)
		true
	}

  override def intersectQuestion: DialogQuestion = DialogQuestion("Schnittfläche mit Fläche", Seq(new AnswerDefinition("andere Fläche wählen", DataType.ObjectRefTyp,
    None, "F" + TypeInfos.areaPolyElemType.toString + "," + TypeInfos.wohnflaechenElementType.toString)))

  override def cutQuestion: DialogQuestion = DialogQuestion("Fläche abschneiden", Seq(new AnswerDefinition("andere Fläche wählen", DataType.ObjectRefTyp,
    None, "F" + TypeInfos.areaPolyElemType.toString + "," + TypeInfos.wohnflaechenElementType.toString)))

  override def addQuestion(): DialogQuestion = DialogQuestion("Fläche hinzufügen", Seq(new AnswerDefinition("andere Fläche wählen", DataType.ObjectRefTyp,
    None, "F" + TypeInfos.areaPolyElemType.toString + "," + TypeInfos.wohnflaechenElementType.toString)))
  
  override def doTrans(func:(Polygon,Polygon)=>Polygon)(u:AbstractUserSocket,owner:OwnerReference,data:Iterable[InstanceData],param:Iterable[(String,Constant)]):Boolean= {
    val otherPoly=StorageManager.getInstanceData(param.head._2.toObjectReference).fieldValue(4).toPolygon.createCopy().asInstanceOf[Polygon]
    for(inst <-data) {
      val oldPoly=inst.fieldValue(4).toPolygon.createCopy().asInstanceOf[Polygon]
      updateAreaValue(inst, func(oldPoly, otherPoly))
    }
    true
  }

  def convertWohnflaecheAction=new ActionIterator("In Wohnfläche",None,convertToWohnflaeche)

  def convertToWohnflaeche(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    for(d<-data;if d.ref.typ==TypeInfos.areaPolyElemType){
      val inst=TransactionManager.tryCreateInstance(TypeInfos.wohnflaechenElementType,d.owners,notifyRefandColl = true)
      for(i<-1 to 9)
        TransactionManager.tryWriteInstanceField(inst.ref,i.toByte,d.fieldData(i))
      val poly=d.fieldValue(4).toPolygon
      TransactionManager.tryWriteInstanceField(inst.ref, 10, GraphElemModule.generateWohnExpression(inst,poly) match {
        case EMPTY_EX => DoubleConstant(poly.getAreaValue)
        case ex: Expression => ex
      })
      TransactionManager.tryWriteInstanceField(inst.ref,11.toByte,d.fieldData(11))
      TransactionManager.tryDeleteInstance(d.ref,None,None)
    }
    true
  }

  def convertLineAction=new ActionIterator("In Messlinie",None,convertToMeasureLine)

  def convertToMeasureLine(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    for (d <- data; if d.ref.typ == TypeInfos.areaPolyElemType) {
      val inst = TransactionManager.tryCreateInstance(TypeInfos.measureLineElemType, d.owners, notifyRefandColl = true)
      for (i <- 1 to 3)
        TransactionManager.tryWriteInstanceField(inst.ref, i.toByte, d.fieldData(i))
      val oldPoly = d.fieldValue(4).toPolygon
      val poly = new Polygon(Seq(d.ref), oldPoly.pathList.map(list =>
        new PointList(list.points :+ list.points.head)
      ))
      TransactionManager.tryWriteInstanceField(inst.ref, 4.toByte, poly)
      TransactionManager.tryWriteInstanceField(inst.ref, 11, GraphElemModule.getUmfang(poly, addStartPoint = false))
      TransactionManager.tryWriteInstanceField(inst.ref, 10.toByte, d.fieldData(9))

      TransactionManager.tryDeleteInstance(d.ref, None, None)
    }
    true
  }
  def factorQuestion: DialogQuestion = DialogQuestion("Faktor eingeben", Seq(new AnswerDefinition("Faktor", DataType.DoubleTyp,
    None)))

  def setFactorAction=new ActionIterator("Faktor eingeben",Some(factorQuestion),doSetFactor)
  def doSetFactor(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    val factor=param.head._2
    for(elem<-data)
      TransactionManager.tryWriteInstanceField(elem.ref,11,factor)
    true
  }
}

class WohnflaechenModul extends AreaPolygonModule {
  override def createPolyAction = new CreateActionImpl("Wohnfläche", Some(CommandQuestion(ModuleType.Graph,
      "PolyTo")), doCreatePoly)

  override def onFieldChanged(self: InstanceData, fieldNr: Byte, newValue: Expression): Unit = if (fieldNr == 13) {updateAreaValue(self, self.fieldValue(4).toPolygon)}

  override val notifyFieldChanged = true

  protected override def generateExpression(inst: InstanceData, poly: Polygon): Expression = GraphElemModule.generateWohnExpression(inst,poly)

  override val actions=List(intersectAction, cutAction, addAction(), setStartPointAction(),takeOverAction,convertToMessFlaecheAction)

  def convertToMessFlaecheAction=new ActionIterator("In MessFläche",None,convertToMessflaeche)

  def convertToMessflaeche(u: AbstractUserSocket, owner: OwnerReference, data: Iterable[InstanceData], param: Iterable[(String, Constant)]): Boolean = {
    for(d<-data;if d.ref.typ==TypeInfos.wohnflaechenElementType){
      val inst=TransactionManager.tryCreateInstance(TypeInfos.areaPolyElemType,d.owners,notifyRefandColl = true)
      for(i<-1 to 9)
        TransactionManager.tryWriteInstanceField(inst.ref,i.toByte,d.fieldData(i))
      val poly=d.fieldValue(4).toPolygon
      TransactionManager.tryWriteInstanceField(inst.ref, 10, GraphElemModule.generateAreaExpression(inst,poly) match {
        case EMPTY_EX => DoubleConstant(poly.getAreaValue)
        case ex: Expression => ex
      })
      TransactionManager.tryWriteInstanceField(inst.ref,11.toByte,d.fieldData(11))
      TransactionManager.tryDeleteInstance(d.ref,None,None)
    }
    true
  }

}


object TypeInfos {
  val moduleMap: mutable.HashMap[Int, GraphActionModule] = collection.mutable.HashMap[Int, GraphActionModule]()
	val polyElemType=42 // WARNING ElementType is fixed because it can not be found at runtime
	val lineElemType=40
  val ellipseElemType=43
	val arcElemType=41
	val textElemType=44
	val areaPolyElemType=341
  val wohnflaechenElementType = 343
  val symbolElemType=45
  val symbolFillType=47
  val symbolDefType=411
  val polyLineElemType = 49
  val measureLineElemType = 342
}