package client.importer
import definition.data.InstanceData
import definition.data.Reference
import util.Log
import scala.swing.Window
import java.awt.Point
import definition.data.OwnerReference
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import client.comm.ClientQueryManager
import definition.typ.AllClasses
import definition.expression.VectorConstant
import definition.expression.IntConstant
import client.graphicsView.AcadColor
import definition.expression.DoubleConstant
import definition.expression.Expression
import definition.expression.EMPTY_EX
import client.comm.CreateInstancesBuffer
import client.graphicsView.LineStyleHandler
import definition.expression.StringConstant
import client.graphicsView.HatchHandler
import scala.collection.mutable.ArrayBuffer
import definition.expression.Polygon
import definition.expression.PointList
import client.graphicsView.FontHandler


class FieldBuffer(set:DXFSettings){
	val fieldsMap=collection.mutable.HashMap[Int,String]()
	//val xBuffer=ArrayBuffer[Double]()
	//val yBuffer=ArrayBuffer[Double]()
	val pointBuffer=ArrayBuffer[VectorConstant]()
	val polyBuffer=ArrayBuffer[PointList]()
	var lastX=0d

	def getDouble(f:Int) = fieldsMap.getOrElse(f,"0").trim.toDouble
	def getString(f:Int,default:String="") = fieldsMap.getOrElse(f,default)
	def getInt(f:Int) = fieldsMap.getOrElse(f,"0").trim.toInt

	def clear()= {
		fieldsMap.clear()
		pointBuffer.clear()
		polyBuffer.clear()
	}

	def fill(code:String,fieldValue:String) = {
		code.trim.toInt match {
			case 10 => lastX=fieldValue.trim.toDouble+set.dx
			case 20 => pointBuffer+=new VectorConstant(lastX,fieldValue.trim.toDouble+set.dy,0d)//;println("add Point: "+pointBuffer.last)
			case 93|98 => if(pointBuffer.nonEmpty){
				polyBuffer+= new PointList(pointBuffer.toSeq)
				pointBuffer.clear()
			}
			case 0 => Log.e("Code 0 in fill, value: "+fieldValue)
			case fieldNr => fieldsMap(fieldNr) = fieldValue
		}
	}
}

class DXFImporter extends FileImportDescriptor {

	val name="Zeichnungen"
		val PI_32=math.Pi*3/2 
		val dbNameField=1.toByte
		val STANDARD_LAYER="Standard"
		val bufferMap=collection.mutable.HashMap[String,CreateInstancesBuffer]()
	  var lineNumber=0

		lazy val lineTyp=AllClasses.get.getClassIDByName("LineElem")
		lazy val arcTyp=AllClasses.get.getClassIDByName("ArcElem")
		lazy val ellTyp=AllClasses.get.getClassIDByName("EllipseElem")
		lazy val polyTyp=AllClasses.get.getClassIDByName("PolyElem")
		lazy val textTyp=AllClasses.get.getClassIDByName("TextElem")
		lazy val lineStyleType=AllClasses.get.getClassIDByName("LinienStil")
		lazy val hatchStyleType=AllClasses.get.getClassIDByName("Schraffur")
		

		def allowedFileTypes:Seq[(String,String)]=Seq(("DXF-Zeichnung","DXF"))		

		def canImport(files:Seq[File],droppedTarget:Option[InstanceData],ownerRef:OwnerReference):Boolean = {
			//println("Can import "+files+" DT:"+droppedTarget+" or:"+ownerRef+" hr:"+ hasRightFileTypes(files))
			hasRightFileTypes(files)
		}


		def showImportDialog(window:Window, wpos:Point,files:Seq[File],dropTarget:Option[InstanceData]):Seq[AnyRef]=  {
			val settings=new DXFSettings	 
			settings.drawingScale=50
			val dialog=new DXFImportDialog(window,settings,files)	 
			if(dialog.showDialog(wpos)) Seq(settings) 
			else Seq.empty 
		}

		def createMissingLineStyles(settings:DXFSettings) = {
		  for(folder<-LineStyleHandler.folderRef) {
		    var lastIx=LineStyleHandler.styles.last.ix
		    val ownerRef=new OwnerReference(1,folder)
		    
		    val lineData=  for(nls<-settings.unknownLineStyles)
		      yield (lineStyleType,Array[Expression](new StringConstant(nls.name),
		          new StringConstant(nls.dots.mkString(";")),EMPTY_EX))
		    ClientQueryManager.createInstances(Array(ownerRef),lineData)
		    // reload 
		    LineStyleHandler.loadSettings(folder)
		    for(nls<-settings.unknownLineStyles){
		      lastIx+=1
		      settings.lineStyleMapping+=((nls.name,lastIx))
		    }
		    settings.unknownLineStyles.clear()
		  }
		}
		
		def createMissingHatches(settings:DXFSettings)= {
		  for(folder<-HatchHandler.folderRef){
		    var lastIx=HatchHandler.hatchList.last.ix
		    val ownerRef=new OwnerReference(1,folder)
		    val hatchData= for (nhs<-settings.unknownHatchStyles)
		      yield (hatchStyleType,nhs match {
		        case uk:HatchStylewithUnknownLS=>
							val ls1=uk.lsName1 match {case Some(lname)=> settings.lineStyleMapping(lname);case None=> uk.lineStyle1}
							val ls2=uk.lsName2 match {case Some(lname)=> settings.lineStyleMapping(lname);case None=> uk.lineStyle2}
							Array[Expression](new StringConstant(name),new DoubleConstant(uk.angle1),new IntConstant(ls1),
                  new DoubleConstant(uk.distance1),new DoubleConstant(uk.angle2),new IntConstant(ls2),new DoubleConstant(uk.distance2),
                  new DoubleConstant(uk.thickness))
						case o=> o.toExpression
		      })
		    ClientQueryManager.createInstances(Array(ownerRef),hatchData)
		    HatchHandler.loadSettings(folder)
		    for(nhs <-settings.unknownHatchStyles){
		      lastIx+=1
		      settings.hatchStyleMapping+=((nhs.name,lastIx))
		    }
		    settings.unknownHatchStyles.clear()
		  }
		}

		def importFile(file:File,baseObject:Reference,settings:Seq[AnyRef],progressListener:(Int)=>Boolean):Boolean= 
			settings match {
				case Seq(set:DXFSettings) =>  
				createMissingLineStyles(set)
					createMissingHatches(set)
					ClientQueryManager.writeInstanceField(baseObject,2.toByte,new IntConstant(set.drawingScaleID))
					//println("Import File:"+file)
					//val startTime=System.currentTimeMillis()
					val reader=new BufferedReader(new FileReader(file))
					val owner=Array(new OwnerReference(0,baseObject))
					val ownerInst=ClientQueryManager.queryInstance(baseObject,-1).head
					val ownerOwner=ownerInst.owners
					val standardBuffer=new CreateInstancesBuffer(owner,20)
					bufferMap.clear()
					bufferMap(STANDARD_LAYER)=standardBuffer
					val fileSize=file.length
					var code="0"
					var section=""
					var bytesRead:Long=0
					var encoding:String=null
					val SpecialMatch="%%(\\d+)".r

					val allowedLayers={val res=set.selectedLayers map(set.layers(_))
                                if(res.isEmpty) Seq("0") else res
          }
					System.out.println("AllowedLayers:"+allowedLayers.mkString(", "))
					lineNumber=0

					def readLineCount:String= {
						val res=reader.readLine
						lineNumber+=1
						if(res!=null)
							bytesRead+=res.length+2l
            val progress={ val prog=(bytesRead*100/fileSize).toInt
                           if(prog>100) 100
                           else prog
            }

						if(progressListener(progress)) {
							//println("Import cancelled")
							throw new Exception("Cancel")
						}
						if(res==null) { Log.e("Line "+lineNumber+"DXF read null");""} else res
					}

					def getBuffer(layerName:String)= bufferMap.getOrElseUpdate(layerName,{
            //println("create buffer"+layerName)
					  val newLayer=ClientQueryManager.createInstance(baseObject.typ,ownerOwner)
					  val newLayerRef=new Reference(baseObject.typ,newLayer)
					  ClientQueryManager.writeInstanceField(newLayerRef, 1, new StringConstant(layerName.replace('_',' ')))
					  ClientQueryManager.writeInstanceField(newLayerRef,2.toByte,new IntConstant(set.drawingScaleID))
					  new CreateInstancesBuffer(Array(new OwnerReference(0,newLayerRef)),20)
					})

					def addElem(layerName:String,typ:Int,fields:Array[Expression])=
					  if(allowedLayers.contains(layerName))
					    getBuffer(layerName).addInstance(typ, fields)

					try {
						while (section!="ENTITIES") {
							/*while(code.trim.toInt!=0 &&value!="SECTION"){
								code=readLineCount
								value=readLineCount
							}*/
							code=readLineCount
							section=readLineCount
							if(encoding==null && section.equals("$DWGCODEPAGE")){
                readLineCount
								encoding=readLineCount
								println("Encoding "+encoding)
							}
						}
						println("Section:"+section+" code:"+code)
            if(encoding==null)encoding="UTF-8"
						var breakit=false
						val fieldBuffer=new FieldBuffer(set)

						var currentElemTyp=""
						while(!breakit) {
								code=readLineCount
							  val fieldValue=readLineCount
							  if(code=="  0") {
									if(fieldValue=="ENDSEC") breakit=true
									val layerName=fieldBuffer.getString(8,"no layer")
									val color=AcadColor.getColor(fieldBuffer.getInt(62))
									val thickness=fieldBuffer.getInt(370)
									val lineStyle=set.lineStyleMapping.getOrElse(fieldBuffer.getString(6,"0"),0)
									currentElemTyp match {
										case "LINE"=>
											val lineFields=new Array[Expression](5)
											lineFields(0)=new IntConstant(color)
											lineFields(1)=new IntConstant(thickness)
											lineFields(2)=new IntConstant(lineStyle)
											lineFields(3)=fieldBuffer.pointBuffer.head
											lineFields(4)=new VectorConstant(fieldBuffer.getDouble(11)+set.dx,fieldBuffer.getDouble(21)+set.dy,0)
											addElem(layerName,lineTyp,lineFields)

										case "LWPOLYLINE" =>
											val colorf=new IntConstant(color)
											val thickf=new IntConstant(thickness)
											val stylef=new IntConstant(lineStyle)
											//println("lwpoly line points:"+fieldBuffer.pointBuffer.mkString("|"))
											for(numLine<-0 until fieldBuffer.pointBuffer.size-1) {
												val lineFields=new Array[Expression](5)
												lineFields(0)=colorf
												lineFields(1)=thickf
												lineFields(2)=stylef
												lineFields(3)=fieldBuffer.pointBuffer(numLine)
												lineFields(4)=fieldBuffer.pointBuffer(numLine+1)
												addElem(layerName,lineTyp,lineFields)
											}
											val closed=fieldBuffer.getInt(70)==1
											if(closed) {
												val lineFields=new Array[Expression](5)
												lineFields(0)=colorf
												lineFields(1)=thickf
												lineFields(2)=stylef
												lineFields(3)=fieldBuffer.pointBuffer.last
												lineFields(4)=fieldBuffer.pointBuffer.head
												addElem(layerName,lineTyp,lineFields)
											}

										case "ARC" =>
											val dia=fieldBuffer.getDouble(40)
											val sA=fieldBuffer.getDouble(50) % 360
											val eA=fieldBuffer.getDouble(51) % 360
											val arcFields=new Array[Expression](7)
											arcFields(0)=new IntConstant(AcadColor.getColor(color))
											arcFields(1)=new IntConstant(thickness)
											arcFields(2)=new IntConstant(lineStyle)
											arcFields(3)=fieldBuffer.pointBuffer.head
											arcFields(4)=new DoubleConstant(dia)
											arcFields(5)=new DoubleConstant(sA)
											arcFields(6)=new DoubleConstant(eA)
											addElem(layerName,arcTyp,arcFields)

										case "CIRCLE" =>
											val dia=fieldBuffer.getDouble(40)
											val arcFields=new Array[Expression](7)
											arcFields(0)=new IntConstant(AcadColor.getColor(color))
											arcFields(1)=new IntConstant(thickness)
											arcFields(2)=new IntConstant(lineStyle)
											arcFields(3)=fieldBuffer.pointBuffer.head
											arcFields(4)=new DoubleConstant(dia)
											arcFields(5)=new DoubleConstant(0d)
											arcFields(6)=new DoubleConstant(360d)
											addElem(layerName,arcTyp,arcFields)

										case "ELLIPSE" =>
											val ex=fieldBuffer.getDouble(11)
											val ey=fieldBuffer.getDouble(21)
											val z=fieldBuffer.getDouble(30)
											val r2sc=fieldBuffer.getDouble(40)
											val sA=fieldBuffer.getDouble(41) % VectorConstant.PI2
											val eA=fieldBuffer.getDouble(42) % VectorConstant.PI2
											val mainAngle=math.atan2(ey,ex)*180d/math.Pi
											val r1=math.sqrt(ex*ex+ey*ey)
											val ellFields=new Array[Expression](9)
											ellFields(0)=new IntConstant(AcadColor.getColor(color))
											ellFields(1)=new IntConstant(thickness)
											ellFields(2)=new IntConstant(lineStyle)
											ellFields(3)=fieldBuffer.pointBuffer.head
											ellFields(4)=new DoubleConstant(r1)
											val r2=r1*r2sc
											ellFields(5)=new DoubleConstant(r2)
											ellFields(6)=new DoubleConstant(mainAngle)
											ellFields(7)=new DoubleConstant(getOuterAngle(sA,r1,r2)*180d/math.Pi)
											ellFields(8)=new DoubleConstant(getOuterAngle(eA,r1,r2)*180d/math.Pi)
											addElem(layerName,ellTyp,ellFields)

										case "HATCH" =>
											val patternName=fieldBuffer.getString(2)
											val angle=fieldBuffer.getDouble(52)
											val solid=fieldBuffer.getInt(70)==1
											val polyFields=new Array[Expression](8)
											polyFields(0)=new IntConstant(if(solid) AcadColor.getColor(color) else -1)
											polyFields(1)=EMPTY_EX
											polyFields(2)=EMPTY_EX
											polyFields(3)=new Polygon(Nil,fieldBuffer.polyBuffer.toSeq)
											polyFields(4)=new IntConstant(AcadColor.getColor(color))
											polyFields(5)=if(solid) EMPTY_EX else new IntConstant(set.hatchStyleMapping(patternName))
											polyFields(6)=EMPTY_EX
											polyFields(7)=new DoubleConstant(angle)
											addElem(layerName,polyTyp,polyFields)

										case "TEXT" =>
											val height=fieldBuffer.getDouble(40)
											val textAngle=fieldBuffer.getDouble(50)
											val obligeAngle=fieldBuffer.getDouble(51)
											val text= SpecialMatch.replaceAllIn(fieldBuffer.getString(1),(el)=>{
												val st=el.group(0);
												new String(Array[Byte](st.substring(2,st.length).toInt.toByte),encoding)
											})
											val xfactor=fieldBuffer.getDouble(41)
											val font=FontHandler.getFont(fieldBuffer.getString(7))
											val halign=fieldBuffer.getInt(72)
											val valign=fieldBuffer.getInt(73)
											val firstPoint =fieldBuffer.pointBuffer.head
											val p2x=fieldBuffer.fieldsMap.getOrElse(11,(firstPoint.x-set.dx).toString).toDouble+set.dx
											val p2y=fieldBuffer.fieldsMap.getOrElse(21,(firstPoint.y-set.dy).toString).toDouble+set.dy
											val textFields=new Array[Expression](10)
											val radAngle=textAngle*math.Pi/180d
											val textYDelta=height*set.textYAdjust
											textFields(0)=new IntConstant(AcadColor.getColor(color))
											textFields(1)=new StringConstant(text)
											textFields(2)=if((halign==5)&&valign==0) new VectorConstant(firstPoint.x-math.sin(radAngle)*textYDelta,firstPoint.y+math.cos(radAngle)*textYDelta,0)
											else new VectorConstant(p2x-math.sin(radAngle)*textYDelta,p2y+math.cos(radAngle)*textYDelta,0)
											textFields(3)=new StringConstant(if(font==null)FontHandler.fontList.head.name else font)
											textFields(4)=new DoubleConstant(height*1000d/set.drawingScale*set.fontScale)
											textFields(5)=new DoubleConstant(xfactor)
											textFields(6)=new IntConstant(if(halign==5&&valign==0) 0
											else(halign match {
												case 1=>8
												case 2=>16
												case _=>0
											}) + valign match {
												case 2=>1
												case 3=>2
												case _=>0
											})
											textFields(7)=new DoubleConstant(textAngle)
											textFields(8)=new DoubleConstant(obligeAngle)
											textFields(9)=EMPTY_EX
											addElem(layerName,textTyp,textFields)
										case "" => // ignore
										case other=> println("Line "+lineNumber+" unknown Element Type "+other)
									}
									fieldBuffer.clear()
									currentElemTyp=fieldValue

								} else {
									fieldBuffer.fill(code,fieldValue)
								}

								}// while
					} catch {
						case e:Exception=> if(e.getMessage()!="Cancel") Log.e("DXF Import line:"+lineNumber+" ",e)
					}

					finally {
            //println("Storing "+bufferMap.keys.mkString)
					  bufferMap.values.foreach(_.flush())
						bufferMap.clear()
						reader.close()
					}
					//val endTime=System.currentTimeMillis()
					//System.out.println("Import ready "+file.getName+" time:"+(endTime-startTime))
					true
				case _=>  false 
		}

		def getOuterAngle(innerAngle:Double,r1:Double,r2:Double)= {

			def getOuterAngleFirstQuadrant(innerAngle:Double)= math.atan(math.tan(innerAngle)*r2/r1)
			if(innerAngle>math.Pi/2 ) {
				if(innerAngle>PI_32) getOuterAngleFirstQuadrant(innerAngle-math.Pi*2)+math.Pi*2 else getOuterAngleFirstQuadrant(innerAngle-math.Pi)+math.Pi 
			}
			else if(innerAngle< -math.Pi/2) { 
				if(innerAngle< -PI_32) getOuterAngleFirstQuadrant(innerAngle+math.Pi*2)-math.Pi*2  else  getOuterAngleFirstQuadrant(innerAngle+math.Pi)-math.Pi 
			}
			else getOuterAngleFirstQuadrant(innerAngle)
		}
}