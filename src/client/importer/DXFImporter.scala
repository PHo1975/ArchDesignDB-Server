package client.importer
import client.comm.{ClientQueryManager, CreateInstancesBuffer}
import client.dialog.symbolbrowser.SymbolBrowserController
import client.graphicsView.Handlers._
import client.graphicsView.{AcadColor, FontHandler, HatchHandler}
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression._
import definition.typ.AllClasses
import runtime.function.TypeInfos
import util.Log

import java.awt.Point
import java.io._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.Window
import scala.util.control.NonFatal
import scala.util.matching.Regex


class FieldBuffer(set:DXFSettings){
	val fieldsMap: mutable.HashMap[Int, String] = collection.mutable.HashMap[Int, String]()
	//val xBuffer=ArrayBuffer[Double]()
	//val yBuffer=ArrayBuffer[Double]()
	val pointBuffer: ArrayBuffer[VectorConstant] = ArrayBuffer[VectorConstant]()
	val polyBuffer: ArrayBuffer[PointList] = ArrayBuffer[PointList]()
	var lastX=0d

	def getDouble(f: Int): Double = fieldsMap.getOrElse(f, "0").trim.toDouble

	def getString(f: Int, default: String = ""): String = fieldsMap.getOrElse(f, default)

	def getInt(f: Int): Int = fieldsMap.getOrElse(f, "0").trim.toInt

	def clear(): Unit = {
		fieldsMap.clear()
		pointBuffer.clear()
		polyBuffer.clear()
	}

	def fill(code: String, fieldValue: String): Unit = {
		code.trim.toInt match {
			case 10 => lastX=fieldValue.trim.toDouble*set.globalScale+set.dx
			case 20 => pointBuffer+=new VectorConstant(lastX,fieldValue.trim.toDouble*set.globalScale+set.dy,0d)//;println("add Point: "+pointBuffer.last)
			case 93|98 => if(pointBuffer.nonEmpty){
				polyBuffer += PointList(pointBuffer.toSeq)
				pointBuffer.clear()
			}
			case 0 => Log.e("Code 0 in fill, value: "+fieldValue)
			case fieldNr => fieldsMap(fieldNr) = fieldValue
		}
	}
}

case class EntityTemplate(typ:Int,fields:Array[Expression])

case class BlockData(name:String,blockType:Int,basePoint:VectorConstant){
	val templateList=ArrayBuffer[EntityTemplate]()

	override def toString:String="Block "+name+" typ:"+blockType+" base:"+basePoint+"\n  "
}

class DXFImporter extends FileImportDescriptor {
	val name="Zeichnungen"
	val PI_32: Double = math.Pi * 3 / 2
	val dbNameField: Byte = 1.toByte
	val STANDARD_LAYER = "Standard"
	val bufferMap: mutable.HashMap[String, CreateInstancesBuffer] = collection.mutable.HashMap[String, CreateInstancesBuffer]()
	var lineNumber = 0
	val blockBuffer = collection.mutable.HashMap[String,BlockData]()

	lazy val lineTyp: Int = AllClasses.get.getClassIDByName("LineElem")
	lazy val arcTyp: Int = AllClasses.get.getClassIDByName("ArcElem")
	lazy val ellTyp: Int = AllClasses.get.getClassIDByName("EllipseElem")
	lazy val polyTyp: Int = AllClasses.get.getClassIDByName("PolyElem")
	lazy val textTyp: Int = AllClasses.get.getClassIDByName("TextElem")
	lazy val symbRefTyp: Int = AllClasses.get.getClassIDByName("SymbolElem")
	lazy val lineStyleType: Int = AllClasses.get.getClassIDByName("LinienStil")
	lazy val hatchStyleType: Int = AllClasses.get.getClassIDByName("Schraffur")

	lazy val symbolImportFolder:Option[Reference]= {
		SymbolBrowserController.symbolRootFolder.map(rf=>{
			ClientQueryManager.queryInstance(rf.ref,1).find(_.fieldValue(0).toString=="Import") match {
				case Some(data)=> data.ref
					case None =>
						println("Create Import Folder")
						val newInst=Reference(110,ClientQueryManager.createInstance(110,Array(OwnerReference(1,rf.ref))))
						ClientQueryManager.writeInstanceField(newInst,0,StringConstant("Import"))
					  newInst
			}
		} )
	}

	lazy val symbolMap: mutable.Map[String, Reference] = {
		val smap =collection.mutable.HashMap[String,Reference]()
		for(importFolder<-symbolImportFolder)
		for(data <-ClientQueryManager.queryInstance(importFolder,1))
		  smap(data.fieldValue(0).toString)=data.ref
		smap
	}

	def allowedFileTypes: Seq[(String, String)] = Seq(("DXF-Zeichnung", "DXF"))

	def canImport(files: Iterable[File], droppedTarget: Option[InstanceData], ownerRef: OwnerReference): Boolean = {
		//println("Can import "+files+" DT:"+droppedTarget+" or:"+ownerRef+" hr:"+ hasRightFileTypes(files))
		hasRightFileTypes(files)
	}

	def showImportDialog(window: Window, wpos: Point, files: Iterable[File], dropTarget: Option[InstanceData]): Seq[AnyRef] = {
		val settings = new DXFSettings
		settings.drawingScale = 50
		val dialog = new DXFImportDialog(window, settings, files)
		if (dialog.showDialog(wpos)) Seq(settings)
		else Seq.empty
	}

	def createMissingLineStyles(settings: DXFSettings): Unit = {
		for (folder <- LineStyleHandler.folderRef) {
			var lastIx = LineStyleHandler.styles.last.ix
			val ownerRef = new OwnerReference(1, folder)

			val lineData = for (nls <- settings.unknownLineStyles)
				yield (lineStyleType, Array[Expression](StringConstant(nls.name),
					StringConstant(nls.dots.mkString(";")), EMPTY_EX))
			ClientQueryManager.createInstances(Array(ownerRef), lineData)
			// reload
			LineStyleHandler.loadSettings(folder)
			for (nls <- settings.unknownLineStyles) {
				lastIx += 1
				settings.lineStyleMapping += ((nls.name, lastIx))
			}
			settings.unknownLineStyles.clear()
		}
	}

	def createMissingHatches(settings: DXFSettings): Unit = {
		for (folder <- HatchHandler.folderRef) {
			var lastIx = HatchHandler.hatchList.last.ix
			val ownerRef = new OwnerReference(1, folder)
			val hatchData = for (nhs <- settings.unknownHatchStyles)
				yield (hatchStyleType, nhs match {
					case uk: HatchStylewithUnknownLS =>
						val ls1 = uk.lsName1 match {case Some(lname) => settings.lineStyleMapping(lname); case None => uk.lineStyle1}
						val ls2 = uk.lsName2 match {case Some(lname) => settings.lineStyleMapping(lname); case None => uk.lineStyle2}
						Array[Expression](StringConstant(name), new DoubleConstant(uk.angle1), IntConstant(ls1),
							new DoubleConstant(uk.distance1), new DoubleConstant(uk.angle2), IntConstant(ls2), new DoubleConstant(uk.distance2),
							new DoubleConstant(uk.thickness))
					case o => o.toExpression
				})
			ClientQueryManager.createInstances(Array(ownerRef), hatchData)
			HatchHandler.loadSettings(folder)
			for (nhs <- settings.unknownHatchStyles) {
				lastIx += 1
				settings.hatchStyleMapping += ((nhs.name, lastIx))
			}
			settings.unknownHatchStyles.clear()
		}
	}


	def checkEncoding(file:File):String= {
		val encodingReader=new BufferedReader(new FileReader(file))
		var encoding:String=null
		while (encoding==null&&encodingReader.ready()){
			val code=encodingReader.readLine()
			val data=encodingReader.readLine()
			if(data.equals("$DWGCODEPAGE")){
				encodingReader.readLine()
				encoding=encodingReader.readLine()
			}
		}
		encodingReader.close()
		encoding
	}


	def getSymbolfromBlock(blockName:String,set:DXFSettings):Option[Reference]=
		if(blockBuffer.contains(blockName))
			symbolImportFolder map( importFolder=> {
				symbolMap.getOrElseUpdate(blockName,{
					val block=blockBuffer(blockName)
					val iOwner=Array(new OwnerReference(1,importFolder))
					val symbolInst=ClientQueryManager.createInstance(TypeInfos.symbolDefType,iOwner)
					val symbRef=Reference(TypeInfos.symbolDefType,symbolInst)
					println("Create Symbol "+blockName+" "+symbRef)
					ClientQueryManager.writeInstanceField(symbRef, 0, StringConstant(blockName))
					val sowner=Array(new OwnerReference(0,symbRef))
					def applyPoint(list:Array[Expression],fieldNr:Int):Unit={
						val oldValue=list(fieldNr).getValue.toVector
						list(fieldNr)=oldValue-block.basePoint
					}

					for(el<-block.templateList) {
						el.typ match {
							case 40 =>
								val fields=el.fields.clone()
								applyPoint(fields,3)
								applyPoint(fields,4)

							case 41=>
								val fields=el.fields.clone()
								applyPoint(fields,3)
							case 42 =>
								val fields=el.fields.clone()
								fields(3)=fields(3).getValue.toPolygon.transform(p=>{
									p-block.basePoint
								})
							case 43 =>
								val fields=el.fields.clone()
								applyPoint(fields,3)

							case 44 =>
								val fields=el.fields.clone()
								applyPoint(fields,2)
								fields(4)=DoubleConstant(fields(4).getValue.toDouble/1000d)
							case _ =>
						}
					}
					ClientQueryManager.createInstances(sowner,block.templateList.map(entity=>(entity.typ,entity.fields)))
					symbRef
				})
			}	)
		else {Log.e("Block with name "+blockName+" not found");None }


	def importFile(file: File, baseObject: Reference, settings: Seq[AnyRef], progressListener: Int => Boolean): Boolean =
		settings match {
			case Seq(set: DXFSettings) =>
				createMissingLineStyles(set)
				createMissingHatches(set)
				ClientQueryManager.writeInstanceField(baseObject, 2.toByte, IntConstant(set.drawingScaleID))
				println("Import File:"+file)
				//val startTime=System.currentTimeMillis()
				val encoding: String = checkEncoding(file) match {
					case null => "UTF-8"
						case "ANSI_1252" => "Cp1252"
					case o=> o
				}
        println("Encoding: "+encoding+" global Scale:"+set.globalScale)
				try{
				val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file),encoding))
				val owner = Array(new OwnerReference(0, baseObject))
				val ownerInst = ClientQueryManager.queryInstance(baseObject, -1).head
				val ownerOwner: Array[OwnerReference] = ownerInst.owners
				val standardBuffer = new CreateInstancesBuffer(owner, 20)
				blockBuffer.clear()
				bufferMap.clear()
				bufferMap(STANDARD_LAYER) = standardBuffer
				val fileSize = file.length
				var code = "0"
				var section = ""
				var bytesRead: Long = 0

				val SpecialMatch = "%%(\\d+)".r

				val allowedLayers = {
					val res: Seq[String] = set.selectedLayers map (set.layers(_))
					if (res.isEmpty) Seq("0") else res
				}
				System.out.println("AllowedLayers:" + allowedLayers.mkString(", "))
				lineNumber = 0

				def readLineCount: String = {
					val res = reader.readLine
					lineNumber += 1
					if (res != null) {
						bytesRead += res.length + 2L
						val progress = {
							val prog = (bytesRead * 100 / fileSize).toInt
							if (prog > 100) 100 else prog
						}
						if (progressListener(progress)) {
							Log.w("Import Cancelled")
							throw new Exception("Cancel")
						}
						res
					} else throw new IllegalArgumentException("End of File at Line NR "+lineNumber)
				}

				def readBlock():Unit ={
					var name=""
					var x=0d
					var y=0d
					var z=0d
					var blType=0
					while (code!="  1"){
						code = readLineCount
						section = readLineCount
						code.trim.toInt match {
							case 2 => name=section;println("read Block "+name)
							case 10 => x=section.trim.toDouble*set.globalScale+ set.dx
							case 20 => y=	section.trim.toDouble*set.globalScale+ set.dy
							case 30 => z=section.trim.toDouble*set.globalScale
							case 70 =>blType=section.trim.toInt
								case _ =>
						}
					}
					val currentBlock=BlockData(name,blType,new VectorConstant(x,y,z))
					loopEntities("ENDBLK",(_,entityType,fields)=>{
						currentBlock.templateList += EntityTemplate(entityType,fields)
					})
					blockBuffer(name)= currentBlock
					println("Block read "+name+" numEntities:"+currentBlock.templateList.length+"\n")
				}

				def getBuffer(layerName: String): CreateInstancesBuffer = bufferMap.getOrElseUpdate(layerName, {
					println("create buffer LayerName:"+layerName)
					val newLayer = ClientQueryManager.createInstance(baseObject.typ, ownerOwner)
					val newLayerRef = new Reference(baseObject.typ, newLayer)
					ClientQueryManager.writeInstanceField(newLayerRef, 1, StringConstant(layerName.replace('_', ' ')))
					ClientQueryManager.writeInstanceField(newLayerRef, 2.toByte, IntConstant(set.drawingScaleID))
					new CreateInstancesBuffer(Array(new OwnerReference(0, newLayerRef)), 20)
				})

				def addElem(layerName: String, typ: Int, fields: Array[Expression]): Unit = {
					//println("add "+layerName+" "+typ+" "+fields.mkString(","))
					if (allowedLayers.contains(layerName))
						getBuffer(layerName).addInstance(typ, fields)
				}



				def loopEntities(endValue:String,receiver: (String,Int,Array[Expression]) => Unit):Unit= {
					var breakit = false
					val fieldBuffer = new FieldBuffer(set)

					var currentElemTyp = ""
					while (!breakit) {
						code = readLineCount
						val fieldValue = readLineCount
						if (code == "  0") {
							if (fieldValue == endValue) breakit = true
							val layerName = fieldBuffer.getString(8, "no layer")
							val color = AcadColor.getColor(fieldBuffer.getInt(62))
							val thickness = fieldBuffer.getInt(370) match {
								case -1 => 10
								case other => other
							}
							val lineStyle = set.lineStyleMapping.getOrElse(fieldBuffer.getString(6, "0"), 0)
							currentElemTyp match {
								case "LINE" =>
									val lineFields = new Array[Expression](5)
									lineFields(0) = IntConstant(color)
									lineFields(1) = IntConstant(thickness)
									lineFields(2) = IntConstant(lineStyle)
									lineFields(3) = fieldBuffer.pointBuffer.head
									lineFields(4) = new VectorConstant(fieldBuffer.getDouble(11)*set.globalScale + set.dx, fieldBuffer.getDouble(21)*set.globalScale + set.dy, 0)
									receiver(layerName, lineTyp, lineFields)

								case "LWPOLYLINE" =>
									val colorf = IntConstant(color)
									val thickf = IntConstant(thickness)
									val stylef = IntConstant(lineStyle)
									//println("lwpoly line points:"+fieldBuffer.pointBuffer.mkString("|"))
									for (numLine <- 0 until fieldBuffer.pointBuffer.size - 1) {
										val lineFields = new Array[Expression](5)
										lineFields(0) = colorf
										lineFields(1) = thickf
										lineFields(2) = stylef
										lineFields(3) = fieldBuffer.pointBuffer(numLine)
										lineFields(4) = fieldBuffer.pointBuffer(numLine + 1)
										receiver(layerName, lineTyp, lineFields)
									}
									val closed = fieldBuffer.getInt(70) == 1
									if (closed) {
										val lineFields = new Array[Expression](5)
										lineFields(0) = colorf
										lineFields(1) = thickf
										lineFields(2) = stylef
										lineFields(3) = fieldBuffer.pointBuffer.last
										lineFields(4) = fieldBuffer.pointBuffer.head
										receiver(layerName, lineTyp, lineFields)
									}

								case "ARC" =>
									val dia = fieldBuffer.getDouble(40)*set.globalScale
									val sA = fieldBuffer.getDouble(50) % 360
									val eA = fieldBuffer.getDouble(51) % 360
									val arcFields = new Array[Expression](7)
									arcFields(0) = IntConstant(AcadColor.getColor(color))
									arcFields(1) = IntConstant(thickness)
									arcFields(2) = IntConstant(lineStyle)
									arcFields(3) = fieldBuffer.pointBuffer.head
									arcFields(4) = new DoubleConstant(dia)
									arcFields(5) = new DoubleConstant(sA)
									arcFields(6) = new DoubleConstant(eA)
									receiver(layerName, arcTyp, arcFields)

								case "CIRCLE" =>
									val dia = fieldBuffer.getDouble(40)*set.globalScale
									val arcFields = new Array[Expression](7)
									arcFields(0) = IntConstant(AcadColor.getColor(color))
									arcFields(1) = IntConstant(thickness)
									arcFields(2) = IntConstant(lineStyle)
									arcFields(3) = fieldBuffer.pointBuffer.head
									arcFields(4) = new DoubleConstant(dia)
									arcFields(5) = new DoubleConstant(0d)
									arcFields(6) = new DoubleConstant(360d)
									receiver(layerName, arcTyp, arcFields)

								case "ELLIPSE" =>
									val ex = fieldBuffer.getDouble(11)*set.globalScale
									val ey = fieldBuffer.getDouble(21)*set.globalScale
									val z = fieldBuffer.getDouble(30)
									val r2sc = fieldBuffer.getDouble(40)
									val sA = fieldBuffer.getDouble(41) % VectorConstant.PI2
									val eA = fieldBuffer.getDouble(42) % VectorConstant.PI2
									val mainAngle = math.atan2(ey, ex) * 180d / math.Pi
									val r1 = math.sqrt(ex * ex + ey * ey)
									val ellFields = new Array[Expression](9)
									ellFields(0) = IntConstant(AcadColor.getColor(color))
									ellFields(1) = IntConstant(thickness)
									ellFields(2) = IntConstant(lineStyle)
									ellFields(3) = fieldBuffer.pointBuffer.head
									ellFields(4) = new DoubleConstant(r1)
									val r2 = r1 * r2sc
									ellFields(5) = new DoubleConstant(r2)
									ellFields(6) = new DoubleConstant(mainAngle)
									ellFields(7) = new DoubleConstant(getOuterAngle(sA, r1, r2) * 180d / math.Pi)
									ellFields(8) = new DoubleConstant(getOuterAngle(eA, r1, r2) * 180d / math.Pi)
									receiver(layerName, ellTyp, ellFields)

								case "HATCH" =>
									val patternName = fieldBuffer.getString(2)
									val angle = fieldBuffer.getDouble(52)
									val solid = fieldBuffer.getInt(70) == 1
									val polyFields = new Array[Expression](8)
									polyFields(0) = IntConstant(if (solid) AcadColor.getColor(color) else -1)
									polyFields(1) = EMPTY_EX
									polyFields(2) = EMPTY_EX
									polyFields(3) = new Polygon(Nil, fieldBuffer.polyBuffer.toSeq)
									polyFields(4) = IntConstant(AcadColor.getColor(color))
									polyFields(5) = if (solid) EMPTY_EX else if (set.hatchStyleMapping.contains(patternName)) IntConstant(set.hatchStyleMapping(patternName)) else EMPTY_EX
									polyFields(6) = EMPTY_EX
									polyFields(7) = new DoubleConstant(angle)
									receiver(layerName, polyTyp, polyFields)

								case "TEXT" =>
									val height = fieldBuffer.getDouble(40)*set.globalScale
									val textAngle = fieldBuffer.getDouble(50)
									val obligeAngle = fieldBuffer.getDouble(51)
									val text = SpecialMatch.replaceAllIn(fieldBuffer.getString(1), (el: Regex.Match) => {
										val st: String = el.group(0)
										new String(Array[Byte](st.substring(2, st.length).toInt.toByte), encoding)
									})
									val xfactor = fieldBuffer.getDouble(41)
									val font = FontHandler.getFont(fieldBuffer.getString(7))
									val halign = fieldBuffer.getInt(72)
									val valign = fieldBuffer.getInt(73)
									val firstPoint = fieldBuffer.pointBuffer.head
									val p2x = fieldBuffer.fieldsMap.getOrElse(11, (((firstPoint.x - set.dx))/set.globalScale).toString).toDouble*set.globalScale + set.dx
									val p2y = fieldBuffer.fieldsMap.getOrElse(21, ((firstPoint.y - set.dy)/set.globalScale).toString).toDouble*set.globalScale + set.dy
									val textFields = new Array[Expression](10)
									val radAngle = textAngle * math.Pi / 180d
									val textYDelta = height * set.textYAdjust
									textFields(0) = IntConstant(AcadColor.getColor(color))
									textFields(1) = StringConstant(text)
									textFields(2) = if ((halign == 5) && valign == 0) new VectorConstant(firstPoint.x - math.sin(radAngle) * textYDelta, firstPoint.y + math.cos(radAngle) * textYDelta, 0)
									else new VectorConstant(p2x - math.sin(radAngle) * textYDelta, p2y + math.cos(radAngle) * textYDelta, 0)
									textFields(3) = StringConstant(if (font == null) FontHandler.fontList.head.name else font)
									textFields(4) = new DoubleConstant(height * 1000d / set.drawingScale * set.fontScale)
									textFields(5) = new DoubleConstant(xfactor)
									textFields(6) = IntConstant(if (halign == 5 && valign == 0) 0
									else (halign match {
										case 1 => 8
										case 2 => 16
										case _ => 0
									}) + valign match {
										case 2 => 1
										case 3 => 2
										case _ => 0
									})
									textFields(7) = new DoubleConstant(textAngle)
									textFields(8) = new DoubleConstant(obligeAngle)
									textFields(9) = EMPTY_EX
									receiver(layerName, textTyp, textFields)

								case "MTEXT" =>
									val height = fieldBuffer.getDouble(40)*set.globalScale
									val textAngle = fieldBuffer.getDouble(50)
									val obligeAngle = fieldBuffer.getDouble(51)
									val texts: Array[String] = SpecialMatch.replaceAllIn(fieldBuffer.getString(1), (el: Regex.Match) => {
										val st: String = el.group(0)
										new String(Array[Byte](st.substring(2, st.length).toInt.toByte), encoding)
									}).split("\\\\P")
									val xfactor = fieldBuffer.getDouble(41)
									val font = FontHandler.getFont(fieldBuffer.getString(7))
									val halign = fieldBuffer.getInt(72)

									val firstPoint = fieldBuffer.pointBuffer.head
									val p2x = fieldBuffer.fieldsMap.getOrElse(11, (firstPoint.x - set.dx).toString).toDouble + set.dx
									val p2y = fieldBuffer.fieldsMap.getOrElse(21, (firstPoint.y - set.dy).toString).toDouble + set.dy
									val textFields = new Array[Expression](10)
									val radAngle = textAngle * math.Pi / 180d
									val textYDelta = -height * set.textYAdjust
									textFields(0) = IntConstant(AcadColor.getColor(color))
									textFields(3) = StringConstant(if (font == null) FontHandler.fontList.head.name else font)
									textFields(4) = new DoubleConstant(height * 1000d / set.drawingScale * set.fontScale)
									textFields(5) = new DoubleConstant(xfactor)
									textFields(6) = IntConstant( halign match {
										case 2 => 8
											case 5=> 8
											case 8=>8
										case 3 => 16
											case 6=>16
											case 9=>16
										case _ => 0}
									)
									textFields(7) = new DoubleConstant(textAngle)
									textFields(8) = new DoubleConstant(obligeAngle)
									textFields(9) = EMPTY_EX
									for(lineNr<-texts.indices;text=texts(lineNr)) {
										val ntf=textFields.clone()
										ntf(1) = StringConstant(text)
										ntf(2) = new VectorConstant(p2x - math.sin(radAngle) * textYDelta*lineNr, p2y + math.cos(radAngle) * textYDelta * lineNr, 0)
										receiver(layerName, textTyp, ntf)
									}

								case "INSERT" => if(allowedLayers.contains(layerName)||endValue=="ENDBLK") {
									val blockName = fieldBuffer.getString(2)
									val pos = fieldBuffer.pointBuffer.head
									val rotation = fieldBuffer.getDouble(50)
									val rotRad = rotation * Math.PI / 180.0
									val cosa = math.cos(rotRad)
									val sina = math.sin(rotRad)

									if (set.explodeBlocks||endValue=="ENDBLK") {
										if (blockBuffer.contains(blockName)) {
											println("Explode '" + blockName + "' x:" + pos.x + " y:" + pos.y + " rotation:" + rotation)
											val block = blockBuffer(blockName)

											def applyPoint(list: Array[Expression], fieldNr: Int): Unit = {
												val oldValue = list(fieldNr).getValue.toVector
												val delta = oldValue - block.basePoint
												list(fieldNr) = new VectorConstant(pos.x + delta.x * cosa - delta.y * sina, pos.y + delta.x * sina + delta.y * cosa, 0)
											}

											for (el <- block.templateList) {
												el.typ match {
													case 40 =>
														val fields = el.fields.clone()
														applyPoint(fields, 3)
														applyPoint(fields, 4)
														receiver(layerName, el.typ, fields)
													case 41 =>
														val fields = el.fields.clone()
														applyPoint(fields, 3)
														fields(5) = new DoubleConstant(fields(5).getValue.toDouble + rotation)
														fields(6) = new DoubleConstant(fields(6).getValue.toDouble + rotation)
														receiver(layerName, el.typ, fields)
													case 42 =>
														val fields = el.fields.clone()
														fields(3) = fields(3).getValue.toPolygon.transform(p => {
															val delta = p - block.basePoint
															new VectorConstant(pos.x + delta.x * cosa - delta.y * sina, pos.y + delta.x * sina + delta.y * cosa, 0)
														})
														receiver(layerName, el.typ, fields)
													case 43 =>
														val fields = el.fields.clone()
														applyPoint(fields, 3)
														fields(6) = new DoubleConstant(fields(6).getValue.toDouble + rotation)
														receiver(layerName, el.typ, fields)
													case 44 =>
														val fields = el.fields.clone()
														applyPoint(fields, 2)
														fields(7) = new DoubleConstant(fields(7).getValue.toDouble + rotation)
														receiver(layerName, el.typ, fields)
													case o => println("Unknown block element type:"+o)
												}
											}
										} else Log.e("Block " + blockName + " not found")
									} else { // use symbols
										getSymbolfromBlock(blockName, set) match {
											case Some(symbolRef) =>
												println("Insert '" + blockName + "' x:" + pos.x + " y:" + pos.y + " rotation:" + rotation + " as:" + symbolRef)
												val fields = new Array[Expression](6)
												fields(0) = EMPTY_EX
												fields(1) = new ObjectReference(symbolRef)
												fields(2) = DoubleConstant(rotation)
												fields(3) = DoubleConstant(1)
												fields(4) = EMPTY_EX
												fields(5) = pos
												receiver(layerName, symbRefTyp, fields)
											case None => println("Try Insert " + blockName + " but symbol not found")
										}
									}
								}
								case "" => // ignore
								case other => Log.w("Line " + lineNumber + " unknown Element Type " + other)
							}
							fieldBuffer.clear()
							currentElemTyp = fieldValue

						} else {
							fieldBuffer.fill(code, fieldValue)
						}

					}
				}

				try {
					while (section != "BLOCKS") {
						code = readLineCount
						section = readLineCount
					}

					Log.w("Section:" + section + " code:" + code)
					while (section != "ENTITIES") {
						code = readLineCount
						section = readLineCount
						if((code=="  0") && (section=="BLOCK")) readBlock()
					}
					println("Blocklist:\n"+blockBuffer.values.mkString("\n")+"\n")
					Log.w("Section:" + section + " code:" + code)

					loopEntities("ENDSEC",(addElem ))


				} catch {
					case e: Exception => if (e.getMessage() != "Cancel") Log.e("DXF Import line:" + lineNumber + " ", e)
				}

				finally {
					println("Storing "+bufferMap.keys.mkString(" | "))
					bufferMap.values.foreach(_.flush())
					bufferMap.clear()
					reader.close()
					Log.w("import done, " + lineNumber + " lines read.")
				}
				//val endTime=System.currentTimeMillis()
				//System.out.println("Import ready "+file.getName+" time:"+(endTime-startTime))
				true
				} catch {
					case NonFatal(e)=> Log.e("Import "+file+" "+e);false
				}
			case _ => false
		}

	def getOuterAngle(innerAngle: Double, r1: Double, r2: Double): Double = {

		def getOuterAngleFirstQuadrant(innerAngle: Double): Double = math.atan(math.tan(innerAngle) * r2 / r1)

		if (innerAngle > math.Pi / 2) {
			if (innerAngle > PI_32) getOuterAngleFirstQuadrant(innerAngle - math.Pi * 2) + math.Pi * 2 else getOuterAngleFirstQuadrant(innerAngle - math.Pi) + math.Pi
		}
		else if (innerAngle < -math.Pi / 2) {
			if (innerAngle < -PI_32) getOuterAngleFirstQuadrant(innerAngle + math.Pi * 2) - math.Pi * 2 else getOuterAngleFirstQuadrant(innerAngle + math.Pi) - math.Pi
		}
		else getOuterAngleFirstQuadrant(innerAngle)
	}
}