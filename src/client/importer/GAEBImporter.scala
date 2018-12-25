package client.importer

import java.awt.Point
import java.io._

import client.comm.{ClientQueryManager, CreateInstancesBuffer}
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.expression.UnitNumber.ordering
import definition.expression._
import util.{Log, StrToInt}

import scala.collection.Seq
import scala.swing.Window
import scala.util.control.NonFatal


class GAEBImporter extends FileImportDescriptor {
  def name:String="GAEB-Import"  
  
  def dbNameField:Byte=2.toByte

	val gewerkTyp=135
	val posTyp=133
	val folderType=110
  
  def allowedFileTypes:Seq[(String,String)]=Seq(("Blanko-LV GAEB 81","D81"),("Blanko-LV GAEB 83","D83"),("Angebotsabgabe GAEB 84","D84"),
      ("Auftrag GAEB 86","D86"))
	

	/** checks if the given files can be inserted. File type checks are done by ImportManager
	 * 
	 */
	def canImport(files:Seq[File],droppedTarget:Option[InstanceData],ownerRef:OwnerReference):Boolean= {
    true
  }
	
	/**
	 *  shows an import Dialog at the given place
	 *  @param wpos position of the dialog window
	 *  @param dropTarget on what object are the files dropped
	 *  @return a list of setting data, to start the import, or Nil to stop it 
	 */
	def showImportDialog(window:Window, wpos:Point,files:Seq[File],dropTarget:Option[InstanceData]):Seq[AnyRef]= {
	  List(Some)
	}
	
	
	/** imports a single file to the database
	 * @param file the dropped datafile	 *
	 * @param settings the settings data from the import dialog
	 * @param progressListener a function to be called after every import step, and to give the current success of the file import in percent
	 * @return success of the file import 
	 */
	def importFile(file:File,baseObject:Reference,settings:Seq[AnyRef],progressListener:(Int)=>Boolean):Boolean = {
    println("import  file:"+file)
    var reader:BufferedReader=null
		try {
      reader= new BufferedReader(new InputStreamReader(new FileInputStream(file), "Cp850"))
			val gewerkOwner = Array(new OwnerReference(2, baseObject))
			//val ownerInst = ClientQueryManager.queryInstance(baseObject, -1).head
			var bytesRead: Long = 0
			val fileSize = file.length()
			var oberGlen = 2
			var unterGlen = 2
			var posLen = 4
      var lastText:String=""
			val vorTextBuffer = new StringBuffer
			var hasLangText: Boolean = false
      var createPosBuffer:Option[CreateInstancesBuffer]= None

			var currentGewerk: Option[OwnerReference] = None
			//var currentTitel: Option[OwnerReference] = None
			var posOZ: Int = 0
			var posMenge: UnitNumber = UnitNumber(0, UnitNumber.emptyFraction)
			var posArt: Int = 0

			var langTextBuffer = new StringBuffer

			var hadText = false
			var readVorbemerkungen = false

			def readLine: String = {
				val res = reader.readLine
				if (res != null)
					bytesRead += res.length + 2l
				val progress = {
					val prog = (bytesRead * 100 / fileSize).toInt
					if (prog > 100) 100 else prog
				}

				if (progressListener(progress)) {
					Log.w("Import Cancelled")
					throw new Exception("Cancel")
				}
				res
			}

			def closeLangText(): Unit = if (hasLangText) {
				createPosBuffer match {
					case Some(buffer) =>
						if (readVorbemerkungen) {
							val vinst = ClientQueryManager.createInstance(folderType, Array(OwnerReference(2, buffer.owners.head.ownerRef)))
							val vref = Reference(folderType, vinst)
							ClientQueryManager.writeInstanceField(vref, 1, StringConstant(langTextBuffer.toString))

						} else {
							val fields =new Array[Expression](7)
              fields(0)=EMPTY_EX
              fields(1)=IntConstant(posOZ)
              fields(2)=IntConstant(posArt)
              fields(3)=posMenge
              fields(4)=EMPTY_EX
              fields(5)=StringConstant(langTextBuffer.toString)
              fields(6)=EMPTY_EX
							buffer.addInstance(posTyp,fields)
						}
					case None => Log.e("No Titel for Pos: " + posOZ)
				}
				langTextBuffer = new StringBuffer
				hasLangText = false
			} else println("no Langtext")

			var line = readLine
			while (line != null&& line.length>0) {
				if (line.charAt(0) == 'T') {
					if (!hadText) hadText = true
					val theText = line.substring(2, 74).trim
					vorTextBuffer.append(theText + (if (theText.length < 55) "\n" else " "))
				}
				else {
					if (hadText) {
						hadText = false // Vorbemerkungen schreiben
						val vbInst = ClientQueryManager.createInstance(folderType, Array(OwnerReference(3, baseObject)))
						ClientQueryManager.writeInstanceField(Reference(folderType, vbInst), 1, StringConstant(vorTextBuffer.toString))
					}
					line.substring(0, 2) match {
						case StrToInt(code) =>
							code match {
								case 0 =>
									val struktString = line.substring(60, 71).trim
									oberGlen = struktString.lastIndexOf('1') + 1
									unterGlen = struktString.lastIndexOf('2')
									if (unterGlen >= 0) unterGlen -= oberGlen - 1
									else unterGlen = 0
									posLen = struktString.lastIndexOf('P') - oberGlen - unterGlen + 1
									Log.w("Importing GAEB type " + line.substring(10, 12) + " G:" + oberGlen + " T:" + unterGlen + " P:" + posLen)

								case 11 => // Gewerk / Titel
									closeLangText()
                  for(buffer<-createPosBuffer)buffer.flush()
									val oz1 = line.substring(2, 2 + oberGlen).trim.toInt
									val oz2 = if (unterGlen > 0) line.substring(2 + oberGlen, 2 + oberGlen + unterGlen).trim else ""
									val secondLine = readLine
									val name = secondLine.substring(2, 74).trim()
									Log.w("Import gewerk "+name+" oz1:"+oz1+" oz2:"+oz2)
									if (oz2 == "") { // Gewerk
										val ginst = ClientQueryManager.createInstance(gewerkTyp, gewerkOwner)
										val gref = Reference(gewerkTyp, ginst)
										ClientQueryManager.writeInstanceField(gref, 1, IntConstant(oz1))
										ClientQueryManager.writeInstanceField(gref, 2, StringConstant(name))
										currentGewerk = Some(OwnerReference(1, gref))
										createPosBuffer = Some(new CreateInstancesBuffer(Array(OwnerReference(1, gref)),50,true))
									} else currentGewerk match { // Titel
										case Some(gewerk) =>
											val ginst = ClientQueryManager.createInstance(gewerkTyp, Array(gewerk))
											val tref = Reference(gewerkTyp, ginst)
                      Log.w("New Titel tref:"+tref+" oz2:"+oz2)
											ClientQueryManager.writeInstanceField(tref, 1, IntConstant(oz2.toInt))
											ClientQueryManager.writeInstanceField(tref, 2, StringConstant(name))
                      createPosBuffer=Some(new CreateInstancesBuffer(Array(OwnerReference(1, tref)),50,true))
										case None => Log.e("kein Gewerk für Titel oz1:" + oz1 + " oz2:" + oz2)
									}
								case 12 => Log.w("kann nicht sein 12 ")

								case 20 => closeLangText()
									readVorbemerkungen = true

								case 21 => closeLangText() // Position
									readVorbemerkungen = false
									if (line.length < 34) Log.e("Fehler Posstring zu kurz:" + line)
									else {
										posOZ = line.substring(2 + oberGlen + unterGlen, 2 + oberGlen + unterGlen + posLen).trim.toInt
										// Positionstyp
										posArt = if (line.charAt(2 + oberGlen + unterGlen + posLen + 1) == 'A') 1
										else if (line.charAt(2 + oberGlen + unterGlen + posLen + 2) == 'E') 2
										else 0
										val menge = try
											line.substring(23, 31).toDouble + line.substring(31, 34).toDouble / 1000
										catch {
											case e: IllegalArgumentException => 0
										}
										val einheit = line.substring(34, 40)
										posMenge = new UnitNumber(menge, UnitFraction(collection.immutable.TreeSet[UnitElem](new UnitElem(einheit, 1))(ordering),
											UnitNumber.emptySet))
										println("Pos "+posOZ+" menge:"+posMenge)
									}

								case 25 =>
									val ktext=line.substring(2, 74).trim
									langTextBuffer.append(ktext+"\n")
                  lastText=ktext
									//println("ktext:"+ktext)
									hasLangText=true

								case 26 => val theText = line.substring(5, 70).trim //langtext
                  if(!theText.equals(lastText))
									  langTextBuffer.append(theText + (if (theText.length < 35) "\n" else " "))
                  lastText=""
									hasLangText = true

								case 99 => closeLangText()
									Log.w("GAEB import finish")
								case other => println("Other Line:"+other)

							}
						case other => println("wrong line Code " + other)
					}
				}
				line = readLine
			}
      for(buffer<-createPosBuffer)buffer.flush()
		} catch {
			case NonFatal(e)=> Log.e("Importing "+e);e.printStackTrace()
		}
    finally {
      reader.close()
    }

	  true
	}
}