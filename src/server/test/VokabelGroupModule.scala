/**
 * Author: Peter Started:14.12.2010
 */
package server.test

import definition.comm.ClientCommands
import definition.data._
import definition.expression._
import definition.typ._
import server.comm.{AbstractUserSocket, JavaClientSocket}
import server.storage._
import transaction.handling.TransactionManager

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Vokabel(val deutsch:String,val englisch:String,val dg:Int,val eg:Int,val ref:Reference) {
  def this(data:InstanceData)=this(data.fieldValue.head.toString.trim,data.fieldValue(1).toString.trim,data.fieldValue(2).toInt,
      data.fieldValue(3).toInt,data.ref)
}

/**
 * 
 */
class VokabelGroupModule extends ActionModule {
  var vokabelTyp: Int = -1
  val lernAlleDeutsch=new ActionImpl("Lerne Deutsch",None,doLerneDeutsch(0,de_Richtung = true))
  val vertiefeDeutsch=new ActionImpl("Vertiefe Deutsch",None,doLerneDeutsch(1,de_Richtung = true))
  val lernAlleEnglisch=new ActionImpl("Lerne Englisch",None,doLerneDeutsch(0,de_Richtung = false))
  val vertiefeEnglisch=new ActionImpl("Vertiefe Englisch",None,doLerneDeutsch(1,de_Richtung = false))
  val sortiere=new ActionImpl("Sortieren",None,doSortiere)
  
  val aQuestion=DialogQuestion("was heisst", Seq(new AnswerDefinition("Was Heisst :", DataType.StringTyp, None)))
  
  val actions=List(lernAlleDeutsch,vertiefeDeutsch,lernAlleEnglisch,vertiefeEnglisch,sortiere) 
  
  def setObjectType(typeID: Int): Unit = {}
  
  def init(): Unit = {
  	if(vokabelTyp== -1)vokabelTyp =AllClasses.get.getClassIDByName("Vokabel")
  }
  
  
  
  def doLerneDeutsch(nurLevel:Int,de_Richtung:Boolean)(u:AbstractUserSocket,data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
  	init()
  	var vokListe=Random.shuffle(StorageManager.loadChildren(data.ref,vokabelTyp,0).map(new Vokabel(_)))
		val unknownWords = ArrayBuffer[Vokabel]()
  	val numVoks=vokListe.count(el=>(if(de_Richtung)el.dg else el.eg)>=nurLevel)
		System.out.println("lerne Deutsch "+data)

		def copyUnknownWords(): Unit ={
			vokListe=Random.shuffle(unknownWords)
			unknownWords.clear()
		}

		def getNextDeutschWert:String = {
  		if(vokListe.isEmpty){
				if(unknownWords.nonEmpty) copyUnknownWords()
				else return ""
			}
  		val head=vokListe.head
  		var dw=if(de_Richtung) head.deutsch else head.englisch
  		var level=if(de_Richtung)head.dg else head.eg
  		while((dw.length==0||level<nurLevel)&& vokListe.lengthCompare(1) > 0) {
  			vokListe=vokListe.drop(1)
  			val head=vokListe.head
  			dw=if(de_Richtung) head.deutsch else head.englisch
  			level=if(de_Richtung)head.dg else head.eg  			
  		}
  		if(level<nurLevel) {
				if(unknownWords.nonEmpty) {
					copyUnknownWords()
					getNextDeutschWert
				}
				else ""
			}
			else dw
  	}
		
		if(vokListe.isEmpty) return true
		var deutschWert=getNextDeutschWert
  	if(deutschWert.length==0) return true
  	
		val question= DialogQuestion("Vokabeln lernen. Bitte eingeben ->", Seq(new AnswerDefinition("Was heisst '" + deutschWert + "' ?", DataType.StringTyp, None)))
		u.askEnquiry(question,handleAnswer)
		
		def handleAnswer(u:JavaClientSocket, params:Seq[(String,Constant)]):Unit= {
  		if(params.isEmpty)System.out.println("Stop")
			else {
				val answer=params.head._2 .toString
				//System.out.println("frage:"+deutschWert+" answer:"+answer)
				val head=vokListe.head
				val rightAnswer=if(de_Richtung) head.englisch else head.deutsch
				val level=if(de_Richtung)head.dg else head.eg
				val response = if(answer.trim==rightAnswer) {
					if (level>0) writeLevel(u,vokListe.head.ref,if(de_Richtung)2 else 3,level-1)
					"'"+rightAnswer+"' ist RICHTIG !"
				}
				else { 
					writeLevel(u,vokListe.head.ref,if(de_Richtung)2 else 3,level+2)
					unknownWords+=head
					"'"+answer+"' ist falsch,<br>'"+(if(de_Richtung)head.deutsch else head.englisch)+"' heißt: '"+rightAnswer+"'\n"
				}
				vokListe=vokListe.drop(1)
				deutschWert=getNextDeutschWert
				if(deutschWert.length==0){
					u.askEnquiry(DialogQuestion(response, Seq(new AnswerDefinition("Alle " + numVoks + " Vokabeln gelernt", DataType.EnumTyp, None, "Fertig"))),
						(a,b)=>{})
				}
				else {
					val question= DialogQuestion(response, Seq(new AnswerDefinition("Was heißt '" + deutschWert + "' ?", DataType.StringTyp, None)))
					u.askEnquiry(question,handleAnswer)
				}
			}
  	}				
		true
	}
  def doSortiere(u:AbstractUserSocket, data:InstanceData, param:Seq[(String,Constant)]):Boolean = {
    TransactionManager.sortProperty(new OwnerReference(0,data.ref), 0)
    false
  }
  
  def writeLevel(user:AbstractUserSocket,ref:Reference,fieldNr:Byte,newValue:Int): Option[Exception] = {
  	TransactionManager.doTransaction(user.userID ,ClientCommands.writeField.id.toShort,ref,false,-1,{
  		TransactionManager.tryWriteInstanceField(ref,fieldNr,IntConstant(newValue))
  	})
  }
  
  
	
  
  
	def handleDeutschAnswer(u:AbstractUserSocket,params:Seq[(String,Constant)]):Unit= {
		if(params.isEmpty)System.out.println("Stop")
		else{
			System.out.println("Anwsers:"+params)
			u.askEnquiry(aQuestion,handleDeutschAnswer )
		}
		
	}
  
}