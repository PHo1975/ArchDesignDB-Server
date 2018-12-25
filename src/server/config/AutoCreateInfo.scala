/**
 * Author: Peter Started:14.05.2011
 */
package server.config

import definition.expression._

import scala.xml.Elem

/** Information about children that should be automatically created when the instance is created
 * @param startValues Sequence of pairs (fieldNr,StartValue)
 */
case class AutoCreateInfo (propField:Byte,childType:Int,startValues:Seq[(Byte,Expression)]){
	
	def startValueToXML(value:(Byte,Expression)): Elem = <sv field={value._1.toString} value={value._2.encode}/>
	
	def toXML:Elem = {
		<AutoCreate prop= {propField.toString} childType= {childType.toString} >
		{
			startValues map startValueToXML
		}
	  </AutoCreate>
	}
}

object AutoCreateInfo {
	def fromXML(node: scala.xml.Node): AutoCreateInfo = 	{
		AutoCreateInfo((node \ "@prop").text.toByte,(node \ "@childType").text.toInt,
		(node \ "sv") map startValueFromXML )
		 
	}
	
	def startValueFromXML(node: scala.xml.Node): (Byte, Expression) = {
		val exText=(node \ "@value").text.trim
		((node \ "@field").text.toByte,if(exText.length==0) EMPTY_EX else Expression.decode(exText)._1)
	}
		
}