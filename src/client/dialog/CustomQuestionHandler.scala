/**
 * Author: Peter Started:21.12.2010
 */
package client.dialog


import definition.typ.{AnswerDefinition, DataType, DialogQuestion, ParamQuestion}

/** superclass of all handlers for custom question types in the client
 * 
 */
trait CustomQuestionHandler {
	/*type AnswerFunc=(Seq[(String,Constant)])=>Unit
	private val answerListeners=collection.mutable.HashSet[AnswerFunc]()
	
	def registerAnswerListener(func:AnswerFunc)= {
		answerListeners+=func
	}*/
	
	//def notifyListeners(answer:Seq[(String,Constant)])= answerListeners.foreach(_(answer))
	
  def load(question:ParamQuestion,container:FocusContainer):Unit

  def singlePointQuestion(actionText: String, questionText: String, strict: Option[Boolean], repeat: Boolean = false) =
    DialogQuestion(actionText, Seq(new AnswerDefinition(questionText, DataType.VectorTyp, None, strict match {
      case Some(true) => AnswerPanelsData.STRICT_HIT
      case Some(false) => AnswerPanelsData.NOSTRICT_HIT
      case None => ""
    })), repeat)

  def singleNumberQuestion(actionText: String, questionText: String, repeat: Boolean = false) = DialogQuestion(actionText, Seq(new AnswerDefinition(questionText, DataType.DoubleTyp, None)), repeat)

  def singleTextQuestion(actionText: String, questionText: String, repeat: Boolean = false) = DialogQuestion(actionText, Seq(new AnswerDefinition(questionText, DataType.StringTyp, None)), repeat)

  def singleIntQuestion(actionText: String, questionText: String, repeat: Boolean = false) = DialogQuestion(actionText,
    Seq(new AnswerDefinition(questionText, DataType.IntTyp, None, AnswerDefinition.NonNullConstraint)), repeat)
  
  lazy val moveStartAnswers=Seq(new AnswerDefinition("'von Punkt' angeben",DataType.VectorTyp, None ),
			  new AnswerDefinition("Delta X eingeben:",DataType.DoubleTyp,None)
			) 
}