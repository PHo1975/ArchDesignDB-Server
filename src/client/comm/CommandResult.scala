package client.comm

import definition.expression.Constant

sealed trait CommandResult {

}

object NoResult extends CommandResult

case class HasResult(ex:Constant) extends CommandResult

case class HasError(error:Throwable) extends CommandResult