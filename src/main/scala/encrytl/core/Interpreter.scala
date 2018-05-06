package encrytl.core

import encrytl.frontend.Ast

import scala.util.{Failure, Success, Try}

class Interpreter {

  import Interpreter._

  def interpret(schema: Ast.Schema): InterpretationResult = Try(Schema(schema.id.name, interpretType(schema.tpe))) match {
    case Success(r) => Right(r)
    case Failure(err: InterpretationError) => Left(err)
  }

  private def interpretType(tpe: Ast.Type): Types.EType = tpe match {
    case Ast.SimpleType(id, tps) =>
      val typeParams = tps.map(interpretType)
      Types.typeByIdent(id.name).map {
        case Types.EList(_) if typeParams.size == 1 => Types.EList(typeParams.head)
        case Types.EOption(_) if typeParams.size == 1 => Types.EOption(typeParams.head)
        case otherT if tps.isEmpty => otherT
        case _ => throw InterpretationError
      }.getOrElse(throw UnresolvedRefError(id.name))
    case Ast.ProductType(flds) => Types.EProduct(flds.map { case (id, tp) => id.name -> interpretType(tp) })
  }
}

object Interpreter {

  type InterpretationResult = Either[InterpretationError, Schema]

  class InterpretationError extends Error

  case class UnresolvedRefError(n: String) extends InterpretationError

  case object InterpretationError extends InterpretationError
}
