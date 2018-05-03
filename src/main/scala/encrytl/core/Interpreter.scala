package encrytl.core

import encrytl.frontend.Ast

import scala.util.{Failure, Success, Try}

class Interpreter {

  import Interpreter._

  def interpret(types: Seq[Ast.Type]): InterpretationResult = Try {
    types.foldLeft(Types.primitives.map(t => t.ident -> t).toMap, Seq.empty[Types.EProduct]) { case ((scope, acc), tpe) =>
      val prod = Types.EProduct(tpe.id.name, tpe.fields.map { case Ast.Field(id, tpeId) =>
        val tpe = {
          val mainT = Types.typeByIdent(tpeId.id.name).getOrElse(throw UnresolvedRefError(tpeId.id.name))
          val typeParams = tpeId.typeParams.map(tp => Types.typeByIdent(tp.name).getOrElse(throw UnresolvedRefError(tp.name)))
          mainT -> typeParams
        } match {
          case (Types.EList(_), tps) if tps.size == 1 => Types.EList(tps.head)
          case (Types.EDict(_, _), tps) if tps.size == 2 => Types.EDict(tps.head, tps.last)
          case (Types.EOption(_), tps) if tps.size == 1 => Types.EOption(tps.head)
          case (otherT, tps) if tps.isEmpty => otherT
          case _ => throw InterpretationError
        }
        id.name -> tpe
      })
      scope.updated(prod.ident, prod) -> (acc :+ prod)
    }._2
  } match {
    case Success(r) => Right(r)
    case Failure(err: InterpretationError) => Left(err)
  }
}

object Interpreter {

  type InterpretationResult = Either[InterpretationError, Seq[Types.EProduct]]

  class InterpretationError extends Error

  case class UnresolvedRefError(n: String) extends InterpretationError

  case object InterpretationError extends InterpretationError
}
