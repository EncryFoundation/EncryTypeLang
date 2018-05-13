package encrytl.common

import encrytl.core.{Interpreter, Schema}
import encrytl.frontend.Parser

import scala.util.Try

object SourceProcessor {

  private val interpreter = new Interpreter()

  def process(source: String): Try[Seq[Schema]] = Try {
    Parser.parse(source).map(_.map(s => interpreter.interpret(s).get))
  }.flatten
}
