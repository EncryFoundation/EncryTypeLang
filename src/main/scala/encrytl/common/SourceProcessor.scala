package encrytl.common

import encrytl.core.{Interpreter, Schema}
import encrytl.frontend.Parser

import scala.util.Try

object SourceProcessor {

  private val interpreter = new Interpreter()

  def process(source: String): Try[Seq[Schema]] = Try {
    val parsed = Parser.parse(source).get.value
    parsed.map(s => interpreter.interpret(s).right.get)
  }
}
