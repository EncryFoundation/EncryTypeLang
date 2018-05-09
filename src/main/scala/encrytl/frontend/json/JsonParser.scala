package encrytl.frontend.json

import encrytl.frontend.WsApi
import fastparse.core
import fastparse.noApi

import scala.util.{Failure, Success, Try}

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
  def apply(t: T): V = f(t)
  override def toString(): String = name
}

object JsonParser {

  import WsApi._
  import fastparse.noApi._

  private val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  private val space: noApi.Parser[Unit] = P( CharsWhileIn(" \r\n").? )
  private val digits: noApi.Parser[Unit] = P( CharsWhileIn("0123456789"))
  private val exponent: noApi.Parser[Unit] = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  private val fractional: noApi.Parser[Unit] = P( "." ~ digits )
  private val integral: noApi.Parser[Unit] = P( "0" | CharIn('1' to '9') ~ digits.? )

  private val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => JsonAst.Num(x.toDouble)
  )

  private val `null`        = P( "null" ).map(_ => JsonAst.Null)
  private val `false`       = P( "false" ).map(_ => JsonAst.False)
  private val `true`        = P( "true" ).map(_ => JsonAst.True)

  private val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  private val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  private val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  private val strChars = P( CharsWhile(StringChars) )
  private val string =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"" ).map(JsonAst.Str)

  private val array =
    P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]" ).map(JsonAst.Arr(_:_*))

  private val pair = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

  private val obj =
    P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}" ).map(JsonAst.Obj(_:_*))

  private def jsonExpr: P[JsonAst.JsonVal] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )

  def parse(source: String): Try[JsonAst.JsonVal] = ( jsonExpr ~ End ).parse(source) match {
    case r: Parsed.Success[JsonAst.JsonVal] => Success(r.value)
    case e: Parsed.Failure => Failure(new Error(e.msg))
  }
}
