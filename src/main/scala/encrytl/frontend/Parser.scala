package encrytl.frontend

import encrytl.frontend.json.JsonAst
import fastparse.{all, core, noApi}

import scala.util.{Failure, Success, Try}

object WsApi extends fastparse.WhitespaceApi.Wrapper(Parser.wsComment)

object Parser {

  import WsApi._
  import fastparse.noApi._

  def ws = P( " " )
  def wss = P( ws.rep(min = 1) )

  def comment: all.Parser[Unit] =   P( "#" ~ CharsWhile(_ != '\n', min = 0) )
  def wsComment: all.Parser[Unit] = P( (CharsWhileIn(" \n") | comment | "\\\n").rep )

  def space: noApi.Parser[Unit] = P( CharIn(" \n") )

  def spaces = P( space.repX )

  def letter: all.Parser[Unit] =    P( lowercase | uppercase )
  def lowercase: all.Parser[Unit] = P( CharIn('a' to 'z') )
  def uppercase: all.Parser[Unit] = P( CharIn('A' to 'Z') )
  def digit: all.Parser[Unit] =     P( CharIn('0' to '9') )

  def Ident: P[Ast.Identifier] = P( letter.rep ).!.map(Ast.Identifier)

  def typeParams: P[Seq[Ast.Type]] = P( "[" ~ tpe.rep(1, ",") ~ ",".? ~ "]" )

  def simpleType: P[Ast.SimpleType] = P( Ident ~ typeParams.? ).map { case (tpeN, tpsOpt) =>
    Ast.SimpleType(tpeN, tpsOpt.map(_.toList).getOrElse(List.empty))
  }

  def field: P[Ast.Field] = P( Ident ~ ":" ~ tpe )

  def fields: P[Seq[Ast.Field]] = P( field.rep(min = 1, ";") ~ ";".? )

  def productType: P[Ast.ProductType] = P( "Object" ~ "(" ~ fields ~ ")" ).map(flds => Ast.ProductType(flds.toList))

  def tpe: P[Ast.Type] = P( productType | simpleType )

  def schema: P[Ast.Schema] = P( "schema" ~ Ident ~ ":" ~ tpe ).map { case (id, tp) => Ast.Schema(id, tp) }

  def schemas: P[Seq[Ast.Schema]] = P( spaces.? ~ schema.repX(0, spaces) ~ spaces.? ).map(_.toSeq)

  def parseType(source: String): Try[Ast.Type] = ( tpe ~ End ).parse(source) match {
    case r: Parsed.Success[Ast.Type] => Success(r.value)
    case e: Parsed.Failure => Failure(new Error(e.msg))
  }

  def parse(source: String): Try[Seq[Ast.Schema]] = ( schemas ~ End ).parse(source) match {
    case r: Parsed.Success[Seq[Ast.Schema]] => Success(r.value)
    case e: Parsed.Failure => Failure(new Error(e.msg))
  }
}
