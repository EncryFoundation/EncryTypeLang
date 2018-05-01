package core

import fastparse.{all, core, noApi}

object WsApi extends fastparse.WhitespaceApi.Wrapper(Parser.wsComment)

object Parser {

  import WsApi._
  import fastparse.noApi._

  import Ast._

  val ws = P( " " )
  val wss = P( ws.rep(min = 1) )

  val comment: all.Parser[Unit] =   P( "#" ~ CharsWhile(_ != '\n', min = 0) )
  val wsComment: all.Parser[Unit] = P( (CharsWhileIn(" \n") | comment | "\\\n").rep )

  val space: noApi.Parser[Unit] = P( CharIn(" \n") )

  val spaces = P( space.repX )

  val letter: all.Parser[Unit] =    P( lowercase | uppercase )
  val lowercase: all.Parser[Unit] = P( CharIn('a' to 'z') )
  val uppercase: all.Parser[Unit] = P( CharIn('A' to 'Z') )
  val digit: all.Parser[Unit] =     P( CharIn('0' to '9') )

  val Ident: P[Identifier] = P( letter.rep ).!.map(Ast.Identifier)

  val typeParams: P[Seq[Identifier]] = P( "[" ~ Ident.rep(1, ",") ~ ",".? ~ "]" )

  val typeDeclaration: P[TypeIdentifier] = P( ":" ~ Ident ~ typeParams.? ).map { case (tpeN, tpsOpt) =>
    Ast.TypeIdentifier(tpeN, tpsOpt.map(_.toList).getOrElse(List.empty))
  }

  val field: P[Field] = P( "field" ~ Ident ~ typeDeclaration ).map { case (id, td) => Field(id, td) }

  val fields: P[Seq[Field]] = P( field.rep(min = 1, ";") ~ ";".? )

  val typeDescr: P[Type] = P( "type" ~ Ident ~ "(" ~ fields ~ ")" ).map { case (id, flds) => Type(id, flds.toList) }

  val schema: P[Seq[Type]] = P( spaces.? ~ typeDescr.repX(0, spaces) ~ spaces.? ).map(_.toSeq)

  def parse(source: String): core.Parsed[Seq[Type], Char, String] = ( schema ~ End ).parse(source)
}
