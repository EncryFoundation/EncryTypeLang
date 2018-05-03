package encrytl.frontend

object Ast {

  case class Type(id: Identifier, fields: List[Field])

  case class Field(id: Identifier, tpe: TypeIdentifier)

  case class TypeIdentifier(id: Identifier, typeParams: List[Identifier])

  case class Identifier(name: String)

  implicit def liftString(s: String): Identifier = Identifier(s)
}
