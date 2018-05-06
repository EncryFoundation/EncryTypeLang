package encrytl.frontend

object Ast {

  type Field = (Identifier, Type)

  case class Schema(id: Identifier, tpe: Type)

  sealed trait Type

  case class SimpleType(id: Identifier, typeParams: List[Type]) extends Type

  case class ProductType(fields: List[Field]) extends Type

  case class Identifier(name: String)

  implicit def liftString(s: String): Identifier = Identifier(s)
}
