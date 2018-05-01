package core

object Schema {

  case class Field(id: String, value: BoxedVal)
  case class TypedObject(id: String, fields: List[Field])

  sealed trait BoxedVal { val tpe: Types.EType }
  case class ObjectBox(inner: TypedObject, override val tpe: Types.EProduct) extends BoxedVal
  case class IntBox(inner: IntBox) extends BoxedVal { override val tpe: Types.EType = Types.EInt }
  case class LongBox(inner: LongBox) extends BoxedVal { override val tpe: Types.EType = Types.ELong }
  case class BoolBox(inner: Boolean) extends BoxedVal { override val tpe: Types.EType = Types.EBoolean }
  case class StrBox(inner: String) extends BoxedVal { override val tpe: Types.EType = Types.EString }
  case class BytesBox(inner: Array[Byte]) extends BoxedVal { override val tpe: Types.EType = Types.EByteVector }
  // TODO: Complete for other types.
}
