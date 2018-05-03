package encrytl.core

sealed trait BoxedVal { val tpe: Types.EType }

object BoxedVal {

  case class IntBox(inner: Int) extends BoxedVal { override val tpe: Types.EType = Types.EInt }
  case class LongBox(inner: Long) extends BoxedVal { override val tpe: Types.EType = Types.ELong }
  case class BoolBox(inner: Boolean) extends BoxedVal { override val tpe: Types.EType = Types.EBoolean }
  case class StrBox(inner: String) extends BoxedVal { override val tpe: Types.EType = Types.EString }
  case class BytesBox(inner: Array[Byte]) extends BoxedVal { override val tpe: Types.EType = Types.EByteVector }

  case class ObjectBox(inner: TypedObject, override val tpe: Types.EProduct) extends BoxedVal
  case class ListBox(inner: List[BoxedVal], override val tpe: Types.EType) extends BoxedVal
  case class DictBox(inner: Map[String, BoxedVal], override val tpe: Types.EType) extends BoxedVal
  case class OptionBox(inner: Option[BoxedVal], override val tpe: Types.EType) extends BoxedVal
}
