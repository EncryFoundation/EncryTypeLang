package core

import core.Types.{EProduct, EType}

case class Val(tpe: EType, value: Any)

class TypedObject private[core](id: String, fields: List[(String, Val)]) {

  def apply(tpe: EProduct, args: List[Any]): TypedObject = {
    assert(tpe.fields.size == args.size)
    val values = args.zip(tpe.fields).map {
      case (i, (n, t)) if i.isInstanceOf[t.Underlying] =>
        n -> Val(t, i.asInstanceOf[t.Underlying])
      case _ => throw new Error("Object construction failed.")
    }
    new TypedObject(tpe.ident, values)
  }
}
