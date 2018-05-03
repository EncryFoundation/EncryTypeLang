package encrytl.core

import java.nio.charset.Charset

import com.google.common.primitives.{Booleans, Bytes, Ints, Longs}
import encrytl.core.Types.{EProduct, EType, TypeFingerprint}
import scorex.crypto.hash.Blake2b256

import scala.util.Try

case class Val(tpe: EType, value: Any) {

  def castedValue: tpe.Underlying = value.asInstanceOf[tpe.Underlying]
}

class TypedObject private[core](val typeFingerprint: TypeFingerprint, fields: Seq[(String, Val)]) {

  private val serializer = TypedObjectSerializer

  lazy val bytes: Array[Byte] = serializer.toBytes(this)

  lazy val validFingerprint: Boolean = Blake2b256.hash(
    fields.foldLeft(Array.empty[Byte]) {
      case (acc, (n, Val(t: EProduct, _))) => acc ++ n.getBytes(Charset.defaultCharset) ++ t.fingerprint
      case (acc, (n, v)) => acc ++ n.getBytes(Charset.defaultCharset) :+ v.tpe.typeCode
    }
  ).take(EProduct.FingerprintLen) sameElements typeFingerprint
}

object TypedObject {

  def apply(tpe: EProduct, args: Seq[Any]): TypedObject = {
    assert(tpe.fields.size == args.size)
    val values = args.zip(tpe.fields).map {
      case (i, (n, t)) if i.isInstanceOf[t.Underlying@unchecked] =>
        n -> Val(t, i.asInstanceOf[t.Underlying])
      case _ => throw new Error("Object construction failed")
    }
    new TypedObject(tpe.fingerprint, values)
  }

  def apply(tpe: EProduct, args: Map[String, Any]): TypedObject = {
    assert(tpe.fields.size == args.size)
    val values = tpe.fields.map { case (key, keyT) =>
      val keyV = args.getOrElse(key, throw new Error("Object construction failed"))
      key -> Val(keyT, keyV.asInstanceOf[keyT.Underlying])
    }
    new TypedObject(tpe.fingerprint, values)
  }
}

object TypedObjectSerializer {

  def toBytes(obj: TypedObject): Array[Byte] = ???

  def parseBytes(bytes: Array[Byte]): Try[TypedObject] = ???
}
