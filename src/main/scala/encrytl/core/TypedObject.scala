package encrytl.core

import java.nio.charset.Charset

import com.google.common.primitives.Bytes
import encrytl.core.Types.{EProduct, EType, TypeFingerprint}
import encrytl.core.codec.{AnyCodec, TypesCodecShallow}
import scorex.crypto.hash.Blake2b256

import scala.util.Try

case class Val(tpe: EType, value: Any) {

  def castedValue: tpe.Underlying = value.asInstanceOf[tpe.Underlying]
}

class TypedObject private[core](val typeFingerprint: TypeFingerprint, val fields: Seq[(String, Val)]) {

  private val serializer = TypedObjectCodec

  lazy val bytes: Array[Byte] = serializer.encode(this)

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

object TypedObjectCodec {

  import scodec.codecs._

  def encode(obj: TypedObject): Array[Byte] = Bytes.concat(
    obj.typeFingerprint,
    obj.fields.foldLeft(Array.empty[Byte]) { case (acc, (n, v @ Val(t, _))) =>
      require(n.length <= 25)
      val name = {
        val nBytes = string(Charset.defaultCharset).encode(n).require.toByteArray
        uint8.encode(nBytes.length).require.toByteArray ++ nBytes
      }
      val value = {
        val vBytes = AnyCodec.encode(v.castedValue)
        require(vBytes.length < Short.MaxValue * 2)
        uint16.encode(vBytes.length).require.toByteArray ++ vBytes
      }
      acc ++ name ++ TypesCodecShallow.encode(t) ++ value
    }
  )

  def decode(bytes: Array[Byte]): Try[TypedObject] = ???
}
