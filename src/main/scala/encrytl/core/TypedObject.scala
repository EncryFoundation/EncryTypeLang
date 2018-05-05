package encrytl.core

import java.nio.charset.Charset

import com.google.common.primitives.Bytes
import encrytl.core.Types.{EProduct, EType, TypeFingerprint}
import encrytl.core.codec.{AnyCodec, TypesCodecShallow}
import scodec.bits.BitVector
import scorex.crypto.encode.Base58
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

  override def toString: String = s"TypedObj(${Base58.encode(typeFingerprint)}, $fields)"
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
    uint8.encode(obj.fields.size).require.toByteArray,
    obj.fields.foldLeft(Array.empty[Byte]) { case (acc, (n, v @ Val(t, _))) =>
      require(n.length <= 25)
      val name = {
        val nBytes = string(Charset.defaultCharset).encode(n).require.toByteArray
        uint8.encode(nBytes.length).require.toByteArray ++ nBytes
      }
      val tpe = {
        val typeBytes = TypesCodecShallow.encode(t)
        uint8.encode(typeBytes.length).require.toByteArray ++ typeBytes
      }
      val value = {
        val vBytes = AnyCodec.encode(v.castedValue)
        require(vBytes.length < Short.MaxValue * 2)
        uint16.encode(vBytes.length).require.toByteArray ++ vBytes
      }
      acc ++ name ++ tpe ++ value
    }
  )

  def decode(bytes: Array[Byte]): Try[TypedObject] = Try {
    val fingerprint = bytes.take(Types.EProduct.FingerprintLen)
    val fieldQty = uint8.decode(BitVector(bytes.slice(Types.EProduct.FingerprintLen, Types.EProduct.FingerprintLen + 1))).require.value
    val fields = (0 until fieldQty).foldLeft(bytes.drop(Types.EProduct.FingerprintLen + 1), Seq.empty[(String, Val)]) { case ((leftBytes, acc), _) =>
      val nameLen = uint8.decode(BitVector(leftBytes.head)).require.value
      val name = string(Charset.defaultCharset).decode(BitVector(leftBytes.slice(1, 1 + nameLen))).require.value
      val tpeLen = uint8.decode(BitVector(leftBytes.slice(1 + nameLen, 1 + nameLen + 1))).require.value
      val tpe = TypesCodecShallow.decode(leftBytes.slice(1 + nameLen + 1, 1 + nameLen + 1 + tpeLen)).get
      val valLen = uint16.decode(BitVector(leftBytes.slice(1 + nameLen + 1 + tpeLen, 1 + nameLen + 1 + tpeLen + 2))).require.value
      val value = AnyCodec.decode(tpe, leftBytes.slice(1 + nameLen + 1 + tpeLen + 2, 1 + nameLen + 1 + tpeLen + 2 + valLen)).get
        leftBytes.drop(1 + nameLen + 1 + tpeLen + 2 + valLen) -> (acc :+ (name, Val(tpe, value)))
    }._2
    new TypedObject(fingerprint, fields)
  }
}
