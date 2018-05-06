package encrytl.core

import java.nio.charset.Charset

import com.google.common.primitives.Bytes
import encrytl.core.Types.{EProduct, EType, TypeFingerprint}
import encrytl.core.codec.{AnyCodec, AnyJsonEncoder, TypesCodecShallow}
import encrytl.frontend.Parser
import scodec.bits.BitVector
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import io.circe.{ACursor, Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

case class Val(tpe: EType, value: Any) {

  def castedValue: tpe.Underlying = value.asInstanceOf[tpe.Underlying]
}

class TypedObject private[core](val typeFingerprint: TypeFingerprint, val fields: Seq[(String, Val)]) {

  private val codec = TypedObjectCodec

  private val jsonCodec = TypedObjectJsonCodec

  lazy val bytes: Array[Byte] = codec.encode(this)

  lazy val json: Json = jsonCodec.encode(this)

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

object TypedObjectJsonCodec {

  type Field = (String, EType, Any)

  def encode(obj: TypedObject): Json = Map(
    "fingerprint" -> Base58.encode(obj.typeFingerprint).asJson,
    "fields" -> obj.fields.map { case (n, v @ Val(t, _)) =>
      Map("key" -> n.asJson, "type" -> t.toString.asJson, "value" -> AnyJsonEncoder.encode(v.castedValue).asJson).asJson }.asJson,
  ).asJson

  // TODO: Implement json decoder properly.
  def decode(json: Json): Try[TypedObject] = io.circe.parser.decode[TypedObject](json.toString).toTry

  private def decodeAs(tpe: EType, c: ACursor): Try[tpe.Underlying] = Try {
    val valF = c.downField("value")
    (tpe match {
      case _: Types.EInt.type => valF.as[Int]
      case _: Types.ELong.type => valF.as[Long]
      case _: Types.EString.type => valF.as[String]
      case _: Types.EBoolean.type => valF.as[Boolean]
      case _: Types.EByteVector.type => valF.as[String].map(s => Base58.decode(s).get)
      case Types.EList(_: Types.EInt.type) => valF.as[List[Int]]
      case Types.EList(_: Types.ELong.type) => valF.as[List[Long]]
      case Types.EList(_: Types.EString.type) => valF.as[List[String]]
      case Types.EList(_: Types.EBoolean.type) => valF.as[List[Boolean]]
      case Types.EList(_: Types.EByteVector.type) => valF.as[List[String]].map(_.map(Base58.decode))
    }).right.get.asInstanceOf[tpe.Underlying]
  }

  implicit val objJsonDecoder: Decoder[TypedObject] = (c: HCursor) => {
    for {
      fingerprint <- c.downField("fingerprint").as[String]
      fields <- c.downField("fields").as[List[Field]]
    } yield {
      new TypedObject(Base58.decode(fingerprint).get, fields.map { case (n, t, v) => n -> Val(t, v) })
    }
  }

  implicit val fieldJsonDecoder: Decoder[Field] = (c: HCursor) => {
    for {
      key <- c.downField("key").as[String]
      tpe <- c.downField("type").as[String]
    } yield {
      val tpeR = new Interpreter().interpretType(Parser.parseType(tpe).get.value, shallow = true)
      val value = if (tpeR.isProduct) decode(c.downField("value").focus.get).get else decodeAs(tpeR, c).get
      (key, tpeR, value)
    }
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
