package encrytl.core.codec

import java.nio.charset.Charset

import com.google.common.primitives.{Ints, Longs}
import encrytl.core.{TypedObject, TypedObjectCodec, Types}
import scodec.bits.BitVector

import scala.util.Try

object AnyCodec {

  import scodec.codecs._

  def encode(v: Any): Array[Byte] = v match {
    case int: Types.EInt.Underlying => Ints.toByteArray(int)
    case long: Types.ELong.Underlying => Longs.toByteArray(long)
    case bool: Types.EBoolean.Underlying => if (bool) Array(1) else Array(0)
    case str: Types.EString.Underlying => str.getBytes(Charset.defaultCharset)
    case bytes: Types.EByteVector.Underlying => bytes
    case obj: TypedObject => TypedObjectCodec.encode(obj)
    case list: List[_] => uint8.encode(list.size).require.toByteArray ++ list.map { el =>
      val elBytes = encode(el)
      uint16.encode(elBytes.length).require.toByteArray ++ elBytes
    }.reduce(_ ++ _)
    case dict: Map[_, _] => dict.map { case (k, vl) =>
      val kBytes = encode(k)
      val vBytes = encode(vl)
      kBytes.length.toByte +: (kBytes ++ uint16.encode(vBytes.length).require.toByteArray ++ vBytes)
    }.reduce(_ ++ _)
    case opt: Option[_] => if (opt.isEmpty) Array(0) else 1.toByte +: opt.map(encode).get
    case _ => throw new Exception("Unsupported type")
  }

  def decode(tpe: Types.EType, bytes: Array[Byte]): Try[tpe.Underlying] = Try {
    (tpe match {
      case _: Types.EInt.type => Ints.fromByteArray(bytes)
      case _: Types.ELong.type => Longs.fromByteArray(bytes)
      case _: Types.EBoolean.type => if (bytes.head == 1) true else false
      case _: Types.EString.type => new String(bytes, Charset.defaultCharset)
      case _: Types.EByteVector.type => bytes
      case _: Types.EProduct => TypedObjectCodec.decode(bytes).get
      case Types.EList(inT) =>
        val eltsQty = uint8.decode(BitVector(bytes.head)).require.value
        (0 until eltsQty).foldLeft(bytes.tail, List.empty[inT.Underlying]) { case ((leftBytes, acc), _) =>
          val len = uint16.decode(BitVector(leftBytes.take(2))).require.value
          leftBytes.drop(2 + len) -> (acc :+ decode(inT, leftBytes.slice(2, len + 2)).get.asInstanceOf[inT.Underlying])
        }._2
      // TODO: Dict and Option
    }).asInstanceOf[tpe.Underlying]
  }
}
