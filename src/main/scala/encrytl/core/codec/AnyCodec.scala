package encrytl.core.codec

import java.nio.charset.Charset

import com.google.common.primitives.{Ints, Longs}
import encrytl.core.{TypedObject, TypedObjectCodec, Types}
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult}

object AnyCodec {

  import scodec.codecs._

  def encode(vl: Any): Array[Byte] = vl match {
    case int: Types.EInt.Underlying => Ints.toByteArray(int)
    case long: Types.ELong.Underlying => Longs.toByteArray(long)
    case bool: Types.EBoolean.Underlying => if (bool) Array(1) else Array(0)
    case str: Types.EString.Underlying =>
      val bytes = str.getBytes(Charset.defaultCharset)
      uint16.encode(bytes.length).require.toByteArray ++ bytes
    case bytes: Types.EByteVector.Underlying => uint16.encode(bytes.length).require.toByteArray ++ bytes
    case obj: TypedObject => TypedObjectCodec.encode(obj)
    case intList: List[Int] => intList.size.toByte +: intList.map(encode).reduce(_ ++ _)
    case longList: List[Long] => longList.size.toByte +: longList.map(encode).reduce(_ ++ _)
    case boolList: List[Boolean] => boolList.size.toByte +: boolList.map(encode).reduce(_ ++ _)
    case list: List[_] => list.map { el =>
      val elBytes = encode(el)
      uint16.encode(elBytes.length).require.toByteArray ++ elBytes
    }.reduce(_ ++ _)
    case dict: Map[_, _] => dict.map { case (k, v) =>
      val kBytes = encode(k)
      val vBytes = encode(v)
      kBytes.length.toByte +: (kBytes ++ uint16.encode(vBytes.length).require.toByteArray ++ vBytes)
    }.reduce(_ ++ _)
    case opt: Option[_] => if (opt.isEmpty) Array(0) else 1.toByte +: opt.map(encode).get
    case _ => throw new Exception("Unsupported type")
  }

  def decode[T](b: BitVector): Attempt[DecodeResult[T]] = ???
}
