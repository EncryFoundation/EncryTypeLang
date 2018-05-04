package encrytl.core.codec

import encrytl.core.{TypedObject, TypedObjectCodec, Types}
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.{BitVector, ByteVector}

object AnyCodec {

  import scodec.codecs.implicits._

  def encode(v: Any): BitVector = v match {
    case int: Types.EInt.Underlying => Codec.encode(int).require
    case long: Types.ELong.Underlying => Codec.encode(long).require
    case bool: Types.EBoolean.Underlying => Codec.encode(bool).require
    case str: Types.EString.Underlying => Codec.encode(str).require
    case bytes: Types.EByteVector.Underlying => Codec.encode(ByteVector(bytes)).require
    case obj: TypedObject => BitVector(TypedObjectCodec.encode(obj))
    case lst: List[_] => Codec.encode(lst.map(encode)).require
    case dict: Map[_, _] => Codec.encode(dict.toList.map { case (a1, a2) => encode(a1) ++ encode(a2) }).require
    case opt: Option[_] => Codec.encode(opt.map(encode)).require
    case _ => throw new Exception("Unsupported type")
  }

  def decode(b: BitVector): Attempt[DecodeResult[Any]] = ???
}
