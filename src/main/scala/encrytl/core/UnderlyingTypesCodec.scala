package encrytl.core

import java.nio.charset.Charset

import scala.util.Try

object UnderlyingTypesCodec {

  import scodec.codecs._

  def encode(v: Any): Array[Byte] = v match {
    case int: Types.EInt.Underlying => int32.encode(int).require.toByteArray
    case long: Types.ELong.Underlying => int64.encode(long).require.toByteArray
    case bln: Types.EBoolean.Underlying => bool.encode(bln).require.toByteArray
    case str: Types.EString.Underlying => string(Charset.defaultCharset).encode(str).require.toByteArray
    case bytes: Types.EByteVector.Underlying => uint16.encode(bytes.length).require.toByteArray ++ bytes
  }

  def decode[T](b: Array[Byte]): Try[T] = ???
}
