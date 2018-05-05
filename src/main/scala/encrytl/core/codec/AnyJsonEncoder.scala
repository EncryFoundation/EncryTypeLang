package encrytl.core.codec

import encrytl.core.{TypedObject, Types}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

object AnyJsonEncoder {

  def encode(v: Any): Json = v match {
    case int: Types.EInt.Underlying => int.asJson
    case long: Types.ELong.Underlying => long.asJson
    case bool: Types.EBoolean.Underlying => bool.asJson
    case str: Types.EString.Underlying => str.asJson
    case bytes: Types.EByteVector.Underlying => Base58.encode(bytes).asJson
    case obj: TypedObject => obj.json
    case list: List[_] => list.map(encode).asJson
    case dict: Map[_, _] => dict.map { case (k, vl) => encode(k) -> encode(vl) }.toList.asJson
    case opt: Option[_] => opt.map(encode).asJson
    case _ => throw new Exception("Unsupported type")
  }
}
