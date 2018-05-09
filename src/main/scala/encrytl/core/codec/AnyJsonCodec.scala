package encrytl.core.codec

import encrytl.core.{TypedObject, TypedObjectJsonCodec, Types}
import encrytl.frontend.json.JsonAst
import encrytl.frontend.json.JsonAst.JsonVal
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

object AnyJsonCodec {

  import Errors._

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

  def decode(json: JsonVal): Try[(Types.EType, Any)] = Try {
    json match {
      case num: JsonAst.Num if num.value <= Long.MaxValue => Types.ELong -> num.value.toLong
      case str: JsonAst.Str if str.value.length <= 1200 => Types.EString -> str.value   // TODO: Magic number. Move to config.
      case _: JsonAst.True.type => Types.EBoolean -> true
      case _: JsonAst.False.type => Types.EBoolean -> false
      case _: JsonAst.Null.type => throw IllegalTypeError
      case arr: JsonAst.Arr if arr.value.size <= 30 =>                                  // TODO: Magic number. Move to config.
        if (arr.value.isEmpty) throw EmptyCollError
        else if (arr.value.head.value.isInstanceOf[JsonAst.Arr]) throw NestedCollError
        val elts = arr.value.map(e => decode(e).getOrElse(throw DecodingError)).ensuring(e => e.forall(_._1 == e.head._1))
        Types.EList(elts.head._1) -> elts.map(_._2).toList
      case obj: JsonAst.Obj =>
        val tlObj = TypedObjectJsonCodec.decode(obj).getOrElse(throw DecodingError)
        Types.ShallowProduct(Base58.encode(tlObj.fingerprint)) -> tlObj
    }
  }
}
