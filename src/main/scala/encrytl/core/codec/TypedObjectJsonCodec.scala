package encrytl.core.codec

import encrytl.core.{Interpreter, TypedObject, Types, Val}
import encrytl.frontend.Parser
import encrytl.frontend.json.{JsonAst, JsonParser}
import encrytl.frontend.json.JsonAst.JsonVal
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

object TypedObjectJsonCodec {

  import Errors._

  def encode(obj: TypedObject): Json = obj.fields.map { case (n, v @ Val(t, _)) =>
    n -> Map("type" -> t.toString.asJson, "value" -> TypedObjectJsonCodec.encodeAny(v.castedValue).asJson).asJson
  }.toMap.asJson

  // TODO: Limit nesting levels of objects.
  def decode(json: JsonVal): Try[TypedObject] = {
    def readField(field: JsonVal) = field match {
      case obj: JsonAst.Obj =>
        val fieldT = obj.value.find(_._1 == "type").map(_._2)
          .flatMap { case s: JsonAst.Str => Parser.parseType(s.value).map(t => new Interpreter().interpretType(t, shallow = true)).toOption }
          .getOrElse(throw DecodingError)
        val fieldV = obj.value.find(_._1 == "value").map(_._2)
          .flatMap(s => decodeAs(fieldT, s).toOption)
          .getOrElse(throw DecodingError)
        fieldT -> fieldV
      case _ => throw new Error(s"$json is not an object")
    }
    Try {
      json match {
        case obj: JsonAst.Obj => new TypedObject(obj.value.map { case (n, v) =>
          val (tp, vl) = readField(v)
          n -> Val(tp, vl)
        })
        case _ => throw new Error(s"$json is not an object")
      }
    }
  }

  def decode(json: Json): Try[TypedObject] = JsonParser.parse(json.noSpaces).flatMap(decode)

  def encodeAny(v: Any): Json = v match {
    case int: Types.EInt.Underlying => int.asJson
    case long: Types.ELong.Underlying => long.asJson
    case bool: Types.EBoolean.Underlying => bool.asJson
    case str: Types.EString.Underlying => str.asJson
    case bytes: Types.EByteVector.Underlying => Base58.encode(bytes).asJson
    case obj: TypedObject => obj.json
    case list: List[_] => list.map(encodeAny).asJson
    case _ => throw new Exception("Unsupported type")
  }

  def decodeAs(tpe: Types.EType, json: JsonVal): Try[Any] = Try {
    (tpe, json) match {
      case (_: Types.EInt.type, n: JsonAst.Num) if n.value <= Int.MaxValue => n.value.toInt
      case (_: Types.ELong.type, n: JsonAst.Num) if n.value <= Long.MaxValue => n.value.toLong
      case (_: Types.EString.type, n: JsonAst.Str) if n.value.length <= 100 => n.value    // TODO: Magic number. Move to config.
      case (_: Types.EByteVector.type, n: JsonAst.Str) if n.value.length <= 100 => Base58.decode(n.value).getOrElse(throw DecodingError)
      case (_: Types.EBoolean.type, n: JsonAst.True.type) => n.value
      case (_: Types.EBoolean.type, n: JsonAst.False.type) => n.value
      case (Types.EList(inT), n: JsonAst.Arr) if n.value.size <= 100 =>                   // TODO: Magic number. Move to config.
        if (n.value.isEmpty) throw EmptyCollError
        else if (n.value.forall(!_.isInstanceOf[JsonAst.Arr])) throw NestedCollError
        n.value.map(v => decodeAs(inT, v).getOrElse(throw DecodingError)).toList
      case (_: Types.ShallowProduct, n: JsonAst.Obj) => decode(n).getOrElse(throw DecodingError)
      case _ => throw DecodingError
    }
  }
}
