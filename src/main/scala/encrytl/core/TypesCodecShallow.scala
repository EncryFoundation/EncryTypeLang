package encrytl.core

import scala.util.Try

class TypesCodecShallow {

  import Types._

  def encode(t: EType): Array[Byte] = t match {
    case EList(elT) => EList.typeCode +: encode(elT)
    case EOption(inT) => EOption.typeCode +: encode(inT)
    case EDict(keyT, valT) => EDict.typeCode +: (encode(keyT) ++ encode(valT))
    case p: EProduct => EProduct.typeCode +: p.fingerprint
    case otherT => Array(otherT.typeCode)
  }

  def decode(b: Array[Byte]): Try[EType] = Try {
    b.head match {
      case EAny.`typeCode` if b.length == 1 => EAny
      case EInt.`typeCode` if b.length == 1 => EInt
      case ELong.`typeCode` if b.length == 1 => ELong
      case EString.`typeCode` if b.length == 1 => EString
      case EBoolean.`typeCode` if b.length == 1 => EBoolean
      case EByteVector.`typeCode` if b.length == 1 => EByteVector
      case EList.`typeCode` if b.length == 2 => EList(decode(b.last).get)
      case EOption.`typeCode` if b.length == 2 => EOption(decode(b.last).get)
      case EDict.`typeCode` if b.length == 3 => EDict(decode(b.tail.head).get, decode(b.last).get)
      case EProduct.`typeCode` if b.length == 1 + EProduct.FingerprintLen => ShallowProduct(b.tail)
      case _ => throw new Error("Unknown typeCode")
    }
  }

  implicit def liftByte(b: Byte): Array[Byte] = Array(b)
}
