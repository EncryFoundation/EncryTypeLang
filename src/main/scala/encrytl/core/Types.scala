package encrytl.core

import java.nio.charset.Charset

import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

object Types {

  type TypeFingerprint = Array[Byte]

  type Field = (String, EType)

  sealed trait EType {
    type Underlying
    val ident: String
    val typeCode: Byte

    def isPrimitive: Boolean = this.isInstanceOf[EPrimitive]

    def isCollection: Boolean = this.isInstanceOf[ECollection]

    def isProduct: Boolean = this.isInstanceOf[EProduct] || this.isInstanceOf[ShallowProduct]

    override def equals(obj: scala.Any): Boolean = obj match {
      case s: EPrimitive => s.ident == this.ident
      case c: ECollection => c == this
      case p: EProduct => p == this
      case _ => false
    }

    override def toString: String = this.ident
  }

  sealed trait EPrimitive extends EType

  case object EAny extends EType with EPrimitive {
    override type Underlying = Unit
    override val ident: String = "Any"
    override val typeCode: Byte = 1.toByte
  }
  case object EBoolean extends EType with EPrimitive {
    override type Underlying = Boolean
    override val ident: String = "Bool"
    override val typeCode: Byte = 2.toByte
  }
  case object EInt extends EType with EPrimitive {
    override type Underlying = Int
    override val ident: String = "Int"
    override val typeCode: Byte = 3.toByte
  }
  case object ELong extends EType with EPrimitive {
    override type Underlying = Long
    override val ident: String = "Long"
    override val typeCode: Byte = 4.toByte
  }
  case object EString extends EType with EPrimitive {
    override type Underlying = String
    override val ident: String = "String"
    override val typeCode: Byte = 5.toByte
  }
  case object EByteVector extends EType with EPrimitive {
    override type Underlying = Array[Byte]
    override val ident: String = "Bytes"
    override val typeCode: Byte = 6.toByte
  }

  sealed trait ECollection extends EType

  case class EList(valT: EType) extends EType with ECollection {
    override type Underlying = List[valT.Underlying]
    override val ident: String = "List"
    override val typeCode: Byte = EList.typeCode

    override def equals(obj: Any): Boolean = obj match {
      case l: EList => l.valT == this.valT
      case _ => false
    }

    override def toString: String = this.ident + s"[$valT]"
  }
  object EList {
    val typeCode: Byte = 7.toByte
  }

  // Placeholder for not inferred type.
  case object NIType extends EType {
    override type Underlying = Nothing
    override val ident: String = "-"
    override val typeCode: Byte = (-1).toByte
  }

  /** Used as full description of some composite type. */
  case class EProduct(fields: List[Field]) extends EType {
    override type Underlying = TypedObject
    override val ident: String = "Object"
    override val typeCode: Byte = EProduct.typeCode

    override def equals(obj: Any): Boolean = obj match {
      case p: EProduct =>
        if (p.fields.size != this.fields.size) false
        else p.fields.zip(this.fields).forall { case ((f1, _), (f2, _)) => f1 == f2 }
      case _ => false
    }

    lazy val fingerprint: TypeFingerprint = Blake2b256.hash(
      fields.foldLeft(Array.empty[Byte]) {
        case (acc, (n, v: EProduct)) => acc ++ n.getBytes(Charset.defaultCharset) ++ v.fingerprint
        case (acc, (n, v)) => acc ++ n.getBytes(Charset.defaultCharset) :+ v.typeCode
      }
    ).take(EProduct.FingerprintLen)
  }
  object EProduct {
    val typeCode: Byte = 10.toByte
    val FingerprintLen: Int = 8
  }

  /**
    * Used as a lightweight reflection of the `EProduct`.
    * Substitutes `EProduct` in self-described objects where full type description is redundant.
    */
  case class ShallowProduct(fingerprintEnc: String) extends EType {
    override type Underlying = TypedObject
    override val ident: String = "Object"
    override val typeCode: Byte = ShallowProduct.typeCode

    override def equals(obj: Any): Boolean = obj match {
      case p: EProduct => p.fingerprint sameElements this.fingerprintEnc
      case sp: ShallowProduct => sp.fingerprintEnc sameElements this.fingerprintEnc
      case _ => false
    }

    def fingerprint: TypeFingerprint = Base58.decode(fingerprintEnc).get
  }
  object ShallowProduct {
    val typeCode: Byte = 11.toByte

    def apply(fingerprint: TypeFingerprint): ShallowProduct = new ShallowProduct(Base58.encode(fingerprint))
  }

  lazy val primitives: Seq[EType] = Seq(
    EAny,
    EInt,
    ELong,
    EString,
    EBoolean,
    EByteVector
  )

  lazy val allTypes: Seq[EType] = primitives ++ Seq(
    EList(NIType),
    ShallowProduct(Array.fill(8)(0.toByte))
  )

  lazy val typesMap: Map[String, EType] = allTypes.map(t => t.ident -> t).toMap

  def typeByIdent(id: String): Option[EType] = typesMap.get(id)
}
