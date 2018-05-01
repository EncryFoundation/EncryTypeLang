package core

object Types {

  sealed trait EType {
    type Underlying
    val ident: String

    def isPrimitive: Boolean = this.isInstanceOf[EPrimitive]

    def isCollection: Boolean = this.isInstanceOf[ECollection]

    def isOption: Boolean = this.isInstanceOf[EOption]

    override def equals(obj: scala.Any): Boolean = obj match {
      case s: EPrimitive => s.ident == this.ident
      case _ => false
    }
  }

  sealed trait EPrimitive extends EType

  case object EAny extends EType with EPrimitive {
    override type Underlying = Unit
    override val ident: String = "Any"
  }
  case object EUnit extends EType with EPrimitive {
    override type Underlying = Unit
    override val ident: String = "Unit"
  }
  case object EBoolean extends EType with EPrimitive {
    override type Underlying = Boolean
    override val ident: String = "Bool"
  }
  case object EInt extends EType with EPrimitive {
    override type Underlying = Int
    override val ident: String = "Int"
  }
  case object ELong extends EType with EPrimitive {
    override type Underlying = Long
    override val ident: String = "Long"
  }
  case object EString extends EType with EPrimitive {
    override type Underlying = String
    override val ident: String = "String"
  }
  case object EByteVector extends EType with EPrimitive {
    override type Underlying = Array[Byte]
    override val ident: String = "Bytes"
  }

  sealed trait Parametrized

  sealed trait ECollection extends EType

  case class EList(valT: EType) extends EType with ECollection {
    override type Underlying = List[valT.Underlying]
    override val ident: String = "List"

    override def equals(obj: Any): Boolean = obj match {
      case l: EList => l.valT == this.valT
      case _ => false
    }
  }

  case class EDict(keyT: EType, valT: EType) extends EType with ECollection {
    override type Underlying = Map[keyT.Underlying, valT.Underlying]
    override val ident: String = "Dict"

    override def equals(obj: Any): Boolean = obj match {
      case d: EDict => d.keyT == this.keyT && d.valT == this.valT
      case _ => false
    }
  }

  case class EOption(inT: EType) extends EType {
    override type Underlying = Option[inT.Underlying]
    override val ident: String = "Option"

    override def equals(obj: Any): Boolean = obj match {
      case o: EOption => o.inT == this.inT
      case _ => false
    }
  }
}
