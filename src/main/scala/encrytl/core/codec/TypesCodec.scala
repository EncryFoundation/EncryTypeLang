package encrytl.core.codec

import encrytl.core.Types
import scodec.Codec
import scodec.codecs.{Discriminated, uint4}

object TypesCodec {

  import scodec.codecs.implicits._
  import Types._

  implicit def dTpe = Discriminated[EType, Int](uint4)
  implicit def dInt = dTpe.bind[Types.EInt.type](0)
  implicit def dLong = dTpe.bind[Types.ELong.type](1)
  implicit def dStr = dTpe.bind[Types.EString.type](2)
  implicit def dBytes = dTpe.bind[Types.EByteVector.type](3)
  implicit def dAny = dTpe.bind[Types.EAny.type](4)
  implicit def dBool = dTpe.bind[Types.EBoolean.type](5)
  implicit def dList = dTpe.bind[Types.EList](5)
  implicit def dOpt = dTpe.bind[Types.EOption](7)
  implicit def dProd = dTpe.bind[Types.EProduct](8)
  implicit def dSProd = dTpe.bind[Types.ShallowProduct](9)
  implicit def dNit = dTpe.bind[Types.NIType.type](10)

  val codec: Codec[EType] = Codec[EType]
}
