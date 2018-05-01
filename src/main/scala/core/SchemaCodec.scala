package core

import scodec.Codec
import scodec.codecs.{Discriminated, uint4}

object SchemaCodec {

  import Schema._
  import Types._

  import scodec.codecs.implicits._

  implicit def dTpe = Discriminated[EType, Int](uint4)
  implicit def dInt = dTpe.bind[Types.EInt.type](0)
  implicit def dLong = dTpe.bind[Types.ELong.type](1)
  implicit def dStr = dTpe.bind[Types.EString.type](2)
  implicit def dBytes = dTpe.bind[Types.EByteVector.type](3)
  implicit def dAny = dTpe.bind[Types.EAny.type](4)
  implicit def dBool = dTpe.bind[Types.EBoolean.type](5)
  implicit def dList = dTpe.bind[Types.EList](5)
  implicit def dDict = dTpe.bind[Types.EDict](6)
  implicit def dOpt = dTpe.bind[Types.EOption](7)
  implicit def dProd = dTpe.bind[Types.EProduct](8)

  implicit def dVal = Discriminated[BoxedVal, Int](uint4)
  implicit def dIntB = dVal.bind[IntBox](0)
  implicit def dLongB = dVal.bind[LongBox](1)
  implicit def dBoolB = dVal.bind[BoolBox](2)
  implicit def dStrB = dVal.bind[StrBox](3)
  implicit def dBytesB = dVal.bind[BytesBox](4)
  implicit def dObjB = dVal.bind[ObjectBox](5)
  implicit def dListB = dVal.bind[ListBox](6)
  implicit def dDictB = dVal.bind[DictBox](7)
  implicit def dOptB = dVal.bind[OptionBox](8)

  //val codec: Codec[Schema] = Codec[Schema]
}
