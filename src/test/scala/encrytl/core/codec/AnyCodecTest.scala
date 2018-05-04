package encrytl.core.codec

import encrytl.core.Types
import org.scalatest.{Matchers, PropSpec}

class AnyCodecTest extends PropSpec with Matchers {

  property("Primitives encoding/decoding") {

    val str = "some_string"
    val int = 1234
    val bool = true

    val strEnc = AnyCodec.encode(str)
    val intEnc = AnyCodec.encode(int)
    val boolEnc = AnyCodec.encode(bool)

    val strDec = AnyCodec.decode(Types.EString, strEnc)
    val intDec = AnyCodec.decode(Types.EInt, intEnc)
    val boolDec = AnyCodec.decode(Types.EBoolean, boolEnc)

    Seq(strDec, intDec, boolDec).forall(_.isSuccess) shouldBe true

    strDec.get == str shouldBe true
    intDec.get == int shouldBe true
    boolDec.get == bool shouldBe true
  }

  property("List encoding/decoding") {

    val strList: List[String] = List("some", "string", "int", "long")

    val strListEnc = AnyCodec.encode(strList)

    val strListDec = AnyCodec.decode(Types.EList(Types.EString), strListEnc)

    strListDec.isSuccess shouldBe true

    strListDec.get sameElements strList shouldBe true
  }
}
