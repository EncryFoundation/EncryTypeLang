package encrytl.core

import org.scalatest.{Matchers, PropSpec}

class TypedObjectCodecSpec extends PropSpec with Matchers {

  property("Simple object encoding/decoding") {

    val obj = new TypedObject(Array.fill(8)(1.toByte), Seq("name" -> Val(Types.EString, "John"), "age" -> Val(Types.EInt, 28)))

    val objEnc = TypedObjectCodec.encode(obj)

    val objDecTry = TypedObjectCodec.decode(objEnc)

    objDecTry.isSuccess shouldBe true

    objDecTry.get.typeFingerprint sameElements obj.typeFingerprint shouldBe true
  }
}
