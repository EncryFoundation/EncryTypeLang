package encrytl.core

import org.scalatest.{Matchers, PropSpec}

class TypedObjectJsonCodecTest extends PropSpec with Matchers {

  property("Complex object encoding/decoding") {

    val obj = new TypedObject(Array.fill(8)(1.toByte), Seq("name" -> Val(Types.EString, "John"), "age" -> Val(Types.EInt, 28)))

    val objEnc = TypedObjectJsonCodec.encode(obj)

    println(objEnc)

    val objDecTry = TypedObjectJsonCodec.decode(objEnc)

    objDecTry.isSuccess shouldBe true

    objDecTry.get.typeFingerprint sameElements obj.typeFingerprint shouldBe true
  }
}
