package encrytl.core

import org.scalatest.{Matchers, PropSpec}

class TypedObjectJsonCodecTest extends PropSpec with Matchers {

  property("Object encoding/decoding") {

    val obj = new TypedObject(Seq("name" -> Val(Types.EString, "John"), "age" -> Val(Types.EInt, 28)))

    val objEnc = TypedObjectJsonCodec.encode(obj)

    val objDecTry = TypedObjectJsonCodec.decode(objEnc)

    objDecTry.isSuccess shouldBe true

    objDecTry.get.fingerprint sameElements obj.fingerprint shouldBe true
  }

  property("Nested objects encoding/decoding") {

    val obj = new TypedObject(Seq("name" -> Val(Types.EString, "John"), "age" -> Val(Types.EInt, 28)))

    val obj2 = new TypedObject(Seq(
      "person" -> Val(Types.ShallowProduct(obj.fingerprint), obj),
      "age" -> Val(Types.EInt, 28))
    )

    val objEnc = TypedObjectJsonCodec.encode(obj2)

    val objDecTry = TypedObjectJsonCodec.decode(objEnc)

    objDecTry.isSuccess shouldBe true

    objDecTry.get.fingerprint sameElements obj2.fingerprint shouldBe true
  }
}
