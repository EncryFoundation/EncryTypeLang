package encrytl.core

import org.scalatest.{Matchers, PropSpec}

class TypedObjectJsonCodecTest extends PropSpec with Matchers {

  property("Object encoding/decoding") {

    val obj = new TypedObject(Array.fill(8)(1.toByte), Seq("name" -> Val(Types.EString, "John"), "age" -> Val(Types.EInt, 28)))

    val objEnc = TypedObjectJsonCodec.encode(obj)

    val objDecTry = TypedObjectJsonCodec.decode(objEnc)

    objDecTry.isSuccess shouldBe true

    objDecTry.get.typeFingerprint sameElements obj.typeFingerprint shouldBe true
  }

  property("Nested objects encoding/decoding") {

    val obj = new TypedObject(Array.fill(8)(1.toByte), Seq("name" -> Val(Types.EString, "John"), "age" -> Val(Types.EInt, 28)))

    val obj2 = new TypedObject(Array.fill(8)(1.toByte), Seq("person" -> Val(Types.ShallowProduct(Array.fill(8)(1.toByte)), obj), "age" -> Val(Types.EInt, 28)))

    val objEnc = TypedObjectJsonCodec.encode(obj2)

    val objDecTry = TypedObjectJsonCodec.decode(objEnc)

    objDecTry.isSuccess shouldBe true

    objDecTry.get.typeFingerprint sameElements obj2.typeFingerprint shouldBe true
  }
}
