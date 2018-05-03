package encrytl.core

import org.scalatest.{Matchers, PropSpec}

class TypesCodecShallowSpec extends PropSpec with Matchers {

  property("Parametrized type encoding and decoding") {

    val t = Types.EDict(Types.EString, Types.ELong)

    val encoded = TypesCodecShallow.encode(t)

    val decodedTry = TypesCodecShallow.decode(encoded)

    decodedTry.isSuccess shouldBe true

    (decodedTry.get == t) shouldBe true
  }

  property("Product type encoding and decoding") {

    val t = Types.EProduct("Person", List("name" -> Types.EString, "age" -> Types.EInt))

    val encoded = TypesCodecShallow.encode(t)

    val decodedTry = TypesCodecShallow.decode(encoded)

    decodedTry.isSuccess shouldBe true

    (decodedTry.get.asInstanceOf[Types.ShallowProduct].fingerprint sameElements t.fingerprint) shouldBe true
  }
}
