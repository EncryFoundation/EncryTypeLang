package encrytl.core.codec

import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class AnyCodecTest extends PropSpec with Matchers {

  property("encode") {

    val str = "some_string"
    val int = 1234
    val bool = true
    val strList = List("some", "string", "int", "long")
    val intList = List(1, 2, 3, 4, 5, 6)

    Try {
      AnyCodec.encode(str)
      AnyCodec.encode(int)
      AnyCodec.encode(bool)
      AnyCodec.encode(strList)
      AnyCodec.encode(intList)
    }.isSuccess shouldBe true
  }
}
