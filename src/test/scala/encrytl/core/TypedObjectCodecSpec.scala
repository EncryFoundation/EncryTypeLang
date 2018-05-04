package encrytl.core

import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class TypedObjectCodecSpec extends PropSpec with Matchers {

  property("Simple object encoding") {

    val obj = new TypedObject(Array.fill(8)(1.toByte), Seq("name" -> Val(Types.EString, "John"), "age" -> Val(Types.EInt, 28)))

    Try(TypedObjectCodec.encode(obj)).isSuccess shouldBe true
  }
}
