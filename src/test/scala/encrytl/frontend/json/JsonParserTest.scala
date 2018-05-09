package encrytl.frontend.json

import org.scalatest.{Matchers, PropSpec}

class JsonParserTest extends PropSpec with Matchers {

  property("Json parsing") {

    val json =
      """
        |{
        |  "person" : {
        |    "name" : "John",
        |    "age" : 28
        |  },
        |  "height" : 168
        |}
      """.stripMargin

    val parsed = JsonParser.parse(json)

    parsed.isSuccess shouldBe true
  }
}
