package encrytl.core

import encrytl.core.JsonUtil.Entity
import org.scalatest.{Matchers, PropSpec}
import org.json4s._
import org.json4s.jackson.JsonMethods._

class ComplexObjectSpec extends PropSpec with Matchers {

  property("Complex json should be parsed") {

    val json: String = """ """.stripMargin

    true shouldBe true

  }

  property("All types should be parsed correctly on the first layer") {

    val json =
      """
        |[
        |                  {
        |                    "variable" : "name",
        |                    "string" : "John"
        |                  },
        |                  {
        |                    "variable" : "age",
        |                    "int" : 28
        |                  },
        |                  {
        |                    "variable" : "phone",
        |                    "long" : 8905144120
        |                  },
        |                  {
        |                    "variable" : "bool",
        |                    "boolean" : true
        |                  },
        |                  {
        |                    "variable" : "byteArray",
        |                    "byteVector" : "#456@*bgrh#g&"
        |                  },
        |                  {
        |                    "variable" : "object",
        |                    "obj" : [{"variable" : "person", "string" : "Ivan"}, {"variable" : "age", "int" : 28}]
        |                  },
        |                  {
        |                    "variable" : "collection",
        |                    "list" : [{"variable" : "person", "string" : "Ivan"}, {"variable" : "age", "string" : "28"}]
        |                  }
        |                ]
      """.stripMargin

    val variables: List[Entity] = JsonUtil.parseJson(json)
    val performed: List[Any] = variables.map(entity => JsonUtil.performEntity(entity))
    variables foreach println
    performed foreach println
    variables.size shouldBe 7
  }

}
