package encrytl.core

import encrytl.frontend.{Ast, Parser}
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class ComplexObjectSpec extends PropSpec with Matchers {

  property("Json should be parsed") {

    val source: String =
      """
        |  [
        |    {
        |      "key" : "person",
        |      "type" : "Object",
        |      "value" : [
        |          {
        |            "key" : "name",
        |            "type" : "String",
        |            "value" : "John"
        |          },
        |          {
        |            "key" : "age",
        |            "type" : "Int",
        |            "value" : 28
        |          }
        |        ]
        |    }
        |  ]
      """.stripMargin
  }

}
