package encrytl.core

import org.scalatest.{Matchers, PropSpec}
import org.json4s._
import org.json4s.jackson.JsonMethods._

class ComplexObjectSpec extends PropSpec with Matchers {

  case class Entity(variable: String,
                    string: Option[String],
                    int: Option[Int],
                    list: Option[List[Int]],
                    listStrings: Option[List[StringVar]],
                    listVars: Option[List[Entity]])

  case class StringVar(varName: String, value: String)

  implicit val formats = DefaultFormats

  property("Complex json should be parsed") {

    val source1: String =
      """
        |  [
        |    {
        |      "key" : "person",
        |      "value" : [
        |          {
        |            "key" : "name",
        |            "value" : "John"
        |          },
        |          {
        |            "key" : "age",
        |            "value" : 28
        |          }
        |        ]
        |    },
        |  {
        |      "key" : "person",
        |      "value" : [
        |          {
        |            "key" : "name",
        |            "value" : "John"
        |          },
        |          {
        |            "key" : "age",
        |            "value" : 28
        |          }
        |        ]
        |    }
        |      ]
      """.stripMargin
    true shouldBe true

  }

  property("Simple json should be parsed correctly") {

    val source2 =
      """
        |[
        |                  {
        |                    "variable" : "person",
        |                    "string" : "John"
        |                  },
        |                  {
        |                    "variable" : "age",
        |                    "int" : 28
        |                  },
        |                  {
        |                    "variable" : "numbers",
        |                    "list" : [4, 5, 8]
        |                  },
        |                  {
        |                    "variable" : "strings",
        |                    "listStrings" : [{"varName" : "testName", "value" : "testValue"}]
        |                  },
        |                  {
        |                    "variable" : "vars",
        |                    "listVars" : [{"variable" : "person", "string" : "Ivan"}, {"variable" : "age", "int" : 28}]
        |                  }
        |                ]
      """.stripMargin


    val json = parse(source2).extract[List[Entity]]
    println(json)
    json.size shouldBe 5
    json.tail.head.int.get shouldBe 28

  }

}
