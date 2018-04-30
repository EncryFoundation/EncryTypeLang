package core

import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class ParserTest extends PropSpec with Matchers {

  property("Type description parsing") {
    val source =
      """
        |type Person(
        |    field name: String;
        |    field age: Int;
        |)
      """.stripMargin

    val res = Parser.parse(source)

    res.isInstanceOf[Parsed.Success[Seq[Ast.Type]]] shouldBe true
  }

  property("Schema parsing (Multiple types)") {
    val source =
      """
        |type Person(
        |    field name: String;
        |    field age: Int;
        |)
        |
        |type Customer(
        |    field name: String;
        |    field phone: String;
        |)
      """.stripMargin

    val res = Parser.parse(source)

    res.isInstanceOf[Parsed.Success[Seq[Ast.Type]]] shouldBe true
  }
}
