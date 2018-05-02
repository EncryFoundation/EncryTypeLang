package core

import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class ParserSpec extends PropSpec with Matchers {

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
        |type User(
        |    field id: Int;
        |    field aliases: List;
        |    field person: Person;
        |    field email: String;
        |)
      """.stripMargin

    val res = Parser.parse(source)

    res.isInstanceOf[Parsed.Success[Seq[Ast.Type]]] shouldBe true
  }

  property("Type description with parametrized type") {
    val source =
      """
        |type User(
        |    field name: String;
        |    field aliases: List[String];
        |)
      """.stripMargin

    val res = Parser.parse(source)

    res.isInstanceOf[Parsed.Success[Seq[Ast.Type]]] shouldBe true
  }
}
