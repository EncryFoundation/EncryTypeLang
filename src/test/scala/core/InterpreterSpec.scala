package core

import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class InterpreterSpec extends PropSpec with Matchers {

  property("Valid schema interpretation") {

    val Product1 = Types.EProduct("Person", List("name" -> Types.EString, "age" -> Types.EInt))
    val Product2 = Types.EProduct("Point", List("x" -> Types.EInt, "y" -> Types.EInt))

    val source =
      """
        |type Person(
        |    field name: String;
        |    field age: Int;
        |)
        |
        |type Point(
        |    field x: Int;
        |    field y: Int;
        |)
      """.stripMargin

    val types = Parser.parse(source).asInstanceOf[Parsed.Success[Seq[Ast.Type]]].value

    val res = new Interpreter().interpret(types)

    res.isRight shouldBe true

    res.right.get.zip(Seq(Product1, Product2)).foreach { case (p1, p2) => (p1 == p2) shouldBe true }
  }

  property("Invalid schema interpretation (Unresolved type)") {

    val source =
      """
        |type Person(
        |    field name: String;
        |    field age: Int;
        |    field email: Email;
        |)
      """.stripMargin

    val types = Parser.parse(source).asInstanceOf[Parsed.Success[Seq[Ast.Type]]].value

    val res = new Interpreter().interpret(types)

    res.isRight shouldBe false
  }
}
