package encrytl.core

import encrytl.frontend.{Ast, Parser}
import fastparse.all._
import org.scalatest.{Matchers, PropSpec}

class InterpreterSpec extends PropSpec with Matchers {

  property("Valid schema interpretation") {

    val Schema1 = Schema("Person", Types.EProduct(List("name" -> Types.EString, "age" -> Types.EInt)))
    val Schema2 = Schema("Point", Types.EProduct(List("x" -> Types.EInt, "y" -> Types.EInt)))

    val source =
      """
        |schema Person:Object(
        |    name:String;
        |    age:Int;
        |)
        |
        |schema Point:Object(
        |    x:Int;
        |    y:Int;
        |)
      """.stripMargin

    val schemas = Parser.parse(source).get

    val interpreter = new Interpreter()

    val schemasInterp = schemas.map(interpreter.interpret)

    schemasInterp.forall(_.isSuccess) shouldBe true

    Seq(Schema1, Schema2).zip(schemasInterp.map(_.get)).forall { case (s1, s2) => s1 == s2 } shouldBe true
  }

  property("Invalid schema interpretation (Unresolved type)") {

    val source =
      """
        |schema Person:Object(
        |    name:String;
        |    age:Age;
        |)
      """.stripMargin

    val types = Parser.parse(source).get

    val schemas = Parser.parse(source).get

    val interpreter = new Interpreter()

    val schemasInterp = schemas.map(interpreter.interpret)

    schemasInterp.forall(_.isSuccess) shouldBe false
  }
}
