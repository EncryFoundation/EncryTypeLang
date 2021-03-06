package encrytl.frontend

import org.scalatest.{Matchers, PropSpec}

class ParserSpec extends PropSpec with Matchers {

  property("Simple schema parsing") {
    val source =
      """
        |schema Person:Object(
        |    name:String;
        |    age:Int;
        |)
      """.stripMargin

    val res = Parser.parse(source)

    res.isSuccess shouldBe true
  }

  property("Schema with nested objects parsing") {
    val source =
      """
        |schema User:Object(
        |    person:Object(name:String;age:Int);
        |    email:String;
        |)
      """.stripMargin

    val res = Parser.parse(source)

    res.isSuccess shouldBe true
  }

  property("Schema with parametrized types parsing") {
    val source =
      """
        |schema User:Object(
        |    person:Object(name:String;age:Int);
        |    sessions:List[Object(id:Long;time:Long)];
        |    email:String;
        |)
      """.stripMargin

    val res = Parser.parse(source)

    res.isSuccess shouldBe true
  }
}
