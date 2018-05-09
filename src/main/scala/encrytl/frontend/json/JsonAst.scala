package encrytl.frontend.json

object JsonAst {

  sealed trait JsonVal extends Any {
    def value: Any
    def apply(i: Int): JsonVal = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): JsonVal =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with JsonVal
  case class Obj(value: (java.lang.String, JsonVal)*) extends AnyVal with JsonVal
  case class Arr(value: JsonVal*) extends AnyVal with JsonVal
  case class Num(value: Double) extends AnyVal with JsonVal
  case object False extends JsonVal {
    def value = false
  }
  case object True extends JsonVal {
    def value = true
  }
  case object Null extends JsonVal {
    def value = null
  }
}
