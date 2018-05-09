package encrytl.core

import org.json4s._
import org.json4s.jackson.JsonMethods._

object JsonUtil {

  implicit val formats = DefaultFormats

  case class Entity(variable: String,
                    string: Option[String],
                    int: Option[Int],
                    long: Option[Long],
                    boolean: Option[Boolean],
                    list: Option[List[Int]],
                    listStrings: Option[List[StringVar]],
                    listVars: Option[List[Entity]])

  case class StringVar(varName: String, value: String)

  def parseJson(json: String): List[Entity] = parse(json).extract[List[Entity]]
}
