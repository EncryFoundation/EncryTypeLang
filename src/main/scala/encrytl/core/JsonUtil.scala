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
                    byteVector: Option[String],
                    obj: Option[List[Entity]],
                    list: Option[List[Entity]]
                   ) {
    override def toString(): String = this match {
      case Entity(title, string, None, None, None, None, None, None) => {
        val output: String = string.getOrElse("");
        s"$title: String = $output"
      }
      case Entity(title, None, int, None, None, None, None, None) => {
        val output: Int = int.getOrElse(0);
        s"$title: Int = $output"
      }
      case Entity(title, None, None, long, None, None, None, None) => {
        val output: Long = long.getOrElse(0);
        s"$title: Long = $output"
      }
      case Entity(title, None, None, None, boolean, None, None, None) => {
        val output: Boolean = boolean.getOrElse(false);
        s"$title: Boolean = $output"
      }
      case Entity(title, None, None, None, None, byteVector, None, None) => {
        val output: String = byteVector.getOrElse("");
        s"$title: ByteVector = $output"
      }
      case Entity(title, None, None, None, None, None, obj, None) => {
        val output: List[Entity] = obj.get;
        s"$title: Object = $output"
      }
      case Entity(title, None, None, None, None, None, None, list) => {
        val output: List[Entity] = list.get;
        s"$title: List = $output"
      }
      case _ => ""
    }
  }

  def parseJson(json: String): List[Entity] = parse(json).extract[List[Entity]]

  def performEntity(entity: Entity): (String, Any) = entity match {
    case Entity(title, string, None, None, None, None, None, None) => (title, string.getOrElse(""))
    case Entity(title, None, int, None, None, None, None, None) => (title, int.getOrElse(0))
    case Entity(title, None, None, long, None, None, None, None) => (title, long.getOrElse(0))
    case Entity(title, None, None, None, boolean, None, None, None) => (title, boolean.getOrElse(false))
    case Entity(title, None, None, None, None, byteVector, None, None) => (title, byteVector.getOrElse(""))
    case Entity(title, None, None, None, None, None, obj, None) => (title, obj.get.map(entity => performEntity(entity)))
    case Entity(title, None, None, None, None, None, None, list) => (title, list.get.map(entity => performEntity(entity)))
    case _ => ("unknown", "unknown")
  }
}
