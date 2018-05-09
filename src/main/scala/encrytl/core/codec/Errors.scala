package encrytl.core.codec

object Errors {

  case object IllegalTypeError extends Error("Illegal type")
  case object DecodingError extends Error("Decoding failed")
  case object NestedCollError extends Error("Nested collections are disallowed")
  case object EmptyCollError extends Error("Empty collections are disallowed")
  case object InconsistentCollError extends Error("Inconsistent collections are disallowed")
}
