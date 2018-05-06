package encrytl.core

case class Schema(ident: String, tpe: Types.EType) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Schema => that.ident == this.ident && that.tpe == this.tpe
    case _ => false
  }
}
