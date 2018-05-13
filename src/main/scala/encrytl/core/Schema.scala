package encrytl.core

import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

case class Schema(ident: String, tpe: Types.EType) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Schema => that.ident == this.ident && that.tpe == this.tpe
    case _ => false
  }

  lazy val id: Schema.SchemaId = tpe match {
    case p: Types.EProduct => Base58.encode(p.fingerprint)
    case sp: Types.ShallowProduct => sp.fingerprintEnc
    case other: Types.EType => Base58.encode(Blake2b256.hash(other.toString.getBytes("UTF-8")).take(8))
  }
}

object Schema {

  type SchemaId = String
}
