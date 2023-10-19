package io.github.jam01.json_schema

sealed trait Schema {
  def getSchemaByPointer(ptr: JsonPointer): Schema
}

sealed abstract class BooleanSchema extends Schema {
  def value: Boolean

  override def getSchemaByPointer(ptr: JsonPointer): Schema = {
    if (ptr.refTokens.length == 1 && ptr.refTokens.head.isEmpty) return this
    throw new IllegalStateException("cannot evaluate a JSON pointer against a boolean schema")
  }
}

object BooleanSchema {
  def apply(value: Boolean): BooleanSchema = if (value) True else False

  def unapply(bs: BooleanSchema): Some[Boolean] = Some(bs.value)
}

case object True extends BooleanSchema {
  def value = true
}

case object False extends BooleanSchema {
  def value = false
}

// see: https://contributors.scala-lang.org/t/pre-sip-sealed-enumerating-allowed-sub-types/3768
// https://contributors.scala-lang.org/t/possibility-to-spread-sealed-trait-to-different-files/5304
// https://users.scala-lang.org/t/refactoring-class-hierarchy-into-adt/6997
// https://github.com/BalmungSan/scala-multifile-adt
final case class ObjectSchema(private val mMap: LinkedHashMap[String, Any],
                        private val base: String,
                        private val location: String = null) extends ObjSchema(mMap, base, location) with Schema
