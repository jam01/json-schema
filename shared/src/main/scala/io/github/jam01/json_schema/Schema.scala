package io.github.jam01.json_schema

sealed trait Schema {
  def schBy(ptr: JsonPointer): Schema = {
    if (ptr.refTokens.length == 1 && ptr.refTokens.head.isEmpty) return this
    schBy0(ptr)
  }

  def schBy0(ptr: JsonPointer): Schema
}

sealed abstract class BooleanSchema extends Schema {
  def value: Boolean

  override def schBy0(ptr: JsonPointer): Schema = {
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

// see: https://github.com/BalmungSan/scala-multifile-adt
// https://users.scala-lang.org/t/refactoring-class-hierarchy-into-adt/6997
// https://contributors.scala-lang.org/t/pre-sip-sealed-enumerating-allowed-sub-types/3768
// https://contributors.scala-lang.org/t/possibility-to-spread-sealed-trait-to-different-files/5304

/**
 *
 * @param mMap     underlying Map of keywords -> values
 * @param docbase  a base uri assigned by the application
 * @param prel     the relative JSON pointer from the parent schema
 * @param parent   the parent schema, if any
 */
final case class ObjectSchema(protected val mMap: LinkedHashMap[String, Any],
                              docbase: String,
                              prel: Option[String] = None,
                              parent: Option[ObjectSchema] = None) extends ObjSchema with Schema
