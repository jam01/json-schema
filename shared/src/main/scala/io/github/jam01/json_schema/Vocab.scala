package io.github.jam01.json_schema

trait Vocab[-T] extends JsonVisitor[T, collection.Seq[OutputUnit]]
trait VocabFactory[A <: Vocab[?], B <: Vocab[?]] {
  def uri: String
  
  def from(schema: ObjectSchema,
           ctx: Context,
           path: JsonPointer,
           dynParent: Option[A]): B
  
  def appliesTo(schema: ObjectSchema): Boolean
}
