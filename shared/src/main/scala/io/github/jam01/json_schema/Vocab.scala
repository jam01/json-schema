package io.github.jam01.json_schema

trait Vocab[-T](val schema: ObjectSchema, 
                val dynParent: Option[Vocab[?]]) extends JsonVisitor[T, collection.Seq[OutputUnit]]

trait VocabFactory[B <: Vocab[?]] {
  def uri: String
  def from(schema: ObjectSchema,
           ctx: Context,
           path: JsonPointer,
           dynParent: Option[Vocab[?]]): B
  def appliesTo(schema: ObjectSchema): Boolean
}
