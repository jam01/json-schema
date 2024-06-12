package io.github.jam01.json_schema

/**
 * A JSON schema dialect.
 *
 * @param uri identifier for this dialect
 * @param vocabularies set of [[VocabFactory]]s in this dialect
 */
case class Dialect(uri: Uri, vocabularies: Seq[VocabFactory[?]])

object Dialect {
  /**
   * A [[Dialect]] implementing the <a href=https://json-schema.org/draft/2020-12/json-schema-core>JSON Schema 2020-12 specification</a>
   * @see <a href=https://json-schema.org/draft/2020-12/schema>JSON Schema 2020-12 Meta-Schema</a>
   */
  val _2020_12: Dialect  = Dialect(Uri("https://json-schema.org/draft/2020-12/schema"),
    Seq(vocab.Validation, vocab.Applicator, vocab.Core, vocab.Unevaluated))
}

/**
 * A JSON Schema validator implementing a single vocabulary
 *
 * A schema vocabulary, or simply a vocabulary, is a set of keywords, their syntax, and their semantics.
 *
 * @see <a href="https://json-schema.org/draft/2020-12/json-schema-core#section-4.3.3">JSON Schema § Vocabularies</a>
 * @see [[upickle.core.Visitor]]
 * @tparam T the type of subvisitor result for array or object elements. Usually safe to wildcard.
 */
trait Vocab[-T](val schema: ObjectSchema,
                val dynParent: Option[Vocab[?]]) extends JsonVisitor[T, Seq[OutputUnit]]

/**
 * Factory for creating vocabulary validator instances.
 *
 * @see [[upickle.core.Visitor]]
 * @tparam B the type of subvisitor result for array or object elements. Usually safe to wildcard.
 */
trait VocabFactory[B <: Vocab[?]] {
  /**
   * @return the URI identifying this vocabulary
   */
  def uri: String

  /**
   * Create a vocabulary validator instance.
   * 
   * @param schema the schema to apply
   * @param ctx validation context
   * @param path schema evaluation path
   * @param dynParent the dynamic scope parent, if any
   * @return a [[Vocab]] validator instance
   */
  def create(schema: ObjectSchema,
             ctx: Context,
             path: JsonPointer,
             dynParent: Option[Vocab[?]]): B

  /**
   * Check whether this vocabulary should be applied to the given schema.
   * 
   * @param schema the schema to apply
   * @return if the vocabulary applies
   */
  def shouldApply(schema: ObjectSchema): Boolean
}

class InvalidVectorException(val results: Seq[OutputUnit]) extends RuntimeException
