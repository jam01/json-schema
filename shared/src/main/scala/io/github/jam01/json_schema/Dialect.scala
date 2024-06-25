package io.github.jam01.json_schema

import scala.collection.mutable

/**
 * A JSON schema dialect.
 *
 * @param uri identifier for this dialect
 * @param vocabularies set of [[VocabFactory]]s in this dialect
 */
case class Dialect(uri: Uri, vocabularies: Seq[VocabFactory[?]]) {
  def vocabsFor(schema: ObjectSchema): Seq[VocabFactory[?]] = {
    vocabularies.filter(v => v.shouldApply(schema))
  }
}

object Dialect {
  /**
   * A [[Dialect]] implementing the <a href=https://json-schema.org/draft/2020-12/json-schema-core>JSON Schema 2020-12 specification</a>
   * @see <a href=https://json-schema.org/draft/2020-12/schema>JSON Schema 2020-12 Meta-Schema</a>
   */
  val _2020_12_Basic: Dialect = Dialect(Uri("https://github.io/jam01/json_schema/2012-10/basic"),
    Seq(vocab.Validation, vocab.Applicator, vocab.Core, vocab.Unevaluated))

  val _2020_12_FormatAssertion: Dialect = Dialect(Uri("https://github.io/jam01/json_schema/2012-10/format-assertion"),
    Seq(vocab.Validation, vocab.Applicator, vocab.Core, vocab.Unevaluated, vocab.FormatAssertion))

  val _2020_12_Full: Dialect = Dialect(Uri("https://json-schema.org/draft/2020-12/schema"),
    Seq(vocab.Validation, vocab.Applicator, vocab.Core, vocab.Unevaluated, vocab.Format, vocab.Metadata, vocab.Content))

  /**
   * Creates a dialect for the given schema.
   * <br/><br/>
   * This function will attempt to find or create a dialect based on the meta-schema identifier of the given schema. It
   * will lookup the referenced dialect in the given dialects, if not found it will try to create one based on the
   * referenced vocabularies in the meta-schema and the available vocabularies of the given dialects.
   *
   * @param schema for which to find or create a dialect
   * @param dialects to lookup existing [[Dialect]] or use their [[Vocab]]s to create a new one
   * @param registry to lookup the meta-schema
   * @return optionally the found or constructed [[Dialect]]
   */
  def tryDialect(schema: Schema,
                 dialects: Seq[Dialect] = Seq(Dialect._2020_12_Full),
                 registry: Registry): Option[Dialect] = {
    if (schema.isInstanceOf[BooleanSchema]) return Some(Dialect._2020_12_Basic)

    val dialectUri = schema.asInstanceOf[ObjectSchema].getMetaSchema
    if (dialectUri.isEmpty) return Some(Dialect._2020_12_Basic)

    val found = dialects.find(d => dialectUri.contains(d.uri))
    if (found.nonEmpty) return found

    val msch = registry.get(dialectUri.get)
    if (msch.isEmpty) return None
    if (msch.get.isInstanceOf[BooleanSchema]) return Some(Dialect._2020_12_Basic)

    val vocabs = msch.get.asInstanceOf[ObjectSchema].getVocabularies
    if (vocabs.isEmpty) return None

    val supported = dialects.flatMap(d => d.vocabularies)
    val res = new mutable.ListBuffer[VocabFactory[?]]
    val it = vocabs.iterator
    while (it.hasNext) {
      val (uri, req) = it.next()
      val found0 = supported.find(vf => vf.uri == uri)
      if (found0.isEmpty && req) return None
      else if (found0.nonEmpty) res.addOne(found0.get)
    }

    Some(Dialect(dialectUri.get, res.toSeq))
  }
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

/**
 * An exception to interrupt validation vector values, i.e.: as JSON arrays and objects.
 * 
 * @param results the accumulated results at the point of failure
 */
class InvalidVectorException(val results: Seq[OutputUnit]) extends RuntimeException
