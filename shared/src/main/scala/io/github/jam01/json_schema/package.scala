package io.github.jam01

import io.github.jam01.json_schema.SchemaValidator.of
import upickle.core.Visitor

import scala.collection.mutable

package object json_schema {

  /**
   * Creates a validator visitor for the given schema and configuration..
   * 
   * Visiting this validator results in the validation results for the visited structure.
   *
   * @param schema schema to apply
   * @param config validation configuration
   * @param schemaRegistry registry to lookup referenced schemas
   * @return the schema validator visitor
   */
  def validator(schema: Schema,
                config: Config = Config.Default,
                schemaRegistry: collection.Map[Uri, Schema] = Map.empty): Visitor[?, OutputUnit] = {
    val ctx = SimpleContext(schemaRegistry, config)
    PointerDelegate(ctx, SchemaValidator.of(schema, ctx, JsonPointer.Root, None))
  } // perf: when Mode.Flag, don't need pointer tracker

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
                 dialects: Seq[Dialect] = Seq(Dialect._2020_12),
                 registry: collection.Map[Uri, Schema]): Option[Dialect] = {
    if (schema.isInstanceOf[BooleanSchema]) return Some(Dialect._2020_12)

    val dialectUri = schema.asInstanceOf[ObjectSchema].getMetaSchema
    if (dialectUri.isEmpty) return Some(Dialect._2020_12)

    val found = dialects.find(d => dialectUri.contains(d.uri))
    if (found.nonEmpty) return found

    val msch = registry.get(dialectUri.get)
    if (msch.isEmpty) return None
    if (msch.get.isInstanceOf[BooleanSchema]) return Some(Dialect._2020_12)

    val vocabs = msch.get.asInstanceOf[ObjectSchema].getVocabularies
    if (vocabs.isEmpty) return None

    val supported = dialects.flatMap(d => d.vocabularies)
    val res = mutable.Buffer[VocabFactory[?]]()
    for ((uri, req) <- vocabs) {
      val found0 = supported.find(vf => vf.uri == uri)
      if (found0.isEmpty && req) return None
      else if (found0.nonEmpty) res.addOne(found0.get)
    }

    Some(Dialect(dialectUri.get, res.toSeq))
  }
}
