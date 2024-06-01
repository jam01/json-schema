package io.github.jam01

import io.github.jam01.json_schema.SchemaValidator.of
import upickle.core.Visitor

import scala.collection.mutable

package object json_schema {
  def validator(sch: Schema,
                schemaRegistry: collection.Map[Uri, Schema] = Map.empty,
                config: Config = Config.Default): Visitor[?, OutputUnit] = {
    val ctx = SimpleContext(schemaRegistry, config)
    PointerDelegate(ctx, SchemaValidator.of(sch, ctx, JsonPointer.Root, None))
  } // perf: when Mode.Flag, don't need pointer tracker

  def tryDialect(sch: Schema, registry: collection.Map[Uri, Schema], dialects: Seq[Dialect] = Seq(Dialect._2020_12)): Option[Dialect] = {
    if (sch.isInstanceOf[BooleanSchema]) return Some(Dialect._2020_12)

    val dialectUri = sch.asInstanceOf[ObjectSchema].getMetaSchema
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
