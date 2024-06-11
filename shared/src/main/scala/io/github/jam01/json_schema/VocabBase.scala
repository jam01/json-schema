package io.github.jam01.json_schema

import io.github.jam01.json_schema.vocab.Core

import scala.collection.mutable

/**
 * A base class for implementing validators using a given Schema. 
 *
 * Implementations can validate whole vocabularies or single keywords only, as it returns a sequence of OutputUnits  
 *
 * @param schema    schema to apply
 * @param ctx       validation context
 * @param path      the path followed to the given schema
 * @param dynParent dynamic scope parent validator
 */
abstract class VocabBase(schema: ObjectSchema,
                         val ctx: Context,
                         val path: JsonPointer,
                         dynParent: Option[Vocab[?]]) extends Vocab[Nothing](schema, dynParent) {
  
  // TODO: reuse path.appended(kw) across implementations

  protected def mkUnit(isValid: Boolean,
             kw: String,
             error: String | Null = null,
             errors: Seq[OutputUnit] = Nil,
             annotation: Value | Null = null,
             verbose: Seq[OutputUnit] = Nil): OutputUnit = {
    val kwLoc = path.appended(kw)
    val absKwLoc = if (hasRef) schema.location.appendedFragment(s"/$kw") else null
    if (isValid && annotation != null) ctx.offerAnnotation(kwLoc, annotation)
    ctx.config.format.make(isValid, kwLoc, absKwLoc, ctx.instanceLoc, error, errors, ctx.config.allowList.ifAllowed(kw, annotation), verbose)
  }

  // perf: @inline?
  protected def accumulate(buff: mutable.Growable[OutputUnit], unit: OutputUnit): Boolean = {
    ctx.config.format.accumulate(buff, unit)
    unit.vvalid || !ctx.config.ffast // perf: cost of boolean logic when config.ffast is unchanging
  }
  }

  private def hasRef: Boolean = {
    path.refTokens.exists(s => Core._Ref == s || Core._DynRef == s)
  }

  protected def compose(kw: String, units: Seq[OutputUnit], ann: Value | Null = null): OutputUnit = {
    val (valid, invalid) = units.partition(_.vvalid)
    mkUnit(invalid.isEmpty, kw, errors = invalid, annotation = ann, verbose = valid)
  }
}

trait VocabBaseFactory extends VocabFactory[VocabBase]
