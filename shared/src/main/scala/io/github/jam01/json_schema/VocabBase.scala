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
  
  // TODO: move this to ObjSchValidator 
  private var countRefs = 0
  private var head: Vocab[?] = this
  while (head.dynParent.nonEmpty) {
    countRefs += 1
    head = head.dynParent.get
  }
  // a naive way to guard against infinite loops from circular reference logic in schemas, which results in StackOverflow
  if (countRefs > 32) throw new IllegalStateException("Depth limit exceeded")

  // TODO: reuse path.appended(kw) across implementations

  protected def mkUnit(isValid: Boolean,
             kw: String,
             error: String | Null = null,
             errors: collection.Seq[OutputUnit] = Nil,
             annotation: Value | Null = null,
             verbose: collection.Seq[OutputUnit] = Nil): OutputUnit = {
    val kwLoc = path.appended(kw)
    val absKwLoc = if (hasRef) schema.location.appendedFragment(s"/$kw") else null
    if (isValid && annotation != null) ctx.offerAnnotation(kwLoc, annotation)
    ctx.config.format.make(isValid, kwLoc, absKwLoc, ctx.instanceLoc, error, errors, ctx.config.allowList.ifAllowed(kw, annotation), verbose)
  }

  // perf: @inline?
  def accumulate(units: mutable.Growable[OutputUnit], unit: OutputUnit): Boolean = {
    ctx.config.format.accumulate(units, unit)
    unit.vvalid || !ctx.config.ffast
  }
  
  def accumulateOpt(results: mutable.Growable[OutputUnit], unitOpt: Option[OutputUnit]): Boolean = {
    if (unitOpt.isEmpty) return true
    accumulate(results, unitOpt.get)
  }

  private def hasRef: Boolean = {
    path.refTokens.exists(s => Core._Ref == s || Core._DynRef == s)
  }

  protected def compose(kw: String, units: collection.Seq[OutputUnit], ann: Value | Null = null): OutputUnit = {
    val (valid, invalid) = units.partition(_.vvalid)
    mkUnit(invalid.isEmpty, kw, errors = invalid, annotation = ann, verbose = valid)
  }
}

trait VocabBaseFactory extends VocabFactory[VocabBase]
