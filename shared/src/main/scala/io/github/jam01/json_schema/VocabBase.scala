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

  protected inline def mkUnit(isValid: Boolean,
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

  // returns if to continue
  protected inline def accumulate(buff: mutable.Growable[OutputUnit], unit: OutputUnit): Boolean = {
    ctx.config.format.accumulate(buff, unit)
    unit.vvalid || !ctx.config.ffast
  }

  // returns if to continue
  protected inline def accumulate(buff: mutable.Growable[OutputUnit],
                           isValid: Boolean,
                           kw: String,
                           error: String | Null = null,
                           errors: Seq[OutputUnit] = Nil,
                           annotation: Value | Null = null,
                           verbose: Seq[OutputUnit] = Nil): Boolean = {
    val kwLoc = path.appended(kw)
    val absKwLoc = if (hasRef) schema.location.appendedFragment(s"/$kw") else null
    ctx.config.format.accumulate(buff, isValid, kwLoc, absKwLoc, ctx.instanceLoc, error, errors, ctx.config.allowList.ifAllowed(kw, annotation), verbose)
    isValid || !ctx.config.ffast
  }

  /**
   * Accumulate kw results for vectors, potentially throwing if the result is not valid and ffast is enabled. Should be
   * called form visitArr/Obj implementations only.
   * @param buff
   * @param unit
   * @return whether to continue
   */
  protected inline def accumulateVec(buff: mutable.Buffer[OutputUnit],
                              isValid: Boolean,
                              kw: String,
                              error: String | Null = null,
                              errors: Seq[OutputUnit] = Nil,
                              annotation: Value | Null = null,
                              verbose: Seq[OutputUnit] = Nil): Boolean = {
    if (accumulate(buff, isValid, kw, error, errors, annotation, verbose)) true
    else throw new InvalidVectorException(Seq.from(buff))
  }
  
  /**
   * Accumulate kw results for vectors, potentially throwing if the result is not valid and ffast is enabled. Should be
   * called form visitArr/Obj implementations only.
   * @param buff
   * @param unit
   * @return whether to continue
   */
  protected inline def accumulateVec(buff: mutable.Buffer[OutputUnit], unit: OutputUnit): Boolean = {
    if (accumulate(buff, unit)) true
    else throw new InvalidVectorException(Seq.from(buff))
  }

  /**
   * Accumulate and throw. Should be called when a vector's child is invalid, and from visitKey/Value methods only.
   * @param buff
   * @param unit
   * @return
   */
  protected inline def ffastChild(buff: mutable.Buffer[OutputUnit],
                                  isValid: Boolean,
                                  kw: String,
                                  error: String | Null = null,
                                  errors: Seq[OutputUnit] = Nil,
                                  annotation: Value | Null = null,
                                  verbose: Seq[OutputUnit] = Nil): Boolean = {
    if (isValid || !ctx.config.ffast) true
    else {
      accumulate(buff, isValid, kw, error, errors, annotation, verbose)
      throw new InvalidVectorException(Seq.from(buff))
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
