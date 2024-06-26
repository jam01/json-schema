package io.github.jam01.json_schema

import io.github.jam01.json_schema.vocab.Core

import scala.collection.mutable

/**
 * A base class for implementing vocabulary validators using a given Schema.
 *
 * Implementations can validate whole vocabularies or single keywords only, as it returns a sequence of OutputUnits.
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

  /**
   * Create a keyword [[OutputUnit]] with the given parameters.
   *
   * This method should be used as it will take care of implementation details such as delegating unit creation to the
   * effective output format, potentially avoiding object instantiation (see [[OutputFormat]]). More importantly this
   * also offer the given annotation to the internal annotation dependency tracking.
   *
   * @param isValid whether the unit is valid
   * @param kw the generating keyword
   * @param error the keyword error message
   * @param errors the keyword error sub-schema units
   * @param annotation the keyword annotation
   * @param verbose the keyword verbose sub-schema units
   * @return the resulting unit
   */
  protected inline def mkUnit(isValid: Boolean,
             kw: String,
             error: String | Null = null,
             errors: Seq[OutputUnit] = Nil,
             annotation: Value | Null = null,
             verbose: Seq[OutputUnit] = Nil): OutputUnit = { // perf: kwLoc and absKwLoc may be computed and discarded, inline?
    val kwLoc = path.appended(kw)
    val absKwLoc = if (hasRef) schema.location.appendedFragment(s"/$kw") else null
    if (isValid && annotation != null) ctx.offerAnnotation(kwLoc, annotation)
    ctx.config.format.make(isValid, kwLoc, absKwLoc, ctx.instanceLoc, error, errors, ctx.config.allowList.ifAllowed(kw, annotation), verbose)
  }

  /**
   * Accumulate the given unit in the results, according to the effective output format.
   *
   * @param buff the accumulated results
   * @param unit the unit to accumulate
   * @return whether validation should continue; enables short circuiting validation.
   */
  protected inline def accumulate(buff: mutable.Growable[OutputUnit], unit: OutputUnit): Boolean = {
    ctx.config.format.accumulate(buff, unit)
    unit.vvalid || !ctx.config.ffast
  }

  /**
   * Accumulate the unit-to-be in the results, according to the effective output format.
   *
   * This method should be used as it will take care of implementation details such as delegating unit creation to the
   * effective output format, potentially avoiding object instantiation (see [[OutputFormat]]). More importantly this
   * also offer the given annotation to the internal annotation dependency tracking.
   *
   * @param buff the accumulated results
   * @param isValid whether the unit is valid
   * @param kw the generating keyword
   * @param error the keyword error message
   * @param errors the keyword error sub-schema units
   * @param annotation the keyword annotation
   * @param verbose the keyword verbose sub-schema units
   * @return whether validation should continue; enables short circuiting validation.
   */
  protected inline def accumulate(buff: mutable.Growable[OutputUnit],
                           isValid: Boolean,
                           kw: String,
                           error: String | Null = null,
                           errors: Seq[OutputUnit] = Nil,
                           annotation: Value | Null = null,
                           verbose: Seq[OutputUnit] = Nil): Boolean = {
    val kwLoc = path.appended(kw)
    val absKwLoc = if (hasRef) schema.location.appendedFragment(s"/$kw") else null
    if (isValid && annotation != null) ctx.offerAnnotation(kwLoc, annotation)
    ctx.config.format.accumulate(buff, isValid, kwLoc, absKwLoc, ctx.instanceLoc, error, errors, ctx.config.allowList.ifAllowed(kw, annotation), verbose)
    isValid || !ctx.config.ffast
  }

  /**
   * Accumulate the unit-to-be in the results, potentially throwing if the result is not valid and `config.ffast` is
   * enabled.
   *
   * Should only be called form visitArr/Obj constructors or methods.
   *
   * @throws InvalidVectorException signals to short-circuit validation
   * @param buff the accumulated results
   * @param isValid whether the unit is valid
   * @param kw the generating keyword
   * @param error the keyword error message
   * @param errors the keyword error sub-schema units
   * @param annotation the keyword annotation
   * @param verbose the keyword verbose sub-schema units
   */
  protected inline def accumulateVec(buff: mutable.Buffer[OutputUnit],
                              isValid: Boolean,
                              kw: String,
                              error: String | Null = null,
                              errors: Seq[OutputUnit] = Nil,
                              annotation: Value | Null = null,
                              verbose: Seq[OutputUnit] = Nil): Unit = {
    if (accumulate(buff, isValid, kw, error, errors, annotation, verbose)) ()
    else throw new InvalidVectorException(Seq.from(buff))
  }

  /**
   * Accumulate the given unit in the results, potentially throwing if the result is not valid and `config.ffast` is
   * enabled.
   *
   * Should only be called form visitArr/Obj constructors or methods.
   *
   * @throws InvalidVectorException signals to short-circuit validation
   * @param buff the accumulated results
   * @param unit the unit to accumulate
   */
  protected inline def accumulateVec(buff: mutable.Buffer[OutputUnit], unit: OutputUnit): Unit = {
    if (accumulate(buff, unit)) ()
    else throw new InvalidVectorException(Seq.from(buff))
  }

  /**
   * If not valid and `config.ffast` is enabled, accumulate the unit-to-be and throw.
   * 
   * Should only be called  
   * be called when a vector's child is invalid, and from visitKey/Value methods only.
   * 
   * @throws InvalidVectorException signals to short-circuit validation
   * @param buff the accumulated results
   * @param isValid whether the unit is valid
   * @param kw the generating keyword
   * @param error the keyword error message
   * @param errors the keyword error sub-schema units
   * @param annotation the keyword annotation
   * @param verbose the keyword verbose sub-schema units
   */
  protected inline def ffastChild(buff: mutable.Buffer[OutputUnit],
                                  isValid: Boolean,
                                  kw: String,
                                  error: String | Null = null,
                                  errors: Seq[OutputUnit] = Nil,
                                  annotation: Value | Null = null,
                                  verbose: Seq[OutputUnit] = Nil): Unit = {
    if (!isValid && ctx.config.ffast) {
      accumulate(buff, isValid, kw, error, errors, annotation, verbose)
      throw new InvalidVectorException(Seq.from(buff))
    }
  }

  /**
   * Compose a keyword output unit with sub-schema results, according to the effective output format.
   *
   * @param kw the generating keyword
   * @param results the keyword sub-schema results
   * @param ann the keyword annotation
   * @return the resulting unit
   */
  protected def compose(kw: String, results: Seq[OutputUnit], ann: Value | Null = null): OutputUnit = {
    val (valid, invalid) = results.partition(_.vvalid)
    mkUnit(invalid.isEmpty, kw, errors = invalid, annotation = ann, verbose = valid)
  }

  private def hasRef: Boolean = {
    path.refTokens.exists(s => Core._Ref == s || Core._DynRef == s)
  }
}
