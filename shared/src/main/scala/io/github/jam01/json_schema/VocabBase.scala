/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import io.github.jam01.json_schema.vocab.Core
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

import scala.collection.mutable

/**
 * A base class for implementing vocabulary validators using a given [[ObjectSchema]].
 *
 * Implementations can validate whole vocabularies or single keywords only, as it returns a collection of [[OutputUnit]].
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
   * This method should be used by implementations as it will take care of details such as delegating unit creation to
   * the effective [[OutputFormat]], potentially avoiding object instantiation. More importantly this also offers the
   * given annotation to the internal annotation dependency tracking mechanism.
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
   * This method should be used by implementations as it will take care of details such as delegating unit creation to
   * the effective [[OutputFormat]], potentially avoiding object instantiation. More importantly this also offers the
   * given annotation to the internal annotation dependency tracking mechanism.
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
    // Skip kwLoc/absKwLoc compute when the format would drop a valid+non-annotated unit;
    // the format declares its retention preference via `retainsValidUnannotated`.
    if (!isValid || annotation != null || ctx.config.format.retainsValidUnannotated) {
      val kwLoc = path.appended(kw)
      val absKwLoc = if (hasRef) schema.location.appendedFragment(s"/$kw") else null
      if (isValid && annotation != null) ctx.offerAnnotation(kwLoc, annotation)
      ctx.config.format.accumulate(buff, isValid, kwLoc, absKwLoc, ctx.instanceLoc, error, errors, ctx.config.allowList.ifAllowed(kw, annotation), verbose)
    }
    isValid || !ctx.config.ffast
  }

  /**
   * Accumulate the unit-to-be in the results, potentially throwing if the result is not valid and `config.ffast` is
   * enabled.
   *
   * Should only be called form `visitArr/Obj` constructors or methods.
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
   * Should only be called form `visitArr/Obj` constructors or methods.
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
   * be called when a vector's child is invalid, and from `visitKey/Value` methods only.
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
   * Compose a keyword output unit with sub-schema results, according to the effective [[OutputFormat]].
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

  private lazy val hasRef: Boolean =
    path.refTokens.exists(s => Core._Ref == s || Core._DynRef == s)

  // Default no-op `visit*` implementations. Subclasses override only the JSON node types their
  // keywords apply to. Returning `Nil` from a visit means this vocab produces no unit for that
  // node — equivalent to opting out for that input kind.
  override def visitNull(index: Int): Seq[OutputUnit] = Nil
  override def visitTrue(index: Int): Seq[OutputUnit] = Nil
  override def visitFalse(index: Int): Seq[OutputUnit] = Nil
  override def visitInt64(i: Long, index: Int): Seq[OutputUnit] = Nil
  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = Nil
  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Seq[OutputUnit] = Nil
  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = Nil
  override def visitArray(length: Int, index: Int): ArrVisitor[Nothing, Seq[OutputUnit]] = VocabBase.NoArr
  override def visitObject(length: Int, index: Int): ObjVisitor[Nothing, Seq[OutputUnit]] = VocabBase.NoObj
}

object VocabBase {
  private val NoArr: ArrVisitor[Nothing, Seq[OutputUnit]] = new ArrVisitor[Any, Seq[OutputUnit]] {
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }
  private val NoObj: ObjVisitor[Nothing, Seq[OutputUnit]] = new ObjVisitor[Any, Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }
}
