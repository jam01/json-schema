/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Validation.*
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.math.BigInteger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class Validation private(schema: ObjectSchema,
                         ctx: Context,
                         path: JsonPointer,
                         dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {
  private val tyype: Seq[String] = schema.getAsStringArray(Tyype)
  private val const: Option[Value] = schema.get(Const)
  private val enuum: Option[Seq[Value]] = schema.getArrayOpt(Enuum)
  private val maximum: Option[Num] = schema.getNumber(Maximum)
  private val minimum: Option[Num] = schema.getNumber(Minimum)
  private val exclusiveMax: Option[Num] = schema.getNumber(ExclusiveMax)
  private val exclusiveMin: Option[Num] = schema.getNumber(ExclusiveMin)
  private val multipleOf: Option[Num] = schema.getNumber(MultipleOf)
  private val maxLength: Option[Int] = schema.getInt(MaxLength)
  private val minLength: Option[Int] = schema.getInt(MinLength)
  private val pattern: Option[CompiledPattern] = schema.getString(Pattern).map(RegexSupport.compilePattern)
  private val maxItems: Option[Int] = schema.getInt(MaxItems)
  private val minItems: Option[Int] = schema.getInt(MinItems)
  private val uniqueItems: Option[Boolean] = schema.getBoolean(UniqueItems)
  private val depReq: Option[collection.Map[String, Value]] = schema.getObjectOpt(DepRequired)
  private val maxProperties: Option[Int] = schema.getInt(MaxProperties)
  private val minProperties: Option[Int] = schema.getInt(MinProperties)
  private val required: Seq[String] = schema.getStringArray(Required)

  override def visitNull(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, tyype.contains("null"), Tyype, s"Expected $tyype, got null")) &&
      const.forall(c => accumulate(buff, c == Null, Const, "null does not match expected constant")) &&
      enuum.forall(e => accumulate(buff, e.contains(Null), Enuum, "null not found in enumeration"))
    buff.result()
  }

  override def visitFalse(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, tyype.contains("boolean"), Tyype, s"Expected $tyype, got false")) &&
      const.forall(c => accumulate(buff, c == False, Const, "false does not match expected constant")) &&
      enuum.forall(e => accumulate(buff, e.contains(False), Enuum, "false not found in enumeration"))
    buff.result()
  }

  override def visitTrue(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, tyype.contains("boolean"), Tyype, s"Expected $tyype, got true")) &&
      const.forall(c => accumulate(buff, c == True, Const, "true does not match expected constant")) &&
      enuum.forall(e => accumulate(buff, e.contains(True), Enuum, "true not found in enumeration"))
    buff.result()
  }

  override def visitInt64(num: Long, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, tyype.exists(t => "integer" == t || "number" == t), Tyype, s"Expected $tyype, got number")) &&
      multipleOf.forall(mult => accumulate(buff, isMultiple(num, mult.value), MultipleOf, "Number is not a multiple")) &&
      visitNumber(num, buff)
    buff.result()
  }

  override def visitFloat64(num: Double, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, tyype.exists(t => "number" == t || "integer" == t && num.isWhole), Tyype, s"Expected $tyype, got number")) &&
      multipleOf.forall(mult => accumulate(buff, isMultiple(num, mult.value), MultipleOf, "Number is not a multiple")) &&
      visitNumber(num, buff)
    buff.result()
  }

  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    val num = numOf(s.toString, decIndex, expIndex)
    (tyype.isEmpty || accumulate(buff, tyype.exists(t => "number" == t || "integer" == t && isWhole(num)), Tyype, s"Expected $tyype, got number")) &&
      multipleOf.forall(mult => accumulate(buff, isMultiple(num, mult.value), MultipleOf, "Number is not a multiple")) &&
      visitNumber(num, buff)
    buff.result()
  }

  private def visitNumber(num: Any, buff: mutable.Growable[OutputUnit]): Boolean = {
    const.forall(c => accumulate(buff, c.isInstanceOf[Num] && compareTo(c.value, num) == 0, Const, "Number does not match expected constant")) &&
      enuum.forall(e => accumulate(buff, e.exists(v => v.isInstanceOf[Num] && compareTo(v.value, num) == 0), Enuum, "Number not found in enumeration")) &&
      maximum.forall(max => accumulate(buff, lteq(num, max.value), Maximum, "Number is greater than maximum")) &&
      minimum.forall(min => accumulate(buff, gteq(num, min.value), Minimum, "Number is less than minimum")) &&
      exclusiveMax.forall(max => accumulate(buff, lt(num, max.value), ExclusiveMax, "Number is greater than or equal to exclusive maximum")) &&
      exclusiveMin.forall(min => accumulate(buff, gt(num, min.value), ExclusiveMin, "Number is less than or equal to exclusive minimum"))
  }

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, tyype.contains("string"), Tyype, s"Expected $tyype, got string")) &&
      const.forall(c => accumulate(buff, c.value == s.toString, Const, "String does not match expected constant")) &&
      enuum.forall(e => accumulate(buff, e.exists(v => v.value == s.toString), Enuum, "String not found in enumeration")) &&
      maxLength.forall(max => accumulate(buff, s.toString.codePointCount(0, s.length()) <= max, MaxLength, "String is greater than maximum length")) &&
      minLength.forall(min => accumulate(buff, s.toString.codePointCount(0, s.length()) >= min, MinLength, "String is less than minimum length")) &&
      pattern.forall(p => accumulate(buff, p.matches(s), Pattern, "String does not match pattern"))
    buff.result()
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Nothing, Seq[OutputUnit]] = {
    val buff = new ListBuffer[OutputUnit]
    if (tyype.nonEmpty) accumulateVec(buff, tyype.contains("array"), Tyype, s"Expected $tyype, got array")

    val insVisitor: ArrVisitor[Nothing, Seq[OutputUnit]] =
      if (const.isEmpty && enuum.isEmpty && !uniqueItems.contains(true)) NilArrayVis
      else new MapArrContext(LiteralVisitor.visitArray(length, index), jsVal => { // Vis[Value, coll.Seq[OUnit]]
        val buff0 = new ListBuffer[OutputUnit]
        const.foreach(c => accumulate(buff0, valueEquals(c, jsVal), Const, "Array does not match expected constant"))
        enuum.foreach(e => accumulate(buff0, e.exists(v => valueEquals(v, jsVal)), Enuum, "Array not found in enumeration"))
        // `uniqueItems: false` imposes nothing; the check below only makes sense for `true`. The
        // NilArrayVis branch above already skips it on that basis, but not when const/enum put us
        // on this branch anyway.
        if (uniqueItems.contains(true)) {
          val set = new mutable.HashSet[Value](jsVal.arr.size, 1) // perf: avoid Set
          accumulate(buff0, jsVal.arr.forall(e => set.add(canonical(e))), UniqueItems, "Values in array are not unique")
        }
        buff0.result()
      })

    new ArrVisitor[Any, Seq[OutputUnit]] {
      private var nextIdx = 0

      override def subVisitor: Visitor[?, ?] = {
        maxItems.foreach(max => ffastChild(buff, nextIdx <= max, MaxItems, "Array has more items than allowed"))
        insVisitor.subVisitor
      }
      override def visitValue(v: Any, index: Int): Unit = {
        insVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): Seq[OutputUnit] = {
        buff.addAll(insVisitor.visitEnd(index))
        maxItems.foreach(max => accumulate(buff, nextIdx <= max, MaxItems, "Array has more items than allowed"))
        minItems.foreach(min => accumulate(buff, nextIdx >= min, MinItems, "Array has less items than allowed"))
        buff.result()
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Nothing, Seq[OutputUnit]] =  {
    val buff = new ListBuffer[OutputUnit]
    if (tyype.nonEmpty) accumulateVec(buff, tyype.contains("object"), Tyype, s"Expected $tyype, got object")

    val propsVisited = new ListBuffer[String]
    val insVisitor: ObjVisitor[?, Seq[OutputUnit]] =
      // `uniqueItems` constrains arrays only, so it has no say here: without this,
      // `{"uniqueItems": true}` against an object materialized the whole thing for nothing.
      if (const.isEmpty && enuum.isEmpty) NilObjVis
      else new MapObjContext(LiteralVisitor.visitObject(length, index), obj => { // Vis[Value, coll.Seq[OUnit]]
        val buff0 = new ListBuffer[OutputUnit]
        const.foreach(c => accumulate(buff0, valueEquals(c, obj), Const, "Object does not match expected constant"))
        enuum.foreach(e => accumulate(buff0, e.exists(v => valueEquals(v, obj)), Enuum, "Object not found in enumeration"))
        buff0.result()
      })

    new ObjVisitor[Any, Seq[OutputUnit]] {
      private var currentKey: String = "?"

      override def visitKey(index: Int): Visitor[?, ?] = {
        maxProperties.foreach(max => ffastChild(buff, propsVisited.size <= max, MaxProperties, "Object has more properties than allowed"))
        new SimpleVisitor[Nothing, Any] {
          def expectedMsg = "Expected string"
          override def visitString(s: CharSequence, index1: Int): Any = {
            currentKey = s.toString
            propsVisited.addOne(currentKey)
            insVisitor.visitKey(index).visitString(s, index1)
          }
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)
      override def subVisitor: Visitor[?, ?] = insVisitor.subVisitor
      override def visitValue(v: Any, index: Int): Unit = insVisitor.narrow.visitValue(v, index)
      override def visitEnd(index: Int): Seq[OutputUnit] = {
        buff.addAll(insVisitor.visitEnd(index))
        required.foreach(req => accumulate(buff, propsVisited.contains(req), Required, s"Object does not contain required property $req"))
        maxProperties.foreach(max => accumulate(buff, propsVisited.size <= max, MaxProperties, "Object has more properties than allowed"))
        minProperties.foreach(min => accumulate(buff, propsVisited.size >= min, MinProperties, "Object has less properties than allowed"))
        depReq.foreach(depReqs => accumulate(buff, depReqs.filter((k, reqs) => propsVisited.contains(k)) // all depRequired that apply (found in obj) as (dependent key, required)
          .map((k, reqs) => reqs.arr.forall(rreq => propsVisited.contains(rreq.str))) // whether the required props were found
          .forall(identity), DepRequired, "Object does not contain dependent required properties")) // whether all entries were satisfied
        buff.result()
      }
    }
  }

}

object Validation extends VocabFactory[Validation] {
  private def gt(a: Any, b: Any) = compareTo(a, b) == 1
  private def lt(a: Any, b: Any) = compareTo(a, b) == -1
  private def lteq(a: Any, b: Any) = compareTo(a, b) != 1
  private[json_schema] def gteq(a: Any, b: Any) = compareTo(a, b) != -1

  /**
   * `v` with every number rewritten to one canonical [[Num]] case, at any depth, so that hashing a
   * [[Value]] agrees with [[valueEquals]] on numbers: `1`, `1.0`, `1e0` and `1.00` all collapse to
   * the same key, while `0`/`false` and `1`/`true` stay distinct.
   *
   * `uniqueItems` needs this because it detects duplicates with a `HashSet`, i.e. by `Value`
   * equality, which is per-case - so without canonicalizing it reported `[1, 1.0]` as unique. The
   * alternative, comparing every pair with `valueEquals`, would make the keyword quadratic.
   *
   * Whole numbers canonicalize to [[Int64]] where they fit, which keeps the overwhelmingly common
   * array-of-integers case allocation-free.
   */
  private def canonical(v: Value): Value = v match
    case i: Int64 => i
    case Float64(d) if !d.isFinite => v // no BigDecimal form; NaN/Infinity can only be built by hand
    case n: Num =>
      val d = n.toBigDecimal
      if (d.isValidLong) Int64(d.toLong) else Decimal(d)
    case Arr(items) => Arr(items.map(canonical))
    case Obj(fields) => Obj(fields.map((k, fv) => (k, canonical(fv))))
    case _ => v

  /**
   * JSON Schema equality: numbers compare by mathematical value regardless of their [[Value]]
   * subtype (e.g. `0` and `0.0` are equal even though they're represented as [[Int64]] and
   * [[Float64]] respectively), and arrays/objects compare deeply using the same rule.
   */
  private[json_schema] def valueEquals(a: Value, b: Value): Boolean = (a, b) match {
    case (an: Num, bn: Num) => compareTo(an.value, bn.value) == 0
    case (Arr(as), Arr(bs)) => as.length == bs.length && as.lazyZip(bs).forall(valueEquals)
    case (Obj(am), Obj(bm)) => am.size == bm.size && am.forall((k, v) => bm.get(k).exists(valueEquals(v, _)))
    case _ => a == b
  }

  /**
   * Exact at any width, mixing widths freely - see README § Numbers.
   *
   * Widening a `BigInt` with plain `BigDecimal(i)` is exact: the `MathContext` it attaches is
   * sized to the value, so nothing rounds, and `compareTo` never consults one regardless. An
   * earlier bound here threw past 34 digits, which crashed the most ordinary schemas on a large
   * instance - `{"minimum": 1.5}` against a 60-digit integer went through this and blew up.
   */
  private def compareTo(a: Any, b: Any): Int = {
    (a, b) match {
      case (x: Long, y: Long) => x.compareTo(y)
      case (x: Long, y: Double) => _64(x, y)
      case (x: Long, y: BigInt) => BigDecimal(x).compareTo(BigDecimal(y))
      case (x: Long, y: BigDecimal) => BigDecimal(x).compareTo(y)

      case (x: Double, y: Long) => -_64(y, x)
      case (x: Double, y: Double) =>
        if (x == 0 && y == -0) return 0
        if (x == -0 && y == 0) return 0
        x.compareTo(y)
      case (x: Double, y: BigInt) => BigDecimal(x).compareTo(BigDecimal(y))
      case (x: Double, y: BigDecimal) => BigDecimal(x).compareTo(y)

      case (x: BigInt, y: Long) => BigDecimal(x).compareTo(BigDecimal(y))
      case (x: BigInt, y: Double) => BigDecimal(x).compareTo(BigDecimal(y))
      case (x: BigInt, y: BigInt) => x.compareTo(y)
      case (x: BigInt, y: BigDecimal) => BigDecimal(x).compareTo(y)

      case (x: BigDecimal, y: Long) => x.compareTo(BigDecimal(y))
      case (x: BigDecimal, y: Double) => x.compareTo(BigDecimal(y))
      case (x: BigDecimal, y: BigInt) => x.compareTo(BigDecimal(y))
      case (x: BigDecimal, y: BigDecimal) => x.compareTo(y)

      case _ => throw new IllegalStateException
    }
  }
  private def _64(x: Long, y: Double): Int = {
    if (x == 0 && y == -0) return 0
    val convertedX = x.toDouble
    val result = convertedX + y
    if (result.isInfinity || convertedX != x) BigDecimal(x).compareTo(BigDecimal(y)) // Handle precision or overflow
    else convertedX.compareTo(y)
  }

  private def isMultiple(a: Any, b: Any): Boolean = {
    try { divides(unwrap(b), unwrap(a)) } catch {
      case e @ (_: ArithmeticException | _: IllegalArgumentException) =>
        throw new IllegalArgumentException("Number overflow while computing multipleOf", e)
    }
  }

  /** The `java.math.BigDecimal` holding exactly what this number case holds. */
  private def unwrap(x: Any): java.math.BigDecimal = x match {
    case x: Long => java.math.BigDecimal.valueOf(x)
    case x: Double => java.math.BigDecimal.valueOf(x)
    case x: BigInt => new java.math.BigDecimal(x.bigInteger)
    case x: BigDecimal => x.bigDecimal
  }

  /**
   * True if `dividend` is an exact multiple of `divisor`.
   *
   * Deliberately not `BigDecimal.remainder`. Both operands are `unscaled * 10^-scale`, so the
   * question is really integer divisibility: with `shift = divisor.scale - dividend.scale`,
   * `dividend` is a multiple of `divisor` exactly when `unscaled(dividend) * 10^shift` is
   * divisible by `unscaled(divisor)`, and `10^shift mod n` is one `modPow` away. That matters
   * because numbers carry no exponent bound - `1e100000000` is thirty bytes of instance whose
   * scale is -100000000, and any formulation that materializes the value first inflates it into
   * a hundred-million-digit integer. Here no power of ten is ever built, so cost follows the
   * digits actually written rather than the magnitude they denote.
   *
   * It is also decisive where `remainder` is not: `scala.math.BigDecimal`'s `%` carries
   * `DECIMAL128` and abandons some divisions as "Division impossible", which is a statement about
   * the representation of the quotient rather than about divisibility.
   */
  private def divides(divisor: java.math.BigDecimal, dividend: java.math.BigDecimal): Boolean = {
    if (divisor.signum == 0) throw new ArithmeticException("multipleOf is zero")
    if (dividend.signum == 0) return true

    val num = dividend.unscaledValue.abs
    val den = divisor.unscaledValue.abs
    val shift = divisor.scale.toLong - dividend.scale.toLong
    if (shift >= 0)
      num.mod(den).multiply(BigInteger.TEN.modPow(BigInteger.valueOf(shift), den)).mod(den).signum == 0
    else {
      // Scaling the divisor up instead: it can only divide `num` while 10^scaleUp stays within
      // it, and log10(num) < bitLength * 0.302, so past that there is nothing to compute.
      val scaleUp = -shift
      scaleUp <= num.bitLength.toLong * 302 / 1000 + 1 &&
        num.remainder(den.multiply(BigInteger.TEN.pow(scaleUp.toInt))).signum == 0
    }
  }

  /**
   * Narrowest exact representation of a JSON number literal: `Long`, `Double`, `BigInt` or
   * `BigDecimal`. `decIndex`/`expIndex` are the literal's '.' and 'e' offsets, as handed over by
   * `Visitor.visitFloat64StringParts`, or -1 when absent.
   *
   * The whole point here is that `String.toDoubleOption` cannot be used the way `toLongOption` is.
   * `toLongOption` returns `None` when the literal doesn't fit, so falling back on it is sound.
   * `Double.parseDouble` never fails: it rounds silently past ~15-17 significant digits, saturates
   * to an infinity past ~1.8e308, and flushes to zero below ~4.9e-324 - all reported as a
   * successful parse. Every one of those has to be ruled out by hand before trusting the result.
   */
  private[json_schema] def numOf(s: String, decIndex: Int, expIndex: Int): Any = {
    if (decIndex == -1 && expIndex == -1) s.toLongOption.getOrElse(BigInt(s))
    else {
      val digits = sigDigits(s, expIndex)
      if (digits > DoubleSafeDigits) BigDecimal(s) // more precision than Double can round-trip
      else s.toDoubleOption match
        case Some(d) if digits == 0 => d // the literal is zero, whatever its exponent
        // Rules out both saturation to +/-Infinity and flush-to-zero, and keeps subnormals - where
        // Double loses precision well before 15 digits - on the exact path too.
        case Some(d) if d.isFinite && Math.abs(d) >= java.lang.Double.MIN_NORMAL => d
        case _ => BigDecimal(s)
    }
  }

  /** The most significant decimal digits `Double` is guaranteed to round-trip exactly. */
  private val DoubleSafeDigits = 15

  /**
   * Counts significant digits in `s`'s mantissa, i.e. excluding sign, '.', and any exponent.
   * The '.' needs no position of its own: the scan counts digits and skips everything else.
   * Returns 0 for a literal whose mantissa is all zeros, which `numOf` reads as "this is zero".
   */
  private def sigDigits(s: String, expIndex: Int): Int = {
    val mantissaEnd = if (expIndex == -1) s.length else expIndex
    var i = 0
    var digits = 0
    var seenNonZero = false
    while (i < mantissaEnd) {
      val c = s.charAt(i)
      if (c >= '0' && c <= '9') {
        if (c != '0') seenNonZero = true
        if (seenNonZero) digits += 1
      }
      i += 1
    }
    digits
  }
  private def isWhole(n: Any) = n match
    case d: Double => d.isWhole
    // NB: `case _: Long | BigInt` (without repeating `_:`) never matches a BigInt at runtime -
    // Scala parses it as pattern alternation with `BigInt` as a bare stable-id pattern (comparing
    // equality to the BigInt companion object), not as a `Long | BigInt` type-union ascription.
    // Silently fell through to MatchError for any BigInt-valued number.
    case _: Long | _: BigInt => true
    case d: BigDecimal => d.isWhole

  private val NilArrayVis = new ArrVisitor[Any, Seq[OutputUnit]] {
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }

  private val NilObjVis = new ObjVisitor[Any, Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }

  private val Tyype = "type"
  private val Const = "const"
  private val Enuum = "enum"
  private val MultipleOf = "multipleOf"
  private val Maximum = "maximum"
  private val Minimum = "minimum"
  private val ExclusiveMax = "exclusiveMaximum"
  private val ExclusiveMin = "exclusiveMinimum"
  private val MaxLength = "maxLength"
  private val MinLength = "minLength"
  private val MaxItems = "maxItems"
  private val MinItems = "minItems"
  private val MaxContains = "maxContains"
  private val MinContains = "minContains"
  private val MaxProperties = "maxProperties"
  private val MinProperties = "minProperties"
  private val Pattern = "pattern"
  private val UniqueItems = "uniqueItems"
  private val Required = "required"
  private val DepRequired = "dependentRequired"
  
  val Keys: Set[String] = Set(Tyype, Const, Enuum, MultipleOf, Maximum, Minimum, ExclusiveMax, ExclusiveMin, MaxLength,
    MinLength, MaxItems, MinItems, MaxContains, MinContains, MaxProperties, MinProperties, Pattern, UniqueItems,
    Required, DepRequired)

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/validation"
  override def shouldApply(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): Validation = 
    new Validation(schema, ctx, path, dynParent)
}
