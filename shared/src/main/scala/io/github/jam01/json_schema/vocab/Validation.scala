/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Validation.*
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.math.MathContext
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

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
  private val pattern: Option[Regex] = schema.getString(Pattern).map(s => new Regex(s).unanchored)
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
    const.forall(c => accumulate(buff, c.value == num, Const, "Number does not match expected constant")) &&
      enuum.forall(e => accumulate(buff, e.exists(v => v.value == num), Enuum, "Number not found in enumeration")) &&
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
      if (const.isEmpty && enuum.isEmpty && (uniqueItems.isEmpty || !uniqueItems.get)) NilArrayVis
      else new MapArrContext(LiteralVisitor.visitArray(length, index), jsVal => { // Vis[Value, coll.Seq[OUnit]]
        val buff0 = new ListBuffer[OutputUnit]
        const.foreach(c => accumulate(buff0, c == jsVal, Const, "Array does not match expected constant"))
        enuum.foreach(e => accumulate(buff0, e.contains(jsVal), Enuum, "Array not found in enumeration"))
        uniqueItems.foreach(b => {
          val set = new mutable.HashSet[Value](jsVal.arr.size, 1) // perf: avoid Set
          accumulate(buff0, jsVal.arr.forall(e => set.add(e)), UniqueItems, "Values in array are not unique")
        })
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
      if (const.isEmpty && enuum.isEmpty && (uniqueItems.isEmpty || !uniqueItems.get)) NilObjVis
      else new MapObjContext(LiteralVisitor.visitObject(length, index), obj => { // Vis[Value, coll.Seq[OUnit]]
        val buff0 = new ListBuffer[OutputUnit]
        const.foreach(c => accumulate(buff0, c == obj, Const, "Object does not match expected constant"))
        enuum.foreach(e => accumulate(buff0, e.contains(obj), Enuum, "Object not found in enumeration"))
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
  private def decOf(i: BigInt): BigDecimal = {
    val d = BigDecimal(i)
    if (d.precision > MathContext.DECIMAL128.getPrecision) throw new ArithmeticException("Decimal128 overflow")
    d
  }

  private def gt(a: Any, b: Any) = compareTo(a, b) == 1
  private def lt(a: Any, b: Any) = compareTo(a, b) == -1
  private def lteq(a: Any, b: Any) = compareTo(a, b) != 1
  private[json_schema] def gteq(a: Any, b: Any) = compareTo(a, b) != -1

  private def compareTo(a: Any, b: Any): Int = {
    (a, b) match {
      case (x: Long, y: Long) => x.compareTo(y)
      case (x: Long, y: Double) => _64(x, y)
      case (x: Long, y: BigInt) => BigDecimal(x).compareTo(decOf(y))
      case (x: Long, y: BigDecimal) => BigDecimal(x).compareTo(y)

      case (x: Double, y: Long) => -_64(y, x)
      case (x: Double, y: Double) =>
        if (x == 0 && y == -0) return 0
        if (x == -0 && y == 0) return 0
        x.compareTo(y)
      case (x: Double, y: BigInt) => BigDecimal(x).compareTo(decOf(y))
      case (x: Double, y: BigDecimal) => BigDecimal(x).compareTo(y)

      case (x: BigInt, y: Long) => BigDecimal(x).compareTo(BigDecimal(y))
      case (x: BigInt, y: Double) => decOf(x).compareTo(BigDecimal(y))
      case (x: BigInt, y: BigInt) => x.compareTo(y)
      case (x: BigInt, y: BigDecimal) => decOf(x).compareTo(y)

      case (x: BigDecimal, y: Long) => x.compareTo(BigDecimal(y))
      case (x: BigDecimal, y: Double) => x.compareTo(BigDecimal(y))
      case (x: BigDecimal, y: BigInt) => x.compareTo(decOf(y))
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
    try { mod(a, b) } catch {
      case e: ArithmeticException if e.getMessage.contains("Division impossible") => false
      case e @ (_: ArithmeticException | _: IllegalArgumentException) =>
        throw new IllegalArgumentException("Number overflow while computing multipleOf", e)
    }
  }
  private def mod(a: Any, b: Any): Boolean = (a, b) match {
    case (x: Long, y: Long) => BigDecimal(x) % BigDecimal(y) == 0
    case (x: Long, y: Double) => BigDecimal(x) % BigDecimal(y) == 0
    case (x: Long, y: BigInt) => BigDecimal(x) % BigDecimal(y) == 0
    case (x: Long, y: BigDecimal) => BigDecimal(x) % y == 0

    case (x: Double, y: Long) => BigDecimal(x) % BigDecimal(y) == 0
    case (x: Double, y: Double) => BigDecimal(x) % BigDecimal(y) == 0
    case (x: Double, y: BigInt) => BigDecimal(x) % decOf(y) == 0
    case (x: Double, y: BigDecimal) => BigDecimal(x) % y == 0

    case (x: BigInt, y: Long) => BigDecimal(x) % BigDecimal(y) == 0
    case (x: BigInt, y: BigInt) => BigDecimal(x) % BigDecimal(y) == 0
    case (x: BigInt, y: Double) => decOf(x) % BigDecimal(y) == 0
    case (x: BigInt, y: BigDecimal) => decOf(x) % y == 0

    case (x: BigDecimal, y: Long) => x % BigDecimal(y) == 0
    case (x: BigDecimal, y: Double) => x % BigDecimal(y) == 0
    case (x: BigDecimal, y: BigInt) => x % decOf(y) == 0
    case (x: BigDecimal, y: BigDecimal) => x % y == 0
  }

  private[json_schema] def numOf(s: String, decIndex: Int, expIndex: Int): Any = {
    if (decIndex == -1 && expIndex == -1) s.toLongOption.getOrElse(BigInt(s))
    else s.toDoubleOption.getOrElse(BigDecimal(s))
  }
  private def isWhole(n: Any) = n match
    case d: Double => d.isWhole
    case _: Long | BigInt  => true
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
