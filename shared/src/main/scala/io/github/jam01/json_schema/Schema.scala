/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import upickle.core.LinkedHashMap

import java.math.MathContext
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.hashing.MurmurHash3

/**
 * An ADT representing JSON Schema values.
 *
 * @see <a href="https://github.com/com-lihaoyi/upickle/blob/3.3.1/ujson/src/ujson/Value.scala">ujson.Value</a>
 */
sealed trait Value {
  def value: Any

  /**
   * Returns the `String` value of this [[Value]], fails if it is not
   * a [[Str]]
   */
  def str: String = this match {
    case Str(value) => value
    case _ => throw IllegalStateException("Expected Str")
  }

  /**
   * Returns the key/value map of this [[Value]], fails if it is not
   * a [[Obj]]
   */
  def obj: collection.Map[String, Value] = this match {
    case Obj(value) => value
    case _ => throw IllegalStateException("Expected Object")
  }

  /**
   * Returns the elements of this [[Value]], fails if it is not
   * a [[Arr]]
   */
  def arr: Seq[Value] = this match {
    case Arr(value) => value
    case _ => throw IllegalStateException("Expected Array")
  }

  /**
   * Returns the `Long` value of this [[Value]], fails if it is not
   * a [[Int64]]
   */
  def int64: Long = this match {
    case Int64(value) => value
    case _ => throw IllegalStateException("Expected Number")
  }

  /**
   * Returns the `Double` value of this [[Value]], fails if it is not
   * a [[Float64]]
   */
  def float64: Double = this match {
    case Float64(value) => value
    case _ => throw IllegalStateException("Expected Number")
  }

  /**
   * Returns the `BigInt` value of this [[Value]], fails if it is not
   * a [[Int128]]
   */
  def int128: BigInt = this match {
    case Int128(value) => value
    case Int64(value) => BigInt(value)
    case _ => throw IllegalStateException("Expected Number")
  }

  /**
   * Returns the `BigDecimal` value of this [[Value]], fails if it is not
   * a [[Dec128]]
   */
  def dec128: BigDecimal = this match {
    case Dec128(value) => value
    case _ => throw IllegalStateException("Expected Number")
  }

  /**
   * Returns the `Boolean` value of this [[Value]], fails if it is not
   * a [[Bool]]
   */
  def bool: Boolean = this match {
    case Bool(value) => value
    case _ => throw IllegalStateException("Expected Boolean")
  }

  /**
   * Returns the `Schema` value of this [[Value]], fails if it is not
   * a [[Schema]]
   */
  def sch: Schema = this match {
    case value: Schema => value
    case _ => throw IllegalStateException("Expected Schema")
  }
}

object Value {
  given iterable2Arr[T](using f: T => Value): Conversion[IterableOnce[T], Arr] with
    override def apply(it: IterableOnce[T]): Arr = Arr(it.iterator.map(x => f(x)).toSeq)
  given iterable2Obj[T](using f: T => Value): Conversion[IterableOnce[(String, T)], Obj] with
    override def apply(it: IterableOnce[(String, T)]): Obj = Obj(Map.from(it.iterator.map(x => (x._1, f(x._2)))))
  given Conversion[Boolean, Bool] = (i: Boolean) => if (i) True else False
  given Conversion[Int, Num] = (i: Int) => Int64(i: Long)
  given Conversion[Long, Num] = (i: Long) => Int64(i)
  given Conversion[Float, Num] = (i: Float) => Float64(i)
  given Conversion[Double, Num] = (i: Double) => Float64(i)
  given Conversion[BigInt, Num] = (i: BigInt) => Int128(i)
  given Conversion[BigDecimal, Num] = (i: BigDecimal) => Dec128(i)
  given Conversion[Null, Null.type] = (i: Null) => Null
  given Conversion[CharSequence, Str] = (s: CharSequence) => Str(s.toString)
}

case class Str(value: String) extends Value

case class Obj(value: Map[String, Value]) extends Value

object Obj {
  def apply[V](item: (String, V),
               items: (String, Value)*)(using conv: V => Value): Obj = {
    new Obj(Map.newBuilder
      .addOne((item._1, conv(item._2)))
      .addAll(items)
      .result())
  }

  def apply(): Obj = new Obj(Map.empty)
}

case class Arr(value: Seq[Value]) extends Value

object Arr {
  def apply[T](head: T, tail: Value*)(using conv: T => Value): Arr = {
    val buf = ListBuffer(conv(head))
    tail.iterator.foreach{ item =>
      buf += item
    }
    new Arr(buf.result())
 }

  def apply(): Arr = new Arr(Nil)
}

abstract class Num extends Value

case class Int64(value: Long) extends Num
case class Float64(value: Double) extends Num
case class Int128(value: BigInt) extends Num {
  if (value.bitLength > 127) throw new IllegalArgumentException("Integer value exceeds 128 bits")
}
case class Dec128(value: BigDecimal) extends Num {
  if (value.mc != MathContext.DECIMAL128) throw new IllegalArgumentException("Decimal value exceeds 128 bits")
}

sealed abstract class Bool extends Value {
  def value: Boolean
}

object Bool {
  def apply(value: Boolean): Bool = if (value) True else False

  def unapply(bool: Bool): Some[Boolean] = Some(bool.value)
}

case object False extends Bool {
  def value = false
}

case object True extends Bool {
  def value = true
}

case object Null extends Value {
  def value: Null = null
}

/**
 * A JSON Schema.
 */
sealed trait Schema extends Value {
  /**
   * Exception thrown when sub-schema retrieval fails.
   *
   * @param message A description of the failure
   * @param cause The underlying cause (if any)
   */
  class SchemaRetrievalException(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)
  
  /**
   * Retrieve the sub-schema located at the given [[JsonPointer]].
   * @throws SchemaRetrievalException if a sub-schema is not present at the given location or if the location traverses
   *                                  a scalar value, or if applying a non-root JsonPointer against a [[BooleanSchema]]
   * @param subschLocation the sub-schema location
   * @return the identified Schema
   */
  @throws[SchemaRetrievalException]
  def schBy(subschLocation: JsonPointer): Schema = {
    if (JsonPointer.Root == subschLocation) return this
    schBy0(subschLocation)
  }

  protected def schBy0(ptr: JsonPointer): Schema

  def validate[I](reader: upickle.core.Transformer[I], readable: I,
                  config: Config = Config.Default,
                  registry: Registry = Registry.Empty): OutputUnit = {
    reader.transform(readable, validator(this, config, registry))
  }
}

/**
 * A JSON Boolean Schema.
 */
sealed abstract class BooleanSchema extends Schema {
  def value: Boolean

  override def schBy0(ptr: JsonPointer): Schema = {
    throw new SchemaRetrievalException("Cannot evaluate a JSON Pointer against a boolean schema")
  }
}

object BooleanSchema {
  def apply(value: Boolean): BooleanSchema = if (value) TrueSchema else FalseSchema

  def unapply(bool: BooleanSchema): Some[Boolean] = Some(bool.value)
}

/**
 * JSON True Schema.
 */
case object TrueSchema extends BooleanSchema {
  def value = true
}

/**
 * JSON False Schema.
 */
case object FalseSchema extends BooleanSchema {
  def value = false
}

// see: https://github.com/BalmungSan/scala-multifile-adt
// https://users.scala-lang.org/t/refactoring-class-hierarchy-into-adt/6997
// https://contributors.scala-lang.org/t/pre-sip-sealed-enumerating-allowed-sub-types/3768
// https://contributors.scala-lang.org/t/possibility-to-spread-sealed-trait-to-different-files/5304

/**
 * A JSON Object Schema.
 *
 * @see <a href="https://json-schema.org/draft/2020-12/json-schema-core#section-9.1.1">JSON Schema § Initial Base URI</a>
 *
 * @param value the underlying Map of Values
 * @param docbase the base Uri for this schema
 * @param parent optionally the lexical parent schema
 * @param prel optionally the JSON Pointer relative to the lexical parent schema
 */
final class ObjectSchema private[json_schema](val value: collection.Map[String, Value],
                        // TODO: make immutable.Map
                        //  only using coll.Map in order for SchemaR to create Schema for children schemas while still
                        //  mutating the underlying map as it's being parsed. Could possibly create ObjSch builder class?
                        protected val docbase: Uri,
                        protected val parent: Option[ObjectSchema] = None,
                        protected val prel: Option[String] = None) extends ObjSchema with Schema {
                        /* relative pointer from parent */

  // equals and hashCode ignores parent in order to avoid circular references
  // (schema's children will reference it back)
  // this is ok since the structure is still validated, including the exact parent-child traversal
  // this is only an issue if SchemaR and/or manual object creation incorrectly set parents
  override def equals(obj: Any): Boolean = {
    obj match
      case osch: ObjectSchema =>
        value == osch.value &&
          docbase == osch.docbase &&
          prel == osch.prel
      case _ => false
  }

  override def hashCode(): Int = {
    var h = "ObjectSchema".##
    h = MurmurHash3.mix(h, value.##)
    h = MurmurHash3.mix(h, docbase.##)
    h = MurmurHash3.mix(h, prel.##)
    MurmurHash3.finalizeHash(h, 3)
  }

  override def toString: String = location.toString
}

object ObjectSchema {
  def empty(docbase: Uri = Uri.random): ObjectSchema = new ObjectSchema(LinkedHashMap(), docbase)

  def unapply(arg: ObjectSchema): Some[collection.Map[String, Value]] = {
    Some(arg.value)
  }
}
