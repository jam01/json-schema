package io.github.jam01.json_schema

import upickle.core.LinkedHashMap

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
 * An ADT representing JSON Schema values
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
  def arr: collection.Seq[Value] = this match {
    case Arr(value) => value
    case _ => throw IllegalStateException("Expected Array")
  }

  /**
   * Returns the `Double` value of this [[Value]], fails if it is not
   * a [[Num]]
   */
  def num: Long | Double = this match {
    case Num(value) => value
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
    override def apply(x: IterableOnce[T]): Arr = Arr.from(x.iterator.map(f))
  given iterable2Obj[T](using f: T => Value): Conversion[IterableOnce[(String, T)], Obj] with
    override def apply(it: IterableOnce[(String, T)]): Obj = Obj(LinkedHashMap(it.iterator.map(x => (x._1, f(x._2)))))
  given Conversion[Boolean, Bool] = (i: Boolean) => if (i) True else False
  given Conversion[Int, Num] = (i: Int) => Num(i: Long)
  given Conversion[Long, Num] = (i: Long) => Num(i)
  given Conversion[Float, Num] = (i: Float) => Num(i)
  given Conversion[Double, Num] = (i: Double) => Num(i)
  given Conversion[Null, Null.type] = (i: Null) => Null
  given Conversion[CharSequence, Str] = (s: CharSequence) => Str(s.toString)
}

case class Str(value: String) extends Value

case class Obj(value: LinkedHashMap[String, Value]) extends Value // TODO: make collection.Map 

object Obj {
  def apply[V](item: (String, V),
               items: (String, Value)*)(using conv: V => Value): Obj = {
    val map = LinkedHashMap[String, Value]()
    map.put(item._1, conv(item._2))
    for (i <- items) map.put(i._1, i._2)
    Obj(map)
  }

  def apply(): Obj = Obj(LinkedHashMap[String, Value]())
}

case class Arr(value: mutable.ArrayBuffer[Value]) extends Value // TODO: make collection.Seq 

object Arr {
  def from[T](items: IterableOnce[T])(using conv: T => Value): Arr = {
    val buf = new mutable.ArrayBuffer[Value]()
    items.iterator.foreach{ item =>
      buf += (conv(item): Value)
    }
    Arr(buf)
  }
  
  def apply(items: Value*): Arr = {
    val buf = new mutable.ArrayBuffer[Value](items.length)
    items.foreach { item =>
      buf += item
    }
    Arr(buf)
  }
}

// TODO: make part of ADT
case class Num(value: Long | Double) extends Value

object Num {
  def apply(i: Int): Num = Num(i.toLong)
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

sealed trait Schema extends Value {
  def schBy(subschLocation: JsonPointer): Schema = {
    if (JsonPointer.Root == subschLocation) return this
    schBy0(subschLocation)
  }

  def schBy0(ptr: JsonPointer): Schema
}

sealed abstract class BooleanSchema extends Schema {
  def value: Boolean

  override def schBy0(ptr: JsonPointer): Schema = {
    throw new IllegalStateException("cannot evaluate a JSON pointer against a boolean schema")
  }
}

object BooleanSchema {
  def apply(value: Boolean): BooleanSchema = if (value) TrueSchema else FalseSchema

  def unapply(bool: BooleanSchema): Some[Boolean] = Some(bool.value)
}

case object TrueSchema extends BooleanSchema {
  def value = true
}

case object FalseSchema extends BooleanSchema {
  def value = false
}

// see: https://github.com/BalmungSan/scala-multifile-adt
// https://users.scala-lang.org/t/refactoring-class-hierarchy-into-adt/6997
// https://contributors.scala-lang.org/t/pre-sip-sealed-enumerating-allowed-sub-types/3768
// https://contributors.scala-lang.org/t/possibility-to-spread-sealed-trait-to-different-files/5304

/**
 * A JSON Schema Object
 *
 * @see <a href="https://json-schema.org/draft/2020-12/json-schema-core#section-9.1.1">JSON Schema § Initial Base URI</a>
 *
 * @param value the underlying Map of Values
 * @param docbase the base Uri for this schema
 * @param parent optionally the lexical parent schema
 * @param prel optionally the JSON Pointer relative to the lexical parent schema
 */
final class ObjectSchema private[json_schema](val value: LinkedHashMap[String, Value], // TODO: make collection.Map 
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

  def unapply(arg: ObjectSchema): Some[LinkedHashMap[String, Value]] = {
    Some(arg.value)
  }
}
