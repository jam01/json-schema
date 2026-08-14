/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import io.github.jam01.json_schema.ObjSchema.{arrayIndex, getOrThrow, refError}

import scala.collection.Map

// see: https://docs.scala-lang.org/tour/self-types.html
private[json_schema] trait ObjSchema { this: ObjectSchema =>
  /**
   * Optionally retrieve the value of `$id`.
   *
   * @return an Option of the string value, or None if the entry does not exist
   */
  def getId: Option[String] = {
    getString("$id")
  }

  private var _loc: Uri = _ // lazy val is overkill

  /**
   * The resolved Uri of this Schema.
   */
  def location: Uri = {
    if (_loc != null) return _loc
    _loc = effectiveId.map(id => base.resolve(id))
      .getOrElse(parent.map(p => p.location)
        .map(u => u.appendedFragment(prel.get))
        .getOrElse(base))
    _loc
  }

  /**
   * `$id`, unless this schema is one a JSON Pointer landed on inside a non-schema location.
   *
   * Such a schema does not establish a schema resource: `SchemaR` neither registers it nor flushes
   * the `$id`/`$anchor`s under it, so honouring `$id` here would derive a base no [[Registry]] has
   * ever heard of, and every `$ref` inside it would resolve to a URI nothing can retrieve. Core
   * § 9.4.2 leaves this undefined precisely because these structures "would be subject to the
   * processing rules for `$id`" and cannot be identified reliably; of the two readings, not
   * establishing a resource is the one that never mints an unresolvable URI. See
   * [[https://github.com/jam01/json-schema/blob/main/docs/decisions/010-pointer-into-non-schema.md decision-010]].
   *
   * `$id` is still readable through [[getId]]; it just carries no identity.
   */
  private def effectiveId: Option[String] = if (isResource) getId else None

  private var _base: Uri = _ // lazy val is overkill

  /**
   * The document base Uri of this Schema.
   */
  def base: Uri = {
    if (_base != null) return _base
    val effbase = parent.map(_.base).getOrElse(docbase)
    _base = effectiveId.map(id => effbase.resolve(id)).getOrElse(effbase)
    _base
  }

  /**
   * Optionally retrieve the resolved Uri for `$ref`.
   *
   * @return an Option of the reference Uri, or None if the entry does not exist.
   */
  def getRef: Option[Uri] = {
    getString("$ref").map(ref => base.resolve(ref))
  }

  /**
   * Optionally retrieve the resolved Uri for `$dynamicRef`.
   *
   * @return an Option of the reference Uri, or None if the entry does not exist.
   */
  def getDynRef: Option[Uri] = {
    // see: https://github.com/json-schema-org/json-schema-spec/issues/1140
    getString("$dynamicRef").map(dynref => base.resolve(dynref, true))
  }

  /**
   * Optionally retrieve the value associated with the given key.
   *
   * @param k the key
   * @return an Option of the value, or None if the entry does not exist
   */
  def get(k: String): Option[Value] = {
    value.get(k)
  }

  /**
   * Optionally retrieve the Uri for `$schema`.
   *
   * @return an Option of the meta-schema Uri, or None if the entry does not exist.
   */
  def getMetaSchema: Option[Uri] = getString("$schema").map(s => Uri(s))

  def getVocabularies: Map[String, Boolean] =
    value.get("$vocabulary") match
      case None => Map.empty
      case Some(value) => value.obj.map((k, v) => (k, v.bool))

  /**
   * Optionally retrieve the boolean associated with the given key.
   *
   * @throws IllegalStateException if the value is not a boolean
   * @param k the entry key
   * @return an Option of the boolean value, or None if the entry does not exist
   */
  def getBoolean(k: String): Option[Boolean] = {
    value.get(k).map {
      case Bool(b) => b
      case _ => throw IllegalStateException("Expected Boolean")
    }
  }

  /**
   * Optionally retrieve the int associated with the given key.
   *
   * @throws IllegalStateException if the value is not a int
   * @param k the entry key
   * @return an Option of the int value, or None if the entry does not exist
   */
  def getInt(k: String): Option[Int] = {
    value.get(k).map {
      case Int64(i) if i.isValidInt => i.toInt
      case Int128(i) if i.isValidInt => i.toInt
      case Float64(i) if i.isValidInt => i.toInt
      case Decimal(i) if i.isValidInt => i.toInt
      case _ => throw IllegalStateException("Expected Integer")
    }
  }

  /**
   * Optionally retrieve the number associated with the given key.
   *
   * @throws IllegalStateException if the value is not a double
   * @param k the entry key
   * @return an Option of the number value, or None if the entry does not exist
   */
  def getNumber(k: String): Option[Num] = {
    value.get(k).map {
      case n: Num => n
      case _ => throw IllegalStateException("Expected Number")
    }
  }

  /**
   * Optionally retrieve the string associated with the given key.
   *
   * @throws IllegalStateException if the value is not a String
   * @param k the entry key
   * @return an Option of the String value, or None if the entry does not exist
   */
  def getString(k: String): Option[String] = {
    value.get(k).map {
      case Str(str) => str
      case _ => throw IllegalStateException("Expected String")
    }
  }

  /**
   * Optionally retrieve the JSON object associated with the given key.
   *
   * @throws IllegalStateException if the value is not a JSON object
   * @param k the entry key
   * @return an Option of the Map[String, Value] value, or None if the entry does not exist
   */
  def getObjectOpt(k: String): Option[Map[String, Value]] = {
    value.get(k).map {
      case Obj(obj) => obj
      case _ => throw IllegalStateException("Expected Object")
    }
  }

  /**
   * Optionally retrieve the JSON array associated with the given key.
   *
   * @throws IllegalStateException if the value is not a JSON array
   * @param k the entry key
   * @return an Option of the Seq[Value] value, or None if the entry does not exist
   */
  def getArrayOpt(k: String): Option[Seq[Value]] = {
    value.get(k).map {
      case Arr(arr) => arr
      case _ => throw IllegalStateException("Expected Array")
    }
  }

  /**
   * The JSON array associated with the given key.
   *
   * @throws IllegalStateException if the value is not an Array
   * @param k the entry key
   * @return the Seq[Value] value, or an empty Seq if the entry does not exist
   */
  def getArray(k: String): Seq[Value] = {
    value.get(k) match
      case None => Seq.empty
      case Some(unk) => unk match
        case Arr(arr) => arr
        case _ => throw IllegalStateException("Expected Array")
  }

  /**
   * The JSON array of strings associated with the given key.
   *
   * @throws IllegalStateException if the value is not an Array
   * @param k the entry key
   * @return the Seq[String] value, or an empty Seq if the entry does not exist
   */
  def getStringArray(k: String): Seq[String] = {
    value.get(k) match
      case None => Nil
      case Some(unk) => unk match
        case Arr(arr) => arr.map(_.str) // perf: consider using getArray and casting at item use-site
        case _ => throw IllegalStateException("Expected Array")
  }

  /**
   * The JSON value associated with the given key as an array of strings, wrapping a single string value if
   * found.
   *
   * @throws IllegalStateException if the value is not an Array of strings or a String
   * @param k the entry key
   * @return the Seq[String] value, possibly wrapping a string, or an empty Seq if the entry does not exist
   */
  def getAsStringArray(k: String): Seq[String] = {
    value.get(k) match
      case None => Nil
      case Some(unk) => unk match
        case Arr(arr) => arr.map(_.str) // perf: consider using getArray and casting at item use-site
        case Str(str) => Seq(str)
        case _ => throw IllegalStateException("Expected Array or String")
  }

  /**
   * Optionally retrieve the Schema associated with the given key.
   *
   * @throws IllegalStateException if the value is not a Schema
   * @param k the entry key
   * @return an Option of the Schema value, or None if the entry does not exist
   */
  def getSchemaOpt(k: String): Option[Schema] = {
    value.get(k).map {
      case sch: Schema => sch
      case _ => throw IllegalStateException("Expected Schema")
    }
  }

  /**
   * Optionally retrieve the JSON array of Schemas associated with the given key.
   *
   * @throws IllegalStateException if the value is not a JSON array of Schemas
   * @param k the entry key
   * @return an Option of the Seq[Schema] value, or None if the entry does not exist
   */
  def getSchemaArrayOpt(k: String): Option[Seq[Schema]] = {
    value.get(k).map(_.arr.map(_.sch))
  }

  /**
   * Optionally retrieve the JSON object of Schemas associated with the given key.
   *
   * @throws IllegalStateException if the value is not a JSON object of Schemas
   * @param k the entry key
   * @return an Option of the Map[String, Schema] value, or None if the entry does not exist
   */
  def getSchemaObjectOpt(k: String): Option[Map[String, Schema]] = {
    value.get(k).map(_.obj.map((k, v) => (k, v.sch)))
  }

  override def schBy0(ptr: JsonPointer): Schema = {
    var res: Value = this
    val it = ptr.refTokens.iterator; it.next() // skip first empty string token
    for (key <- it) {
      res = res match
        case ObjectSchema(value) => getOrThrow(value, key, ptr)
        case Obj(value) => getOrThrow(value, key, ptr)
        case Arr(value) =>
          val i = arrayIndex(key)
          if (i < 0 || value.length <= i) throw refError(ptr)
          value(i)
        case x: Any => throw new IllegalStateException(s"Unsupported type ${x.getClass.getName} for reference $ptr")
    }

    res match
      // The location wasn't recognized as a schema-bearing keyword while parsing (e.g. nested under
      // an unknown/arbitrary keyword, or under a known non-applicator like `examples`), so it's still
      // a raw literal here. Per Core § Fragment Identifiers, a JSON Pointer fragment resolves against
      // the schema resource as plain JSON; any object/boolean found this way is a valid subschema
      // regardless of which keyword contains it - compile it now, anchored to this schema so its
      // `location`/`base` resolve the same as if it had been recognized up front.
      //
      // This runs the literal back through SchemaR rather than just wrapping it in an ObjectSchema:
      // ObjectSchema's keyword accessors don't parse, they assume SchemaR already turned every
      // schema-position child into a Schema, so a wrap-only conversion is one node deep and any
      // applicator inside (`properties`, `items`, `allOf`, ...) blows up on `Value.sch`.
      case sch: Schema => sch
      case obj: Obj => compiledLiteral(ptr, obj)
      case True => TrueSchema
      case False => FalseSchema
      case _ => throw refError(ptr)
  }

  @volatile private var _compiled: java.util.concurrent.ConcurrentHashMap[JsonPointer, Schema] = _ // only allocated if a literal is reached

  /**
   * The compiled form of the raw literal at `ptr`, compiled once per location.
   *
   * Resolution is per-reference and per-`Core`-instance, so without this every `$ref` into the
   * same literal would run the whole subtree back through `SchemaR` again and hand back a
   * different `Schema` graph for one location. A self-referential literal makes that per level of
   * recursion, which left the depth guard as the only bound on repeated compilation.
   *
   * Compiling does not resolve `$ref`, so this cannot re-enter. The lock guards only the lazy map
   * allocation; `ConcurrentHashMap.computeIfAbsent` then serializes compilation per `ptr`, not
   * across every literal this instance ever compiles.
   */
  private def compiledLiteral(ptr: JsonPointer, obj: Obj): Schema = {
    var m = _compiled
    if (m == null) synchronized {
      m = _compiled
      if (m == null) {
        m = new java.util.concurrent.ConcurrentHashMap()
        _compiled = m
      }
    }

    m.computeIfAbsent(ptr, _ => SchemaW.transform(obj, SchemaR.subschema(docbase, this, ptr.toString)))
  }
}

object ObjSchema {
  private def getOrThrow[K, V](map: Map[K, V], k: K, ptr: JsonPointer): V = {
    map.get(k) match
      case Some(value) => value
      case None => throw refError(ptr)
  }

  private def refError(ptr: JsonPointer): Exception =
    new SchemaRetrievalException(s"Invalid reference location $ptr")

  /**
   * The array index a reference token denotes, or -1 if it denotes none.
   *
   * RFC 6901 § 4 spells an index as `0` or a digit sequence with no leading zero, and gives `-`
   * the element after the last - which no array has. Every other token, including one too wide for
   * `Int`, addresses nothing, and per § 5 a pointer that addresses nothing is an error: the same
   * `SchemaRetrievalException` any unresolvable reference reports, not the token parse escaping.
   */
  private def arrayIndex(key: String): Int = {
    if (key.isEmpty || (key.charAt(0) == '0' && key.length > 1)) return -1
    var i = 0
    while (i < key.length) {
      val c = key.charAt(i)
      if (c < '0' || c > '9') return -1 // Char.isDigit would take non-ASCII decimal digits too
      i += 1
    }
    key.toIntOption.getOrElse(-1)
  }
}

