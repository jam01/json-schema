package io.github.jam01.json_schema

import io.github.jam01.json_schema.ObjSchema.{getOrThrow, refError}

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
    _loc = getId.map(id => base.resolve(id))
      .getOrElse(parent.map(p => p.location)
        .map(u => u.appendedFragment(prel.get))
        .getOrElse(base))
    _loc
  }

  private var _base: Uri = _ // lazy val is overkill

  /**
   * The document base Uri of this Schema.
   */
  def base: Uri = {
    if (_base != null) return _base
    val effbase = parent.map(_.base).getOrElse(docbase)
    _base = getId.map(id => effbase.resolve(id)).getOrElse(effbase)
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
      case Num(num) => num match
        case l: Long => l.intValue
        case d: Double => d.intValue
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
  def getNumber(k: String): Option[Long | Double] = {
    value.get(k).map {
      case Num(num) => num
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
    var i = 0
    var res: Value = this
    val it = ptr.refTokens.iterator; it.next() // skip first empty string token
    for (key <- it) {
      res = res match
        case ObjectSchema(value) => getOrThrow(value, key, ptr)
        case Obj(value) => getOrThrow(value, key, ptr)
        case Arr(value) =>
          val i = key.toInt
          if (value.length <= i) throw refError(ptr)
          value(i)
        case x: Any => throw new IllegalArgumentException(s"unsupported type ${x.getClass.getName}")

      i = i + 1
    }

    res.asInstanceOf[Schema]
  }
}

object ObjSchema {
  private def getOrThrow[K, V](map: Map[K, V], k: K, ptr: JsonPointer): V = {
    map.get(k) match
      case Some(value) => value
      case None => throw refError(ptr)
  }

  private def refError(ptr: JsonPointer): Exception =
    new IllegalArgumentException(s"invalid location $ptr")
}

