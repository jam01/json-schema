package io.github.jam01.json_schema

import io.github.jam01.json_schema.ObjSchema.{getOrThrow, refError}

import java.net.URI
import scala.collection.{Map, Seq, immutable}

private[json_schema] trait ObjSchema { this: ObjectSchema => // https://docs.scala-lang.org/tour/self-types.html
  def getId: Option[String] = {
    getString("$id")
  }

  def getBase: Uri = {
    val base = parent.map(_.getBase).getOrElse(docbase)
    getId.map(id => base.resolve(id)).getOrElse(base)
  }

  def getRef: Option[Uri] = {
    getString("$ref").map(ref => getBase.resolve(ref))
  }

  def getDynRef: Option[Uri] = {
    getString("$dynamicRef").map(dynref => getBase.resolve(dynref, true))
  }

  def get(k: String): Option[Value] = {
    value.get(k)
  }

  /**
   * Optionally returns the boolean associated with the given key.
   *
   * @throws ClassCastException if the value is not a boolean
   * @param s the entry key
   * @return an Option of the value cast as a boolean, or None if the entry has a null value or does not exist
   */
  def getBoolean(k: String): Option[Boolean] = {
    value.get(k).map {
      case Bool(b) => b
      case _ => throw IllegalStateException("Expected Boolean")
    }
  }

  // see: https://stackoverflow.com/q/11338954
  /**
   * Optionally returns the int associated with the given key.
   *
   * @throws ClassCastException if the value is not a int
   * @param s the entry key
   * @return an Option of the value cast as a int, or None if the entry has a null value or does not exist
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
   * Optionally returns the long or double associated with the given key.
   *
   * @throws ClassCastException if the value is not a double
   * @param s the entry key
   * @return an Option of the value cast as a long, or None if the entry has a null value or does not exist
   */
  def getNumber(k: String): Option[Long | Double] = {
    value.get(k).map {
      case Num(num) => num
      case _ => throw IllegalStateException("Expected Number")
    }
  }

  /**
   * Optionally returns the String associated with the given key.
   *
   * @throws ClassCastException if the value is not a String
   * @param s the entry key
   * @return an Option of the value cast as a String, or None if the entry has a null value or does not exist
   */
  def getString(k: String): Option[String] = {
    value.get(k).map {
      case Str(str) => str
      case _ => throw IllegalStateException("Expected String")
    }
  }

  /**
   * Optionally returns the JSON Object associated with the given key.
   *
   * @throws ClassCastException if the value is not a JSON Object
   * @param s the entry key
   * @return an Option of the value cast as a Map[String, Value], or None if the entry has a null value or does not exist
   */
  def getObjectOpt(k: String): Option[Map[String, Value]] = {
    value.get(k).map {
      case Obj(obj) => obj
      case _ => throw IllegalStateException("Expected Object")
    }
  }

  /**
   * Optionally returns the JSON Array associated with the given key.
   *
   * @throws ClassCastException if the value is not a JSON Array
   * @param s the entry key
   * @return an Option of the value cast as a Seq[Value], or None if the entry has a null value or does not exist
   */
  def getArrayOpt(k: String): Option[Seq[Value]] = {
    value.get(k).map {
      case Arr(arr) => arr
      case _ => throw IllegalStateException("Expected Array")
    }
  }

  /**
   * Returns the JSON Array associated with the given key.
   *
   * @throws ClassCastException if the value is not an Array
   * @param s the entry key
   * @return the value cast as Seq[Value], or an empty Seq if the entry has a null value or does not exist
   */
  def getArray(k: String): Seq[Value] = {
    value.get(k) match
      case None => Seq.empty
      case Some(unk) => unk match
        case Arr(arr) => arr
        case _ => throw IllegalStateException("Expected Array")
  }

  /**
   * Returns the JSON Array of Strings associated with the given key.
   *
   * @throws ClassCastException if the value is not an Array
   * @param s the entry key
   * @return the value cast as Seq[String], or an empty Seq if the entry has a null value or does not exist
   */
  def getStringArray(k: String): Seq[String] = {
    value.get(k) match
      case None => Nil
      case Some(unk) => unk match
        case Arr(arr) => arr.map(_.str) // perf: consider using getArray and casting at item use-site
        case _ => throw IllegalStateException("Expected Array")
  }

  /**
   * Returns the JSON value associated with the given key as an Array of Strings, wrapping the String value if it's not
   * an Array.
   *
   * @throws ClassCastException if the value is not an Array of Strings or an single String
   * @param s the entry key
   * @return the value cast or wrapped as Seq[String], or an empty Seq if the entry has a null value or does not exist
   */
  def getAsStringArray(k: String): collection.Seq[String] = {
    value.get(k) match
      case None => Nil
      case Some(unk) => unk match
        case Arr(arr) => arr.map(_.str) // perf: consider using getArray and casting at item use-site
        case Str(str) => Seq(str)
        case _ => throw IllegalStateException("Expected Array or String")
  }

  /**
   * Optionally returns the Schema associated with the given key, potentially converting a JSON value into a Schema.
   *
   * @throws ClassCastException if the value is not a Schema or cannot be converted into one
   * @param s the entry key
   * @return an Option of the value cast or converted to a Schema, or None if the entry has a null value or does not exist
   */
  def getSchemaOpt(k: String): Option[Schema] = {
    value.get(k).map {
      case sch: Schema => sch
      case _ => throw IllegalStateException("Expected Schema")
    }
  }

  /**
   * Optionally returns the JSON Array of Schemas associated with the given key, potentially converting JSON value into
   * Schemas.
   *
   * @throws ClassCastException if the value is not a JSON Array of Schemas or its items cannot be converted to Schemas
   * @param s the entry key
   * @return an Option of the value cast as a Seq[Schema], or None if the entry has a null value or does not exist
   */
  def getSchemaArrayOpt(k: String): Option[Seq[Schema]] = {
    value.get(k).map(_.arr.map(_.sch))
  }

  def getSchemaObjectOpt(k: String): Option[Map[String, Schema]] = {
    value.get(k).map(_.obj.map((k, v) => (k, v.sch)))
  }

  override def schBy0(ptr: JsonPointer): Schema = {
    var i = 0
    var res: Value = this
    val it = ptr.refTokens.iterator; it.next() // skip first empty string token // TODO: consider a ROOT Ptr
    for (key <- it) { // TODO: unescape?
      res = res match
        case ObjectSchema(obj, _, _, _) => getOrThrow(obj, key, refError(ptr, i))
        case Obj(value) => getOrThrow(value, key, refError(ptr, i))
        case Arr(value) =>
          val i = key.toInt;
          if (value.length <= i) throw refError(ptr, i)
          value(i)
        case x: Any => throw new IllegalArgumentException(s"unsupported type ${x.getClass.getName}")

      i = i + 1
    }

    res.asInstanceOf[Schema]
  }

  override def toString: String = value.toString()
}

object ObjSchema {
  private def getOrThrow[K, V](map: Map[K, V], k: K, err: Exception): V = {
    val ret = map.get(k)
    if (ret.isEmpty) throw err
    else ret.get
  }

  // check it fragment exists if not add it
  private def refError(ptr: JsonPointer, idx: Int): Exception =
    new IllegalArgumentException(s"invalid location ${ptr.refTokens.iterator.drop(idx + 1).mkString("/")}")
}
