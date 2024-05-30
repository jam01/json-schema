package io.github.jam01.json_schema

import io.github.jam01.json_schema.ObjSchema.{getOrThrow, refError}

import scala.collection.{Map, Seq}

private[json_schema] trait ObjSchema { this: ObjectSchema => // https://docs.scala-lang.org/tour/self-types.html
  def getId: Option[String] = {
    getString("$id")
  }

  def getLocation: Uri = {
    val effbase = parent.map(_.getBase).getOrElse(docbase)
    val parentRel = parent.map(p => p.getLocation).map(u => u.resolve(appendedFrag(u, prel.get)))
    val r = getId.map(id => effbase.resolve(id)).getOrElse(parentRel.getOrElse(effbase))
    r
  }

  private def appendedFrag(u: Uri, frag: String): String = {
    val rfrag = java.net.URI(null, null, null, frag).getRawFragment // 
    if (u.uri.getFragment eq null) "#" + rfrag
    else "#" + JsonPointer(u.uri.getRawFragment).appended(JsonPointer(rfrag))
  }

  def getBase: Uri = { // TODO: make sure to remove suffix #
    val effbase = parent.map(_.getBase).getOrElse(docbase)
    getId.map(id => effbase.resolve(id)).getOrElse(effbase)
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
   * Optionally returns the boolean associated with the given key
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
   * Optionally returns the int associated with the given key
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
   * Optionally returns the number associated with the given key
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
   * Optionally returns the string associated with the given key
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
   * Optionally returns the JSON object associated with the given key
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
   * Optionally returns the JSON array associated with the given key
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
   * Returns the JSON array associated with the given key
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
   * Returns the JSON array of strings associated with the given key
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
   * Returns the JSON value associated with the given key as an array of strings, wrapping a single string value if 
   * found
   *
   * @throws IllegalStateException if the value is not an Array of strings or a String
   * @param k the entry key
   * @return the Seq[String] value, possibly wrapping a string, or an empty Seq if the entry does not exist
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
   * Optionally returns the Schema associated with the given key
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
   * Optionally returns the JSON array of Schemas associated with the given key
   *
   * @throws IllegalStateException if the value is not a JSON array of Schemas
   * @param k the entry key
   * @return an Option of the Seq[Schema] value, or None if the entry does not exist
   */
  def getSchemaArrayOpt(k: String): Option[Seq[Schema]] = {
    value.get(k).map(_.arr.map(_.sch))
  }

  /**
   * Optionally returns the JSON object of Schemas associated with the given key
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
}

object ObjSchema {
  private def getOrThrow[K, V](map: Map[K, V], k: K, err: Exception): V = {
    val ret = map.get(k)
    if (ret.isEmpty) throw err
    else ret.get
  }

  // TODO: check it fragment exists if not add it
  private def refError(ptr: JsonPointer, idx: Int): Exception =
    new IllegalArgumentException(s"invalid location ${ptr.refTokens.iterator.drop(idx + 1).mkString("/")}")
}
