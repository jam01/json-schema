package io.github.jam01.json_schema

import io.github.jam01.json_schema.ObjSchema.refError

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Map, Seq, immutable}

// TODO: do we need indexedSeq?
private[json_schema] trait ObjSchema { this: ObjectSchema => // https://docs.scala-lang.org/tour/self-types.html
  def getId: Option[String] = {
    getString("$id")
  }

  def getLocation: String = {
    getId.getOrElse(parent.map(_.getLocation + prel.get).getOrElse(initbase))
  }

  override def schBy0(ptr: JsonPointer): Schema = {
    var i = 0
    var res: Any = this
    for (key <- ptr.refTokens.iterator) { // TODO: unescape?
      res = res match
        case ObjectSchema(omMap, _, _, _) => omMap.getOrElse(key, refError(ptr, i))
        case obj: collection.Map[String, Any] => obj.getOrElse(key, refError(ptr, i))
        case arr: collection.IndexedSeq[Any] =>
          val i = key.toInt;
          if (arr.length <= i) ObjSchema.refError(ptr, i)
          arr(i)
        case x: Any => throw new IllegalArgumentException(s"unsupported type ${x.getClass.getName}")

      i = i + 1
    }

    res match
//      case b: Boolean => BooleanSchema(b)
//      case obj: LinkedHashMap[String, Any] => ObjectSchema(obj, base, Some(this)) // TODO: loc + ptr.mkstring escaped
      // consider specialized StringMap[V]-like
      case x => x.asInstanceOf[Schema]
  }

  /**
   * Optionally returns the boolean associated with the given key.
   *
   * @throws ClassCastException if the value is not a boolean
   * @param s the entry key
   * @return an Option of the value cast as a boolean, or None if the entry has a null value or does not exist
   */
  def getBoolean(s: String): Option[Boolean] = {
    Option(mMap.getValue(s).asInstanceOf[Boolean])
  }

  // see: https://stackoverflow.com/q/11338954

  /**
   * Optionally returns the int associated with the given key.
   *
   * @throws ClassCastException if the value is not a int
   * @param s the entry key
   * @return an Option of the value cast as a int, or None if the entry has a null value or does not exist
   */
  def getInt(s: String): Option[Int] = {
    mMap.getValue(s) match
      case null => None
      case x => Some(x.asInstanceOf[Int])
  }

  /**
   * Optionally returns the long associated with the given key.
   *
   * @throws ClassCastException if the value is not a long
   * @param s the entry key
   * @return an Option of the value cast as a long, or None if the entry has a null value or does not exist
   */
  def getLong(s: String): Option[Long] = {
    mMap.getValue(s) match
      case null => None
      case x => Some(x.asInstanceOf[Long])
  }

  /**
   * Optionally returns the double associated with the given key.
   *
   * @throws ClassCastException if the value is not a double
   * @param s the entry key
   * @return an Option of the value cast as a long, or None if the entry has a null value or does not exist
   */
  def getDouble(s: String): Option[Double] = {
    mMap.getValue(s) match
      case null => None
      case x => Some(x.asInstanceOf[Double])
  }

  /**
   * Optionally returns the long or double associated with the given key.
   *
   * @throws ClassCastException if the value is not a double
   * @param s the entry key
   * @return an Option of the value cast as a long, or None if the entry has a null value or does not exist
   */
  def getNumber(s: String): Option[Long | Double] = {
    mMap.getValue(s) match
      case null => None
      case l: Long => Option(l)
      case d: Double => Option(d)
      case x => throw ClassCastException(s"class ${x.getClass.getName} cannot be cast to class java.lang.Long or java.lang.Double")
  }

  /**
   * Optionally returns the String associated with the given key.
   *
   * @throws ClassCastException if the value is not a String
   * @param s the entry key
   * @return an Option of the value cast as a String, or None if the entry has a null value or does not exist
   */
  def getString(s: String): Option[String] = {
    Option(mMap.getValue(s).asInstanceOf[String])
  }

  /**
   * Optionally returns the JSON Object associated with the given key.
   *
   * @throws ClassCastException if the value is not a JSON Object
   * @param s the entry key
   * @return an Option of the value cast as a Map[String, Any], or None if the entry has a null value or does not exist
   */
  def getObjectOpt(s: String): Option[Map[String, Any]] = {
    Option(mMap.getValue(s).asInstanceOf[Map[String, Any]])
  }

  /**
   * Optionally returns the JSON Array associated with the given key.
   *
   * @throws ClassCastException if the value is not a JSON Array
   * @param s the entry key
   * @return an Option of the value cast as a Seq[Any], or None if the entry has a null value or does not exist
   */
  def getArrayOpt(s: String): Option[Seq[Any]] = {
    Option(mMap.getValue(s).asInstanceOf[Seq[Any]])
  }

  /**
   * Returns the JSON Array associated with the given key.
   *
   * @throws ClassCastException if the value is not an Array
   * @param s the entry key
   * @return the value cast as Seq[Any], or an empty Seq if the entry has a null value or does not exist
   */
  def getArray(s: String): Seq[Any] = {
    mMap.getValue(s) match
      case null => Seq.empty
      case x: Any => x.asInstanceOf[Seq[Any]]
  }

  /**
   * Optionally returns the JSON Array of Strings associated with the given key.
   *
   * @throws ClassCastException if the value is not a JSON Array of Strings
   * @param s the entry key
   * @return an Option of the value cast as a Seq[String], or None if the entry has a null value or does not exist
   */
  def getStringArrayOpt(s: String): Option[Seq[String]] = {
    Option(mMap.getValue(s).asInstanceOf[Seq[String]])
  }

  /**
   * Returns the JSON Array of Strings associated with the given key.
   *
   * @throws ClassCastException if the value is not an Array
   * @param s the entry key
   * @return the value cast as Seq[String], or an empty Seq if the entry has a null value or does not exist
   */
  def getStringArray(s: String): Seq[String] = {
    mMap.getValue(s) match
      case null => Seq.empty
      case x: Any => x.asInstanceOf[Seq[String]]
  }

  /**
   * Returns the JSON value associated with the given key as an Array of Strings, wrapping the String value if it's not
   * an Array.
   *
   * @throws ClassCastException if the value is not an Array of Strings or an single String
   * @param s the entry key
   * @return the value cast or wrapped as Seq[String], or an empty Seq if the entry has a null value or does not exist
   */
  def getAsStringArray(s: String): collection.Seq[String] = {
    mMap.getValue(s) match
      case seq: collection.Seq[Any] => seq.asInstanceOf[collection.Seq[String]]
      case null => Nil
      case x => immutable.IndexedSeq(x.asInstanceOf[String])
  }

  /**
   * Returns the JSON Array of Objects associated with the given key.
   *
   * @throws ClassCastException if the value is not an Array
   * @param s the entry key
   * @return the value cast as Seq[Map[String, Any]], or an empty Seq if the entry has a null value or does not exist
   */
  def getObjectArray(s: String): collection.IndexedSeq[Map[String, Any]] = {
    mMap.getValue(s) match
      case seq: collection.IndexedSeq[Any] => seq.asInstanceOf[collection.IndexedSeq[Map[String, Any]]]
      case x => immutable.IndexedSeq(x.asInstanceOf[Map[String, Any]])
  }

  /**
   * Optionally returns the Schema associated with the given key, potentially converting a JSON value into a Schema.
   *
   * @throws ClassCastException if the value is not a Schema or cannot be converted into one
   * @param s the entry key
   * @return an Option of the value cast or converted to a Schema, or None if the entry has a null value or does not exist
   */
  def getAsSchemaOpt(s: String): Option[Schema] = {
    mMap.getValue(s) match
//      case b: Boolean => Option(BooleanSchema(b))
//      case obj: LinkedHashMap[String, Any] => Option(ObjectSchema(obj, base, Some(this))) // consider specialized StringMap[V]-like
      case x => Option(x.asInstanceOf[Schema])
  }

  /**
   * Optionally returns the JSON Array of Schemas associated with the given key, potentially converting JSON value into
   * Schemas.
   *
   * @throws ClassCastException if the value is not a JSON Array of Schemas or its items cannot be converted to Schemas
   * @param s the entry key
   * @return an Option of the value cast as a Seq[Schema], or None if the entry has a null value or does not exist
   */
  def getAsSchemaArrayOpt(s: String): Option[Seq[Schema]] = {
    val elems = mMap.getValue(s).asInstanceOf[ArrayBuffer[Any]]
    val schs = elems.mapInPlace {
//      case b: Boolean => BooleanSchema(b)
//      case obj: LinkedHashMap[String, Any] => ObjectSchema(obj, base, Some(this)) // consider specialized StringMap[V]-like
      case _ => asInstanceOf[Schema]
    }

    Option(schs.asInstanceOf[Seq[Schema]])
  }

  def getAsSchemaObjectOpt(s: String): Option[Map[String, Schema]] = {
    mMap.getValue(s) match
      case null => None
      case lhm: LinkedHashMap[String, Any] => Some(lhm.mapValuesInPlace {
//        case (_, b: Boolean) => BooleanSchema(b)
//        case (_, obj: LinkedHashMap[String, Any]) => ObjectSchema(obj, base, Some(this)) // consider specialized StringMap[V]-like
        case (_, x) => x.asInstanceOf[Schema]
      }.asInstanceOf[Map[String, Schema]])
      case x => Option(x.asInstanceOf[Map[String, Schema]])
  }

  /**
   * Returns the JSON Array of Schemas associated with the given key, potentially converting JSON value into
   * Schemas.
   *
   * @throws ClassCastException if the value is not a JSON Array of Schemas or its items cannot be converted to Schemas
   * @param s the entry key
   * @return the value cast as a Seq[Schema], or None if the entry has a null value or does not exist
   */
  def getAsSchemaArray(s: String): Seq[Schema] = {
    val arr = mMap.getValue(s)
    val elems = if (arr != null) arr.asInstanceOf[ArrayBuffer[Any]] else ArrayBuffer.empty
    val schs = elems.mapInPlace {
//      case b: Boolean => BooleanSchema(b)
//      case omMap: LinkedHashMap[String, Any] => ObjectSchema(omMap, base, Some(this)) // consider specialized StringMap[V]-like
      case x => x.asInstanceOf[Schema]
    }

    schs.asInstanceOf[Seq[Schema]]
  }
}

object ObjSchema {
  private def appendedRefToken(loc: String, refTok: String): String =
    loc + "/" + refTok // TODO: escape and check if needs to add #

  // check it fragment exists if not add it
  private def refError(ptr: JsonPointer, idx: Int): Unit =
    throw new IllegalArgumentException(s"invalid location ${ptr.refTokens.iterator.slice(0, idx).mkString("/")}")
}
