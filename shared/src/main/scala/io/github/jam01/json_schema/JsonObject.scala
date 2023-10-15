package io.github.jam01.json_schema

import scala.collection.mutable.ArrayBuffer

class JsonObject(val mMap: LinkedHashMap[String, Any]) {
  def getBoolean(s: String): Option[Boolean] = {
    Option(mMap.getValue(s).asInstanceOf[Boolean])
  }

  def getInt(s: String): Option[Int] = {
    Option(mMap.getValue(s).asInstanceOf[Int])
  }

  def getLong(s: String): Option[Long] = {
    Option(mMap.getValue(s).asInstanceOf[Long])
  }

  def getDouble(s: String): Option[Double] = {
    Option(mMap.getValue(s).asInstanceOf[Double])
  }

  def getString(s: String): Option[String] = {
    Option(mMap.getValue(s).asInstanceOf[String])
  }

  def getJsonObject(s: String): Option[JsonObject] = {
    Option(mMap.getValue(s).asInstanceOf[JsonObject])
  }

  def getStringArrayOpt(s: String): Option[ArrayBuffer[String]] = {
    Option(mMap.getValue(s).asInstanceOf[ArrayBuffer[String]])
  }

  def getStringArray(s: String): ArrayBuffer[String] = {
    val arr = mMap.getValue(s)
    if (arr != null) arr.asInstanceOf[ArrayBuffer[String]] else new ArrayBuffer[String](0)
  }

  def getAsStringArray(s: String): ArrayBuffer[String] = {
    val o = mMap.getValue(s)
    o match
      case x: collection.Seq[Any] => x.asInstanceOf[ArrayBuffer[String]]
      case _ => ArrayBuffer(o.asInstanceOf[String])
  }

  def getJsonObjectArrayOpt(s: String): Option[ArrayBuffer[JsonObject]] = {
    Option(mMap.getValue(s).asInstanceOf[ArrayBuffer[JsonObject]])
  }

  def getJsonObjectArray(s: String): ArrayBuffer[JsonObject] = {
    val arr = mMap.getValue(s)
    if (arr != null) arr.asInstanceOf[ArrayBuffer[JsonObject]] else new ArrayBuffer[JsonObject](0)
  }
}
