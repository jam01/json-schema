package io.github.jam01.json_schema

import scala.collection.mutable.ArrayBuffer

class ObjectSchema(mMap: LinkedHashMap[String, Any]) extends JsonObject(mMap) {
  def getSchema(s: String): Option[_ >: Schema] = {
    val sch = mMap.getValue(s)
    sch match
      case b: Boolean => Option(if (b) BooleanSchema.True else BooleanSchema.False)
      case obj: JsonObject => Option(new ObjectSchema(obj.mMap))
      case _ => Option(sch.asInstanceOf[Schema])
  }

  def getSchemaArrayOpt(s: String): Option[ArrayBuffer[_ >: Schema]] = {
    val elems = mMap.getValue(s).asInstanceOf[ArrayBuffer[Any]]
    val schs = elems.mapInPlace {
      case b: Boolean => if (b) BooleanSchema.True else BooleanSchema.False
      case obj: JsonObject => new ObjectSchema(obj.mMap)
      case _ => asInstanceOf[Schema]
    }

    Option(schs.asInstanceOf[ArrayBuffer[_ >: Schema]])
  }

  def getSchemaArray(s: String): ArrayBuffer[_ >: Schema] = {
    val arr = mMap.getValue(s)
    val elems = if (arr != null) arr.asInstanceOf[ArrayBuffer[Any]] else new ArrayBuffer[Any](0)
    val schs = elems.mapInPlace {
      case b: Boolean => if (b) BooleanSchema.True else BooleanSchema.False
      case obj: JsonObject => new ObjectSchema(obj.mMap)
      case _ => asInstanceOf[Schema]
    }

    schs.asInstanceOf[ArrayBuffer[_ >: Schema]]
  }
}
