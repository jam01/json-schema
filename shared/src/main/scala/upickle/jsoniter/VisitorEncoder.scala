package upickle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import io.github.jam01.json_schema.{Null, SchemaW, Value}

object VisitorEncoder extends JsonValueCodec[Value] {
  override def nullValue: Value = Null

  override def decodeValue(in: JsonReader, default: Value): Value =
    throw new UnsupportedOperationException("Codec only supports encoding")

  override def encodeValue(x: Value, out: JsonWriter): Unit =
    SchemaW.transform(x, new JsonWriterVisitor(out))
}
