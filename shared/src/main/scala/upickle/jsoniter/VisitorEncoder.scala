package upickle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import io.github.jam01.json_schema.Reader

class VisitorEncoder extends JsonValueCodec[Any] {
  override def nullValue: Any = null

  override def decodeValue(in: JsonReader, default: Any): Any =
    throw new UnsupportedOperationException("only supports encoding")

  override def encodeValue(x: Any, out: JsonWriter): Unit =
    Reader.transform(x, new JsonWriterVisitor(out))
}
