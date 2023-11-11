package upickle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter
import io.github.jam01.json_schema.JsonVisitor
import upickle.core.{ArrVisitor, ObjVisitor, StringVisitor, Visitor}

// see: https://github.com/rallyhealth/weePickle/pull/105
class JsonWriterVisitor(writer: JsonWriter) extends JsonVisitor[Any, JsonWriter] {
  override def visitObject(length: Int, index: Int): ObjVisitor[Any, JsonWriter] = {
    writer.writeObjectStart()

    new ObjVisitor[Any, JsonWriter] {
      override def visitKey(index: Int): Visitor[_, _] = StringVisitor

      override def visitKeyValue(v: Any): Unit = writer.writeKey(v.toString)

      override def subVisitor: Visitor[_, _] = JsonWriterVisitor.this

      override def visitValue(v: Any, index: Int): Unit = ()

      override def visitEnd(index: Int): JsonWriter = {
        writer.writeObjectEnd()
        writer
      }
    }
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Any, JsonWriter] = {
    writer.writeArrayStart()

    // TODO: consider noOp if length == 0
    new ArrVisitor[Any, JsonWriter] {
      override def subVisitor: Visitor[_, _] = JsonWriterVisitor.this

      override def visitValue(v: Any, index: Int): Unit = ()

      override def visitEnd(index: Int): JsonWriter = {
        writer.writeArrayEnd()
        writer
      }
    }
  }
  override def visitNull(index: Int): JsonWriter = {
    writer.writeNull()
    writer
  }

  override def visitFalse(index: Int): JsonWriter = {
    writer.writeVal(false)
    writer
  }

  override def visitTrue(index: Int): JsonWriter = {
    writer.writeVal(true)
    writer
  }

  override def visitFloat64(d: Double, index: Int): JsonWriter = {
    writer.writeVal(d)
    writer
  }


  override def visitFloat64String(s: String, index: Int): JsonWriter = {
    writer.writeNonEscapedAsciiVal(s)
    writer
  }


  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): JsonWriter = {
    writer.writeNonEscapedAsciiVal(s.toString)
    writer
  }

  override def visitInt64(i: Long, index: Int): JsonWriter = {
    writer.writeVal(i)
    writer
  }

  override def visitString(s: CharSequence, index: Int): JsonWriter = {
    writer.writeVal(s.toString)
    writer
  }

  override def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int): JsonWriter = {
    val trimmed =
      if (offset == 0 && bytes.length <= len) bytes
      else bytes.slice(offset, offset + len)

    writer.writeBase64Val(trimmed, true)
    writer
  }
}
