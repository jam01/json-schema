package upickle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import upickle.core.Visitor
import upickle.jsoniter.VisitorDecoder.NIDX

import java.nio.charset.StandardCharsets

// see: https://github.com/rallyhealth/weePickle/pull/105
// https://github.com/plokhotnyuk/jsoniter-scala/blob/v2.23.5/jsoniter-scala-circe/shared/src/main/scala/io/circe/JsoniterScalaCodec.scala
// https://github.com/evolution-gaming/play-json-tools/blob/v1.0.0/play-json-jsoniter/shared/src/main/scala/play/api/libs/json/JsonValueCodecJsValue.scala
// https://github.com/com-lihaoyi/upickle/pull/467#issuecomment-1473358589
// https://github.com/com-lihaoyi/upickle/blob/3.1.3/ujson/src/ujson/JsVisitor.scala
object VisitorDecoder {
  private val NIDX = -1

//  def defaultNumberParser[N](v: Visitor[_, N]): JsonReader => N = in => {
//  def defaultNumberParser0[N]: (JsonReader, Visitor[_, N]) => N = (in, v) => {
  val defaultNumberParser: (JsonReader, Visitor[_, _]) => Any = (in, v) => {
    in.setMark()
    var isNeg = false
    var digits = 0
    var b = in.nextByte()
    if (b >= '0' && b <= '9') digits += 1
    else if (b == '-') {
      b = in.nextByte()
      isNeg = true
    }
    while ((b >= '0' && b <= '9') && in.hasRemaining()) {
      b = in.nextByte()
      digits += 1
    }
    in.rollbackToMark()
    if ((b | 0x20) != 'e' && b != '.') {
      if (digits < 19) {
        if (digits < 10) v.visitInt32(in.readInt(), NIDX)
        else v.visitInt64(in.readLong(), NIDX)
      } else {
        val x = in.readBigInt(null)
        if (x.isValidLong) v.visitInt64(x.longValue, NIDX)
        else v.visitString(x.toString(), NIDX) // see: ujson/JsVisitor.scala#L33
        // alt: v.visitFloat64StringParts(x.toString(), -1, -1, NIDX)
      }
    } else {
      in.setMark()
      val y = in.readDouble() // readDouble() returns Double.Infinity if too large
      if (y.isFinite) { // https://github.com/openjdk/jdk/pull/9238
        v.visitFloat64(y, NIDX)
        // in.setMark(); in.rollbackToMark() // clear mark needed ???
      }
      // alt: readBigDecimal and check BigDecimal.isDecimalDouble
      else {
        in.rollbackToMark()
        v.visitString(new String(in.readRawValAsBytes(), StandardCharsets.US_ASCII), NIDX) // see: ujson/JsVisitor.scala#L33
        // alt: v.visitFloat64StringParts(new String(in.readRaw...), -1, -1, NIDX)
      }
    }
  }
}

final class VisitorDecoder[J](
                               maxDepth: Int,
                               numberParser: (JsonReader, Visitor[_, _]) => Any,
                               v: Visitor[_, J]) extends JsonValueCodec[J] {
  override def nullValue: J = null.asInstanceOf[J]

  override def encodeValue(x: J, out: JsonWriter): Unit =
    throw new UnsupportedOperationException("only supports decoding")

  override def decodeValue(in: JsonReader, default: J): J =
    decode(in, maxDepth, v)

  private[this] def decode[Z](in: JsonReader, depth: Int, v: Visitor[_, Z]): Z = {
    val b = in.nextToken()
    if (b == '"') {
      in.rollbackToken()
      v.visitString(in.readString(null), NIDX)
    } else if (b == 'f' || b == 't') {
      in.rollbackToken()
      if (in.readBoolean()) v.visitTrue(NIDX)
      else v.visitFalse(NIDX)
    } else if (b >= '0' && b <= '9' || b == '-') {
      in.rollbackToken()
      numberParser(in, v).asInstanceOf[Z] // TODO: performance cost?
    } else if (b == '[') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      val isEmpty = in.isNextToken(']')
      val arrV = v.visitArray(if (isEmpty) 0 else -1, NIDX).narrow
      if (!isEmpty) {
        in.rollbackToken()
        while ( {
          arrV.visitValue(decode(in, depthM1, arrV.subVisitor), NIDX)
          in.isNextToken(',')
        }) ()
        if (!in.isCurrentToken(']')) in.arrayEndOrCommaError()
      }
      arrV.visitEnd(NIDX)
    } else if (b == '{') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      val isEmpty = in.isNextToken('}')
      val objV = v.visitObject(if (isEmpty) 0 else -1, true, NIDX).narrow
      if (!isEmpty) {
        in.rollbackToken()
        var key = "?"
        while ( {
          key = in.readKeyAsString()
          objV.visitKeyValue(objV.visitKey(NIDX).visitString(key, NIDX))
          objV.visitValue(decode(in, depthM1, objV.subVisitor), NIDX)
          in.isNextToken(',')
        }) ()
        if (!in.isCurrentToken('}')) in.objectEndOrCommaError()
      }
      objV.visitEnd(NIDX)
    } else in.readNullOrError(v.visitNull(NIDX), "expected JSON value")
  }
}
