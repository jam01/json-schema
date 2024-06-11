package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, OffsetDateTime}
import java.util.UUID

abstract class Format(schema: ObjectSchema,
             ctx: Context,
             path: JsonPointer,
             dynParent: Option[VocabBase]) extends VocabBase(schema, ctx, path, dynParent) {

  private val format: Option[String] = schema.getString("format")

  override def visitNull(index: Int): Seq[OutputUnit] = ???

  override def visitFalse(index: Int): Seq[OutputUnit] = ???

  override def visitTrue(index: Int): Seq[OutputUnit] = ???

  override def visitInt64(l: Long, index: Int): Seq[OutputUnit] = ???

  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = ???

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    ???
    
//    format.forall(_ match // TODO: use regexs
//      case "date-time" => try {
//        OffsetDateTime.parse(s);
//        true
//      } catch
//        case ex: DateTimeParseException => false
//      case "date" => try {
//        LocalDate.parse(s);
//        true
//      } catch
//        case ex: DateTimeParseException => false
//      case "duration" => try {
//        Duration.parse(s);
//        true
//      } catch
//        case ex: DateTimeParseException => false
//      case "uuid" => try {
//        UUID.fromString(_);
//        true
//      } catch
//        case ex: IllegalArgumentException => false
//      case _ => true) // TODO: throw unsupported exc
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[?, Seq[OutputUnit]] =
    ??? //BooleanSchemaValidator.True.visitArray(length, index)

  override def visitObject(length: Int, index: Int): ObjVisitor[?, Seq[OutputUnit]] =
    ??? //BooleanSchemaValidator.True.visitObject(length, index)}
}