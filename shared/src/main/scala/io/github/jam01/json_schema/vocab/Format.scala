package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema._
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, OffsetDateTime}
import java.util.UUID

class Format(schema: ObjectSchema,
             ctx: Context = Context.Empty,
             path: JsonPointer = JsonPointer(),
             dynParent: Option[VocabValidator] = None) extends VocabValidator(schema, ctx, path, dynParent) {

  private val format: Option[String] = schema.getString("format")

  override def visitNull(index: Int): Boolean = true
  override def visitFalse(index: Int): Boolean = true
  override def visitTrue(index: Int): Boolean = true
  override def visitInt64(l: Long, index: Int): Boolean = true
  override def visitFloat64(d: Double, index: Int): Boolean = true

  override def visitString(s: CharSequence, index: Int): Boolean = {
    format.forall(_ match // TODO: use regexs
      case "date-time" => try {
        OffsetDateTime.parse(s); true
      } catch
        case ex: DateTimeParseException => false
      case "date" => try {
        LocalDate.parse(s); true
      } catch
        case ex: DateTimeParseException => false
      case "duration" => try {
        Duration.parse(s); true
      } catch
        case ex: DateTimeParseException => false
      case "uuid" => try {
        UUID.fromString(_); true
      } catch
        case ex: IllegalArgumentException => false
      case _ => true) // TODO: throw unsupported exc
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[_, Boolean] =
    BooleanSchemaValidator.True.visitArray(length, index)

  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] =
    BooleanSchemaValidator.True.visitObject(length, index)}
