package io.github.jam01.json_schema

import java.time.{Duration, LocalDate, OffsetDateTime}
import upickle.core.{ArrVisitor, ObjVisitor, StringVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.time.format.DateTimeParseException
import scala.collection.mutable

class ObjectSchemaVisitor(val schema: ObjectSchema) extends JsonVisitor[Any, Boolean] {
  val tyype: collection.Seq[String] = schema.getAsStringArray("type")
  val pattern: Option[String] = schema.getString("pattern")
  val format: Option[String] = schema.getString("format")
  val maxLength: Option[Int] = schema.getInt("maxLength")
  val minLength: Option[Int] = schema.getInt("minLength")
  val maximum: Option[Long | Double] = schema.getLongOrDouble("maximum")
  val minimum: Option[Long | Double] = schema.getLongOrDouble("minimum")
  val maxItems: Option[Int] = schema.getInt("maxItems")
  val minItems: Option[Int] = schema.getInt("minItems")
  val required: collection.Seq[String] = schema.getStringArray("required")
  val maxProperties: Option[Int] = schema.getInt("maxProperties")
  val minProperties: Option[Int] = schema.getInt("minProperties")
  val items: Option[_ >: Schema] = schema.getAsSchemaOpt("items")

  override def visitNull(index: Int): Boolean = {
    tyype.exists("null".eq)
  }

  override def visitFalse(index: Int): Boolean = {
    tyype.exists("boolean".eq)
  }

  override def visitTrue(index: Int): Boolean = {
    tyype.exists("boolean".eq)
  }

  override def visitInt64(i: Long, index: Int): Boolean = {
    tyype.exists("integer".eq) &&
      maximum.forall(_ match
        case mxi: Long => i <= mxi
        case mxd: Double => i <= mxd) &&
      minimum.forall(_ match
        case mxi: Long => i >= mxi
        case mxd: Double => i >= mxd)
  }

  override def visitFloat64(d: Double, index: Int): Boolean = {
    tyype.exists("number".eq) &&
      maximum.forall(_ match
        case mxi: Long => d <= mxi
        case mxd: Double => d <= mxd) &&
      minimum.forall(_ match
        case mxi: Long => d >= mxi
        case mxd: Double => d >= mxd)
  }

  override def visitString(s: CharSequence, index: Int): Boolean = {
    tyype.exists("string".eq) &&
      pattern.forall(_.r.matches(s)) && // TODO: memoize
      minLength.forall(s.length() > _) &&
      maxLength.forall(s.length() <= _) &&
      format.forall(_ match // TODO: use regexs
        case "date-time" => try { OffsetDateTime.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "date" => try { LocalDate.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "duration" => try { Duration.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "hostname" => try { URI(_); true } catch
          case ex: URISyntaxException => false
        case _ => true)
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Any, Boolean] = new ArrVisitor[Any, Boolean] {
    private var counter = 0
    private var subsch = true

    override def subVisitor: Visitor[_, _] = {
      if (items.isEmpty) BooleanSchemaVisitor.True
      else items.get match
        case bs: BooleanSchema => BooleanSchemaVisitor.of(bs)
        case os: ObjectSchema => ObjectSchemaVisitor(os)
    }

    override def visitValue(v: Any, index: Int): Unit = {
      counter += 1
      subsch = subsch && v.asInstanceOf[Boolean]
    }

    override def visitEnd(index: Int): Boolean = {
      subsch &&
        minItems.forall(counter >= _) &&
        maxItems.forall(counter <= _)
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Any, Boolean] = new ObjVisitor[Any, Boolean] {
    private val props: mutable.Buffer[String] = mutable.Buffer()

    override def visitKey(index: Int): Visitor[_, _] = StringVisitor

    override def visitKeyValue(v: Any): Unit = {
      props.addOne(v.asInstanceOf[String])
    }

    override def subVisitor: Visitor[_, _] = ???

    override def visitValue(v: Any, index: Int): Unit = ()

    override def visitEnd(index: Int): Boolean = {
      required.forall(props.contains(_)) &&
        maxProperties.forall(props.size <= _) &&
        minProperties.forall(props.size >= _)
    }
  }
}
