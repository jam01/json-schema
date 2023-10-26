package io.github.jam01.json_schema

import io.github.jam01.json_schema
import upickle.core.Visitor.{Delegate, MapArrContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, OffsetDateTime}
import scala.collection.{immutable, mutable}

class ObjectSchemaValidator(val schema: ObjectSchema,
                            val ctx: Context = Context.empty) extends JsonVisitor[_, Boolean] {
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
  val items: Option[Schema] = schema.getAsSchemaOpt("items")
  val properties: Option[collection.Map[String, Schema]] = None
  val _refVis: Option[JsonVisitor[_, Boolean]] = schema
    .getString("$ref") // TODO: resolve URI to current schema
    .map(s => ctx.reg.getOrElse(s, throw new IllegalArgumentException(s"unavailable schema $s")))
    .map(sch => SchemaValidator.of(sch, ctx))

  override def visitNull(index: Int): Boolean = {
    tyype.exists("null".eq) &&
      _refVis.forall(_.visitNull(index))
  }

  override def visitFalse(index: Int): Boolean = {
    tyype.exists("boolean".eq) &&
      _refVis.forall(_.visitFalse(index))
  }

  override def visitTrue(index: Int): Boolean = {
    tyype.exists("boolean".eq) &&
      _refVis.forall(_.visitTrue(index))
  }

  override def visitInt64(i: Long, index: Int): Boolean = {
    tyype.exists("integer".eq) &&
      maximum.forall(_ match
        case mxi: Long => i <= mxi
        case mxd: Double => i <= mxd) &&
      minimum.forall(_ match
        case mxi: Long => i >= mxi
        case mxd: Double => i >= mxd) &&
      _refVis.forall(_.visitInt64(i, index))
  }

  override def visitFloat64(d: Double, index: Int): Boolean = {
    tyype.exists("number".eq) &&
      maximum.forall(_ match
        case mxi: Long => d <= mxi
        case mxd: Double => d <= mxd) &&
      minimum.forall(_ match
        case mxi: Long => d >= mxi
        case mxd: Double => d >= mxd) &&
      _refVis.forall(_.visitFloat64(d, index))
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
        case _ => true) &&
      _refVis.forall(_.visitString(s, index))
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[_, Boolean] = {
    ctx.insloc.push("0")

    val delBuilder = immutable.Seq.newBuilder[ArrVisitor[_, Boolean]]
    if (items.nonEmpty) delBuilder.addOne(new ArrVisitor[Boolean, Boolean] {
      private var subsch = true
      override def subVisitor: Visitor[_, _] = SchemaValidator.of(items.get, ctx)
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })
    if (_refVis.nonEmpty) delBuilder.addOne(_refVis.get.visitArray(length, index))
    val delegates = delBuilder.result()

    val delegate: ArrVisitor[_, Boolean] =
      if (delegates.isEmpty) BooleanSchemaValidator.True.visitArray(length, index)
      else if (delegates.length == 1) delegates.head
      else new CompositeArrVisitorReducer(_.forall(identity), delegates: _*)

    new DynDelegateArrVisitor(delegate) {
      private var counter = 0

      override def visitValue(v: Any, index: Int): Unit = {
        ctx.insloc.pop
        counter += 1
        ctx.insloc.push(String.valueOf(counter))
        super.visitValue(v, index)
      }

      override def visitEnd(index: Int): Boolean = {
        ctx.insloc.pop
        tyype.exists("array".eq) && // TODO: up front?
          minItems.forall(counter >= _) &&
          maxItems.forall(counter <= _) &&
          super.visitEnd(index)
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[_], Boolean] = ???
//  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] = new ObjVisitor[Seq[Boolean], Boolean] {
//    private val props: mutable.Buffer[String] = mutable.Buffer()
//    private var subsch = true
//    private var key: String = "?"
//
//    override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
//      override def expectedMsg: String = "expected string,"
//
//      override def visitString(s: CharSequence, index: Int): Any = {
//        ctx.insloc.push(s.toString)
//      }
//    }
//
//    override def visitKeyValue(v: Any): Unit = {
//      key = v.asInstanceOf[String]
//      props.addOne(key)
//    }
//
//    override def subVisitor: Visitor[_, _] = {
//      if (properties.isEmpty) return BooleanSchemaVisitor.True
//      properties.get.get(key) match
//        case None => BooleanSchemaVisitor.True // TODO: check patternProps if not in properties, or unknown props
//        case Some(sch) => SchemaValidator.from(sch, ctx)
//    }
//
//    override def visitValue(v: Boolean, index: Int): Unit = ctx.insloc.pop
//
//    override def visitEnd(index: Int): Boolean = {
//      ctx.insloc.pop
//      required.forall(props.contains(_)) &&
//        maxProperties.forall(props.size <= _) &&
//        minProperties.forall(props.size >= _)
//    }
//  }
}