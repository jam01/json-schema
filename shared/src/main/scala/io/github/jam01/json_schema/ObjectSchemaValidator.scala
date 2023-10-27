package io.github.jam01.json_schema

import io.github.jam01.json_schema
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

  // TODO: these can be fail-fast
  val maxItems: Option[Int] = schema.getInt("maxItems")
  val minItems: Option[Int] = schema.getInt("minItems")
  val maxProperties: Option[Int] = schema.getInt("maxProperties")
  val minProperties: Option[Int] = schema.getInt("minProperties")

  val items: Option[Schema] = schema.getAsSchemaOpt("items")
  val properties: Option[collection.Map[String, Schema]] = None
  val required: collection.Seq[String] = schema.getStringArray("required")
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
    val builder = immutable.Seq.newBuilder[ArrVisitor[_, Boolean]]
    if (items.nonEmpty) builder.addOne(new ArrVisitor[Boolean, Boolean] {
      private var subsch = true
      override def subVisitor: Visitor[_, _] = SchemaValidator.of(items.get, ctx)
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })
    if (_refVis.nonEmpty) builder.addOne(_refVis.get.visitArray(length, index))
    // TODO: add other applicators
    val delegates: Seq[ArrVisitor[_, Boolean]] = builder.result()

    val delegate: ArrVisitor[_, Boolean] =
      if (delegates.isEmpty) BooleanSchemaValidator.True.visitArray(length, index) // no annotation for this
      else if (delegates.length == 1) delegates.head
      else new CompositeArrVisitorReducer(_.forall(identity), delegates: _*)

    new ArrVisitor[Any, Boolean] {
      private var counter = 0

      override def subVisitor: Visitor[_, _] = delegate.subVisitor

      override def visitValue(v: Any, index: Int): Unit = {
        delegate.narrow.visitValue(v, index)
        counter += 1
      }

      override def visitEnd(index: Int): Boolean = {
        tyype.exists("array".eq) && // TODO: up front to fail-fast
          minItems.forall(counter >= _) &&
          maxItems.forall(counter <= _) &&
          delegate.visitEnd(index)
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[_], Boolean] = {
    val builder = immutable.Seq.newBuilder[ObjVisitor[_, Boolean]]
    if (_refVis.nonEmpty) builder.addOne(_refVis.get.visitObject(length, index))
    // TODO: add other applicators
    val delegates = builder.result()

    val delegate: ObjVisitor[_, Boolean] =
      if (delegates.isEmpty) BooleanSchemaValidator.True.visitObject(length, index)
      else if (delegates.length == 1) delegates.head
      else new CompositeObjVisitorReducer(_.forall(identity), delegates: _*)

    new ObjVisitor[Any, Boolean] {
      val props: mutable.Buffer[String] = mutable.Buffer()
      var key: String = "?"
      private var counter = 0

      override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          key = s.toString
          // TODO: propertyNames
          props.addOne(key)
          delegate.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = delegate.visitKeyValue(v)

      override def subVisitor: Visitor[_, _] = {
        val subbuilder = new mutable.ArrayBuffer[Visitor[_, _]]()
        val subprop = properties
          .flatMap(m => m.get(key))
          .map(sch => SchemaValidator.of(sch, ctx))
        if (subprop.nonEmpty) subbuilder.addOne(subprop.get)
        // TODO: add patternProperties, additionalProperties

        if (subbuilder.isEmpty) delegate.subVisitor
        else new CompositeVisitorReducer(_.forall(x => identity(x.asInstanceOf[Boolean])), subbuilder.addOne(delegate.subVisitor).toSeq : _*)
      }

      override def visitValue(v: Any, index: Int): Unit = {
        delegate.narrow.visitValue(v, index)
        counter += 1
      }

      override def visitEnd(index: Int): Boolean = {
        tyype.exists("object".eq) && // TODO: up front to fail-fast
          required.forall(props.contains(_)) &&
          maxProperties.forall(props.size <= _) &&
          minProperties.forall(props.size >= _) &&
          delegate.visitEnd(index)
      }
    }
  }
}
