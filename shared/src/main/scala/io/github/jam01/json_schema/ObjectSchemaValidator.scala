package io.github.jam01.json_schema

import io.github.jam01.json_schema
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, OffsetDateTime}
import scala.collection.mutable.ArrayBuffer
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
  val properties: Option[collection.Map[String, Schema]] = schema.getAsSchemaObjectOpt("properties")
  val required: collection.Seq[String] = schema.getStringArray("required")
  val _refVis: Option[JsonVisitor[_, Boolean]] = schema
    .getString("$ref") // TODO: resolve URI to current schema
    .map(s => ctx.reg.getOrElse(s, throw new IllegalArgumentException(s"unavailable schema $s")))
    .map(sch => SchemaValidator.of(sch, ctx))

  override def visitNull(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("null")) &&
      _refVis.forall(_.visitNull(index))
  }

  override def visitFalse(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      _refVis.forall(_.visitFalse(index))
  }

  override def visitTrue(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      _refVis.forall(_.visitTrue(index))
  }

  override def visitInt64(i: Long, index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("integer")) &&
      maximum.forall(_ match
        case mxi: Long => i <= mxi
        case mxd: Double => i <= mxd) &&
      minimum.forall(_ match
        case mxi: Long => i >= mxi
        case mxd: Double => i >= mxd) &&
      _refVis.forall(_.visitInt64(i, index))
  }

  override def visitFloat64(d: Double, index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("number")) &&
      maximum.forall(_ match
        case mxi: Long => d <= mxi
        case mxd: Double => d <= mxd) &&
      minimum.forall(_ match
        case mxi: Long => d >= mxi
        case mxd: Double => d >= mxd) &&
      _refVis.forall(_.visitFloat64(d, index))
  }

  override def visitString(s: CharSequence, index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("string")) &&
      pattern.forall(_.r.matches(s)) && // TODO: memoize
      minLength.forall(s.length() >= _) &&
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
    val itemsVisitor: Option[ArrVisitor[_, Boolean]] = items.map(sch => new ArrVisitor[Boolean, Boolean] {
      private var subsch = true
      override def subVisitor: Visitor[_, _] = SchemaValidator.of(sch, ctx)
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })

    val builder = immutable.Seq.newBuilder[ArrVisitor[_, Boolean]]
    itemsVisitor.foreach(builder.addOne) // TODO: only if no prefixItems
    _refVis.foreach(vis => builder.addOne(vis.visitArray(length, index)))
    // TODO: add other applicators
    val insVisitors: Seq[ArrVisitor[_, Boolean]] = builder.result()

    val compInsVisitor: ArrVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeArrVisitorReducer(_.forall(identity), insVisitors: _*)

    new ArrVisitor[Any, Boolean] {
      private var counter = 0

      override def subVisitor: Visitor[_, _] = compInsVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = {
        compInsVisitor.narrow.visitValue(v, index)
        counter += 1
      }

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("array")) && // TODO: up front to fail-fast
          minItems.forall(counter >= _) &&
          maxItems.forall(counter <= _) &&
          compInsVisitor.visitEnd(index)
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] = {
    val builder = immutable.Seq.newBuilder[ObjVisitor[_, Boolean]]
    _refVis.foreach(vis => builder.addOne(vis.visitObject(length, index)))
    // TODO: add other applicators
    val insVisitors = builder.result()

    val compInsVisitor: ObjVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeObjVisitorReducer(_.forall(identity), insVisitors: _*)

    var childInsVisitor: ObjVisitor[_, _] = null

    new ObjVisitor[Any, Boolean] {
      val props: mutable.Buffer[String] = mutable.Buffer()
      var key: String = "?"
      private var counter = 0

      val propsVisitor: Option[ObjVisitor[_, Boolean]] = properties.map(m => new ObjVisitor[Boolean, Boolean] {
        private var subsch = true
        override def visitKey(index: Int): Visitor[_, _] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[_, _] = SchemaValidator.of(m(key), ctx)
        override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
        override def visitEnd(index: Int): Boolean = subsch
      })

      override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          key = s.toString
          // TODO: propertyNames
          props.addOne(key)
          compInsVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = compInsVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[_, _] = {
        val subbuilder = immutable.Seq.newBuilder[ObjVisitor[_, Boolean]]
        subbuilder.addOne(compInsVisitor)
        properties.flatMap(m => m.get(key)).foreach(sch => subbuilder.addOne(propsVisitor.get))
        // TODO: add other sub-applicators
        val childInsVisitors = subbuilder.result()

        childInsVisitor =
          if (insVisitors.length == 1) childInsVisitors.head
          else new CompositeObjVisitor(childInsVisitors: _*)
        childInsVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = {
        childInsVisitor.narrow.visitValue(v, index)

        counter += 1
      }

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("object")) && // TODO: up front to fail-fast
          required.forall(props.contains(_)) &&
          maxProperties.forall(props.size <= _) &&
          minProperties.forall(props.size >= _) &&
          compInsVisitor.visitEnd(index) &&
          propsVisitor.forall(_.visitEnd(index))
      }
    }
  }
}
