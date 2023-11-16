package io.github.jam01.json_schema

import io.github.jam01.json_schema
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, OffsetDateTime}
import scala.collection.{immutable, mutable}

/**
 * An ObjectSchema validator
 *
 * @param schema the schema to apply
 * @param schloc the schema location path followed
 * @param ctx    the validation context
 */
class ObjectSchemaValidator(val schema: ObjectSchema,
                            val schloc: JsonPointer = JsonPointer(),
                            val ctx: Context = Context.empty) extends JsonVisitor[_, Boolean] {
  private val tyype: collection.Seq[String] = schema.getAsStringArray("type")
  private val pattern: Option[String] = schema.getString("pattern")
  private val format: Option[String] = schema.getString("format")
  private val maxLength: Option[Int] = schema.getInt("maxLength")
  private val minLength: Option[Int] = schema.getInt("minLength")
  private val maximum: Option[Long | Double] = schema.getNumber("maximum")
  private val minimum: Option[Long | Double] = schema.getNumber("minimum")

  // TODO: these can be fail-fast
  private val maxItems: Option[Int] = schema.getInt("maxItems")
  private val minItems: Option[Int] = schema.getInt("minItems")
  private val maxProperties: Option[Int] = schema.getInt("maxProperties")
  private val minProperties: Option[Int] = schema.getInt("minProperties")
  private val required: collection.Seq[String] = schema.getStringArray("required")
  private val properties: Option[collection.Map[String, Schema]] = schema.getAsSchemaObjectOpt("properties")
  private val _refVis: Option[JsonVisitor[_, Boolean]] = schema
    .getString("$ref") // TODO: resolve URI to current schema
    .map(s => ctx.reg.getOrElse(s, throw new IllegalArgumentException(s"unavailable schema $s")))
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("$ref"), ctx))
  private val itemsVis: Option[ArrVisitor[_, Boolean]] = schema.getAsSchemaOpt("items")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("items"), ctx))
    .map(schValidator => new ArrVisitor[Boolean, Boolean] {
      private var subsch = true
      override def subVisitor: Visitor[_, _] = schValidator
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })

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
    (tyype.isEmpty || tyype.exists(t => "integer".eq(t) || "number".eq(t) )) &&
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
        case "hostname" => try { new URI(_); true } catch
          case ex: URISyntaxException => false
        case _ => true) &&
      _refVis.forall(_.visitString(s, index))
  }

  /*
   * When visiting an arr/obj, there are keywords that apply to the given instance using this schema eg: maxProperties,
   * there are applicator kws that apply to the given instance eg: $ref, and there are applicator kws that only apply to
   * child elements eg: properties. Further, some of the kws for child elements may apply conditionally.
   *
   * The implementation chosen is to create a `insVisitor` which is a Arr/ObjVisitor potentially composing all of the
   * applicators for the given instance. Also a variable `childVisitor` which is an Arr/ObjVisitor potentially
   * composing `insVisitor` and all of the child element applicators that apply to the next child, usually determined
   * by its index or preceding key for Objects.
   *
   * The Arr/ObjVisitor returned applies the non-applicator kws for the current schema, but more interestingly it tracks
   * which visitors ought to be visited on each method. Specifically `insVisitor` is invoked on all methods, whereas
   * `childVisitor` is recreated based on the child to be visited and invoked only in child methods.
   *
   * When visitEnd() is invoked on the returned ObjVisitor, it composes the results from: all non-applicator kws,
   * `insVisitor` and all of child visitors invoked before through the `childVisitor` variable.
   */

  override def visitArray(length: Int, index: Int): ArrVisitor[_, Boolean] = {
    val builder = immutable.Seq.newBuilder[ArrVisitor[_, Boolean]]
    itemsVis.foreach(builder.addOne) // TODO: only if no prefixItems
    _refVis.foreach(vis => builder.addOne(vis.visitArray(length, index)))
    // TODO: add other applicators
    val insVisitors: Seq[ArrVisitor[_, Boolean]] = builder.result()

    val insVisitor: ArrVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeArrVisitorReducer(_.forall(identity), insVisitors: _*)

    new ArrVisitor[Any, Boolean] {
      private var counter = 0

      override def subVisitor: Visitor[_, _] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = {
        insVisitor.narrow.visitValue(v, index)
        counter += 1
      }

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("array")) && // TODO: up front to fail-fast
          minItems.forall(counter >= _) &&
          maxItems.forall(counter <= _) &&
          insVisitor.visitEnd(index)
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] = {
    val builder = immutable.Seq.newBuilder[ObjVisitor[_, Boolean]]
    _refVis.foreach(vis => builder.addOne(vis.visitObject(length, index)))
    // TODO: add other applicators
    val insVisitors = builder.result()

    val insVisitor: ObjVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeObjVisitorReducer(_.forall(identity), insVisitors: _*)

    var childVisitor: ObjVisitor[_, _] = null

    new ObjVisitor[Any, Boolean] {
      val props: mutable.Buffer[String] = mutable.Buffer()
      var key: String = "?"
      private var counter = 0

      val propsVisitor: Option[ObjVisitor[_, Boolean]] = properties.map(m => new ObjVisitor[Boolean, Boolean] {
        private var subsch = true
        override def visitKey(index: Int): Visitor[_, _] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[_, _] = SchemaValidator.of(m(key), schloc.appendRefTokens("properties", key), ctx)
        override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
        override def visitEnd(index: Int): Boolean = subsch
      })

      override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          key = s.toString
          // TODO: propertyNames
          props.addOne(key)
          insVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[_, _] = {
        val subbuilder = immutable.Seq.newBuilder[ObjVisitor[_, Boolean]]
        subbuilder.addOne(insVisitor)
        properties.flatMap(m => m.get(key)).foreach(sch => subbuilder.addOne(propsVisitor.get))
        // TODO: add other sub-applicators
        val childVisitors = subbuilder.result()

        childVisitor =
          if (childVisitors.length == 1) childVisitors.head
          else new CompositeObjVisitor(childVisitors: _*)
        childVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = {
        childVisitor.narrow.visitValue(v, index)
        counter += 1
      }

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("object")) && // TODO: up front to fail-fast
          required.forall(props.contains(_)) &&
          maxProperties.forall(props.size <= _) &&
          minProperties.forall(props.size >= _) &&
          propsVisitor.forall(_.visitEnd(index)) &&
          insVisitor.visitEnd(index)
      }
    }
  }
}
