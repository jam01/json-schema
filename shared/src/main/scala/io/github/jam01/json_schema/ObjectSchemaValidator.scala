package io.github.jam01.json_schema

import io.github.jam01.json_schema
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, OffsetDateTime}
import java.util.{Objects, UUID}
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

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
  // all
  private val tyype: collection.Seq[String] = schema.getAsStringArray("type")
  private val notVis: Option[JsonVisitor[_, Boolean]] = schema.getAsSchemaOpt("not")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("not"), ctx))
  private val _refVis: Option[JsonVisitor[_, Boolean]] = schema
    .getRef
    .map(s => ctx.getSch(s) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"unavailable schema $s"))
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("$ref"), ctx))
  private val allOfVis: Option[JsonVisitor[_, Boolean]] = schema.getAsSchemaArrayOpt("allOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, schloc.appendRefTokens("allOf", schidx._2.toString), ctx)))
    .map(schViss => new CompositeVisitorReducer(_.forall(identity), schViss.toSeq: _*))
  private val oneOfVis: Option[JsonVisitor[_, Boolean]] = schema.getAsSchemaArrayOpt("oneOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, schloc.appendRefTokens("anyOf", schidx._2.toString), ctx)))
    .map(schViss => new CompositeVisitorReducer(_.count(identity) == 1, schViss.toSeq: _*))
  private val anyOfVis: Option[JsonVisitor[_, Boolean]] = schema.getAsSchemaArrayOpt("anyOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, schloc.appendRefTokens("oneOf", schidx._2.toString), ctx)))
    .map(schViss => new CompositeVisitorReducer(_.exists(identity), schViss.toSeq: _*))
  private val const: Option[Any] = schema.get("const")
  private val enuum: Option[collection.Seq[Any]] = schema.getArrayOpt("enum")

  // strings
  private val pattern: Option[Regex] = schema.getString("pattern").map(s => new Regex(s).unanchored)
  private val format: Option[String] = schema.getString("format")
  private val maxLength: Option[Int] = schema.getInt("maxLength")
  private val minLength: Option[Int] = schema.getInt("minLength")

  // numbers
  private val maximum: Option[Long | Double] = schema.getNumber("maximum")
  private val minimum: Option[Long | Double] = schema.getNumber("minimum")
  private val exclusiveMax: Option[Long | Double] = schema.getNumber("exclusiveMaximum")
  private val exclusiveMin: Option[Long | Double] = schema.getNumber("exclusiveMinimum")
  private val multipleOf: Option[Long | Double] = schema.getNumber("multipleOf")

  // TODO: max/min can be fail-fast
  // arrays
  private val maxItems: Option[Int] = schema.getInt("maxItems")
  private val minItems: Option[Int] = schema.getInt("minItems")
  private val prefixItems: Option[collection.Seq[Schema]] = schema.getAsSchemaArrayOpt("prefixItems")
  private val itemsVis: Option[ArrVisitor[_, Boolean]] = schema.getAsSchemaOpt("items")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("items"), ctx))
    .map(schValidator => new ArrVisitor[Boolean, Boolean] {
      private var subsch = true

      override def subVisitor: Visitor[_, _] = schValidator
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })

  //objects
  private val maxProperties: Option[Int] = schema.getInt("maxProperties")
  private val minProperties: Option[Int] = schema.getInt("minProperties")
  private val required: collection.Seq[String] = schema.getStringArray("required")
  private val properties: Option[collection.Map[String, Schema]] = schema.getAsSchemaObjectOpt("properties")
  private val patternProperties: Option[collection.Map[Regex, Schema]] = schema.getAsSchemaObjectOpt("patternProperties")
    .map(obj => obj.map(entry => (new Regex(entry._1).unanchored, entry._2)))
  private val addlPropsVis: Option[ObjVisitor[_, Boolean]] = schema.getAsSchemaOpt("additionalProperties")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("additionalProperties"), ctx))
    .map(schValidator => new ObjVisitor[Boolean, Boolean] {
      private var subsch = true

      override def visitKey(index: Int): Visitor[_, _] = ???
      override def visitKeyValue(v: Any): Unit = ???
      override def subVisitor: Visitor[_, _] = schValidator
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })

  override def visitNull(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("null")) &&
      _refVis.forall(_.visitNull(index)) &&
      notVis.forall(!_.visitNull(index)) &&
      allOfVis.forall(_.visitNull(index)) &&
      anyOfVis.forall(_.visitNull(index)) &&
      oneOfVis.forall(_.visitNull(index)) &&
      const.forall(c => c == null) &&
      (enuum.isEmpty || enuum.get.contains(null))
  }

  override def visitFalse(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      _refVis.forall(_.visitFalse(index)) &&
      notVis.forall(!_.visitFalse(index)) &&
      allOfVis.forall(_.visitFalse(index)) &&
      anyOfVis.forall(_.visitFalse(index)) &&
      oneOfVis.forall(_.visitFalse(index)) &&
      const.forall(c => Objects.equals(c, false)) &&
      (enuum.isEmpty || enuum.get.contains(false))
  }

  override def visitTrue(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      _refVis.forall(_.visitTrue(index)) &&
      notVis.forall(!_.visitTrue(index)) &&
      allOfVis.forall(_.visitTrue(index)) &&
      const.forall(c => Objects.equals(c, true)) &&
      (enuum.isEmpty || enuum.get.contains(true))
  }

  override def visitInt64(l: Long, index: Int): Boolean = {
    (tyype.isEmpty || tyype.exists(t => "integer".eq(t) || "number".eq(t) )) &&
      maximum.forall(_ match
        case mxl: Long => l <= mxl
        case mxd: Double => l <= mxd) &&
      minimum.forall(_ match
        case mnl: Long => l >= mnl
        case mnd: Double => l >= mnd) &&
      exclusiveMax.forall(_ match
        case mxl: Long => l < mxl
        case mxd: Double => l < mxd) &&
      exclusiveMin.forall(_ match
        case mnl: Long => l > mnl
        case mnd: Double => l > mnd) &&
      multipleOf.forall(_ match
        case ml: Long => l % ml == 0
        case md: Double => if (md.isWhole) l % md.longValue == 0 else false) &&
      _refVis.forall(_.visitInt64(l, index)) &&
      notVis.forall(!_.visitInt64(l, index)) &&
      allOfVis.forall(_.visitInt64(l, index)) &&
      anyOfVis.forall(_.visitInt64(l, index)) &&
      oneOfVis.forall(_.visitInt64(l, index)) &&
      const.forall(c => Objects.equals(c, l)) &&
      (enuum.isEmpty || enuum.get.contains(l))
  }

  override def visitFloat64(d: Double, index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("number") || (tyype.contains("integer") && d.isWhole)) &&
      maximum.forall(_ match
        case mxl: Long => d <= mxl
        case mxd: Double => d <= mxd) &&
      minimum.forall(_ match
        case mnl: Long => d >= mnl
        case mnd: Double => d >= mnd) &&
      exclusiveMax.forall(_ match
        case mxl: Long => d < mxl
        case mxd: Double => d < mxd) &&
      exclusiveMin.forall(_ match
        case mnl: Long => d > mnl
        case mnd: Double => d > mnd) &&
      multipleOf.forall(num => try {
        BigDecimal.valueOf(d)
          .remainder(asBigDec(num))
          .compareTo(java.math.BigDecimal.ZERO) == 0
      } catch case ex: ArithmeticException => false) && // TODO: not sure if we have to do this
      _refVis.forall(_.visitFloat64(d, index)) &&
      notVis.forall(!_.visitFloat64(d, index)) &&
      allOfVis.forall(_.visitFloat64(d, index)) &&
      anyOfVis.forall(_.visitFloat64(d, index)) &&
      oneOfVis.forall(_.visitFloat64(d, index)) &&
      const.forall(c => Objects.equals(c, d)) &&
      (enuum.isEmpty || enuum.get.contains(d))
  }

  private def asBigDec(num: Long | Double) = num match
    case l: Long => BigDecimal.valueOf(l)
    case d: Double => BigDecimal.valueOf(d)

  override def visitString(s: CharSequence, index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("string")) &&
      pattern.forall(_.matches(s)) && // TODO: memoize
      minLength.forall(s.toString.codePointCount(0, s.length()) >= _) &&
      maxLength.forall(s.toString.codePointCount(0, s.length()) <= _) &&
      format.forall(_ match // TODO: use regexs
        case "date-time" => try { OffsetDateTime.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "date" => try { LocalDate.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "duration" => try { Duration.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "uuid" => try { UUID.fromString(_); true } catch
          case ex: IllegalArgumentException => false
        case _ => true) && // TODO: throw unsupported exc
      _refVis.forall(_.visitString(s, index)) &&
      notVis.forall(!_.visitString(s, index)) &&
      allOfVis.forall(_.visitString(s, index)) &&
      anyOfVis.forall(_.visitString(s, index)) &&
      oneOfVis.forall(_.visitString(s, index)) &&
      const.forall(c => Objects.equals(c, s)) &&
      (enuum.isEmpty || enuum.get.exists(el => Objects.equals(el, s)))
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
    val insVisitors = mutable.ArrayBuffer.empty[ArrVisitor[_, Boolean]]
    _refVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    notVis.foreach(vis => insVisitors.addOne(new MapArrContext(vis.visitArray(length, index), b => !b)))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))

    if (const.nonEmpty || enuum.nonEmpty)
      insVisitors.addOne(new MapArrContext(LiteralVisitor.visitArray(length, index), arr => {
        if (const.isEmpty) enuum.get.exists(el => Objects.equals(el, arr))
        else if (enuum.isEmpty) Objects.equals(const.get, arr)
        else enuum.get.exists(el => Objects.equals(el, arr) && Objects.equals(const.get, arr))
      }))
    // TODO: add other instance applicators

    val insVisitor: ArrVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeArrVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)

    var childVisitor: ArrVisitor[_, _] = null // to be assigned based on child

    new ArrVisitor[Any, Boolean] {
      private var nextIdx = 0

      // returns subVisitor based on child index
      val prefixItemsVisitor: Option[ArrVisitor[_, Boolean]] = prefixItems.map(arr => new ArrVisitor[Boolean, Boolean] {
        private var subsch = true

        override def subVisitor: Visitor[_, _] = SchemaValidator.of(arr(nextIdx), schloc.appendRefTokens("prefixItems", nextIdx.toString), ctx)
        override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
        override def visitEnd(index: Int): Boolean = subsch
      })

      override def subVisitor: Visitor[_, _] = {
        val childVisitors = mutable.ArrayBuffer.empty[ArrVisitor[_, Boolean]]
        childVisitors.addOne(insVisitor)

        if (prefixItems.nonEmpty && prefixItems.get.length >= nextIdx + 1) { childVisitors.addOne(prefixItemsVisitor.get) }
        else if (itemsVis.nonEmpty) childVisitors.addOne(itemsVis.get)
        // TODO: add other child-applicators

        childVisitor =
          if (childVisitors.length == 1) childVisitors.head
          else new CompositeArrVisitor(childVisitors.toSeq: _*)
        childVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = {
        childVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("array")) && // TODO: up front to fail-fast
          minItems.forall(nextIdx >= _) &&
          maxItems.forall(nextIdx <= _) &&
          prefixItemsVisitor.forall(_.visitEnd(index)) &&
          itemsVis.forall(_.visitEnd(index)) &&
          insVisitor.visitEnd(index)
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] = {
    val insVisitors = mutable.ArrayBuffer.empty[ObjVisitor[_, Boolean]]
    _refVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    notVis.foreach(vis => insVisitors.addOne(new MapObjContext(vis.visitObject(length, index), b => !b)))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))

    if (const.nonEmpty || enuum.nonEmpty)
      insVisitors.addOne(new MapObjContext(LiteralVisitor.visitObject(length, index), obj => {
        if (const.isEmpty) enuum.get.exists(el => Objects.equals(el, obj))
        else if (enuum.isEmpty) Objects.equals(const.get, obj)
        else enuum.get.exists(el => Objects.equals(el, obj) && Objects.equals(const.get, obj))
      }))
    // TODO: add other instance applicators

    val insVisitor: ObjVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeObjVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)

    var childVisitor: ObjVisitor[_, _] = null // to be assigned based on child

    new ObjVisitor[Any, Boolean] {
      private val propsVisited = mutable.ArrayBuffer.empty[String] // properties visited
      private var currentKey: String = "?"

      // returns subVisitor based on currentKey
      val propsVisitor: Option[ObjVisitor[_, Boolean]] = properties.map(m => new ObjVisitor[Boolean, Boolean] {
        private var subsch = true
        override def visitKey(index: Int): Visitor[_, _] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[_, _] = SchemaValidator.of(m(currentKey), schloc.appendRefTokens("properties", currentKey), ctx)
        override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
        override def visitEnd(index: Int): Boolean = subsch
      })

      private var matchedPatternSchs: Seq[Schema] = Nil // to be assigned based on key visited
      // returns subVisitor based on assigned matchedPatternSchs
      val patternPropsVisitor: Option[ObjVisitor[_, Boolean]] = patternProperties.map(m => new ObjVisitor[Seq[Boolean], Boolean] {
        private var subsch = true

        override def visitKey(index: Int): Visitor[_, _] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[_, _] = new CompositeVisitor(matchedPatternSchs.map(sch => SchemaValidator.of(sch, schloc.appendRefTokens("patternProperties", currentKey), ctx)): _*)
        override def visitValue(v: Seq[Boolean], index: Int): Unit = subsch = v.forall(identity)
        override def visitEnd(index: Int): Boolean = subsch
      })

      override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          // TODO: propertyNames
          propsVisited.addOne(currentKey)
          matchedPatternSchs = patternProperties.map(m => m
            .withFilter(entry => entry._1.matches(currentKey))
            .map(entry => entry._2)
            .toSeq).getOrElse(Nil)

          insVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[_, _] = {
        val childVisitors = mutable.ArrayBuffer.empty[ObjVisitor[_, Boolean]]
        childVisitors.addOne(insVisitor)

        var isAddl = true // if not in properties or matched patterns
        if (properties.nonEmpty && properties.get.contains(currentKey)) { isAddl = false; childVisitors.addOne(propsVisitor.get) }
        if (matchedPatternSchs.nonEmpty && matchedPatternSchs.nonEmpty) { isAddl = false; childVisitors.addOne(patternPropsVisitor.get) }
        if (isAddl) addlPropsVis.foreach(vis => childVisitors.addOne(vis))

        // TODO: add other child-applicators

        childVisitor =
          if (childVisitors.length == 1) childVisitors.head
          else new CompositeObjVisitor(childVisitors.toSeq: _*)
        childVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = childVisitor.narrow.visitValue(v, index)

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("object")) && // TODO: up front to fail-fast
          required.forall(propsVisited.contains(_)) &&
          maxProperties.forall(propsVisited.size <= _) &&
          minProperties.forall(propsVisited.size >= _) &&
          propsVisitor.forall(_.visitEnd(index)) &&
          patternPropsVisitor.forall(_.visitEnd(index)) &&
          addlPropsVis.forall(_.visitEnd(index)) &&
          insVisitor.visitEnd(index)
      }
    }
  }
}
