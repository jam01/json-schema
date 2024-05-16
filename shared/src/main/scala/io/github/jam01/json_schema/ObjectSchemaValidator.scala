package io.github.jam01.json_schema

import io.github.jam01.json_schema
import io.github.jam01.json_schema.ObjectSchemaValidator.{asBigDec, if_then_else}
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

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
                            val ctx: Context = Context.empty,
                            val dynParent: Option[ObjectSchemaValidator] = None) extends JsonVisitor[_, Boolean] {
  // all
  private val tyype: collection.Seq[String] = schema.getAsStringArray("type")
  private val notVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("not")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("not"), ctx, Some(this)))
  private val _refVis: Option[JsonVisitor[_, Boolean]] = schema
    .getRef
    .map(s => ctx.getSch(s) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"unavailable schema $s"))
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("$ref"), ctx, Some(this)))
  private val _dynRefVis: Option[JsonVisitor[_, Boolean]] = schema
    .getDynRef
    .map(s => ctx.getDynSch(s, this) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"unavailable schema $s"))
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("$dynamicRef"), ctx, Some(this)))
  private val allOfVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaArrayOpt("allOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, schloc.appendRefTokens("allOf", schidx._2.toString), ctx, Some(this))))
    .map(schViss => new CompositeVisitorReducer(_.forall(identity), schViss.toSeq: _*))
  private val oneOfVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaArrayOpt("oneOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, schloc.appendRefTokens("oneOf", schidx._2.toString), ctx, Some(this))))
    .map(schViss => new CompositeVisitorReducer(_.count(identity) == 1, schViss.toSeq: _*))
  private val anyOfVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaArrayOpt("anyOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, schloc.appendRefTokens("anyOf", schidx._2.toString), ctx, Some(this))))
    .map(schViss => new CompositeVisitorReducer(_.exists(identity), schViss.toSeq: _*))
  private val const = schema.value.get("const")
  private val enuum: Option[collection.Seq[Value]] = schema.getArrayOpt("enum")
  private val ifVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("if")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("if"), ctx, Some(this)))
  private val thenVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("then")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("then"), ctx, Some(this)))
  private val elseVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("else")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("else"), ctx, Some(this)))

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
  private val prefixItems: Option[collection.Seq[Schema]] = schema.getSchemaArrayOpt("prefixItems")
  private val uniqueItems: Option[Boolean] = schema.getBoolean("uniqueItems")
  private val maxContains: Option[Int] = schema.getInt("maxContains")
  private val minContains: Option[Int] = schema.getInt("minContains")
  private val contains: Option[ArrVisitor[_, Boolean]] = schema.getSchemaOpt("contains")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("contains"), ctx, Some(this)))
    .map(schValidator => new ArrVisitor[Boolean, Boolean] {
      private var matched = 0

      override def subVisitor: Visitor[_, _] = schValidator
      override def visitValue(v: Boolean, index: Int): Unit = if (v) matched = matched + 1
      override def visitEnd(index: Int): Boolean = {
        var res = matched > 0
        if (minContains.nonEmpty) {
          if (minContains.get == 0) res = true
          else res = res && (matched >= minContains.get)
        }
        if (maxContains.nonEmpty) {
          res = res && (matched <= maxContains.get)
        }
        res
      }
    })
  private val itemsVis: Option[ArrVisitor[_, Boolean]] = schema.getSchemaOpt("items")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("items"), ctx, Some(this)))
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
  private val propNymVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("propertyNames")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("propertyNames"), ctx, Some(this)))
  private val properties: Option[collection.Map[String, Schema]] = schema.getSchemaObjectOpt("properties")
  private val patternProperties: Option[collection.Map[Regex, Schema]] = schema.getSchemaObjectOpt("patternProperties")
    .map(obj => obj.map(entry => (new Regex(entry._1).unanchored, entry._2)))
  private val addlPropsVis: Option[ObjVisitor[_, Boolean]] = schema.getSchemaOpt("additionalProperties")
    .map(sch => SchemaValidator.of(sch, schloc.appendRefToken("additionalProperties"), ctx, Some(this)))
    .map(schValidator => new ObjVisitor[Boolean, Boolean] {
      private var subsch = true

      override def visitKey(index: Int): Visitor[_, _] = ???
      override def visitKeyValue(v: Any): Unit = ???
      override def subVisitor: Visitor[_, _] = schValidator
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })
  private val depSchsViss: Option[collection.Map[String, JsonVisitor[_, Boolean]]] = schema.getSchemaObjectOpt("dependentSchemas")
    .map(obj => obj.map(entry => (entry._1, SchemaValidator.of(entry._2, schloc.appendRefTokens("dependentSchemas", entry._1), ctx, Some(this)))))
  private val depReq: Option[collection.Map[String, Any]] = schema.getObjectOpt("dependentRequired")

  override def visitNull(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("null")) &&
      _refVis.forall(_.visitNull(index)) &&
      _dynRefVis.forall(_.visitNull(index)) &&
      notVis.forall(!_.visitNull(index)) &&
      allOfVis.forall(_.visitNull(index)) &&
      anyOfVis.forall(_.visitNull(index)) &&
      oneOfVis.forall(_.visitNull(index)) &&
      const.forall(c => c == Null) &&
      (enuum.isEmpty || enuum.get.contains(Null)) &&
      ifVis.forall(vis => if_then_else(vis.visitNull(index),
        thenVis.map(_.visitNull(index)), elseVis.map(_.visitNull(index))))
  }

  override def visitFalse(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      _refVis.forall(_.visitFalse(index)) &&
      _dynRefVis.forall(_.visitFalse(index)) &&
      notVis.forall(!_.visitFalse(index)) &&
      allOfVis.forall(_.visitFalse(index)) &&
      anyOfVis.forall(_.visitFalse(index)) &&
      oneOfVis.forall(_.visitFalse(index)) &&
      const.forall(c => Objects.equals(c, False)) &&
      (enuum.isEmpty || enuum.get.contains(False)) &&
      ifVis.forall(vis => if_then_else(vis.visitFalse(index),
        thenVis.map(_.visitFalse(index)), elseVis.map(_.visitFalse(index))))
  }

  override def visitTrue(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      _refVis.forall(_.visitTrue(index)) &&
      _dynRefVis.forall(_.visitTrue(index)) &&
      notVis.forall(!_.visitTrue(index)) &&
      allOfVis.forall(_.visitTrue(index)) &&
      const.forall(c => Objects.equals(c, True)) &&
      (enuum.isEmpty || enuum.get.contains(True)) &&
      ifVis.forall(vis => if_then_else(vis.visitTrue(index),
        thenVis.map(_.visitTrue(index)), elseVis.map(_.visitTrue(index))))
  }

  override def visitInt64(l: Long, index: Int): Boolean = {
    (tyype.isEmpty || tyype.exists(t => "integer".eq(t) || "number".eq(t))) &&
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
      _dynRefVis.forall(_.visitInt64(l, index)) &&
      notVis.forall(!_.visitInt64(l, index)) &&
      allOfVis.forall(_.visitInt64(l, index)) &&
      anyOfVis.forall(_.visitInt64(l, index)) &&
      oneOfVis.forall(_.visitInt64(l, index)) &&
      const.forall(c => Objects.equals(c.value, l)) &&
      (enuum.isEmpty || enuum.get.contains(Num(l))) &&
      ifVis.forall(vis => if_then_else(vis.visitInt64(l, index),
        thenVis.map(_.visitInt64(l, index)), elseVis.map(_.visitInt64(l, index))))
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
      _dynRefVis.forall(_.visitFloat64(d, index)) &&
      notVis.forall(!_.visitFloat64(d, index)) &&
      allOfVis.forall(_.visitFloat64(d, index)) &&
      anyOfVis.forall(_.visitFloat64(d, index)) &&
      oneOfVis.forall(_.visitFloat64(d, index)) &&
      const.forall(c => Objects.equals(c.value, d)) &&
      (enuum.isEmpty || enuum.get.contains(Num(d))) &&
      ifVis.forall(vis => if_then_else(vis.visitFloat64(d, index),
        thenVis.map(_.visitFloat64(d, index)), elseVis.map(_.visitFloat64(d, index))))
  }

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
      _dynRefVis.forall(_.visitString(s, index)) &&
      notVis.forall(!_.visitString(s, index)) &&
      allOfVis.forall(_.visitString(s, index)) &&
      anyOfVis.forall(_.visitString(s, index)) &&
      oneOfVis.forall(_.visitString(s, index)) &&
      const.forall(c => Objects.equals(c.value, s)) &&
      (enuum.isEmpty || enuum.get.exists(el => Objects.equals(el.value, s))) &&
      ifVis.forall(vis => if_then_else(vis.visitString(s, index),
        thenVis.map(_.visitString(s, index)), elseVis.map(_.visitString(s, index))))
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
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    notVis.foreach(vis => insVisitors.addOne(new MapArrContext(vis.visitArray(length, index), b => !b)))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    contains.foreach(vis => insVisitors.addOne(vis))
    ifVis.foreach(vis =>
      if (thenVis.isEmpty && elseVis.isEmpty) ()
      else {
        val viss: mutable.Buffer[ArrVisitor[_, Boolean]] = mutable.ArrayBuffer(vis.visitArray(length, index))
        if (thenVis.nonEmpty) viss.addOne(thenVis.get.visitArray(length, index))
        if (elseVis.nonEmpty) viss.addOne(elseVis.get.visitArray(length, index))
        insVisitors.addOne(new CompositeArrVisitorReducer(iffBools => {
          if_then_else(iffBools.head,
            thenVis.map(_ => iffBools(1)),
            elseVis.map(_ => if (thenVis.isEmpty) iffBools(1) else iffBools(2)))
        }, viss.toSeq: _*))})

    if (const.nonEmpty || enuum.nonEmpty || (uniqueItems.nonEmpty && uniqueItems.get))
      insVisitors.addOne(new MapArrContext(LiteralVisitor.visitArray(length, index), arr => {
        var res = true
        const.foreach(c => res = res && Objects.equals(const.get, arr))
        enuum.foreach(en => res = res && en.exists(el => Objects.equals(el, arr)))
        uniqueItems.foreach(b => res = {
          val set = collection.mutable.Set[Any]()
          res && arr.value.forall(set.add)
        })

        res
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

        override def subVisitor: Visitor[_, _] = SchemaValidator.of(arr(nextIdx), schloc.appendRefTokens("prefixItems", nextIdx.toString), ctx, Some(ObjectSchemaValidator.this))
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
    val propsVisited = mutable.ArrayBuffer.empty[String] // properties visited

    val insVisitors = mutable.ArrayBuffer.empty[ObjVisitor[_, Boolean]]
    _refVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    notVis.foreach(vis => insVisitors.addOne(new MapObjContext(vis.visitObject(length, index), b => !b)))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    ifVis.foreach(vis =>
      if (thenVis.isEmpty && elseVis.isEmpty) ()
      else {
        val viss: mutable.Buffer[ObjVisitor[_, Boolean]] = mutable.ArrayBuffer(vis.visitObject(length, index))
        if (thenVis.nonEmpty) viss.addOne(thenVis.get.visitObject(length, index))
        if (elseVis.nonEmpty) viss.addOne(elseVis.get.visitObject(length, index))
        insVisitors.addOne(new CompositeObjVisitorReducer(iffBools => {
          if_then_else(iffBools.head,
            thenVis.map(_ => iffBools(1)),
            elseVis.map(_ => if (thenVis.isEmpty) iffBools(1) else iffBools(2)))
        }, viss.toSeq: _*))
      })

    if (const.nonEmpty || enuum.nonEmpty)
      insVisitors.addOne(new MapObjContext(LiteralVisitor.visitObject(length, index), obj => {
        var res = true
        const.foreach(c => res = res && Objects.equals(const.get, obj))
        enuum.foreach(en => res = res && en.exists(el => Objects.equals(el, obj)))
        res
      }))

    val depSchsObjViss: Option[collection.Map[String, ObjVisitor[_, (String, Boolean)]]] =
      depSchsViss.map(viss => viss.map(entry => (entry._1, MapObjContext(entry._2.visitObject(length, index), b => (entry._1, b)))))
    depSchsObjViss.foreach(viss =>
      insVisitors.addOne(MapObjContext(new CompositeObjVisitor(viss.values.toSeq: _*), tups => {
        tups.filter((k, b) => propsVisited.contains(k))
          .forall((k, b) => b)
      }))
    )
    // TODO: add other instance applicators

    val insVisitor: ObjVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeObjVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)

    var childVisitor: ObjVisitor[_, _] = null // to be assigned based on child

    new ObjVisitor[Any, Boolean] {
      private var propNamesValid = true
      private var currentKey: String = "?"

      // returns subVisitor based on currentKey
      val propsVisitor: Option[ObjVisitor[_, Boolean]] = properties.map(m => new ObjVisitor[Boolean, Boolean] {
        private var subsch = true
        override def visitKey(index: Int): Visitor[_, _] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[_, _] = SchemaValidator.of(m(currentKey), schloc.appendRefTokens("properties", currentKey), ctx, Some(ObjectSchemaValidator.this))
        override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
        override def visitEnd(index: Int): Boolean = subsch
      })

      private var matchedPatternSchs: Seq[(String, Schema)] = Nil // to be assigned based on key visited
      // returns subVisitor based on assigned matchedPatternSchs
      val patternPropsVisitor: Option[ObjVisitor[_, Boolean]] = patternProperties.map(m => new ObjVisitor[Seq[Boolean], Boolean] {
        private var subsch = true

        override def visitKey(index: Int): Visitor[_, _] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[_, _] = new CompositeVisitor(matchedPatternSchs.map(pattSch =>
          SchemaValidator.of(pattSch._2, schloc.appendRefTokens("patternProperties", pattSch._1), ctx, Some(ObjectSchemaValidator.this))): _*)
        override def visitValue(v: Seq[Boolean], index: Int): Unit = subsch = v.forall(identity)
        override def visitEnd(index: Int): Boolean = subsch
      })

      override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          if (propNymVis.nonEmpty) propNamesValid = propNamesValid && propNymVis.get.visitString(s, index1)
          propsVisited.addOne(currentKey)
          matchedPatternSchs = patternProperties.map(m => m
            .withFilter(entry => entry._1.matches(currentKey))
            .map(entry => (entry._1.toString(), entry._2))
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
        if (matchedPatternSchs.nonEmpty) { isAddl = false; childVisitors.addOne(patternPropsVisitor.get) }
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
          insVisitor.visitEnd(index) &&
          propNamesValid &&
          depReq.forall(map => map.filter((k, reqs) => propsVisited.contains(k))
          .map((k, reqs) => reqs.asInstanceOf[Arr].value.forall(rreq => propsVisited.contains(rreq.str)))
          .forall(identity))
      }
    }
  }
}

object ObjectSchemaValidator {
  private def if_then_else(iff: Boolean, thenn: Option[Boolean], els: Option[Boolean]): Boolean = {
    if (iff && thenn.nonEmpty) thenn.get
    else if (!iff && els.nonEmpty) els.get
    else true
  }

  private def asBigDec(num: Long | Double) = num match
    case l: Long => BigDecimal.valueOf(l)
    case d: Double => BigDecimal.valueOf(d)
}
