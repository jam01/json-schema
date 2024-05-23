package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

class Applicator(schema: ObjectSchema,
                 ctx: Context = Context.Empty,
                 path: JsonPointer = JsonPointer(),
                 dynParent: Option[BaseValidator] = None) extends BaseValidator(schema, ctx, path, dynParent) {

  private val prefixItems: Option[collection.Seq[Schema]] = schema.getSchemaArrayOpt("prefixItems")
  private val itemsVis: Option[ArrVisitor[?, OutputUnit]] = schema.getSchemaOpt("items")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("items"), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer

      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = units.addOne(u)
      override def visitEnd(index: Int): OutputUnit = and(path.appended("items"), units)
    })

  private val maxContains: Option[Int] = schema.getInt("maxContains")
  private val minContains: Option[Int] = schema.getInt("minContains")
  private val contains: Option[ArrVisitor[?, OutputUnit]] = schema.getSchemaOpt("contains")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("contains"), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
      private var matched = 0

      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = if (u.valid) matched = matched + 1
      override def visitEnd(index: Int): OutputUnit = {
        var res = matched > 0
        if (minContains.nonEmpty) {
          if (minContains.get == 0) res = true
          else res = res && (matched >= minContains.get)
        }
        if (maxContains.nonEmpty) {
          res = res && (matched <= maxContains.get)
        }
        validate("contains", "Array does not contain given elements", res) // TODO: include if failed bc min/max
      }
    })

  private val addlPropsVis: Option[ObjVisitor[?, OutputUnit]] = schema.getSchemaOpt("additionalProperties")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("additionalProperties"), Some(this)))
    .map(schValidator => new ObjVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
      private var subsch = true

      override def visitKey(index: Int): Visitor[?, ?] = ???
      override def visitKeyValue(v: Any): Unit = ???
      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = units.addOne(u)
      override def visitEnd(index: Int): OutputUnit = and(path.appended("additionalProperties"), units)
    })
  private val properties: Option[collection.Map[String, Schema]] = schema.getSchemaObjectOpt("properties")
  private val patternProperties: Option[collection.Map[Regex, Schema]] = schema.getSchemaObjectOpt("patternProperties")
    .map(obj => obj.map(entry => (new Regex(entry._1).unanchored, entry._2)))
  private val depSchsViss: Option[collection.Map[String, Visitor[?, OutputUnit]]] = schema.getSchemaObjectOpt("dependentSchemas")
    .map(obj => obj.map(entry => (entry._1, SchemaValidator.of(entry._2, ctx, path.appended("dependentSchemas", entry._1), Some(this)))))
  private val propNymVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt("propertyNames")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("propertyNames"), Some(this)))
  private val ifVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt("if")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("if"), Some(this)))
  private val thenVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt("then")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("then"), Some(this)))
  private val elseVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt("else")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("else"), Some(this)))
  private val allOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt("allOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended("allOf", schidx._2.toString), Some(this))))
    .map(schViss => new CompositeVisitorReducer(units => and(path.appended("allOf"), units), schViss.toSeq*))
  private val oneOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt("oneOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended("oneOf", schidx._2.toString), Some(this))))
    .map(schViss => new CompositeVisitorReducer(units => one(path.appended("oneOf"), units), schViss.toSeq*))
  private val anyOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt("anyOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended("anyOf", schidx._2.toString), Some(this))))
    .map(schViss => new CompositeVisitorReducer(units => any(path.appended("anyOf"), units), schViss.toSeq*))
  private val notVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt("not")
    .map(sch => SchemaValidator.of(sch, ctx, path.appended("not"), Some(this)))

  override def visitNull(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, v.visitNull(index).not()))
    allOfVis.foreach(v => addUnit(units, v.visitNull(index)))
    anyOfVis.foreach(v => addUnit(units, v.visitNull(index)))
    oneOfVis.foreach(v => addUnit(units, v.visitNull(index)))
    ifVis.foreach(v => {
      val u = v.visitNull(index)
      if (u.valid) thenVis.map(tv => tv.visitNull(index)).foreach(u => addUnit(units, u))
      else elseVis.map(ev => ev.visitNull(index)).foreach(u => addUnit(units, u))
    })
    units
  }

  override def visitFalse(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, v.visitFalse(index).not()))
    allOfVis.foreach(v => addUnit(units, v.visitFalse(index)))
    anyOfVis.foreach(v => addUnit(units, v.visitFalse(index)))
    oneOfVis.foreach(v => addUnit(units, v.visitFalse(index)))
    ifVis.foreach(v => {
      val u = v.visitFalse(index)
      if (u.valid) thenVis.map(tv => tv.visitFalse(index)).foreach(u => addUnit(units, u))
      else elseVis.map(ev => ev.visitFalse(index)).foreach(u => addUnit(units, u))
    })
    units
  }

  override def visitTrue(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, v.visitTrue(index).not()))
    allOfVis.foreach(v => addUnit(units, v.visitTrue(index)))
    anyOfVis.foreach(v => addUnit(units, v.visitTrue(index)))
    oneOfVis.foreach(v => addUnit(units, v.visitTrue(index)))
    ifVis.foreach(v => {
      val u = v.visitTrue(index)
      if (u.valid) thenVis.map(tv => tv.visitTrue(index)).foreach(u => addUnit(units, u))
      else elseVis.map(ev => ev.visitTrue(index)).foreach(u => addUnit(units, u))
    })
    units
  }

  override def visitInt64(l: Long, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, v.visitInt64(l, index).not()))
    allOfVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    anyOfVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    oneOfVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    ifVis.map(iv => iv.visitInt64(l, index)).foreach(u => {
      if (u.valid) thenVis.map(tv => tv.visitInt64(l, index)).foreach(u => addUnit(units, u))
      else elseVis.map(ev => ev.visitInt64(l, index)).foreach(u => addUnit(units, u))
    })
    units
  }

  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, v.visitFloat64(d, index).not()))
    allOfVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    anyOfVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    oneOfVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    ifVis.map(iv => iv.visitFloat64(d, index)).foreach(u => {
      if (u.valid) thenVis.map(tv => tv.visitFloat64(d, index)).foreach(u => addUnit(units, u))
      else elseVis.map(ev => ev.visitFloat64(d, index)).foreach(u => addUnit(units, u))
    })
    units
  }

  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, v.visitString(s, index).not()))
    allOfVis.foreach(v => addUnit(units, v.visitString(s, index)))
    anyOfVis.foreach(v => addUnit(units, v.visitString(s, index)))
    oneOfVis.foreach(v => addUnit(units, v.visitString(s, index)))
    ifVis.map(v => v.visitString(s, index)).foreach(u => {
      if (u.valid) thenVis.foreach(v => addUnit(units, v.visitString(s, index)))
      else elseVis.foreach(v => addUnit(units, v.visitString(s, index)))
    })
    units
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

  override def visitArray(length: Int, index: Int): ArrVisitor[?, collection.Seq[OutputUnit]] = {
    val insVisitors: mutable.ArrayBuffer[ArrVisitor[?, OutputUnit]] = new mutable.ArrayBuffer(7)
    notVis.foreach(vis => insVisitors.addOne(new MapArrContext(vis.visitArray(length, index), unit => unit.not())))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    contains.foreach(vis => insVisitors.addOne(vis))
    ifVis.foreach(vis =>
      if (thenVis.isEmpty && elseVis.isEmpty) ()
      else {
        val viss: mutable.ArrayBuffer[ArrVisitor[?, OutputUnit]] = new mutable.ArrayBuffer(3)
        viss.addOne(vis.visitArray(length, index))
        if (thenVis.nonEmpty) viss.addOne(thenVis.get.visitArray(length, index))
        if (elseVis.nonEmpty) viss.addOne(elseVis.get.visitArray(length, index))

        insVisitors.addOne(new MapArrContext(new CompositeArrVisitor(viss.toSeq*), units => {
          if_then_else(units.head,
            thenVis.map(_ => units(1)),
            elseVis.map(_ => if (thenVis.isEmpty) units(1) else units(2)))
        }))
      })

    val insVisitor: ArrVisitor[?, collection.Seq[OutputUnit]] = new CompositeArrVisitor(insVisitors.toSeq*)
    var childVisitor: ArrVisitor[?, ?] = null // to be assigned based on child

    new ArrVisitor[Any, collection.Seq[OutputUnit]] {
      private var nextIdx = 0

      // returns subVisitor based on child index
      val prefixItemsVisitor: Option[ArrVisitor[?, OutputUnit]] = prefixItems.map(arr => new ArrVisitor[OutputUnit, OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer

        override def subVisitor: Visitor[?, ?] = SchemaValidator.of(arr(nextIdx), ctx, path.appended("prefixItems", nextIdx.toString), Some(Applicator.this))
        override def visitValue(u: OutputUnit, index: Int): Unit = units.addOne(u)
        override def visitEnd(index: Int): OutputUnit = and(path.appended("prefixItemsVisitor"), units)
      })

      override def subVisitor: Visitor[?, ?] = {
        val childVisitors: mutable.ArrayBuffer[ArrVisitor[?, OutputUnit]] = mutable.ArrayBuffer.from(insVisitors)

        if (prefixItems.nonEmpty && prefixItems.get.length >= nextIdx + 1) { childVisitors.addOne(prefixItemsVisitor.get) }
        else if (itemsVis.nonEmpty) childVisitors.addOne(itemsVis.get)

        childVisitor =
          if (childVisitors.length == 1) childVisitors.head
          else new CompositeArrVisitor(childVisitors.toSeq*)
        childVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = {
        childVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val units: mutable.ArrayBuffer[OutputUnit] = mutable.ArrayBuffer.from(insVisitor.visitEnd(index))
        units.sizeHint(units.size + 2)

        prefixItemsVisitor.foreach(pv => units.addOne(pv.visitEnd(index)))
        itemsVis.foreach(iv => units.addOne(iv.visitEnd(index)))
        units
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[?, collection.Seq[OutputUnit]] = {
    val propsVisited = mutable.ArrayBuffer.empty[String] // properties visited

    val insVisitors = mutable.ArrayBuffer.empty[ObjVisitor[?, OutputUnit]]
    notVis.foreach(vis => insVisitors.addOne(new MapObjContext(vis.visitObject(length, true, index), unit => unit.not())))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    ifVis.foreach(vis =>
      if (thenVis.isEmpty && elseVis.isEmpty) ()
      else {
        val viss: mutable.ArrayBuffer[ObjVisitor[?, OutputUnit]] = new mutable.ArrayBuffer(3)
        viss.addOne(vis.visitObject(length, true, index))
        if (thenVis.nonEmpty) viss.addOne(thenVis.get.visitObject(length, true, index))
        if (elseVis.nonEmpty) viss.addOne(elseVis.get.visitObject(length, true, index))

        insVisitors.addOne(new MapObjContext(new CompositeObjVisitor(viss.toSeq*), units => {
          if_then_else(units.head,
            thenVis.map(_ => units(1)),
            elseVis.map(_ => if (thenVis.isEmpty) units(1) else units(2)))
        }))
      })

    val depSchsObjViss: Option[collection.Map[String, ObjVisitor[?, (String, OutputUnit)]]] =
      depSchsViss.map(viss => viss.map(entry => (entry._1,
        MapObjContext(entry._2.visitObject(length, true, index), b => (entry._1, b)))))
    depSchsObjViss.foreach(viss =>
      insVisitors.addOne(MapObjContext(new CompositeObjVisitor(viss.values.toSeq*), k_units => {
        val apply = k_units.filter((k, unit) => propsVisited.contains(k))
        validate("dependentSchemas", "Some schemas did not successfully apply", apply.forall((k, u) => u.valid))
      })))

    val insVisitor: ObjVisitor[?, collection.Seq[OutputUnit]] = new CompositeObjVisitor(insVisitors.toSeq*)

    var childVisitor: ObjVisitor[?, ?] = null // to be assigned based on child

    new ObjVisitor[Any, collection.Seq[OutputUnit]] {
      private var propNamesValid = true
      private var currentKey: String = "?"

      // returns subVisitor based on currentKey
      val propsVisitor: Option[ObjVisitor[?, OutputUnit]] = properties.map(m => new ObjVisitor[OutputUnit, OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private var subsch = true

        override def visitKey(index: Int): Visitor[?, ?] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[?, ?] = SchemaValidator.of(m(currentKey), ctx, path.appended("properties", currentKey), Some(Applicator.this))
        override def visitValue(u: OutputUnit, index: Int): Unit = units.addOne(u)
        override def visitEnd(index: Int): OutputUnit = and(path.appended("prefixItemsVisitor"), units)
      })

      private var matchedPatternSchs: Seq[(String, Schema)] = Nil // to be assigned based on key visited
      // returns subVisitor based on assigned matchedPatternSchs
      val patternPropsVisitor: Option[ObjVisitor[?, OutputUnit]] = patternProperties.map(m => new ObjVisitor[Seq[OutputUnit], OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer

        override def visitKey(index: Int): Visitor[?, ?] = ???
        override def visitKeyValue(v: Any): Unit = ???
        override def subVisitor: Visitor[?, ?] = new CompositeVisitor(matchedPatternSchs.map(pattSch =>
          SchemaValidator.of(pattSch._2, ctx, path.appended("patternProperties", pattSch._1), Some(Applicator.this)))*)
        override def visitValue(mult: Seq[OutputUnit], index: Int): Unit = mult.foreach(u => units.addOne(u))
        override def visitEnd(index: Int): OutputUnit = and(path.appended("patternProperties"), units)
      })

      override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          if (propNymVis.nonEmpty) propNamesValid = propNamesValid && propNymVis.get.visitString(s, index1).valid
          propsVisited.addOne(currentKey)
          matchedPatternSchs = patternProperties.map(m => m
            .withFilter(entry => entry._1.matches(currentKey))
            .map(entry => (entry._1.toString(), entry._2))
            .toSeq).getOrElse(Nil)

          insVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[?, ?] = {
        val childVisitors: mutable.ArrayBuffer[ObjVisitor[?, OutputUnit]] = mutable.ArrayBuffer.from(insVisitors)

        var isAddl = true // if not in properties or matched patterns
        if (properties.nonEmpty && properties.get.contains(currentKey)) { isAddl = false; childVisitors.addOne(propsVisitor.get) }
        if (matchedPatternSchs.nonEmpty) { isAddl = false; childVisitors.addOne(patternPropsVisitor.get) }
        if (isAddl) addlPropsVis.foreach(vis => childVisitors.addOne(vis))

        childVisitor =
          if (childVisitors.length == 1) childVisitors.head
          else new CompositeObjVisitor(childVisitors.toSeq*)
        childVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = childVisitor.narrow.visitValue(v, index)

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val units: mutable.ArrayBuffer[OutputUnit] = mutable.ArrayBuffer.from(insVisitor.visitEnd(index))
        units.sizeHint(units.size + 4)

        propsVisitor.foreach(pv => units.addOne(pv.visitEnd(index)))
        patternPropsVisitor.foreach(iv => units.addOne(iv.visitEnd(index)))
        addlPropsVis.foreach(iv => units.addOne(iv.visitEnd(index)))
        validate(Applicator.PropertyNames, "", units, propNamesValid)
        units
      }
    }
  }

  /* helper methods */
  private def and(kw: JsonPointer, units: collection.Seq[OutputUnit]): OutputUnit = { // TODO: if verbose?
    if (units.map(_.valid).forall(identity)) {
      OutputUnit(true, Some(kw), None, Some(ctx.currentLoc), None, units.filter(_.valid), None, Nil)
    } else {
      OutputUnit(false, Some(kw), None, Some(ctx.currentLoc), None, units.filterNot(_.valid), None, Nil)
    }
  }

  private def one(kw: JsonPointer, units: collection.Seq[OutputUnit]): OutputUnit = { // TODO: if verbose?
    if (units.count(_.valid) == 1) {
      OutputUnit(true, Some(kw), None, Some(ctx.currentLoc), None, units.filter(_.valid), None, Nil)
    } else {
      OutputUnit(false, Some(kw), None, Some(ctx.currentLoc), None, units.filterNot(_.valid), None, Nil)
    }
  }

  private def any(kw: JsonPointer, units: collection.Seq[OutputUnit]): OutputUnit = { // TODO: if verbose?
    if (units.exists(_.valid)) {
      OutputUnit(true, Some(kw), None, Some(ctx.currentLoc), None, units.filter(_.valid), None, Nil)
    } else {
      OutputUnit(false, Some(kw), None, Some(ctx.currentLoc), None, units.filterNot(_.valid), None, Nil)
    }
  }

  private def if_then_else(iff: OutputUnit, thenn: Option[OutputUnit], els: Option[OutputUnit]): OutputUnit = {
    if (iff.valid && thenn.nonEmpty) {
      if (thenn.get.valid) { valid("then") }
      else OutputUnit(false, Some(path.appended("then")), None, Some(ctx.currentLoc), None, Seq(thenn.get), None, Nil)
    }
    else if (!iff.valid && els.nonEmpty) {
      if (els.get.valid) { valid("else") }
      else OutputUnit(false, Some(path.appended("else")), None, Some(ctx.currentLoc), None, Seq(thenn.get), None, Nil)
    }
    else ???
  }
}

object Applicator {
  private val PropertyNames = "propertyNames"
}
