package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Applicator.*
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

final class Applicator private(schema: ObjectSchema,
                         ctx: Context,
                         path: JsonPointer,
                         dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {

  private val prefixItems: Option[collection.Seq[Schema]] = schema.getSchemaArrayOpt(PrefixItems)
  private val itemsVis: Option[ArrVisitor[OutputUnit, OutputUnit]] = schema.getSchemaOpt(Items)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Items), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer

      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = addUnit(units, u)
      override def visitEnd(index: Int): OutputUnit = and(Items, units, Some(True))
    })

  private val maxContains: Option[Int] = schema.getInt(MaxContains)
  private val minContains: Option[Int] = schema.getInt(MinContains)
  private val contains: Option[ArrVisitor[OutputUnit, OutputUnit]] = schema.getSchemaOpt(Contains)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Contains), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
      private var nextIdx = 0
      private val matched: mutable.ArrayBuffer[Num] = new ArrayBuffer()

      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = { if (u.vvalid) matched.addOne(Num(nextIdx)); nextIdx += 1 }
      override def visitEnd(index: Int): OutputUnit = { // TODO: break up contains into contains, min/max
        var res = matched.nonEmpty
        if (minContains.nonEmpty) {
          if (minContains.get == 0) res = true
          else res = res && (matched.size >= minContains.get)
        }
        if (maxContains.nonEmpty) {
          res = res && (matched.size <= maxContains.get)
        }
        unitOf(res, Contains, Some("Array does not contain given elements"), Nil, Some(Arr.from(matched)), Nil) // TODO: include if failed bc min/max
      }
    })
  private val addlPropsVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(AdditionalProperties)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(AdditionalProperties), Some(this)))
  private val properties: Option[collection.Map[String, Schema]] = schema.getSchemaObjectOpt(Properties)
  private val patternProperties: Option[collection.Map[Regex, Schema]] = schema.getSchemaObjectOpt(PatternProperties)
    .map(obj => obj.map(entry => (new Regex(entry._1).unanchored, entry._2)))
  private val depSchsViss: Option[collection.Map[String, Visitor[?, OutputUnit]]] = schema.getSchemaObjectOpt(DependentSchemas)
    .map(obj => obj.map(entry => (entry._1, SchemaValidator.of(entry._2, ctx, path.appended(DependentSchemas, entry._1), Some(this)))))
  private val propNymVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(PropertyNames)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(PropertyNames), Some(this)))
  private val ifVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(If)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(If), Some(this)))
  private val thenVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(Then)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Then), Some(this)))
  private val elseVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(Else)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Else), Some(this)))
  private val allOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt(AllOf)
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended(AllOf, schidx._2.toString), Some(this))))
    .map(schViss => new FlatCompositeVisitor(units => allOf(AllOf, units), schViss.toSeq*))
  private val oneOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt(OneOf)
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended(OneOf, schidx._2.toString), Some(this))))
    .map(schViss => new FlatCompositeVisitor(units => oneOf(OneOf, units), schViss.toSeq*))
  private val anyOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt(AnyOf)
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended(AnyOf, schidx._2.toString), Some(this))))
    .map(schViss => new FlatCompositeVisitor(units => anyOf(AnyOf, units), schViss.toSeq*))
  private val notVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(Not)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Not), Some(this)))

  override def visitNull(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, not(v.visitNull(index))))
    allOfVis.foreach(v => addUnit(units, v.visitNull(index)))
    anyOfVis.foreach(v => addUnit(units, v.visitNull(index)))
    oneOfVis.foreach(v => addUnit(units, v.visitNull(index)))
    ifVis.foreach(iv => {
      val u = iv.visitNull(index); addUnit(units, OutputUnit.info(u))
      if (u.vvalid) thenVis.foreach(v => addUnit(units, v.visitNull(index)))
      else elseVis.map(v => addUnit(units, v.visitNull(index)))
    })
    units
  }

  override def visitFalse(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, not(v.visitFalse(index))))
    allOfVis.foreach(v => addUnit(units, v.visitFalse(index)))
    anyOfVis.foreach(v => addUnit(units, v.visitFalse(index)))
    oneOfVis.foreach(v => addUnit(units, v.visitFalse(index)))
    ifVis.foreach(iv => {
      val u = iv.visitFalse(index); addUnit(units, OutputUnit.info(u))
      if (u.vvalid) thenVis.foreach(v => addUnit(units, v.visitFalse(index)))
      else elseVis.map(v => addUnit(units, v.visitFalse(index)))
    })
    units
  }

  override def visitTrue(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, not(v.visitTrue(index))))
    allOfVis.foreach(v => addUnit(units, v.visitTrue(index)))
    anyOfVis.foreach(v => addUnit(units, v.visitTrue(index)))
    oneOfVis.foreach(v => addUnit(units, v.visitTrue(index)))
    ifVis.foreach(iv => {
      val u = iv.visitTrue(index); addUnit(units, OutputUnit.info(u))
      if (u.vvalid) thenVis.foreach(v => addUnit(units, v.visitTrue(index)))
      else elseVis.map(v => addUnit(units, v.visitTrue(index)))
    })
    units
  }

  override def visitInt64(l: Long, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, not(v.visitInt64(l, index))))
    allOfVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    anyOfVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    oneOfVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    ifVis.foreach(iv => {
      val u = iv.visitInt64(l, index); addUnit(units, OutputUnit.info(u))
      if (u.vvalid) thenVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
      else elseVis.map(v => addUnit(units, v.visitInt64(l, index)))
    })
    units
  }

  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, not(v.visitFloat64(d, index))))
    allOfVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    anyOfVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    oneOfVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    ifVis.foreach(iv => {
      val u = iv.visitFloat64(d, index); addUnit(units, OutputUnit.info(u))
      if (u.vvalid) thenVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
      else elseVis.map(v => addUnit(units, v.visitFloat64(d, index)))
    })
    units
  }

  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(7) // perf: should be re-used?

    notVis.foreach(v => addUnit(units, not(v.visitString(s, index))))
    allOfVis.foreach(v => addUnit(units, v.visitString(s, index)))
    anyOfVis.foreach(v => addUnit(units, v.visitString(s, index)))
    oneOfVis.foreach(v => addUnit(units, v.visitString(s, index)))
    ifVis.foreach(v => {
      val u = v.visitString(s, index); addUnit(units, OutputUnit.info(u))
      if (u.vvalid) thenVis.foreach(v => addUnit(units, v.visitString(s, index)))
      else elseVis.map(v => addUnit(units, v.visitString(s, index)))
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

  override def visitArray(length: Int, index: Int): ArrVisitor[Any, collection.Seq[OutputUnit]] = {
    val insVisitors: mutable.ArrayBuffer[ArrVisitor[?, OutputUnit]] = new mutable.ArrayBuffer(6)
    notVis.foreach(vis => insVisitors.addOne(new MapArrContext(vis.visitArray(length, index), unit => not(unit))))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    contains.foreach(vis => insVisitors.addOne(vis)) // Vis[OUnit, OUnit]
    ifVis.foreach(v => insVisitors.addOne(v.visitArray(length, index)))
    thenVis.foreach(v => insVisitors.addOne(v.visitArray(length, index)))
    elseVis.foreach(v => insVisitors.addOne(v.visitArray(length, index)))

    val insVisitor: ArrVisitor[Seq[Nothing], Seq[OutputUnit]] = new CompositeArrVisitor(insVisitors.toSeq*)
    var childVisitor: ArrVisitor[?, ?] = null // to be assigned based on child

    new ArrVisitor[Any, collection.Seq[OutputUnit]] {
      private var nextIdx = 0

      // returns subVisitor based on child index
      val prefixItemsVisitor: Option[ArrVisitor[?, OutputUnit]] = prefixItems.map(arr => new ArrVisitor[OutputUnit, OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private var idx = -1

        override def subVisitor: Visitor[?, ?] =
          SchemaValidator.of(arr(nextIdx), ctx, path.appended(PrefixItems, nextIdx.toString), Some(Applicator.this))
        override def visitValue(u: OutputUnit, index: Int): Unit = { addUnit(units, u); if (u.vvalid) idx = nextIdx }
        override def visitEnd(index: Int): OutputUnit = and(PrefixItems, units, Some(Num(idx)))
      })

      override def subVisitor: Visitor[?, ?] = {
        val childVisitors: mutable.ArrayBuffer[ArrVisitor[Nothing, OutputUnit]] = mutable.ArrayBuffer.from(insVisitors)
        childVisitors.sizeHint(childVisitors.size + 2)

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
        val insUnits = insVisitor.visitEnd(index)
        var iff: OutputUnit = null
        var thenn: OutputUnit = null
        var els: OutputUnit = null
        val units: mutable.ArrayBuffer[OutputUnit] = new mutable.ArrayBuffer(insUnits.size + 5)
        insUnits.foreach(u => {
          if (u.kwLoc.refTokens.last == If) iff = u
          else if (u.kwLoc.refTokens.last == Then) thenn = u
          else if (u.kwLoc.refTokens.last == Else) els = u
          else addUnit(units, u)
        })

        prefixItemsVisitor.foreach(b => addUnit(units, b.visitEnd(index)))
        itemsVis.foreach(v => addUnit(units, v.visitEnd(index)))

        ifVis.foreach(_ => {
          val u = iff; addUnit(units, OutputUnit.info(u))
          if (u.vvalid) {
            if (elseVis.nonEmpty) ctx.onInvalidated(Seq(els))
            thenVis.foreach(_ => addUnit(units, thenn)) // warning these could fail if for some reason iff/thenn/els are note set
          } else {
            if (thenVis.nonEmpty) ctx.onInvalidated(Seq(thenn))
            elseVis.foreach(_ => addUnit(units, els))
          }
        })

        units
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Any, collection.Seq[OutputUnit]] = {
    val propsVisited = mutable.ArrayBuffer[String]() // properties visited

    val insVisitors: mutable.ArrayBuffer[ObjVisitor[?, OutputUnit]] = new mutable.ArrayBuffer(6)
    notVis.foreach(vis => insVisitors.addOne(new MapObjContext(vis.visitObject(length, true, index), unit => not(unit))))
    allOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    anyOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    oneOfVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    ifVis.foreach(v => insVisitors.addOne(v.visitObject(length, true, index)))
    thenVis.foreach(v => insVisitors.addOne(v.visitObject(length, true, index)))
    elseVis.foreach(v => insVisitors.addOne(v.visitObject(length, true, index)))

    depSchsViss.map(viss => viss.map(k_vis =>
        (k_vis._1, MapObjContext(k_vis._2.visitObject(length, true, index), b => (k_vis._1, b))))) // Option[collection.Map[String, ObjVisitor[?, (String, OutputUnit)]]]
      .foreach(viss =>
        insVisitors.addOne(MapObjContext(new CompositeObjVisitor(viss.values.toSeq *), k_units => { // Vis[Seq[Nothing], OUnit]
          val (applied, invalid) = k_units.partition((k, _) => propsVisited.contains(k))
          ctx.onInvalidated(invalid.map((_, u) => u))
          and(DependentSchemas, applied.map((_, u) => u))
        })))

    val insVisitor: CompositeObjVisitor[Nothing, OutputUnit] = new CompositeObjVisitor(insVisitors.toSeq*) // ObjVisitor[Seq[Nothing], Seq[OutputUnit]]
    var childVisitor: ObjVisitor[?, ?] = null // to be assigned based on child

    new ObjVisitor[Any, collection.Seq[OutputUnit]] {
      private var propNamesValid = true
      private var currentKey: String = "?"

      // returns subVisitor based on currentKey
      val propsVisitor: Option[ObjVisitor[OutputUnit, OutputUnit]] = properties.map(m => new ObjVisitor[OutputUnit, OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private val annot: mutable.Buffer[Value] = new ArrayBuffer()

        override def visitKey(index: Int): Visitor[?, ?] = throw new UnsupportedOperationException("Should not be invoked")
        override def visitKeyValue(v: Any): Unit = throw new UnsupportedOperationException("Should not be invoked")
        override def subVisitor: Visitor[?, ?] = SchemaValidator.of(m(currentKey), ctx, path.appended(Properties, currentKey), Some(Applicator.this))
        override def visitValue(u: OutputUnit, index: Int): Unit = { addUnit(units, u); if (u.vvalid) annot.addOne(Str(currentKey))}
        override def visitEnd(index: Int): OutputUnit = and(Properties, units, Some(Arr.from(annot)))
      })

      private var matchedPatternSchs: Seq[(String, Schema)] = Nil // to be assigned based on key visited
      // returns subVisitor based on assigned matchedPatternSchs
      val patternPropsVisitor: Option[ObjVisitor[Seq[OutputUnit], OutputUnit]] = patternProperties.map(m => new ObjVisitor[Seq[OutputUnit], OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private val annot: mutable.Buffer[Value] = new ArrayBuffer()

        override def visitKey(index: Int): Visitor[?, ?] = throw new UnsupportedOperationException("Should not be invoked")
        override def visitKeyValue(v: Any): Unit = throw new UnsupportedOperationException("Should not be invoked")
        override def subVisitor: Visitor[?, ?] = new CompositeVisitor(matchedPatternSchs.map(pattSch =>
          SchemaValidator.of(pattSch._2, ctx, path.appended(PatternProperties, pattSch._1), Some(Applicator.this)))*)
        override def visitValue(us: Seq[OutputUnit], index: Int): Unit = us.foreach(u => { addUnit(units, u); if (u.vvalid) annot.addOne(Str(currentKey))})
        override def visitEnd(index: Int): OutputUnit = and(PatternProperties, units, Some(Arr.from(annot)))
      })

      val addlPropsObjVis: Option[ObjVisitor[OutputUnit, OutputUnit]] = addlPropsVis.map(schValidator => new ObjVisitor[OutputUnit, OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private val annot: mutable.Buffer[Value] = new ArrayBuffer()

        override def visitKey(index: Int): Visitor[?, ?] = throw new UnsupportedOperationException("Should not be invoked")
        override def visitKeyValue(v: Any): Unit = throw new UnsupportedOperationException("Should not be invoked")
        override def subVisitor: Visitor[?, ?] = schValidator
        override def visitValue(u: OutputUnit, index: Int): Unit = { addUnit(units, u); if (u.vvalid) annot.addOne(Str(currentKey)) }
        override def visitEnd(index: Int): OutputUnit = and(AdditionalProperties, units, Some(Arr.from(annot)))
      })

      override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "Expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          if (propNymVis.nonEmpty) propNamesValid = propNamesValid && propNymVis.get.visitString(s, index1).vvalid
          propsVisited.addOne(currentKey)
          matchedPatternSchs = patternProperties.map(m => m
              .withFilter(rgx_sch => rgx_sch._1.matches(currentKey))
              .map(rgx_sch => (rgx_sch._1.toString(), rgx_sch._2))
              .toSeq)
            .getOrElse(Nil)

          insVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[?, ?] = {
        val childVisitors: mutable.ArrayBuffer[ObjVisitor[Nothing, OutputUnit]] = mutable.ArrayBuffer.from(insVisitors)
        childVisitors.sizeHint(childVisitors.size + 6)

        var isAddl = true // if not in properties or matched patterns
        if (properties.nonEmpty && properties.get.contains(currentKey)) { isAddl = false; childVisitors.addOne(propsVisitor.get) }
        if (matchedPatternSchs.nonEmpty) { isAddl = false; childVisitors.addOne(patternPropsVisitor.get) }
        if (isAddl) addlPropsObjVis.foreach(vis => childVisitors.addOne(vis))

        childVisitor =
          if (childVisitors.length == 1) childVisitors.head
          else new CompositeObjVisitor(childVisitors.toSeq*)
        childVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = childVisitor.narrow.visitValue(v, index)

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val insUnits = insVisitor.visitEnd(index)
        var iff: OutputUnit = null
        var thenn: OutputUnit = null
        var els: OutputUnit = null
        val units: mutable.ArrayBuffer[OutputUnit] = new mutable.ArrayBuffer(insUnits.size + 7)
        insUnits.foreach(u => {
            if (u.kwLoc.refTokens.last == If) iff = u
            else if (u.kwLoc.refTokens.last == Then) thenn = u
            else if (u.kwLoc.refTokens.last == Else) els = u
            else addUnit(units, u)
          })

        propsVisitor.foreach(v => addUnit(units, v.visitEnd(index)))
        patternPropsVisitor.foreach(v => addUnit(units, v.visitEnd(index)))
        addlPropsObjVis.foreach(v => addUnit(units, v.visitEnd(index)))
        addUnit(units, unitOf(propNamesValid, PropertyNames, ""))

        ifVis.foreach(_ => {
          val u = iff; addUnit(units, OutputUnit.info(u))
          if (u.vvalid) {
            if (elseVis.nonEmpty) ctx.onInvalidated(Seq(els))
            thenVis.foreach(_ => addUnit(units, thenn)) // warning these could fail if for some reason iff/thenn/els are not set
          } else {
            if (thenVis.nonEmpty) ctx.onInvalidated(Seq(thenn))
            elseVis.foreach(_ => addUnit(units, els))
          }
        })

        units
      }
    }
  }

  /* helper methods */
  private def and(kw: String, units: collection.Seq[OutputUnit], ann: Option[Value] = None): OutputUnit = {
    val (annots, errs) = units.partition(_.vvalid)
    unitOf(errs.isEmpty, kw, None, errs, ann, annots)
  }

  private def allOf(kw: String, units: collection.Seq[OutputUnit]): OutputUnit = {
    val (annots, errs) = units.partition(_.vvalid)
    unitOf(errs.isEmpty, kw, None, errs, None, annots)
  }

  private def oneOf(kw: String, units: collection.Seq[OutputUnit]): OutputUnit = {
    val (annots, errs) = units.partition(_.vvalid)
    unitOf(annots.size == 1, kw, None, errs, None, annots)
  }

  private def anyOf(kw: String, units: collection.Seq[OutputUnit]): OutputUnit = {
    val (annots, errs) = units.partition(_.vvalid)
    unitOf(annots.nonEmpty, kw, None, errs, None, annots)
  }

  private def not(n: OutputUnit): OutputUnit = {
    if (n.vvalid) unitOf(false, Not, None, Nil, None, n.annotations)
    else unitOf(true, Not, None, n.errors, None, Nil)
  }
}

object Applicator extends VocabBaseFactory {
  val PrefixItems = "prefixItems"
  val Items = "items"
  val MaxContains = "maxContains"
  val MinContains = "minContains"
  val Contains = "contains"
  val AdditionalProperties = "additionalProperties"
  val Properties = "properties"
  val PatternProperties = "patternProperties"
  val DependentSchemas = "dependentSchemas"
  val PropertyNames = "propertyNames"
  val If = "if"
  val Then = "then"
  val Else = "else"
  val Not = "not"
  val AllOf = "allOf"
  val AnyOf = "anyOf"
  val OneOf = "oneOf"

  val Keys: Seq[String] = Seq(PrefixItems, Items, MaxContains, MinContains, Contains, AdditionalProperties, Properties,
    PatternProperties, DependentSchemas, PropertyNames, Properties, If, Then, Else, Not, AllOf, AnyOf, OneOf)

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/applicator"
  
  override def from(schema: ObjectSchema,
                    ctx: Context,
                    path: JsonPointer,
                    dynParent: Option[Vocab[?]]): Applicator = new Applicator(schema, ctx, path, dynParent)

  override def appliesTo(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
}
