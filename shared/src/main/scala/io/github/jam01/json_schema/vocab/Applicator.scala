package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Applicator.*
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

final class Applicator private(schema: ObjectSchema,
                               ctx: Context,
                               path: JsonPointer,
                               dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {

  private val addlPropsVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(AdditionalProperties)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(AdditionalProperties), Some(this)))
  private val patternPropsViss: Option[collection.Map[Regex, Visitor[?, OutputUnit]]] = schema.getSchemaObjectOpt(PatternProperties)
    .map(obj => obj.map((pttrn, sch) => (new Regex(pttrn).unanchored, SchemaValidator.of(sch, ctx, path.appended(PatternProperties, pttrn), Some(Applicator.this)))))
  private val propsViss: Option[collection.Map[String, Visitor[?, OutputUnit]]] = schema.getSchemaObjectOpt(Properties)
    .map(obj => obj.map((key, sch) => (key, SchemaValidator.of(sch, ctx, path.appended(Properties, key), Some(this)))))
  private val depSchsViss: Option[collection.Map[String, Visitor[?, OutputUnit]]] = schema.getSchemaObjectOpt(DependentSchemas)
    .map(obj => obj.map((key, sch) => (key, SchemaValidator.of(sch, ctx, path.appended(DependentSchemas, key), Some(this)))))
  private val propNymVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(PropertyNames)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(PropertyNames), Some(this)))
  private val ifVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(If)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(If), Some(this)))
  private val thenVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(Then)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Then), Some(this)))
  private val elseVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(Else)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Else), Some(this)))
  private val allOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt(AllOf)
    .map(schs => schs.view.zipWithIndex.map((sch, idx) => SchemaValidator.of(sch, ctx, path.appended(AllOf, idx.toString), Some(this))))
    .map(schViss => new FlatCompositeVisitor(units => allOf(AllOf, units), schViss.toSeq*))
  private val oneOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt(OneOf)
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended(OneOf, schidx._2.toString), Some(this))))
    .map(schViss => new FlatCompositeVisitor(units => oneOf(OneOf, units), schViss.toSeq*))
  private val anyOfVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaArrayOpt(AnyOf)
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, path.appended(AnyOf, schidx._2.toString), Some(this))))
    .map(schViss => new FlatCompositeVisitor(units => anyOf(AnyOf, units), schViss.toSeq*))
  private val notVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(Not)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Not), Some(this)))
  private val itemsVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(Items)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Items), Some(this)))
  private val prefItemsViss: Option[collection.Map[Int, Visitor[?, OutputUnit]]] = schema.getSchemaArrayOpt(PrefixItems)
    .map(obj => obj.view.zipWithIndex
      .map((sch, idx) => (idx, SchemaValidator.of(sch, ctx, path.appended(PrefixItems, idx.toString), Some(Applicator.this))))
      .toMap
    )

  private val maxContains: Option[Int] = schema.getInt(MaxContains)
  private val minContains: Option[Int] = schema.getInt(MinContains)
  private val contains: Option[ArrVisitor[OutputUnit, OutputUnit]] = schema.getSchemaOpt(Contains)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Contains), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
      private var nextIdx = 0
      private val matched: mutable.ArrayBuffer[Num] = new ArrayBuffer()

      override def subVisitor: Visitor[?, ?] = schValidator

      override def visitValue(u: OutputUnit, index: Int): Unit = {
        if (u.vvalid) matched.addOne(Num(nextIdx)); nextIdx += 1
      }

      override def visitEnd(index: Int): OutputUnit = { // TODO: break up contains into contains, min/max
        var res = matched.nonEmpty
        if (minContains.nonEmpty) {
          if (minContains.get == 0) res = true
          else res = res && (matched.size >= minContains.get)
        }
        if (maxContains.nonEmpty) {
          res = res && (matched.size <= maxContains.get)
        }
        mkUnit(res, Contains, "Array does not contain given elements", Nil, Arr.from(matched), Nil) // TODO: include if failed bc min/max
      }
    })

  override def visitNull(index: Int): collection.Seq[OutputUnit] = {
    val buff = ListBuffer[OutputUnit]()
    accumulateOpt(buff, notVis.map(v => not(v.visitNull(index)))) &&
      accumulateOpt(buff, allOfVis.map(v => v.visitNull(index))) &&
      accumulateOpt(buff, anyOfVis.map(v => v.visitNull(index))) &&
      accumulateOpt(buff, oneOfVis.map(v => v.visitNull(index))) &&
      ifVis.forall(ifv => {
        val iff = ifVis.get.visitNull(index); accumulate(buff, OutputUnit.info(iff))
        if (iff.vvalid) accumulateOpt(buff, thenVis.map(v => v.visitNull(index)))
        else accumulateOpt(buff, elseVis.map(v => v.visitNull(index)))
      })

    buff.result
  }

  override def visitFalse(index: Int): collection.Seq[OutputUnit] = {
    val buff = ListBuffer[OutputUnit]()
    accumulateOpt(buff, notVis.map(v => not(v.visitFalse(index)))) &&
      accumulateOpt(buff, allOfVis.map(v => v.visitFalse(index))) &&
      accumulateOpt(buff, anyOfVis.map(v => v.visitFalse(index))) &&
      accumulateOpt(buff, oneOfVis.map(v => v.visitFalse(index))) &&
      ifVis.forall(ifv => {
        val iff = ifv.visitFalse(index); accumulate(buff, OutputUnit.info(iff))
        if (iff.vvalid) accumulateOpt(buff, thenVis.map(v => v.visitFalse(index)))
        else accumulateOpt(buff, elseVis.map(v => v.visitFalse(index)))
      })

    buff.result
  }

  override def visitTrue(index: Int): collection.Seq[OutputUnit] = {
    val buff = ListBuffer[OutputUnit]()
    accumulateOpt(buff, notVis.map(v => not(v.visitTrue(index)))) &&
      accumulateOpt(buff, allOfVis.map(v => v.visitTrue(index))) &&
      accumulateOpt(buff, anyOfVis.map(v => v.visitTrue(index))) &&
      accumulateOpt(buff, oneOfVis.map(v => v.visitTrue(index))) &&
      ifVis.forall(ifv => {
        val iff = ifVis.get.visitTrue(index); accumulate(buff, OutputUnit.info(iff))
        if (iff.vvalid) accumulateOpt(buff, thenVis.map(v => v.visitTrue(index)))
        else accumulateOpt(buff, elseVis.map(v => v.visitTrue(index)))
      })

    buff.result
  }

  override def visitInt64(l: Long, index: Int): collection.Seq[OutputUnit] = {
    val buff = ListBuffer[OutputUnit]()
    accumulateOpt(buff, notVis.map(v => not(v.visitInt64(l, index)))) &&
      accumulateOpt(buff, allOfVis.map(v => v.visitInt64(l, index))) &&
      accumulateOpt(buff, anyOfVis.map(v => v.visitInt64(l, index))) &&
      accumulateOpt(buff, oneOfVis.map(v => v.visitInt64(l, index))) &&
      ifVis.forall(ifv => {
        val iff = ifVis.get.visitInt64(l, index); accumulate(buff, OutputUnit.info(iff))
        if (iff.vvalid) accumulateOpt(buff, thenVis.map(v => v.visitInt64(l, index)))
        else accumulateOpt(buff, elseVis.map(v => v.visitInt64(l, index)))
      })

    buff.result
  }

  override def visitFloat64(num: Double, index: Int): collection.Seq[OutputUnit] = {
    val buff = ListBuffer[OutputUnit]()
    accumulateOpt(buff, notVis.map(v => not(v.visitFloat64(num, index)))) &&
      accumulateOpt(buff, allOfVis.map(v => v.visitFloat64(num, index))) &&
      accumulateOpt(buff, anyOfVis.map(v => v.visitFloat64(num, index))) &&
      accumulateOpt(buff, oneOfVis.map(v => v.visitFloat64(num, index))) &&
      ifVis.forall(ifv => {
        val iff = ifVis.get.visitFloat64(num, index); accumulate(buff, OutputUnit.info(iff))
        if (iff.vvalid) accumulateOpt(buff, thenVis.map(v => v.visitFloat64(num, index)))
        else accumulateOpt(buff, elseVis.map(v => v.visitFloat64(num, index)))
      })

    buff.result
  }

  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = {
    val buff = ListBuffer[OutputUnit]()
    accumulateOpt(buff, notVis.map(v => not(v.visitString(s, index)))) &&
      accumulateOpt(buff, allOfVis.map(v => v.visitString(s, index))) &&
      accumulateOpt(buff, anyOfVis.map(v => v.visitString(s, index))) &&
      accumulateOpt(buff, oneOfVis.map(v => v.visitString(s, index))) &&
      ifVis.forall(ifv => {
        val iff = ifVis.get.visitString(s, index); accumulate(buff, OutputUnit.info(iff))
        if (iff.vvalid) accumulateOpt(buff, thenVis.map(v => v.visitString(s, index)))
        else accumulateOpt(buff, elseVis.map(v => v.visitString(s, index)))
      })

    buff.result
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
    val instanceVisitors = Seq(
      notVis.map(v => new MapArrContext(v.visitArray(length, index), unit => not(unit))),
      allOfVis.map(v => v.visitArray(length, index)),
      anyOfVis.map(v => v.visitArray(length, index)),
      oneOfVis.map(v => v.visitArray(length, index)),
      contains,
      ifVis.map(v => v.visitArray(length, index)),
      thenVis.map(v => v.visitArray(length, index)),
      elseVis.map(v => v.visitArray(length, index))
    ).flatten

    val insVisitor: ArrVisitor[Seq[Nothing], Seq[OutputUnit]] = new CompositeArrVisitor(instanceVisitors*)
    var childVisitor: ArrVisitor[?, ?] = null // to be assigned based on child

    new ArrVisitor[Any, collection.Seq[OutputUnit]] {
      private var nextIdx = 0

      // returns subVisitor based on child index
      val prefixItemsArrVis: Option[ArrVisitor[?, OutputUnit]] = prefItemsViss.map(vismap => new ArrVisitor[OutputUnit, OutputUnit] {
        private val buff = ListBuffer[OutputUnit]()
        private var idx = -1
        override def subVisitor: Visitor[?, ?] = vismap(nextIdx)
        override def visitValue(u: OutputUnit, index: Int): Unit = { accumulate(buff, u); if (u.vvalid) idx = nextIdx }
        override def visitEnd(index: Int): OutputUnit = compose(PrefixItems, buff.result, Num(idx))
      })
      
      val itemsArrVis: Option[ArrVisitor[OutputUnit, OutputUnit]] = itemsVis.map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
        private val buff = ListBuffer[OutputUnit]()
        override def subVisitor: Visitor[?, ?] = schValidator
        override def visitValue(u: OutputUnit, index: Int): Unit = accumulate(buff, u)
        override def visitEnd(index: Int): OutputUnit = compose(Items, buff.result, True)
      })

      override def subVisitor: Visitor[?, ?] = {
        val childVisitors = ListBuffer.from(instanceVisitors)
        if (prefItemsViss.nonEmpty && prefItemsViss.get.size >= nextIdx + 1) { childVisitors.addOne(prefixItemsArrVis.get) }
        else if (itemsVis.nonEmpty) childVisitors.addOne(itemsArrVis.get)

        childVisitor =
          if (childVisitors.length == 1) childVisitors.head
          else new CompositeArrVisitor(childVisitors.result*)
        childVisitor.subVisitor
      }

      override def visitValue(v: Any, index: Int): Unit = {
        childVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val buff = new ListBuffer[OutputUnit]
        val insVisResults = insVisitor.visitEnd(index)
        var iff: OutputUnit = null
        var thenn: OutputUnit = null
        var els: OutputUnit = null
        insVisResults.foreach(unit => {
          if (unit.kwLoc.refTokens.last == If) iff = unit
          else if (unit.kwLoc.refTokens.last == Then) thenn = unit
          else if (unit.kwLoc.refTokens.last == Else) els = unit
          else accumulate(buff, unit)
        })

        accumulateOpt(buff, prefixItemsArrVis.map(v => v.visitEnd(index)))
        accumulateOpt(buff, itemsArrVis.map(v => v.visitEnd(index)))
        ifVis.foreach(_ => {
          val iff0 = iff; accumulate(buff, OutputUnit.info(iff0))
          if (iff0.vvalid) {
            if (elseVis.nonEmpty) ctx.notifyInvalid(Seq(els))
            thenVis.foreach(_ => accumulate(buff, thenn)) // warning these could fail if for some reason iff/thenn/els are note set
          } else {
            if (thenVis.nonEmpty) ctx.notifyInvalid(Seq(thenn))
            elseVis.foreach(_ => accumulate(buff, els))
          }
        })

        buff.result
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

    depSchsViss.map(viss => viss.map((key, v) => (key, MapObjContext(v.visitObject(length, true, index), unit => (key, unit))))) // Option[collection.Map[String, ObjVisitor[?, (String, OutputUnit)]]]
      .foreach(viss =>
        insVisitors.addOne(MapObjContext(new CompositeObjVisitor(viss.values.toSeq *), key_unit_tuples => { // Vis[Seq[Nothing], OUnit]
          val (applied, invalid) = key_unit_tuples.partition((key, _) => propsVisited.contains(key))
          ctx.notifyInvalid(invalid.map((_, unit) => unit))
          compose(DependentSchemas, applied.map((_, unit) => unit))
        })))

    val insVisitor: CompositeObjVisitor[Nothing, OutputUnit] = new CompositeObjVisitor(insVisitors.toSeq*) // ObjVisitor[Seq[Nothing], Seq[OutputUnit]]
    var childVisitor: ObjVisitor[?, ?] = null // to be assigned based on child

    new ObjVisitor[Any, collection.Seq[OutputUnit]] {
      private var propNamesValid = true
      private var currentKey: String = "?"

      // returns subVisitor based on currentKey
      val propsVisitor: Option[ObjVisitor[OutputUnit, OutputUnit]] = propsViss.map(vismap => new ObjVisitor[OutputUnit, OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private val annot: mutable.Buffer[Value] = new ArrayBuffer()

        override def visitKey(index: Int): Visitor[?, ?] = throw new IllegalStateException
        override def visitKeyValue(v: Any): Unit = throw new IllegalStateException
        override def subVisitor: Visitor[?, ?] = vismap(currentKey)
        override def visitValue(u: OutputUnit, index: Int): Unit =
        { accumulate(units, u); if (u.vvalid) annot.addOne(Str(currentKey))}
        override def visitEnd(index: Int): OutputUnit = compose(Properties, units, Arr.from(annot))
      })

      private var matchedPatternSchs: Seq[(String, Visitor[?, OutputUnit])] = Nil // to be assigned based on key visited
      // returns subVisitor based on assigned matchedPatternSchs
      val patternPropsVisitor: Option[ObjVisitor[Seq[OutputUnit], OutputUnit]] = patternPropsViss.map(_ => new ObjVisitor[Seq[OutputUnit], OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private val annot: mutable.Buffer[Value] = new ArrayBuffer()

        override def visitKey(index: Int): Visitor[?, ?] = throw new IllegalStateException
        override def visitKeyValue(v: Any): Unit = throw new IllegalStateException
        override def subVisitor: Visitor[?, ?] = new CompositeVisitor(matchedPatternSchs.map((_, v) => v)*)
        override def visitValue(us: Seq[OutputUnit], index: Int): Unit = us.foreach(u => { accumulate(units, u); if (u.vvalid) annot.addOne(Str(currentKey))})
        override def visitEnd(index: Int): OutputUnit = compose(PatternProperties, units, Arr.from(annot))
      })

      val addlPropsObjVis: Option[ObjVisitor[OutputUnit, OutputUnit]] = addlPropsVis.map(schValidator => new ObjVisitor[OutputUnit, OutputUnit] {
        private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
        private val annot: mutable.Buffer[Value] = new ArrayBuffer()

        override def visitKey(index: Int): Visitor[?, ?] = throw new IllegalStateException
        override def visitKeyValue(v: Any): Unit = throw new IllegalStateException
        override def subVisitor: Visitor[?, ?] = schValidator
        override def visitValue(u: OutputUnit, index: Int): Unit = { accumulate(units, u); if (u.vvalid) annot.addOne(Str(currentKey)) }
        override def visitEnd(index: Int): OutputUnit = compose(AdditionalProperties, units, Arr.from(annot))
      })

      override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "Expected string"
        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          if (propNymVis.nonEmpty) propNamesValid = propNamesValid && propNymVis.get.visitString(s, index1).vvalid
          propsVisited.addOne(currentKey)
          matchedPatternSchs = patternPropsViss.map(vismap => vismap
              .withFilter((rgx, _) => rgx.matches(currentKey))
              .map((rgx, v) => (rgx.toString(), v))
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
        if (propsViss.nonEmpty && propsViss.get.contains(currentKey)) { isAddl = false; childVisitors.addOne(propsVisitor.get) }
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
          else accumulate(units, u)
        })

        propsVisitor.foreach(v => accumulate(units, v.visitEnd(index)))
        patternPropsVisitor.foreach(v => accumulate(units, v.visitEnd(index)))
        addlPropsObjVis.foreach(v => accumulate(units, v.visitEnd(index)))
        accumulate(units, mkUnit(propNamesValid, PropertyNames, ""))

        ifVis.foreach(_ => {
          val u = iff; accumulate(units, OutputUnit.info(u))
          if (u.vvalid) {
            if (elseVis.nonEmpty) ctx.notifyInvalid(Seq(els))
            thenVis.foreach(_ => accumulate(units, thenn)) // warning these could fail if for some reason iff/thenn/els are not set
          } else {
            if (thenVis.nonEmpty) ctx.notifyInvalid(Seq(thenn))
            elseVis.foreach(_ => accumulate(units, els))
          }
        })

        units
      }
    }
  }

  /* helper methods */
  private def allOf(kw: String, units: collection.Seq[OutputUnit]): OutputUnit = {
    val (valid, invalid) = units.partition(_.vvalid)
    mkUnit(invalid.isEmpty, kw, errors = invalid, verbose = valid)
  }

  private def oneOf(kw: String, units: collection.Seq[OutputUnit]): OutputUnit = {
    val (valid, invalid) = units.partition(_.vvalid)
    if (valid.size == 1) mkUnit(true, kw, verbose = units) // if one valid, no errors and all results are verbose
    else if (valid.size > 1) mkUnit(false, kw, errors = valid, verbose = invalid) // if 1+ valid, errors are all valid, and errors are verbose
    else mkUnit(false, kw, errors = units) // if none valid, all results are errors
  }

  private def anyOf(kw: String, units: collection.Seq[OutputUnit]): OutputUnit = {
    val (valid, invalid) = units.partition(_.vvalid)
    if (valid.nonEmpty) mkUnit(true, kw, verbose = units) // if some succeeded, no errors and all results are verbose
    else mkUnit(false, kw, errors = units) // if none succeeded, all results are errors
  }

  private def not(n: OutputUnit): OutputUnit = {
    if (n.vvalid) mkUnit(false, Not, verbose = Seq(n)) // if valid, no errors and result is verbose
    else mkUnit(true, Not, errors = Seq(n)) // if invalid, error is result and no verbose
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

  val Keys: Seq[String] = Seq(PrefixItems, Items, MaxContains, MinContains, Contains, AdditionalProperties,
    PatternProperties, DependentSchemas, PropertyNames, Properties, If, Then, Else, Not, AllOf, AnyOf, OneOf)

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/applicator"

  override def create(schema: ObjectSchema,
                      ctx: Context,
                      path: JsonPointer,
                      dynParent: Option[Vocab[?]]): Applicator = new Applicator(schema, ctx, path, dynParent)

  override def shouldApply(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
}
