package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema._
import io.github.jam01.json_schema.vocab.Applicator.if_then_else
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, OffsetDateTime}
import java.util.{Objects, UUID}
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

class Applicator(schema: ObjectSchema, 
                 ctx: Context = Context.empty, 
                 schloc: JsonPointer = JsonPointer(), 
                 dynParent: Option[VocabValidator] = None) extends VocabValidator(schema, ctx, schloc, dynParent) {

  private val prefixItems: Option[collection.Seq[Schema]] = schema.getSchemaArrayOpt("prefixItems")
  private val itemsVis: Option[ArrVisitor[_, Boolean]] = schema.getSchemaOpt("items")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("items"), Some(this)))
    .map(schValidator => new ArrVisitor[Boolean, Boolean] {
      private var subsch = true

      override def subVisitor: Visitor[_, _] = schValidator
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })

  private val maxContains: Option[Int] = schema.getInt("maxContains")
  private val minContains: Option[Int] = schema.getInt("minContains")
  private val contains: Option[ArrVisitor[_, Boolean]] = schema.getSchemaOpt("contains")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("contains"), Some(this)))
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
  
  private val addlPropsVis: Option[ObjVisitor[_, Boolean]] = schema.getSchemaOpt("additionalProperties")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("additionalProperties"), Some(this)))
    .map(schValidator => new ObjVisitor[Boolean, Boolean] {
      private var subsch = true

      override def visitKey(index: Int): Visitor[_, _] = ???
      override def visitKeyValue(v: Any): Unit = ???
      override def subVisitor: Visitor[_, _] = schValidator
      override def visitValue(v: Boolean, index: Int): Unit = subsch = subsch && v
      override def visitEnd(index: Int): Boolean = subsch
    })
  private val properties: Option[collection.Map[String, Schema]] = schema.getSchemaObjectOpt("properties")
  private val patternProperties: Option[collection.Map[Regex, Schema]] = schema.getSchemaObjectOpt("patternProperties")
    .map(obj => obj.map(entry => (new Regex(entry._1).unanchored, entry._2)))
  private val depSchsViss: Option[collection.Map[String, JsonVisitor[_, Boolean]]] = schema.getSchemaObjectOpt("dependentSchemas")
    .map(obj => obj.map(entry => (entry._1, SchemaValidator.of(entry._2, ctx, schloc.appended("dependentSchemas", entry._1), Some(this)))))
  private val propNymVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("propertyNames")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("propertyNames"), Some(this)))
  private val ifVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("if")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("if"), Some(this)))
  private val thenVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("then")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("then"), Some(this)))
  private val elseVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("else")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("else"), Some(this)))
  private val allOfVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaArrayOpt("allOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, schloc.appended("allOf", schidx._2.toString), Some(this))))
    .map(schViss => new CompositeVisitorReducer(_.forall(identity), schViss.toSeq: _*))
  private val oneOfVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaArrayOpt("oneOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, schloc.appended("oneOf", schidx._2.toString), Some(this))))
    .map(schViss => new CompositeVisitorReducer(_.count(identity) == 1, schViss.toSeq: _*))
  private val anyOfVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaArrayOpt("anyOf")
    .map(schs => schs.view.zipWithIndex.map(schidx => SchemaValidator.of(schidx._1, ctx, schloc.appended("anyOf", schidx._2.toString), Some(this))))
    .map(schViss => new CompositeVisitorReducer(_.exists(identity), schViss.toSeq: _*))
  private val notVis: Option[JsonVisitor[_, Boolean]] = schema.getSchemaOpt("not")
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("not"), Some(this)))

  override def visitNull(index: Int): Boolean = {
    notVis.forall(!_.visitNull(index)) &&
      allOfVis.forall(_.visitNull(index)) &&
      anyOfVis.forall(_.visitNull(index)) &&
      oneOfVis.forall(_.visitNull(index)) &&
      ifVis.forall(vis => if_then_else(vis.visitNull(index),
        thenVis.map(_.visitNull(index)), elseVis.map(_.visitNull(index))))
  }

  override def visitFalse(index: Int): Boolean = {
    notVis.forall(!_.visitFalse(index)) &&
      allOfVis.forall(_.visitFalse(index)) &&
      anyOfVis.forall(_.visitFalse(index)) &&
      oneOfVis.forall(_.visitFalse(index)) &&
      ifVis.forall(vis => if_then_else(vis.visitFalse(index),
        thenVis.map(_.visitFalse(index)), elseVis.map(_.visitFalse(index))))
  }

  override def visitTrue(index: Int): Boolean = {
    notVis.forall(!_.visitTrue(index)) &&
      allOfVis.forall(_.visitFalse(index)) &&
      anyOfVis.forall(_.visitFalse(index)) &&
      oneOfVis.forall(_.visitFalse(index)) &&
      ifVis.forall(vis => if_then_else(vis.visitTrue(index),
        thenVis.map(_.visitTrue(index)), elseVis.map(_.visitTrue(index))))
  }

  override def visitInt64(l: Long, index: Int): Boolean = {
    notVis.forall(!_.visitInt64(l, index)) &&
      allOfVis.forall(_.visitInt64(l, index)) &&
      anyOfVis.forall(_.visitInt64(l, index)) &&
      oneOfVis.forall(_.visitInt64(l, index)) &&
      ifVis.forall(vis => if_then_else(vis.visitInt64(l, index),
        thenVis.map(_.visitInt64(l, index)), elseVis.map(_.visitInt64(l, index))))
  }

  override def visitFloat64(d: Double, index: Int): Boolean = {
    notVis.forall(!_.visitFloat64(d, index)) &&
      allOfVis.forall(_.visitFloat64(d, index)) &&
      anyOfVis.forall(_.visitFloat64(d, index)) &&
      oneOfVis.forall(_.visitFloat64(d, index)) &&
      ifVis.forall(vis => if_then_else(vis.visitFloat64(d, index),
        thenVis.map(_.visitFloat64(d, index)), elseVis.map(_.visitFloat64(d, index))))
  }

  override def visitString(s: CharSequence, index: Int): Boolean = {
    notVis.forall(!_.visitString(s, index)) &&
      allOfVis.forall(_.visitString(s, index)) &&
      anyOfVis.forall(_.visitString(s, index)) &&
      oneOfVis.forall(_.visitString(s, index)) &&
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
    
    val insVisitor: ArrVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeArrVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)
    var childVisitor: ArrVisitor[_, _] = null // to be assigned based on child

    new ArrVisitor[Any, Boolean] {
      private var nextIdx = 0

      // returns subVisitor based on child index
      val prefixItemsVisitor: Option[ArrVisitor[_, Boolean]] = prefixItems.map(arr => new ArrVisitor[Boolean, Boolean] {
        private var subsch = true

        override def subVisitor: Visitor[_, _] = SchemaValidator.of(arr(nextIdx), ctx, schloc.appended("prefixItems", nextIdx.toString), Some(Applicator.this))
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
        prefixItemsVisitor.forall(_.visitEnd(index)) &&
          itemsVis.forall(_.visitEnd(index)) &&
          insVisitor.visitEnd(index)
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] = {
    val propsVisited = mutable.ArrayBuffer.empty[String] // properties visited

    val insVisitors = mutable.ArrayBuffer.empty[ObjVisitor[_, Boolean]]
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

    val depSchsObjViss: Option[collection.Map[String, ObjVisitor[_, (String, Boolean)]]] =
      depSchsViss.map(viss => viss.map(entry => (entry._1, MapObjContext(entry._2.visitObject(length, index), b => (entry._1, b)))))
    depSchsObjViss.foreach(viss =>
      insVisitors.addOne(MapObjContext(new CompositeObjVisitor(viss.values.toSeq: _*), tups => {
        tups.filter((k, b) => propsVisited.contains(k))
          .forall((k, b) => b)
      }))
    )

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
        override def subVisitor: Visitor[_, _] = SchemaValidator.of(m(currentKey), ctx, schloc.appended("properties", currentKey), Some(Applicator.this))
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
          SchemaValidator.of(pattSch._2, ctx, schloc.appended("patternProperties", pattSch._1), Some(Applicator.this))): _*)
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
        propsVisitor.forall(_.visitEnd(index)) &&
          patternPropsVisitor.forall(_.visitEnd(index)) &&
          addlPropsVis.forall(_.visitEnd(index)) &&
          insVisitor.visitEnd(index) &&
          propNamesValid
      }
    }
  }
}

object Applicator {
  private def if_then_else(iff: Boolean, thenn: Option[Boolean], els: Option[Boolean]): Boolean = {
    if (iff && thenn.nonEmpty) thenn.get
    else if (!iff && els.nonEmpty) els.get
    else true
  }
}
