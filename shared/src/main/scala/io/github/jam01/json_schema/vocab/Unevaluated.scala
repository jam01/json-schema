package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Unevaluated.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Unevaluated(schema: ObjectSchema,
                  ctx: Context = Context.Empty,
                  path: JsonPointer = JsonPointer(),
                  dynParent: Option[BaseValidator] = None) extends BaseValidator(schema, ctx, path, dynParent) {
  private val itemsVis: Option[ArrVisitor[OutputUnit, OutputUnit]] = schema.getSchemaOpt(UnevaluatedItems)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(UnevaluatedItems), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer

      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = addUnit(units, u)
      override def visitEnd(index: Int): OutputUnit = ???
    })
  private val propsVis: Option[ObjVisitor[OutputUnit, collection.Seq[OutputUnit]]] = schema.getSchemaOpt(UnevaluatedProperties)
    .map(sch => new ObjVisitor[OutputUnit, collection.Seq[OutputUnit]] {
      private var currentKey: String = "?"
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
      private val annot: mutable.Buffer[Value] = new ArrayBuffer
      private val propsVisited: mutable.ArrayBuffer[String] = new ArrayBuffer

      override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "Expected string"
        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          propsVisited.addOne(currentKey)
        }
      }
      override def visitKeyValue(v: Any): Unit = ()
      override def subVisitor: Visitor[?, ?] = SchemaValidator.of(sch, ctx, path.appended(UnevaluatedProperties, currentKey), Some(Unevaluated.this))
      override def visitValue(u: OutputUnit, index: Int): Unit = { addUnit(units, u); if (u.vvalid) annot.addOne(Str(currentKey))}
      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val evaluated = getAnnotations(ctx.annots.filter(ann => path.isRelative(ann.kwLoc)))
        Seq(and(UnevaluatedProperties, units.filterNot(u => evaluated.contains(u.kwLoc.refTokens.last)), Some(Arr.from(annot))))
      }
    })

  private def getAnnotations(units: collection.Seq[OutputUnit]): collection.Seq[String] = { // warning: this depends on units being on the right "branch"
    units.withFilter(ann => PropertiesAnnotations.contains(ann.kwLoc.refTokens.last) && ann.annotation.nonEmpty)
      .flatMap(ann => ann.annotation.get.arr.map(v => v.str))
      .appendedAll(units.withFilter(ann => SecondAppl.contains(ann.kwLoc.refTokens.last))
          .flatMap(ann => ann.annotations.flatMap(ann0 => getAnnotations(ann0.annotations))))
      .appendedAll(units.withFilter(ann => DirectAppl.contains(ann.kwLoc.refTokens.last))
          .flatMap(ann => getAnnotations(ann.annotations)))
  }

  override def visitNull(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitFalse(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitTrue(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitInt64(i: Long, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitArray(length: Int, index: Int): ArrVisitor[Any, collection.Seq[OutputUnit]] = new ArrVisitor[Any, collection.Seq[OutputUnit]] {
    val childVisitor: ArrVisitor[?, ?] = itemsVis.getOrElse(NoOpVisitor.visitArray(length, index))
    override def subVisitor: Visitor[?, ?] = childVisitor.subVisitor
    override def visitValue(v: Any, index: Int): Unit = childVisitor.narrow.visitValue(v, index)
    override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
      Nil
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Nothing, collection.Seq[OutputUnit]] =
    propsVis.getOrElse(new ObjVisitor[Any, collection.Seq[OutputUnit]] { // TODO: make object
      override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
      override def visitKeyValue(v: Any): Unit = ()
      override def subVisitor: Visitor[?, ?] = NoOpVisitor
      override def visitValue(v: Any, index: Int): Unit = ()
      override def visitEnd(index: Int): collection.Seq[OutputUnit] = Nil
    })

  /* helper methods */
  private def and(kw: String, units: collection.Seq[OutputUnit], ann: Option[Value] = None): OutputUnit = {
    val (annots, errs) = units.partition(_.vvalid)
    unitOf(errs.isEmpty, kw, None, errs, ann, annots)
  }
}

object Unevaluated {
  val UnevaluatedItems = "unevaluatedItems"
  val UnevaluatedProperties = "unevaluatedProperties"

  private val SecondAppl = Seq(Applicator.AllOf,
    Applicator.AnyOf,
    Applicator.OneOf,
    Applicator.Not,
    Applicator.DependentSchemas)

  private val DirectAppl = Seq(
    Core._Ref, 
    Core._DynRef,
    Applicator.If,
    Applicator.Then,
    Applicator.Else
  )

  private val PropertiesAnnotations = Seq(Applicator.Properties,
    Applicator.PatternProperties,
    Applicator.AdditionalProperties,
    Applicator.AdditionalProperties,
    UnevaluatedProperties)
}
