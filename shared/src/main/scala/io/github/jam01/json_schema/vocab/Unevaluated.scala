package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Unevaluated.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{SeqFactory, mutable}

final class Unevaluated private(schema: ObjectSchema,
                          ctx: Context,
                          path: JsonPointer,
                          dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {

  private val itemsVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(UnevaluatedItems)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(UnevaluatedItems), Some(Unevaluated.this)))
  private val propsVis: Option[Visitor[?, OutputUnit]] = schema.getSchemaOpt(UnevaluatedProperties)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(UnevaluatedProperties), Some(Unevaluated.this)))

  if (itemsVis.nonEmpty) 
    ctx.registerDependant(path, path.appended(UnevaluatedItems), kwLoc => check(kwLoc, ItemsAnnotations))
  if (propsVis.nonEmpty) 
    ctx.registerDependant(path, path.appended(UnevaluatedProperties), kwLoc => check(kwLoc, PropertiesAnnotations))

  private def check(other: JsonPointer, anns: Seq[String]): Boolean = {
    anns.contains(other.refTokens.last) &&
      other.refTokens.drop(path.refTokens.size).count(anns.contains) == 1 // perf: use an iterator to do drop + count
  }

  override def visitNull(index: Int): Seq[OutputUnit] = Nil
  override def visitFalse(index: Int): Seq[OutputUnit] = Nil
  override def visitTrue(index: Int): Seq[OutputUnit] = Nil
  override def visitInt64(i: Long, index: Int): Seq[OutputUnit] = Nil
  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = Nil
  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = Nil
  override def visitArray(length: Int, index: Int): ArrVisitor[Nothing, Seq[OutputUnit]] = {
    if (itemsVis.isEmpty) return NilArrayVis
    new ArrVisitor[OutputUnit, Seq[OutputUnit]] {
      private val buff = new ListBuffer[OutputUnit]

      override def subVisitor: Visitor[?, ?] = itemsVis.get
      override def visitValue(unit: OutputUnit, index: Int): Unit = buff.addOne(unit)
      override def visitEnd(index: Int): Seq[OutputUnit] = {
        val kwLoc = path.appended(UnevaluatedItems)
        val evalItems: Seq[Value] = getItemsAnnotations(ctx.getDependenciesFor(kwLoc), Applicator.Items)
        val evalPrefixItems: Seq[Value] = getItemsAnnotations(ctx.getDependenciesFor(kwLoc), Applicator.PrefixItems)
        val evalContains: Seq[Value] = getItemsAnnotations(ctx.getDependenciesFor(kwLoc), Applicator.Contains)
        val evalUneval: Seq[Value] = getItemsAnnotations(ctx.getDependenciesFor(kwLoc), UnevaluatedItems)

        val (applied, invalid) = buff.result().partition(unit => ! {
          evalItems.contains(True) || evalUneval.contains(True)
            || evalPrefixItems.exists(n => gteq(n.num, unit.insLoc.refTokens.last.toLong))
            || evalContains.exists(is => is.arr.contains(Num(unit.insLoc.refTokens.last.toInt)))
        })

        ctx.notifyInvalid(invalid)
        Seq(compose(UnevaluatedItems, applied, True))
      }
    }
  }

  private def getItemsAnnotations(annotations: Seq[(JsonPointer, Value)], annotName: String): Seq[Value] =
    annotations.withFilter((kwLoc, _) => annotName == kwLoc.refTokens.last)
      .map((_, value) => value)
  private def getPropsAnnotations(annotations: Seq[(JsonPointer, Value)]): Seq[String] =
    annotations.flatMap((_, value) => value.arr.map(v => v.str))

  override def visitObject(length: Int, index: Int): ObjVisitor[Nothing, Seq[OutputUnit]] = {
    if (propsVis.isEmpty) return NilObjVis
    new ObjVisitor[OutputUnit, Seq[OutputUnit]] {
      private val buff = new ListBuffer[OutputUnit]
      private val annot = new ListBuffer[Value]
      private var currentKey: String = "?"

      override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "Expected string"
        override def visitString(s: CharSequence, index1: Int): Unit = currentKey = s.toString
      }

      override def visitKeyValue(v: Any): Unit = ()
      override def subVisitor: Visitor[?, ?] = propsVis.get
      override def visitValue(unit: OutputUnit, index: Int): Unit = {
        buff.addOne(unit)
        if (unit.vvalid) annot.addOne(Str(currentKey))
      }

      override def visitEnd(index: Int): Seq[OutputUnit] = {
        val evaluated = getPropsAnnotations(ctx.getDependenciesFor(path.appended(UnevaluatedProperties)))
        val (applied, invalid) = buff.result().partition(unit => !evaluated.contains(unit.insLoc.refTokens.last))
        ctx.notifyInvalid(invalid)
        Seq(compose(UnevaluatedProperties, applied, Arr(annot.result())))
      }
    }
  }
}

object Unevaluated extends VocabFactory[Unevaluated] {
  val UnevaluatedItems = "unevaluatedItems"
  val UnevaluatedProperties = "unevaluatedProperties"

  private val NilArrayVis = new ArrVisitor[Any, Seq[OutputUnit]] {
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }

  private val NilObjVis = new ObjVisitor[Any, Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }
  
  private def gteq(n1: Long | Double, n2: Long | Double): Boolean = {
    n1 match
      case l1: Long => n2 match
        case l2: Long => l1 >= l2
        case d2: Double => l1 >= d2
      case d1: Double => n2 match
        case l2: Long => d1 >= l2
        case d2: Double => d1 >= d2
  }

  private val PropertiesAnnotations = Seq(Applicator.Properties,
    Applicator.PatternProperties,
    Applicator.AdditionalProperties,
    Applicator.AdditionalProperties,
    UnevaluatedProperties)
  private val ItemsAnnotations = Seq(Applicator.Items,
    Applicator.PrefixItems,
    Applicator.Contains,
    UnevaluatedItems)
  
  val Keys: Set[String] = Set(UnevaluatedItems, UnevaluatedProperties)

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/unevaluated"
  override def shouldApply(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): Unevaluated = 
    new Unevaluated(schema, ctx, path, dynParent)
}
