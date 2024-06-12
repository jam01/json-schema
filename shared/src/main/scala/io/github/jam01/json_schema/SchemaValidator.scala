package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

object SchemaValidator {
  def of(sch: Schema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): Visitor[?, OutputUnit] = {
    sch match
      case BooleanSchema(bool) => new BooleanSchemaValidator(bool, ctx, path)
      case osch: ObjectSchema =>
        guardDepth(dynParent)

        val vocabs: Seq[Vocab[?]] = ctx.config.dialect.vocabularies
          .filter(vocabfact => vocabfact.shouldApply(osch))
          .map(vocabfact => vocabfact.create(osch, ctx, path, dynParent))

        if (ctx.config.ffast) new FFastObjectSchemaValidator[Nothing](vocabs, ctx, path, dynParent)
        else new MapCompositeVisitor[Nothing, Seq[OutputUnit], OutputUnit](vocabs,
          unitss => ctx.onScopeEnd(path, ctx.config.format.compose(path, unitss.flatten, ctx.instanceLoc)))
  }
}

private def guardDepth(dynParent: Option[Vocab[?]]): Unit = {
  if (dynParent.isEmpty) return  
  var countRefs = 0
  var head: Vocab[?] = dynParent.get
  while (head.dynParent.nonEmpty) {
    countRefs += 1
    head = head.dynParent.get
  }

  // a naive way to guard against infinite loops from circular reference logic in schemas, which results in StackOverflow
  if (countRefs > 32) throw new IllegalStateException("Depth limit exceeded")
}

final class BooleanSchemaValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends JsonVisitor[OutputUnit, OutputUnit] {
  override def visitNull(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitFalse(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitTrue(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitFloat64(d: Double, index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitInt64(i: Long, index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitString(s: CharSequence, index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitArray(length: Int, index: Int): ArrVisitor[OutputUnit, OutputUnit] = new BooleanArrValidator(bool, ctx, path)
  override def visitObject(length: Int, index: Int): ObjVisitor[OutputUnit, OutputUnit] = new BooleanObjValidator(bool, ctx, path)
}

final class BooleanArrValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ArrVisitor[Any, OutputUnit] {
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
}

final class BooleanObjValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ObjVisitor[Any, OutputUnit] {
  override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
  override def visitKeyValue(v: Any): Unit = ()
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
}

private class FFastObjectSchemaValidator[T](vocabs: Seq[Vocab[T]], ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]) extends JsonVisitor[Seq[T], OutputUnit] {
  inline private def compose(units: Seq[OutputUnit]): OutputUnit = {
    val result = ctx.onScopeEnd(path, ctx.config.format.compose(path, units, ctx.instanceLoc))
    if (dynParent.isEmpty && !result.vvalid) throw new ValidationException(result)
    result
  }
  inline private def compose(f: Vocab[T] => Seq[OutputUnit]): OutputUnit = {
    var res0: Seq[OutputUnit] = Nil
    val it = vocabs.iterator
    var continue = true
    while (it.hasNext && continue) {
      val res1 = f(it.next())
      ctx.onVocabResults(path, res1)
      res0 = res0 :++ res1
      if (res1.exists(u => !u.vvalid)) continue = false
    }

    compose(res0)
  }
  inline private def ffast(exc: InvalidVectorException): Seq[OutputUnit] = {
    if (dynParent.isEmpty) throw new ValidationException(compose(exc.results))
    exc.results
  }

  override def visitNull(index: Int): OutputUnit = compose(v => v.visitNull(index))
  override def visitFalse(index: Int): OutputUnit = compose(v => v.visitFalse(index))
  override def visitTrue(index: Int): OutputUnit = compose(v => v.visitTrue(index))
  override def visitFloat64(d: Double, index: Int): OutputUnit = compose(v => v.visitFloat64(d, index))
  override def visitInt64(i: Long, index: Int): OutputUnit = compose(v => v.visitInt64(i, index))
  override def visitString(s: CharSequence, index: Int): OutputUnit = compose(v => v.visitString(s, index))

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], OutputUnit] = {
    var failed: Seq[OutputUnit] = Nil
    val delegates: Seq[ArrVisitor[T, Seq[OutputUnit]]] =
      try { vocabs.map(_.visitArray(length, index)) } catch
        case e: InvalidVectorException => failed = ffast(e); Nil

    new MapCompositeArrContext[T, Seq[OutputUnit], OutputUnit](delegates, units => compose(units.flatten)) {
      override def visitValue(v: Seq[T], index: Int): Unit =
        if (failed.nonEmpty) return
        try { super.visitValue(v, index) } catch
          case e: InvalidVectorException => failed = ffast(e)

      override def visitEnd(index: Int): OutputUnit =
        if (failed.nonEmpty) return compose(failed)
        super.visitEnd(index)
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[T], OutputUnit] = {
    var failed: Seq[OutputUnit] = Nil
    val delegates: Seq[ObjVisitor[T, Seq[OutputUnit]]] =
      try { vocabs.map(_.visitObject(length, index)) } catch
        case e: InvalidVectorException => failed = ffast(e); Nil

    new MapCompositeObjContext[T, Seq[OutputUnit], OutputUnit](delegates, units => compose(units.flatten)) {
      override def visitKey(index: Int): Visitor[?, ?] =
        if (failed.nonEmpty) return NoOpVisitor
        try { super.visitKey(index) } catch
          case e: InvalidVectorException => failed = ffast(e); NoOpVisitor

      override def visitKeyValue(v: Any): Unit =
        if (failed.nonEmpty) return
        try { super.visitKeyValue(v) } catch
          case e: InvalidVectorException => failed = ffast(e)

      override def visitValue(v: Seq[T], index: Int): Unit =
        if (failed.nonEmpty) return
        try { super.visitValue(v, index) } catch
          case e: InvalidVectorException => failed = ffast(e)

      override def visitEnd(index: Int): OutputUnit =
        if (failed.nonEmpty) return compose(failed)
        super.visitEnd(index)
    }
  }
}
