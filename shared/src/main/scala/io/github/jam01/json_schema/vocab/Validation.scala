package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Validation.asBigDec
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import java.util.Objects
import scala.collection.mutable
import scala.util.matching.Regex

class Validation(schema: ObjectSchema,
                 ctx: Context = Context.Empty,
                 path: JsonPointer = JsonPointer(),
                 dynParent: Option[VocabValidator] = None) extends VocabValidator(schema, ctx, path, dynParent) {

  private val tyype: collection.Seq[String] = schema.getAsStringArray("type")
  private val const = schema.value.get("const")
  private val enuum: Option[collection.Seq[Value]] = schema.getArrayOpt("enum")
  private val multipleOf: Option[Long | Double] = schema.getNumber("multipleOf")
  private val maximum: Option[Long | Double] = schema.getNumber("maximum")
  private val minimum: Option[Long | Double] = schema.getNumber("minimum")
  private val exclusiveMax: Option[Long | Double] = schema.getNumber("exclusiveMaximum")
  private val exclusiveMin: Option[Long | Double] = schema.getNumber("exclusiveMinimum")
  private val maxLength: Option[Int] = schema.getInt("maxLength")
  private val minLength: Option[Int] = schema.getInt("minLength")
  private val maxItems: Option[Int] = schema.getInt("maxItems")
  private val minItems: Option[Int] = schema.getInt("minItems")
  private val maxContains: Option[Int] = schema.getInt("maxContains")
  private val minContains: Option[Int] = schema.getInt("minContains")
  private val maxProperties: Option[Int] = schema.getInt("maxProperties")
  private val minProperties: Option[Int] = schema.getInt("minProperties")
  private val pattern: Option[Regex] = schema.getString("pattern").map(s => new Regex(s).unanchored)
  private val uniqueItems: Option[Boolean] = schema.getBoolean("uniqueItems")
  private val required: collection.Seq[String] = schema.getStringArray("required")
  private val depReq: Option[collection.Map[String, Any]] = schema.getObjectOpt("dependentRequired")

  override def visitNull(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("null")) &&
      const.forall(c => c == Null) &&
      (enuum.isEmpty || enuum.get.contains(Null))
  }

  override def visitFalse(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      const.forall(c => Objects.equals(c, False)) &&
      (enuum.isEmpty || enuum.get.contains(False))
  }

  override def visitTrue(index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("boolean")) &&
      const.forall(c => Objects.equals(c, True)) &&
      (enuum.isEmpty || enuum.get.contains(True))
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
      const.forall(c => Objects.equals(c.value, l)) &&
      (enuum.isEmpty || enuum.get.contains(Num(l)))
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
      const.forall(c => Objects.equals(c.value, d)) &&
      (enuum.isEmpty || enuum.get.contains(Num(d)))
  }

  override def visitString(s: CharSequence, index: Int): Boolean = {
    (tyype.isEmpty || tyype.contains("string")) &&
      pattern.forall(_.matches(s)) && // TODO: memoize?
      minLength.forall(s.toString.codePointCount(0, s.length()) >= _) &&
      maxLength.forall(s.toString.codePointCount(0, s.length()) <= _) &&
      const.forall(c => Objects.equals(c.value, s)) &&
      (enuum.isEmpty || enuum.get.exists(el => Objects.equals(el.value, s)))
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[_, Boolean] = {
    val insVisitors = mutable.ArrayBuffer[ArrVisitor[_, Boolean]]()
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

    val insVisitor: ArrVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeArrVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)

    new ArrVisitor[Any, Boolean] {
      private var nextIdx = 0

      override def subVisitor: Visitor[_, _] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = {
        insVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("array")) && // TODO: up front to fail-fast
          minItems.forall(nextIdx >= _) &&
          maxItems.forall(nextIdx <= _) &&
          insVisitor.visitEnd(index)
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] = {
    val propsVisited = mutable.ArrayBuffer[String]()

    val insVisitors = mutable.ArrayBuffer[ObjVisitor[_, Boolean]]()
    if (const.nonEmpty || enuum.nonEmpty)
      insVisitors.addOne(new MapObjContext(LiteralVisitor.visitObject(length, index), obj => {
        var res = true
        const.foreach(c => res = res && Objects.equals(const.get, obj))
        enuum.foreach(en => res = res && en.exists(el => Objects.equals(el, obj)))
        res
      }))

    val insVisitor: ObjVisitor[_, Boolean] =
      if (insVisitors.length == 1) insVisitors.head
      else new CompositeObjVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)

    new ObjVisitor[Any, Boolean] {
      private var currentKey: String = "?"

      override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          propsVisited.addOne(currentKey)
          insVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[_, _] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = insVisitor.narrow.visitValue(v, index)

      override def visitEnd(index: Int): Boolean = {
        (tyype.isEmpty || tyype.contains("object")) && // TODO: up front to fail-fast
          required.forall(propsVisited.contains(_)) &&
          maxProperties.forall(propsVisited.size <= _) &&
          minProperties.forall(propsVisited.size >= _) &&
          insVisitor.visitEnd(index) &&
          depReq.forall(map => map.filter((k, reqs) => propsVisited.contains(k))
          .map((k, reqs) => reqs.asInstanceOf[Arr].value.forall(rreq => propsVisited.contains(rreq.str)))
          .forall(identity))
      }
    }
  }
}

object Validation {
  private def asBigDec(num: Long | Double) = num match
    case l: Long => BigDecimal.valueOf(l)
    case d: Double => BigDecimal.valueOf(d)
}
