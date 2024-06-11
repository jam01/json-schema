package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Validation.*
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.matching.Regex

final class Validation private(schema: ObjectSchema,
                         ctx: Context,
                         path: JsonPointer,
                         dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {
  private val tyype: Seq[String] = schema.getAsStringArray(Tyype)
  private val const: Option[Value] = schema.get(Const)
  private val enuum: Option[Seq[Value]] = schema.getArrayOpt(Enuum)
  private val maximum: Option[Long | Double] = schema.getNumber(Maximum)
  private val minimum: Option[Long | Double] = schema.getNumber(Minimum)
  private val exclusiveMax: Option[Long | Double] = schema.getNumber(ExclusiveMax)
  private val exclusiveMin: Option[Long | Double] = schema.getNumber(ExclusiveMin)
  private val multipleOf: Option[Long | Double] = schema.getNumber(MultipleOf)
  private val maxLength: Option[Int] = schema.getInt(MaxLength)
  private val minLength: Option[Int] = schema.getInt(MinLength)
  private val pattern: Option[Regex] = schema.getString(Pattern).map(s => new Regex(s).unanchored)
  private val maxItems: Option[Int] = schema.getInt(MaxItems)
  private val minItems: Option[Int] = schema.getInt(MinItems)
  private val uniqueItems: Option[Boolean] = schema.getBoolean(UniqueItems)
  private val depReq: Option[collection.Map[String, Value]] = schema.getObjectOpt(DepRequired)
  private val maxContains: Option[Int] = schema.getInt(MaxContains)
  private val maxProperties: Option[Int] = schema.getInt(MaxProperties)
  private val minContains: Option[Int] = schema.getInt(MinContains)
  private val minProperties: Option[Int] = schema.getInt(MinProperties)
  private val required: Seq[String] = schema.getStringArray(Required)

  override def visitNull(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, mkUnit(tyype.contains("null"), Tyype, s"Expected $tyype, got null"))) &&
      accumulateOpt(buff, const.map(c => mkUnit(c == Null, Const, "null does not match expected constant"))) &&
      accumulateOpt(buff, enuum.map(e => mkUnit(e.contains(Null), Enuum, "null not found in enumeration")))
    buff.result()
  }

  override def visitFalse(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, mkUnit(tyype.contains("boolean"), Tyype, s"Expected $tyype, got false"))) &&
      accumulateOpt(buff, const.map(c => mkUnit(c == False, Const, "false does not match expected constant"))) &&
      accumulateOpt(buff, enuum.map(e => mkUnit(e.contains(False), Enuum, "false not found in enumeration")))
    buff.result()
  }

  override def visitTrue(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, mkUnit(tyype.contains("boolean"), Tyype, s"Expected $tyype, got true"))) &&
      accumulateOpt(buff, const.map(c => mkUnit(c == True, Const, "true does not match expected constant"))) &&
      accumulateOpt(buff, enuum.map(e => mkUnit(e.contains(True), Enuum, "true not found in enumeration")))
    buff.result()
  }

  override def visitInt64(num: Long, index: Int): Seq[OutputUnit] = {
    def isMultiple(mult: Long | Double) = {
      mult match
        case ml: Long => num % ml == 0
        case md: Double => if (md.isWhole) num % md.longValue == 0 else false
    }

    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, mkUnit(tyype.exists(t => "integer" == t || "number" == t), Tyype, s"Expected $tyype, got number"))) &&
      accumulateOpt(buff, multipleOf.map(mult => mkUnit(isMultiple(mult), MultipleOf, "Number is not a multiple"))) &&
      visitNumber(num, buff)
    buff.result()
  }

  override def visitFloat64(num: Double, index: Int): Seq[OutputUnit] = {
    def isMultiple(mult: Long | Double) = try {
      BigDecimal.valueOf(num)
        .remainder(asBigDec(mult))
        .compareTo(java.math.BigDecimal.ZERO) == 0
    } catch case ex: ArithmeticException => false

    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, mkUnit(tyype.exists(t => "number" == t || "integer" == t && num.isWhole), Tyype, s"Expected $tyype, got number"))) &&
      accumulateOpt(buff, multipleOf.map(mult => mkUnit(isMultiple(mult), MultipleOf, "Number is not a multiple"))) &&
      visitNumber(num, buff)
    buff.result()
  }

  private def visitNumber(num: Long | Double, buff: mutable.Growable[OutputUnit]): Boolean = {
    accumulateOpt(buff, const.map(c => mkUnit(c.value == num, Const, "Number does not match expected constant"))) &&
      accumulateOpt(buff, enuum.map(e => mkUnit(e.exists(v => v.value == num), Enuum, "Number not found in enumeration"))) &&
      accumulateOpt(buff, maximum.map(max => mkUnit(lteq(num, max), Maximum, "Number is greater than maximum"))) &&
      accumulateOpt(buff, minimum.map(min => mkUnit(gteq(num, min), Minimum, "Number is less than minimum"))) &&
      accumulateOpt(buff, exclusiveMax.map(max => mkUnit(lt(num, max), ExclusiveMax, "Number is greater than exclusive maximum"))) &&
      accumulateOpt(buff, exclusiveMin.map(min => mkUnit(gt(num, min), ExclusiveMin, "Number is less than exclusive minimum")))
  }

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    (tyype.isEmpty || accumulate(buff, mkUnit(tyype.contains("string"), Tyype, s"Expected $tyype, got string"))) &&
      accumulateOpt(buff, const.map(c => mkUnit(c.value == s.toString, Const, "String does not match expected constant"))) &&
      accumulateOpt(buff, enuum.map(e => mkUnit(e.exists(v => v.value == s.toString), Enuum, "String not found in enumeration"))) &&
      accumulateOpt(buff, maxLength.map(max => mkUnit(s.toString.codePointCount(0, s.length()) <= max, MaxLength, "String is greater than maximum length"))) &&
      accumulateOpt(buff, minLength.map(min => mkUnit(s.toString.codePointCount(0, s.length()) >= min, MinLength, "String is less than minimum length"))) &&
      accumulateOpt(buff, pattern.map(p => mkUnit(p.matches(s), Pattern, "String does not match pattern")))
    buff.result()
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Nothing, Seq[OutputUnit]] = {
    val insVisitor: ArrVisitor[Nothing, Seq[OutputUnit]] =
      if (const.isEmpty && enuum.isEmpty && (uniqueItems.isEmpty || !uniqueItems.get)) NilArrayVis
      else new MapArrContext(LiteralVisitor.visitArray(length, index), jsVal => { // Vis[Value, coll.Seq[OUnit]]
        val buff = new ListBuffer[OutputUnit]
        const.foreach(c => accumulate(buff, mkUnit(c == jsVal, Const, "Array does not match expected constant")))
        enuum.foreach(e => accumulate(buff, mkUnit(e.contains(jsVal), Enuum, "Array not found in enumeration")))
        uniqueItems.foreach(b => {
          val set = new mutable.HashSet[Value](jsVal.arr.size, 1) // perf: avoid Set
          accumulate(buff, mkUnit(jsVal.arr.forall(e => set.add(e)), UniqueItems, "Values in array are not unique"))
        })
        buff.result()
      })

    new ArrVisitor[Any, Seq[OutputUnit]] {
      private var nextIdx = 0

      override def subVisitor: Visitor[?, ?] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = {
        insVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): Seq[OutputUnit] = {
        val buff = ListBuffer.from(insVisitor.visitEnd(index))
        if (tyype.nonEmpty) accumulate(buff, mkUnit(tyype.contains("array"), Tyype, s"Expected $tyype, got array"))
        maxItems.foreach(max => accumulate(buff, mkUnit(nextIdx <= max, MaxItems, "Array has more items than allowed")))
        minItems.foreach(min => accumulate(buff, mkUnit(nextIdx >= min, MinItems, "Array has less items than allowed")))
        buff.result()
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Nothing, Seq[OutputUnit]] =  {
    val propsVisited: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
    val insVisitor: ObjVisitor[?, Seq[OutputUnit]] =
      if (const.isEmpty && enuum.isEmpty && (uniqueItems.isEmpty || !uniqueItems.get)) NilObjVis
      else new MapObjContext(LiteralVisitor.visitObject(length, index), obj => { // Vis[Value, coll.Seq[OUnit]]
        val buff = new ListBuffer[OutputUnit]
        const.foreach(c => accumulate(buff, mkUnit(c == obj, Const, "Object does not match expected constant")))
        enuum.foreach(e => accumulate(buff, mkUnit(e.contains(obj), Enuum, "Object not found in enumeration")))
        buff.result()
      })

    new ObjVisitor[Any, Seq[OutputUnit]] {
      private var currentKey: String = "?"

      override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "Expected string"
        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          propsVisited.addOne(currentKey)
          insVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[?, ?] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = insVisitor.narrow.visitValue(v, index)

      override def visitEnd(index: Int): Seq[OutputUnit] = {
        val buff = ListBuffer.from(insVisitor.visitEnd(index))
        if (tyype.nonEmpty) accumulate(buff, mkUnit(tyype.contains("object"), Tyype, s"Expected $tyype, got object"))
        required.foreach(req => accumulate(buff, mkUnit(propsVisited.contains(req), Required, s"Object does not contain required property $req")))
        maxProperties.foreach(max => accumulate(buff, mkUnit(propsVisited.size <= max, MaxProperties, "Object has more properties than allowed")))
        minProperties.foreach(min => accumulate(buff, mkUnit(propsVisited.size >= min, MinProperties, "Object has less properties than allowed")))
        depReq.foreach(depReqs => accumulate(buff, mkUnit(depReqs.filter((k, reqs) => propsVisited.contains(k)) // all depRequired that apply (found in obj) as (dependent key, required)
          .map((k, reqs) => reqs.arr.forall(rreq => propsVisited.contains(rreq.str))) // whether the required props were found
          .forall(identity), DepRequired, "Object does not contain dependent required properties"))) // whether all entries were satisfied
        buff.result()
      }
    }
  }
}

object Validation extends VocabBaseFactory {
  private def asBigDec(num: Long | Double) = {
    num match
      case l: Long => BigDecimal.valueOf(l)
      case d: Double => BigDecimal.valueOf(d)
  }
  private def gt(n1: Long | Double, n2: Long | Double) = {
    n1 match
      case l1: Long => n2 match
        case l2: Long => l1 > l2
        case d2: Double => l1 > d2
      case d1: Double => n2 match
        case l2: Long => d1 > l2
        case d2: Double => d1 > d2
  }  
  private def lt(n1: Long | Double, n2: Long | Double) = {
    n1 match
      case l1: Long => n2 match
        case l2: Long => l1 < l2
        case d2: Double => l1 < d2
      case d1: Double => n2 match
        case l2: Long => d1 < l2
        case d2: Double => d1 < d2
  }
  private def lteq(n1: Long | Double, n2: Long | Double) = {
    n1 match
      case l1: Long => n2 match
        case l2: Long => l1 <= l2
        case d2: Double => l1 <= d2
      case d1: Double => n2 match
        case l2: Long => d1 <= l2
        case d2: Double => d1 <= d2
  }
  def gteq(n1: Long | Double, n2: Long | Double): Boolean = {
    n1 match
      case l1: Long => n2 match
        case l2: Long => l1 >= l2
        case d2: Double => l1 >= d2
      case d1: Double => n2 match
        case l2: Long => d1 >= l2
        case d2: Double => d1 >= d2
  }

  private val NilArrayVis = new ArrVisitor[Any, Seq[OutputUnit]] { // TODO: make object
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }

  private val NilObjVis = new ObjVisitor[Any, Seq[OutputUnit]] { // TODO: make object
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }

  private val Tyype = "type"
  private val Const = "const"
  private val Enuum = "enum"
  private val MultipleOf = "multipleOf"
  private val Maximum = "maximum"
  private val Minimum = "minimum"
  private val ExclusiveMax = "exclusiveMaximum"
  private val ExclusiveMin = "exclusiveMinimum"
  private val MaxLength = "maxLength"
  private val MinLength = "minLength"
  private val MaxItems = "maxItems"
  private val MinItems = "minItems"
  private val MaxContains = "maxContains"
  private val MinContains = "minContains"
  private val MaxProperties = "maxProperties"
  private val MinProperties = "minProperties"
  private val Pattern = "pattern"
  private val UniqueItems = "uniqueItems"
  private val Required = "required"
  private val DepRequired = "dependentRequired"
  
  val Keys: Seq[String] = Seq(Tyype, Const, Enuum, MultipleOf, Maximum, Minimum, ExclusiveMax, ExclusiveMin, MaxLength,
    MinLength, MaxItems, MinItems, MaxContains, MinContains, MaxProperties, MinProperties, Pattern, UniqueItems,
    Required, DepRequired)

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/validation"

  override def create(schema: ObjectSchema,
                      ctx: Context,
                      path: JsonPointer,
                      dynParent: Option[Vocab[?]]): Validation = new Validation(schema, ctx, path, dynParent)

  override def shouldApply(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
}
