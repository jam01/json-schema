package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Validation.*
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class Validation(schema: ObjectSchema,
                 ctx: Context = Context.Empty,
                 path: JsonPointer = JsonPointer(),
                 dynParent: Option[BaseValidator] = None) extends BaseValidator(schema, ctx, path, dynParent) {

  private val tyype: collection.Seq[String] = schema.getAsStringArray(Tyype)
  private val const: Option[Value] = schema.get(Const)
  private val enuum: Option[collection.Seq[Value]] = schema.getArrayOpt(Enuum)
  private val multipleOf: Option[Long | Double] = schema.getNumber(MultipleOf)
  private val maximum: Option[Long | Double] = schema.getNumber(Maximum)
  private val minimum: Option[Long | Double] = schema.getNumber(Minimum)
  private val exclusiveMax: Option[Long | Double] = schema.getNumber(ExclusiveMax)
  private val exclusiveMin: Option[Long | Double] = schema.getNumber(ExclusiveMin)
  private val maxLength: Option[Int] = schema.getInt(MaxLength)
  private val minLength: Option[Int] = schema.getInt(MinLength)
  private val maxItems: Option[Int] = schema.getInt(MaxItems)
  private val minItems: Option[Int] = schema.getInt(MinItems)
  private val maxContains: Option[Int] = schema.getInt(MaxContains)
  private val minContains: Option[Int] = schema.getInt(MinContains)
  private val maxProperties: Option[Int] = schema.getInt(MaxProperties)
  private val minProperties: Option[Int] = schema.getInt(MinProperties)
  private val pattern: Option[Regex] = schema.getString(Pattern).map(s => new Regex(s).unanchored)
  private val uniqueItems: Option[Boolean] = schema.getBoolean(UniqueItems)
  private val required: collection.Seq[String] = schema.getStringArray(Required)
  private val depReq: Option[collection.Map[String, Value]] = schema.getObjectOpt(DepRequired)

  override def visitNull(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

    if (tyype.nonEmpty) addUnit(units, unitOf(tyype.contains("null"), Tyype, s"Expected $tyype, got null"))
    const.foreach(c => addUnit(units, unitOf(c == Null, Const, "null does not match expected constant")))
    enuum.foreach(e => addUnit(units, unitOf(e.contains(Null), Enuum, "null not found in enumeration")))
    units
  }

  override def visitFalse(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

    if (tyype.nonEmpty) addUnit(units, unitOf(tyype.contains("boolean"), Tyype, s"Expected $tyype, got false"))
    const.foreach(c => addUnit(units, unitOf(c == False, Const, "false does not match expected constant")))
    enuum.foreach(e => addUnit(units, unitOf(e.contains(False), Enuum, "false not found in enumeration")))
    units
  }

  override def visitTrue(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

    if (tyype.nonEmpty) addUnit(units, unitOf(tyype.contains("boolean"), Tyype, s"Expected $tyype, got true"))
    const.foreach(c => addUnit(units, unitOf(c == True, Const, "true does not match expected constant")))
    enuum.foreach(e => addUnit(units, unitOf(e.contains(True), Enuum, "true not found in enumeration")))
    units
  }

  override def visitInt64(l: Long, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(8) // perf: should be re-used?
    def isMultiple(mult: Long | Double) = {
      mult match
        case ml: Long => l % ml == 0
        case md: Double => if (md.isWhole) l % md.longValue == 0 else false
    }

    if (tyype.nonEmpty) addUnit(units, unitOf(tyype.exists(t => "integer" == t || "number" == t), Tyype, s"Expected $tyype, got number"))
    visitNumber(l, units)
    multipleOf.foreach(mult => addUnit(units, unitOf(isMultiple(mult), MultipleOf, "Number is not a multiple")))
    units
  }

  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(8) // perf: should be re-used?
    def isMultiple(mult: Long | Double) = try {
      BigDecimal.valueOf(d)
        .remainder(asBigDec(mult))
        .compareTo(java.math.BigDecimal.ZERO) == 0
    } catch case ex: ArithmeticException => false

    if (tyype.nonEmpty) addUnit(units, unitOf(tyype.exists(t => "number" == t || "integer" == t && d.isWhole), Tyype, s"Expected $tyype, got number"))
    visitNumber(d, units)
    multipleOf.foreach(mult => addUnit(units, unitOf(isMultiple(mult), MultipleOf, "Number is not a multiple")))
    units
  }

  // TODO: can we compare const & enum without creating a Value? 
  private def visitNumber(num: Long | Double, units: mutable.ArrayBuffer[OutputUnit]): Unit = {
    const.foreach(c => addUnit(units, unitOf(c == Num(num), Const, "Number does not match expected constant")))
    enuum.foreach(e => addUnit(units, unitOf(e.contains(Num(num)), Enuum, "Number not found in enumeration")))
    maximum.foreach(max => addUnit(units, unitOf(lteq(num, max), Maximum, "Number is greater than maximum")))
    minimum.foreach(min => addUnit(units, unitOf(gteq(num, min), Minimum, "Number is less than minimum")))
    exclusiveMax.foreach(max => addUnit(units, unitOf(lt(num, max), ExclusiveMax, "Number is greater than exclusive maximum")))
    exclusiveMin.foreach(min => addUnit(units, unitOf(gt(num, min), ExclusiveMin, "Number is less than exclusive minimum")))
  }

  // TODO: can we compare const & enum without creating a Value? 
  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(6) // perf: should be re-used?

    if (tyype.nonEmpty) addUnit(units, unitOf(tyype.contains("string"), Tyype, s"Expected $tyype, got string"))
    pattern.foreach(p => addUnit(units, unitOf(p.matches(s), Pattern, "String does not match pattern"))) // TODO: include string, opt truncated
    maxLength.foreach(max => addUnit(units, unitOf(s.toString.codePointCount(0, s.length()) <= max, MaxLength, "String is greater than maximum length")))
    minLength.foreach(min => addUnit(units, unitOf(s.toString.codePointCount(0, s.length()) >= min, MinLength, "String is less than minimum length")))
    const.foreach(c => addUnit(units, unitOf(c == Str(s.toString), Const, "String does not match expected constant")))
    enuum.foreach(e => addUnit(units, unitOf(e.contains(Str(s.toString)), Enuum, "String not found in enumeration")))
    units
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Nothing, collection.Seq[OutputUnit]] = {
    val insVisitor: ArrVisitor[Nothing, collection.Seq[OutputUnit]] =
      if (const.nonEmpty || enuum.nonEmpty || (uniqueItems.nonEmpty && uniqueItems.get))
        new MapArrContext(LiteralVisitor.visitArray(length, index), jsVal => { // Vis[Value, coll.Seq[OUnit]]
          val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

          const.foreach(c => addUnit(units, unitOf(c == jsVal, Const, "Array does not match expected constant")))
          enuum.foreach(e => addUnit(units, unitOf(e.contains(jsVal), Enuum, "Array not found in enumeration")))
          uniqueItems.foreach(b => {
            val set = new mutable.HashSet[Value](jsVal.arr.size, 1)
            addUnit(units, unitOf(jsVal.arr.forall(e => set.add(e)), UniqueItems, "Values in array are not unique"))
          })
          units
        })
      else new ArrVisitor[Unit, collection.Seq[OutputUnit]] { // TODO: make object
        override def subVisitor: Visitor[?, ?] = NoOpVisitor
        override def visitValue(v: Unit, index: Int): Unit = ()
        override def visitEnd(index: Int): collection.Seq[OutputUnit] = Nil
      }

    new ArrVisitor[Any, collection.Seq[OutputUnit]] {
      private var nextIdx = 0

      override def subVisitor: Visitor[?, ?] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = {
        insVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val units: mutable.ArrayBuffer[OutputUnit] = mutable.ArrayBuffer.from(insVisitor.visitEnd(index))
        units.sizeHint(units.size + 3)

        if (tyype.nonEmpty) addUnit(units, unitOf(tyype.contains("array"), Tyype, s"Expected $tyype, got array"))
        maxItems.foreach(max => addUnit(units, unitOf(nextIdx <= max, MaxItems, "Array has more items than allowed")))
        minItems.foreach(min => addUnit(units, unitOf(nextIdx >= min, MinItems, "Array has less items than allowed")))
        units
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Nothing, collection.Seq[OutputUnit]] = {
    val propsVisited = mutable.ArrayBuffer[String]()
    val insVisitor: ObjVisitor[?, collection.Seq[OutputUnit]] =
      if (const.nonEmpty || enuum.nonEmpty || (uniqueItems.nonEmpty && uniqueItems.get))
        new MapObjContext(LiteralVisitor.visitObject(length, index), obj => { // Vis[Value, coll.Seq[OUnit]]
          val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

          const.foreach(c => addUnit(units, unitOf(c == obj, Const, "Object does not match expected constant")))
          enuum.foreach(e => addUnit(units, unitOf(e.contains(obj), Enuum, "Object not found in enumeration")))
          units
        })
      else new ObjVisitor[Unit, collection.Seq[OutputUnit]] { // TODO: make object 
        override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
        override def visitKeyValue(v: Any): Unit = ()
        override def subVisitor: Visitor[?, ?] = NoOpVisitor
        override def visitValue(v: Unit, index: Int): Unit = ()
        override def visitEnd(index: Int): collection.Seq[OutputUnit] = Nil
      }


    new ObjVisitor[Any, collection.Seq[OutputUnit]] {
      private var currentKey: String = "?"

      override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
        def expectedMsg = "expected string"

        override def visitString(s: CharSequence, index1: Int): Any = {
          currentKey = s.toString
          propsVisited.addOne(currentKey)
          insVisitor.visitKey(index).visitString(s, index1)
        }
      }

      override def visitKeyValue(v: Any): Unit = insVisitor.visitKeyValue(v)

      override def subVisitor: Visitor[?, ?] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = insVisitor.narrow.visitValue(v, index)

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val units: mutable.ArrayBuffer[OutputUnit] = mutable.ArrayBuffer.from(insVisitor.visitEnd(index))
        units.sizeHint(units.size + 5)

        if (tyype.nonEmpty) addUnit(units, unitOf(tyype.contains("object"), Tyype, s"Expected $tyype, got object"))
        required.foreach(req => addUnit(units, unitOf(propsVisited.contains(req), Required, s"Object does not contain required property $req")))
        maxProperties.foreach(max => addUnit(units, unitOf(propsVisited.size <= max, MaxProperties, "Object has more properties than allowed")))
        minProperties.foreach(min => addUnit(units, unitOf(propsVisited.size >= min, MinProperties, "Object has less properties than allowed")))
        depReq.foreach(depReqs => addUnit(units, unitOf(depReqs.filter((k, reqs) => propsVisited.contains(k)) // all depRequired that apply (found in obj) as (dependent key, required)
          .map((k, reqs) => reqs.arr.forall(rreq => propsVisited.contains(rreq.str))) // whether the required props were found
          .forall(identity), DepRequired, "Object does not contain dependent required properties"))) // whether all entries were satisfied
        units
      }
    }
  }
}

object Validation {
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
  def gteq(n1: Long | Double, n2: Long | Double) = {
    n1 match
      case l1: Long => n2 match
        case l2: Long => l1 >= l2
        case d2: Double => l1 >= d2
      case d1: Double => n2 match
        case l2: Long => d1 >= l2
        case d2: Double => d1 >= d2
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
}
