package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Validation.{asBigDec, gt, gteq, lt, lteq}
import upickle.core.Visitor.{MapArrContext, MapObjContext}
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class Validation(schema: ObjectSchema,
                 ctx: Context = Context.Empty,
                 path: JsonPointer = JsonPointer(),
                 dynParent: Option[BaseValidator] = None) extends BaseValidator(schema, ctx, path, dynParent) {

  private val tyype: collection.Seq[String] = schema.getAsStringArray(Validation.Tyype)
  private val const = schema.get(Validation.Const)
  private val enuum: Option[collection.Seq[Value]] = schema.getArrayOpt(Validation.Enuum)
  private val multipleOf: Option[Long | Double] = schema.getNumber(Validation.MultipleOf)
  private val maximum: Option[Long | Double] = schema.getNumber(Validation.Maximum)
  private val minimum: Option[Long | Double] = schema.getNumber(Validation.Minimum)
  private val exclusiveMax: Option[Long | Double] = schema.getNumber(Validation.ExclusiveMax)
  private val exclusiveMin: Option[Long | Double] = schema.getNumber(Validation.ExclusiveMin)
  private val maxLength: Option[Int] = schema.getInt(Validation.MaxLength)
  private val minLength: Option[Int] = schema.getInt(Validation.MinLength)
  private val maxItems: Option[Int] = schema.getInt(Validation.MaxItems)
  private val minItems: Option[Int] = schema.getInt(Validation.MinItems)
  private val maxContains: Option[Int] = schema.getInt(Validation.MaxContains)
  private val minContains: Option[Int] = schema.getInt(Validation.MinContains)
  private val maxProperties: Option[Int] = schema.getInt(Validation.MaxProperties)
  private val minProperties: Option[Int] = schema.getInt(Validation.MinProperties)
  private val pattern: Option[Regex] = schema.getString(Validation.Pattern).map(s => new Regex(s).unanchored)
  private val uniqueItems: Option[Boolean] = schema.getBoolean(Validation.UniqueItems)
  private val required: collection.Seq[String] = schema.getStringArray(Validation.Required)
  private val depReq: Option[collection.Map[String, Value]] = schema.getObjectOpt(Validation.DepRequired)

  override def visitNull(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

    if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got 'null'", units, tyype.contains("null"))
    const.foreach(c => validate(Validation.Const, "Value 'null' found does not match expected constant", units, c == Null))
    enuum.foreach(e => validate(Validation.Enuum, "Value 'null' not found in enumeration", units, e.contains(Null)))
    units
  }

  override def visitFalse(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

    if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got 'false'", units, tyype.contains("boolean"))
    const.foreach(c => validate(Validation.Const, "Value 'false' found does not match expected constant", units, c == False))
    enuum.foreach(e => validate(Validation.Enuum, "Value 'false' not found in enumeration", units, e.contains(False)))
    units
  }

  override def visitTrue(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

    if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got 'true'", units, tyype.contains("boolean"))
    const.foreach(c => validate(Validation.Const, "Value 'true' found does not match expected constant", units, c == True))
    enuum.foreach(e => validate(Validation.Enuum, "Value 'true' not found in enumeration", units, e.contains(True)))
    units
  }

  override def visitInt64(l: Long, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(8) // perf: should be re-used?
    def isMultiple(mult: Long | Double) = {
      mult match
        case ml: Long => l % ml == 0
        case md: Double => if (md.isWhole) l % md.longValue == 0 else false
    }

    if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got 'true'", units, tyype.exists(t => "integer" == t || "number" == t))
    visitNumber(l, units)
    multipleOf.foreach(mult => validate(Validation.MultipleOf, "Number is not a multiple of", units, isMultiple(mult)))
    units
  }

  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(8) // perf: should be re-used?
    def isMultiple(mult: Long | Double) = try {
      BigDecimal.valueOf(d)
        .remainder(asBigDec(mult))
        .compareTo(java.math.BigDecimal.ZERO) == 0
    } catch case ex: ArithmeticException => false

    if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got 'true'", units, tyype.exists(t => "number" == t || "integer" == t && d.isWhole))
    visitNumber(d, units)
    multipleOf.foreach(mult => validate(Validation.MultipleOf, "Number is not a multiple of", units, isMultiple(mult)))
    units
  }

  // TODO: can we compare const & enum without creating a Value? 
  private def visitNumber(num: Long | Double, units: mutable.ArrayBuffer[OutputUnit]): Unit = {
    const.foreach(c => validate(Validation.Const, "Number found does not match expected constant", units, c == Num(num)))
    enuum.foreach(e => validate(Validation.Enuum, "Number not found in enumeration", units, e.contains(Num(num))))
    maximum.foreach(max => validate(Validation.Maximum, "Number is greater than maximum", units, lteq(num, max)))
    minimum.foreach(min => validate(Validation.Maximum, "Number is less than minimum", units, gteq(num, min)))
    exclusiveMax.foreach(max => validate(Validation.Maximum, "Number is greater than exclusive maximum", units, lt(num, max)))
    exclusiveMin.foreach(min => validate(Validation.Maximum, "Number is less than exclusive minimum", units, gt(num, min)))
  }

  // TODO: can we compare const & enum without creating a Value? 
  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(6) // perf: should be re-used?

    if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got string", units, tyype.contains("string"))
    pattern.foreach(p => validate(Validation.Pattern, "String does not match pattern", units, p.matches(s))) // TODO: include string, opt truncated
    maxLength.foreach(max => validate(Validation.MaxLength, "String is greater than maximum length", units, s.toString.codePointCount(0, s.length()) <= max))
    minLength.foreach(min => validate(Validation.MinLength, "String is less than minimum length", units, s.toString.codePointCount(0, s.length()) >= min))
    const.foreach(c => validate(Validation.Const, "String does not match expected constant", units, c == Str(s.toString)))
    enuum.foreach(e => validate(Validation.Enuum, "String not found in enumeration", units, e.contains(Str(s.toString))))
    units
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[_, collection.Seq[OutputUnit]] = {
    val insVisitor: ArrVisitor[_, collection.Seq[OutputUnit]] =
      if (const.nonEmpty || enuum.nonEmpty || (uniqueItems.nonEmpty && uniqueItems.get))
        new MapArrContext(LiteralVisitor.visitArray(length, index), arr => {
          val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(3) // perf: should be re-used?

          const.foreach(c => validate(Validation.Const, "Array does not match expected constant", units, c == arr))
          enuum.foreach(e => validate(Validation.Enuum, "Array not found in enumeration", units, e.contains(arr)))
          uniqueItems.foreach(b =>
            val set = new mutable.HashSet[Value](arr.value.size, 1) // TODO: would .distinct work?
            arr.value.foreach(v => {
              if (!set.add(v)) units.addOne(invalid(Validation.UniqueItems, "Values in array are not unique"))
              else if (false /* && verbose*/ ) units.addOne(valid(Validation.UniqueItems))
            }))

          units
        })
      else new ArrVisitor[Any, collection.Seq[OutputUnit]] {
        override def subVisitor: Visitor[_, _] = NoOpVisitor
        override def visitValue(v: Any, index: Int): Unit = ()
        override def visitEnd(index: Int): collection.Seq[OutputUnit] = Seq.empty
      }

    new ArrVisitor[Any, collection.Seq[OutputUnit]] {
      private var nextIdx = 0

      override def subVisitor: Visitor[_, _] = insVisitor.subVisitor

      override def visitValue(v: Any, index: Int): Unit = {
        insVisitor.narrow.visitValue(v, index)
        nextIdx += 1
      }

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val units: mutable.ArrayBuffer[OutputUnit] = mutable.ArrayBuffer.from(insVisitor.visitEnd(index))
        units.sizeHint(units.size + 3)

        if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got array", units, tyype.contains("array"))
        maxItems.foreach(max => validate(Validation.MaxItems, "Array has more items than allowed", units, nextIdx <= max))
        minItems.foreach(min => validate(Validation.MinItems, "Array has less items than allowed", units, nextIdx >= min))
        minItems.foreach(min => validate(Validation.MinItems, "Array has less items than allowed", units, nextIdx >= min))
        units
      }
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[_, collection.Seq[OutputUnit]] = {
    val propsVisited = mutable.ArrayBuffer[String]()
    val insVisitor: ObjVisitor[_, collection.Seq[OutputUnit]] =
      if (const.nonEmpty || enuum.nonEmpty || (uniqueItems.nonEmpty && uniqueItems.get))
        new MapObjContext(LiteralVisitor.visitObject(length, index), obj => {
          val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

          const.foreach(c => validate(Validation.Const, "Object does not match expected constant", units, c == obj))
          enuum.foreach(e => validate(Validation.Enuum, "Object not found in enumeration", units, e.contains(obj)))
          units
        })
      else new ObjVisitor[Any, collection.Seq[OutputUnit]] {
        override def visitKey(index: Int): Visitor[_, _] = NoOpVisitor
        override def visitKeyValue(v: Any): Unit = ()
        override def subVisitor: Visitor[_, _] = NoOpVisitor
        override def visitValue(v: Any, index: Int): Unit = ()
        override def visitEnd(index: Int): collection.Seq[OutputUnit] = Seq.empty
      }


    new ObjVisitor[Any, collection.Seq[OutputUnit]] {
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

      override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
        val units: mutable.ArrayBuffer[OutputUnit] = mutable.ArrayBuffer.from(insVisitor.visitEnd(index)); units.sizeHint(5)

        if (tyype.nonEmpty) validate(Validation.Tyype, s"Expected $tyype, got: object", units, tyype.contains("object"))
        required.foreach(req => validate(Validation.Required, s"Object does not contain required property $req", units, propsVisited.contains(req)))
        maxProperties.foreach(max => validate(Validation.MaxProperties, "Object has more properties than allowed", units, propsVisited.size <= max))
        minProperties.foreach(min => validate(Validation.MinProperties, "Object has less properties than allowed", units, propsVisited.size >= min))
        depReq.foreach(depReqs => validate(Validation.DepRequired, "Object does not contain dependent required properties", units,  // TODO: which dep required failed? 
          depReqs.filter((k, reqs) => propsVisited.contains(k)) // all depRequired that apply (found in obj) as (dependent key, required)
          .map((k, reqs) => reqs.arr.value.forall(rreq => propsVisited.contains(rreq.str))) // whether the required props were found
          .forall(identity))) // whether all entries were satisfied
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
  private def gteq(n1: Long | Double, n2: Long | Double) = {
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
