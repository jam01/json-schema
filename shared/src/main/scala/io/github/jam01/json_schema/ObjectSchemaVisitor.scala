package io.github.jam01.json_schema

import java.time.{Duration, LocalDate, OffsetDateTime}
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.time.format.DateTimeParseException
import scala.collection.mutable

class ObjectSchemaVisitor extends JSONVisitor[Any, Boolean] {
  val tyype: Seq[String] = Seq("string")
  val pattern: Option[String] = Some(".*")
  val maxLength: Option[Int] = Some(16)
  val minLength: Option[Int] = Some(4)
  val format: Option[String] = None
  val maximum: Option[Int | Double] = None
  val minimum: Option[Int | Double] = None

  override def visitNull(index: Int): Boolean = {
    tyype.exists("null".eq)
  }

  override def visitFalse(index: Int): Boolean = {
    tyype.exists("boolean".eq)
  }

  override def visitTrue(index: Int): Boolean = {
    tyype.exists("boolean".eq)
  }

  override def visitInt64(i: Long, index: Int): Boolean = {
    tyype.exists("integer".eq) &&
      maximum.forall(_ match
        case mxi: Int => i <= mxi
        case mxd: Double => i <= mxd) &&
      minimum.forall(_ match
        case mxi: Int => i >= mxi
        case mxd: Double => i >= mxd)
  }

  override def visitFloat64(d: Double, index: Int): Boolean = {
    tyype.exists("number".eq) &&
      maximum.forall(_ match
        case mxi: Int => d <= mxi
        case mxd: Double => d <= mxd) &&
      minimum.forall(_ match
        case mxi: Int => d >= mxi
        case mxd: Double => d >= mxd)
  }

  override def visitString(s: CharSequence, index: Int): Boolean = {
    tyype.exists("string".eq) &&
      pattern.forall(_.r.matches(s)) && // TODO: memoize
      minLength.forall(s.length() > _) &&
      maxLength.forall(s.length() <= _) &&
      format.forall(_ match // TODO: use regexs
        case "date-time" => try { OffsetDateTime.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "date" => try { LocalDate.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "duration" => try { Duration.parse(s); true } catch
          case ex: DateTimeParseException => false
        case "hostname" => try { URI(_); true } catch
          case ex: URISyntaxException => false
        case _ => true)
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Any, Boolean] = new ArrVisitor[Any, Boolean] {
    val maxItems: Option[Int] = None
    val minItems: Option[Int] = None

    private var counter = 0

    override def subVisitor: Visitor[_, _] = ???

    override def visitValue(v: Any, index: Int): Unit = counter += 1

    override def visitEnd(index: Int): Boolean = {
      minItems.forall(counter >= _) &&
        maxItems.forall(counter <= _)
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Any, Boolean] = new ObjVisitor[Any, Boolean] {
    val required: Seq[String] = Seq.empty
    val maxProperties: Option[Int] = None
    val minProperties: Option[Int] = None

    private val props: mutable.Buffer[String] = mutable.Buffer()

    override def visitKey(index: Int): Visitor[_, _] = ???

    override def visitKeyValue(v: Any): Unit = {
      props.addOne(v.asInstanceOf[String])
    }

    override def subVisitor: Visitor[_, _] = ???

    override def visitValue(v: Any, index: Int): Unit = ()

    override def visitEnd(index: Int): Boolean = {
      required.forall(props.contains(_)) &&
        maxProperties.forall(props.size <= _) &&
        minProperties.forall(props.size >= _)
    }
  }
}
