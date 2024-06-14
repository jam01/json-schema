package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.FormatAssertion.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.util.regex.{Pattern, PatternSyntaxException}
import java.util.UUID

// unavailable in scala js/native
import java.time.Duration
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

final class FormatAssertion private(schema: ObjectSchema,
                           ctx: Context,
                           path: JsonPointer,
                           dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {
  private val format = schema.get(FormatKw).get
  private val valid = Seq(mkUnit(true, FormatKw, annotation = schema.get(FormatKw).get.asInstanceOf[Str]))
  private val invalid = Seq(mkUnit(false, FormatKw, s"String value does not conform to $format format", annotation = format))

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val isValid = format.str match
      case "date-time" => try { DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(s); true } catch
        case _: DateTimeParseException => false
      case "date" => try { DateTimeFormatter.ISO_LOCAL_DATE.parse(s); true } catch
        case _: DateTimeParseException => false
      case "time" => try { DateTimeFormatter.ISO_OFFSET_TIME.parse(s); true } catch
        case _: DateTimeParseException => false
      case "duration" => try { Duration.parse(s); true } catch
        case _: DateTimeParseException => false
      case "email" => true
      case "idn-email" => true
      case "hostname" => Hostname_r.matches(s)
      case "idn-hostname" => true
      case "ipv4" => IPv4_r.matches(s)
      case "ipv6" => true
      case "uuid" => try { UUID.fromString(s.toString); true } catch
        case _: IllegalArgumentException => false
      case "uri" => try { new URI(s.toString).isAbsolute && s.chars().allMatch(c => c < 0x7F) } catch // https://stackoverflow.com/a/3585791/4814697
        case _: URISyntaxException => false
      case "uri-reference" => try { new URI(s.toString); s.chars().allMatch(c => c < 0x7F) } catch
        case _: URISyntaxException => false
      case "iri" => try { new URI(s.toString).isAbsolute; true } catch
        case _: URISyntaxException => false
      case "iri-reference" => try { new URI(s.toString); true } catch
        case _: URISyntaxException => false
      case "uri-template" => UriTemplate_r.matches(s)
      case "json-pointer" => isJsPtr(s)
      case "relative-json-pointer" => s.length() > 0 && !RelJsPtrInvChs.contains(s.charAt(0)) && {
        var i = 0; var c = s.charAt(0)
        var hasInt = false
        if (c == '0') { i += 1; hasInt = true }
        else if (Character.isDigit(c)) {
          i += 1; hasInt = true
          while (i < s.length() && Character.isDigit(s.charAt(i))) { i += 1 }
        }

        if (i == s.length() && hasInt) true
        else if (!hasInt) false
        else {
          c = s.charAt(i)
          (c == '#' && i == s.length() - 1) || (c == '/' && isJsPtr(s.subSequence(i, s.length())))
        }
      }
      case "regex" => try { Pattern.compile(s.toString); true } catch // perf: compiled then discarded
        case _: PatternSyntaxException => false
        case _: UnsupportedOperationException => false // as thrown by scala native implementation
      case unk => true

    if (isValid) valid else invalid
  }

  override def visitNull(index: Int): Seq[OutputUnit] = valid
  override def visitFalse(index: Int): Seq[OutputUnit] = valid
  override def visitTrue(index: Int): Seq[OutputUnit] = valid
  override def visitInt64(l: Long, index: Int): Seq[OutputUnit] = valid
  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = valid
  override def visitArray(length: Int, index: Int): ArrVisitor[?, Seq[OutputUnit]] = constArrVis
  private val constArrVis = new ArrVisitor[Any, Seq[OutputUnit]] {
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = valid
  }
  override def visitObject(length: Int, index: Int): ObjVisitor[?, Seq[OutputUnit]] = constObjVis
  private val constObjVis = new ObjVisitor[Any, Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = valid
  }
}

object FormatAssertion extends VocabFactory[FormatAssertion] {
  val FormatKw: String = "format"

  private val JsPtrEscChs = Array('0', '1')
  private val RelJsPtrInvChs = Array('+', '-')
  private def isJsPtr(s: CharSequence): Boolean = {
    s.length() == 0 || (s.charAt(0) == '/' && (s.length() == 1 || {
      var i = 1; var continue = true
      while (i < s.length() - 1 && continue) {
        if (s.charAt(i) == '~' && !JsPtrEscChs.contains(s.charAt(i + 1))) continue = false
        i += 1
      }
      i == s.length() - 1 && continue && s.charAt(i) != '~'
    }))
  }
  private val IPv4_r = "^((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)(\\.(?!$)|$)){4}$".r // https://stackoverflow.com/a/36760050/4814697
  private val UriTemplate_r = "^([^\\x00-\\x20\\x7f\"'%<>\\\\^`{|}]|%[0-9A-Fa-f]{2}|\\{[+#./;?&=,!@|]?((\\w|%[0-9A-Fa-f]{2})(\\.?(\\w|%[0-9A-Fa-f]{2}))*(:[1-9]\\d{0,3}|\\*)?)(,((\\w|%[0-9A-Fa-f]{2})(\\.?(\\w|%[0-9A-Fa-f]{2}))*(:[1-9]\\d{0,3}|\\*)?))*})*$".r // https://stackoverflow.com/a/61645285/4814697
  private val Hostname_r = "(?=^.{1,253}$)(^(((?!-)[a-zA-Z0-9-]{1,63}(?<!-))|((?!-)[a-zA-Z0-9-]{1,63}(?<!-)\\.)+[a-zA-Z]{2,63})$)".r // https://stackoverflow.com/a/20204811/4814697

  override def uri: String = "https://json-schema.org/draft/2020-12/meta/format-assertion"
  override def shouldApply(schema: ObjectSchema): Boolean = schema.value.contains(FormatKw)
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): FormatAssertion =
    new FormatAssertion(schema, ctx, path, dynParent)
}
