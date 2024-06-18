package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.FormatAssertion.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.util.regex.{Pattern, PatternSyntaxException}
import java.util.UUID

// unavailable in scala js/native
import java.time.{Duration, Period, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter.{ISO_LOCAL_DATE, ISO_OFFSET_DATE_TIME, ISO_OFFSET_TIME}
import java.time.format.DateTimeParseException
import java.time.temporal.ChronoField

final class FormatAssertion private(schema: ObjectSchema,
                           ctx: Context,
                           path: JsonPointer,
                           dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {
  private val format = schema.get(FormatKw).get
  private val valid = Seq(mkUnit(true, FormatKw, annotation = schema.get(FormatKw).get.asInstanceOf[Str]))
  private val invalid = Seq(mkUnit(false, FormatKw, s"String does not conform to $format format", annotation = format))

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val isValid = format.str match
      case "date-time" => try { ISO_OFFSET_DATE_TIME.parse(s); true } catch // supports +HH:MM:ss
        case e: DateTimeParseException => isLeapDateTime(s, e)
      case "date" => try { ISO_LOCAL_DATE.parse(s); true } catch
        case _: DateTimeParseException => false
      case "time" => try { ISO_OFFSET_TIME.parse(s); true } catch // supports +HH:MM:ss
        case e: DateTimeParseException => isLeapTime(s, e)
      case "duration" => s.length() > 1 && s.charAt(0) == 'P' && isDuration(s)
      case "idn-email" => true
      case "hostname" => s.length() <= 255 && Hostname_r.matches(s)
      case "idn-hostname" => true
      case "ipv4" => s.length() >= 7 && s.length() <= 15 && IPv4_r.matches(s)
      case "ipv6" => (s.length() >= 2 || s.length() <= 45) && isIPv6(s)
      case "uuid" => try { s.length() == 36 && { UUID.fromString(s.toString); true }} catch // https://bugs.openjdk.org/browse/JDK-8202760
        case _: IllegalArgumentException => false
      case "uri" => try { new URI(s.toString).isAbsolute && s.chars().allMatch(c => c < 0x7F) } catch // https://stackoverflow.com/a/3585791/4814697
        case _: URISyntaxException => false
      case "uri-reference" => try { new URI(s.toString); s.chars().allMatch(c => c < 0x7F) } catch
        case _: URISyntaxException => false
      case "iri" => try { new URI(s.toString).isAbsolute } catch
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

        if (!hasInt) false
        else if (i == s.length()) true
        else {
          c = s.charAt(i)
          (c == '#' && i == s.length() - 1) || (c == '/' && isJsPtr(s.subSequence(i, s.length())))
        }
      }
      case "regex" => try { Pattern.compile(s.toString); true } catch // perf: compiled then discarded, sec: ?
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

  private def isLeapDateTime(s: CharSequence, e: DateTimeParseException): Boolean = {
    val str = s.toString
    if (!(e.getMessage.nn.contains("Invalid value for SecondOfMinute") && str.contains(":60")))
      return false

    try {
      val accessor = ISO_OFFSET_DATE_TIME.parse(str.replace(":60", ":59"))
      val offsetSecs = accessor.getLong(ChronoField.OFFSET_SECONDS)
      if (Math.abs(offsetSecs) > 86399) // 24:00 - 1s
        return false

      val date = accessor.query(LocalDateTime.from).minusSeconds(offsetSecs) // make UTC
      date.get(ChronoField.HOUR_OF_DAY) == 23 &&
        date.get(ChronoField.MINUTE_OF_HOUR) == 59 &&
        date.get(ChronoField.SECOND_OF_MINUTE) == 59
    } catch
      case _: RuntimeException => false
  }

  private def isLeapTime(s: CharSequence, e: DateTimeParseException): Boolean = {
    val str = s.toString
    if (!(e.nn.getMessage.contains("Invalid value for SecondOfMinute") && str.contains(":60")))
      return false

    try {
      val accessor = ISO_OFFSET_TIME.parse(str.replace(":60", ":59"))
      val offsetSecs = accessor.getLong(ChronoField.OFFSET_SECONDS)
      if (Math.abs(offsetSecs) > 86399) // 24:00 - 1s
        return false

      val time = accessor.query(LocalTime.from).minusSeconds(offsetSecs) // make UTC
      time.get(ChronoField.HOUR_OF_DAY) == 23 &&
        time.get(ChronoField.MINUTE_OF_HOUR) == 59 &&
        time.get(ChronoField.SECOND_OF_MINUTE) == 59
    } catch
      case _: RuntimeException => false
  }

  private def isDuration(s: CharSequence): Boolean = try {
    val str = s.toString; val t = str.indexOf('T')
    if (t == -1) { Period.parse(s); true }
    else {
      val date = str.substring(0, t); val time = str.substring(t)
      (date.length == 1 || { Period.parse(date); true }) &&
        { Duration.parse("P" + str.substring(t)); true }
    }
  } catch
    case _: DateTimeParseException => false

  private val Hex = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
  def isIPv6(s: CharSequence): Boolean = {
    if (s == "::") return true

    val str = s.toString

    val compact = str.contains("::")
    if (compact && (str.indexOf("::") != str.lastIndexOf("::"))) return false // not more than 1 compact
    if (str.startsWith(":") && !str.startsWith("::")                          // no single empty prefix
      || str.endsWith(":") && !str.endsWith("::")) return false               // no single empty suffix

    val groups = str.split(":", -1)
    if (groups.length < 3 || groups.length > 8) return false  // between 3 and 8 groups

    var i = 0; var valid = true
    while (i < groups.length && valid) {
      val group = groups(i)
      valid = group.length <= 4 && group.forall(Hex.contains)
      i += 1
    }

    if (valid) groups.length == 8 || (groups.length < 8 && compact) // groups are valid and is full, or compact
    else if (i == groups.length &&                                  // last group is invalid
      (groups.length == 7 || (groups.length < 7 && compact)) &&     // and 7 groups, or less and compact
      groups.last.contains('.')) IPv4_r.matches(groups.last)        // and last group is IPv4
    else false
  }

  private val IPv4_r = "^(?:(?:25[0-5]|(?:2[0-4]|1\\d|[1-9]|)\\d)(?:\\.(?!$)|$)){4}$".r // https://stackoverflow.com/a/36760050/4814697
  private val UriTemplate_r = "^([^\\x00-\\x20\\x7f\"'%<>\\\\^`{|}]|%[0-9A-Fa-f]{2}|\\{[+#./;?&=,!@|]?((\\w|%[0-9A-Fa-f]{2})(\\.?(\\w|%[0-9A-Fa-f]{2}))*(:[1-9]\\d{0,3}|\\*)?)(,((\\w|%[0-9A-Fa-f]{2})(\\.?(\\w|%[0-9A-Fa-f]{2}))*(:[1-9]\\d{0,3}|\\*)?))*})*$".r // https://stackoverflow.com/a/61645285/4814697
  // https://stackoverflow.com/a/58347192/4814697
  private val Hostname_r = "^[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$".r // https://www.rfc-editor.org/rfc/rfc1123.html https://www.rfc-editor.org/rfc/rfc952 // https://stackoverflow.com/a/1418724/4814697
  private val Email_r = "^[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$".r // based on https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address on 2024-06-17

  override def uri: String = "https://json-schema.org/draft/2020-12/meta/format-assertion"
  override def shouldApply(schema: ObjectSchema): Boolean = schema.value.contains(FormatKw)
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): FormatAssertion =
    new FormatAssertion(schema, ctx, path, dynParent)
}
