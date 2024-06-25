package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.FormatAssertion.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

import java.net.{URI, URISyntaxException}
import java.util.UUID
import java.util.regex.{Pattern, PatternSyntaxException}

// provided by scala-java-time on sjs/native
import java.time.format.DateTimeFormatter.{ISO_LOCAL_DATE, ISO_OFFSET_DATE_TIME, ISO_OFFSET_TIME}
import java.time.format.DateTimeParseException
import java.time.temporal.ChronoField
import java.time.{Duration, LocalDateTime, LocalTime, Period}

final class FormatAssertion private(schema: ObjectSchema,
                           ctx: Context,
                           path: JsonPointer,
                           dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {
  private val format = schema.get(FormatKw).get

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val isValid = format.str match
      case "date-time" => try { ISO_OFFSET_DATE_TIME.parse(s); true } catch // supports +HH:MM:ss
        case e: DateTimeParseException => isLeapDateTime(s, e)
      case "date" => try { ISO_LOCAL_DATE.parse(s); true } catch
        case _: DateTimeParseException => false
      case "time" => try { ISO_OFFSET_TIME.parse(s); true } catch // supports +HH:MM:ss
        case e: DateTimeParseException => isLeapTime(s, e)
      case "duration" => isDuration(s)
      case "email" => isEmail(s)
      case "idn-email" => isEmail(s, true)
      case "hostname" => s.length() <= 255 && Hostname_r.matches(s)
      case "idn-hostname" => Idn.isHostname(s.toString)
      case "ipv4" => isIPv4(s)
      case "ipv6" => isIPv6(s)
      case "uuid" => try { s.length() == 36 && { UUID.fromString(s.toString); true }} catch // https://bugs.openjdk.org/browse/JDK-8202760
        case _: IllegalArgumentException => false
      case "uri" => try { // https://stackoverflow.com/a/3585791/4814697 https://stackoverflow.com/a/14066594/4814697
        val uri = new URI(s.toString)
        uri.isAbsolute && s.chars().allMatch(isAscii) && !hasBareIPv6(uri)
      } catch
        case _: URISyntaxException => false
      case "uri-reference" => try {
        val uri = new URI(s.toString)
        s.chars().allMatch(isAscii) && !hasBareIPv6(uri)
      } catch
        case _: URISyntaxException => false
      case "iri" => try {
        val uri = new URI(s.toString)
        uri.isAbsolute && !hasBareIPv6(uri)
      } catch
        case _: URISyntaxException => false
      case "iri-reference" => try { !hasBareIPv6(URI(s.toString)) } catch
        case _: URISyntaxException => false
      case "uri-template" => UriTemplate_r.matches(s)
      case "json-pointer" => isJsPtr(s)
      case "relative-json-pointer" => isRelJsPtr(s)
      case "regex" => try { Pattern.compile(s.toString); true } catch // perf: compiled then discarded, sec: ?
        case _: PatternSyntaxException => false
        case _: UnsupportedOperationException => false // as thrown by scala native implementation
      case unk => true

    if (isValid) Seq(mkUnit(true, FormatKw, annotation = format))
    else Seq(mkUnit(false, FormatKw, s"String does not conform to $format format", annotation = format))
  }

  override def visitNull(index: Int): Seq[OutputUnit] = Seq(mkUnit(true, FormatKw, annotation = format))
  override def visitFalse(index: Int): Seq[OutputUnit] = Seq(mkUnit(true, FormatKw, annotation = format))
  override def visitTrue(index: Int): Seq[OutputUnit] = Seq(mkUnit(true, FormatKw, annotation = format))
  override def visitInt64(l: Long, index: Int): Seq[OutputUnit] = Seq(mkUnit(true, FormatKw, annotation = format))
  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = Seq(mkUnit(true, FormatKw, annotation = format))
  override def visitArray(length: Int, index: Int): ArrVisitor[?, Seq[OutputUnit]] = constArrVis
  private val constArrVis = new ArrVisitor[Any, Seq[OutputUnit]] {
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Seq(mkUnit(true, FormatKw, annotation = format))
  }
  override def visitObject(length: Int, index: Int): ObjVisitor[?, Seq[OutputUnit]] = constObjVis
  private val constObjVis = new ObjVisitor[Any, Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = Seq(mkUnit(true, FormatKw, annotation = format))
  }
}

object FormatAssertion extends VocabFactory[FormatAssertion] {
  val FormatKw: String = "format"

  private def isJsPtr(s: CharSequence): Boolean = {
    if (s.length() == 0 || (s.length() == 1 && s.charAt(0) == '/')) return true
    if (s.charAt(0) != '/') return false

    var i = 1
    while (i < s.length() - 1) {
      if (s.charAt(i) == '~' &&
        (s.charAt(i + 1) != '0' && s.charAt(i + 1) != '1')) return false  // '~` must be proceeded by 0-1
      i += 1
    }

    i == s.length() - 1 && s.charAt(i) != '~'
  }
  private def isRelJsPtr(s: CharSequence): Boolean = {
    if (s.isEmpty) return false

    var c = s.charAt(0)
    if (c == '-' && c == '+') return false

    var i = 0; var hasInt = false
    if (c == '0') { i += 1; hasInt = true }
    else if (isNumeric(c)) {
      i += 1; hasInt = true
      while (i < s.length() && isNumeric(s.charAt(i))) i += 1
    }

    if (!hasInt) false
    else if (i == s.length()) true
    else {
      c = s.charAt(i)
      (c == '#' && i == s.length() - 1) || (c == '/' && isJsPtr(s.subSequence(i, s.length())))
    }
  }

  private def isLeapDateTime(s: CharSequence, e: DateTimeParseException): Boolean = {
    val str = s.toString
    if (e.getMessage == null || !e.getMessage.endsWith("Invalid value for SecondOfMinute (valid values 0 - 59): 60"))
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
    if (e.getMessage == null || !e.getMessage.endsWith("Invalid value for SecondOfMinute (valid values 0 - 59): 60"))
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
    if (s.length() ==0 || s.charAt(0) != 'P') return false

    val str = s.toString; val t = str.indexOf('T')
    if (t == -1) { Period.parse(s); true }
    else {
      val date = str.substring(0, t); val time = str.substring(t)
      (date.length == 1 || { Period.parse(date); true }) &&
        { Duration.parse("P" + str.substring(t)); true }
    }
  } catch
    case _: DateTimeParseException => false

  private def hasBareIPv6(uri: URI): Boolean = {
    val auth = uri.getRawAuthority
    if (auth == null || auth.isEmpty) false
    else if (auth.charAt(0) == '[' && auth.charAt(auth.length - 1) == ']') false
    else if (!auth.contains(':')) false
    else isIPv6(auth)
  }

  // https://www.rfc-editor.org/rfc/rfc5321#section-4.1.2 https://www.rfc-editor.org/rfc/rfc5322.html#section-3.2.3
  private val AtomSpecials: Array[Int] = Array('"', '(', ')', ',', ':', ';', '<', '>', '@', '[', ']', '\\') // excluding SP & DEL at ends, and '.' (checked manually)
  private def isEmail(s: CharSequence, i18n: Boolean = false): Boolean = {
    if (s.length() < 3) return false
    val str = s.toString
    var parts = Array.empty[String] // perf: unnecessary if checks are by index

    if (str.charAt(0) == '"') { // quoted-string local part
      val at = str.lastIndexOf('@')                         // may have multiple @
      if (at == -1) return false
      parts = Array(str.substring(0, at), str.substring(at + 1))

      val qstr = parts(0)
      if (qstr.charAt(qstr.length - 1) != '"') return false // must end with DQUOTE

      var i = 1; var c = qstr.charAt(1)
      while (i < qstr.length) {
        c = qstr.charAt(i)

        if (!i18n && !between(c, ' ', '~')) return false              // must be printable
        if (i18n && isAscii(c) && !between(c, ' ', '~')) return false // must be printable or non-ASCII

        if (c == '"' && i != qstr.length - 1)           // if DQUOTE in the middle
          if (qstr.charAt(i - 1) != '\\') return false  //   preceding char must be a backslash
        else if (c == '\\' && i == qstr.length - 2)     // if backslash at end of quoted-string
          if (qstr.charAt(i - 1) != '\\') return false  //   preceding char must be a backslash
        else if (i18n && !isAscii(c))                   // if non-ASCII
          if (qstr.charAt(i - 1) == '\\') return false  //   preceding char must not be a backslash

        i += 1
      }
    } else { // dot-string local part
      parts = str.split("@", -1)
      if (parts.length != 2) return false // must have single @

      val dstr = parts(0)
      if (dstr.charAt(0) == '.' || dstr.charAt(dstr.length - 1) == '.') return false // must start/end with atext

      var i = 1; var c = dstr.charAt(1)
      while (i < dstr.length) {
        c = dstr.charAt(i)
        if (!i18n && (!between(c, '!', '~') || in(c, AtomSpecials))) return false               // must be printable except Specials
        if (i18n && isAscii(c) && (!between(c, '!', '~') || in(c, AtomSpecials))) return false  // must be printable except Specials or non-ASCII
        if (c == '.' && dstr.charAt(i - 1) == '.') return false                                 // dot must be preceded by atext

        i += 1
      }
    }

    val auth = parts(1)
    if (auth.charAt(0) != '[')
      if (!i18n) return auth.length() <= 255 && Hostname_r.matches(auth)  // hostname
      else return Idn.isHostname(auth)
    if (auth.charAt(auth.length - 1) != ']') return false                 // invalid address literal

    if (auth.startsWith("[IPv6:")) isIPv6(auth.substring(6, auth.length - 1))
    else isIPv4(auth.substring(1, auth.length - 1))
  }
  
  private def isIPv4(s: CharSequence): Boolean = {
    if (s.length() < 7 || s.length() > 15) return false
    val str = s.toString

    val octets = str.split("\\.", -1)
    if (octets.length != 4) return false // must be 4
    
    var i = 0; var valid = true
    while (i < octets.length && valid) {
      val octet = octets(i)
      if (octet.isEmpty || octet.length > 3) return false // must be 3 chars

      valid = if (octet.length == 1) isNumeric(octet.charAt(0))                                       // any
        else if (octet.length == 2) between(octet.charAt(0), '1', '9') && isNumeric(octet.charAt(1))  // 1-9, any
        else if (octet.length == 3) {
          val first = octet.charAt(0); val second = octet.charAt(1)
          if (first == '1') isNumeric(second) && isNumeric(octet.charAt(2))                           // 1, any, any
          else if (first == '2') {
            if (between(second, '0', '4')) isNumeric(octet.charAt(2))                                 // 2, 0-4, any
            else if (second == '5') between(octet.charAt(2), '0', '5')                                // 2,   5, 0-5
            else false
          } else false
        } else false

      i += 1
    }

    valid
  }

  private def isIPv6(s: CharSequence): Boolean = {
    if (s.length() < 2 || s.length() > 45) return false
    if (s == "::") return true

    val str = s.toString
    val compact = str.contains("::")
    if (compact && (str.indexOf("::") != str.lastIndexOf("::"))) return false // not more than 1 compact
    if (str.startsWith(":") && !str.startsWith("::")                          // no single empty prefix
      || str.endsWith(":") && !str.endsWith("::")) return false               // no single empty suffix

    val groups = str.split(":", -1)
    if (groups.length < 3 || groups.length > 8) return false // between 3 and 8 groups

    var i = 0; var valid = true
    while (i < groups.length && valid) {
      val group = groups(i)
      valid = group.length <= 4 && group.forall(isHex)
      i += 1
    }

    if (valid) groups.length == 8 || (groups.length < 8 && compact) // groups are valid and is full, or compact
    else if (i == groups.length &&                                  // last group is invalid
      (groups.length == 7 || (groups.length < 7 && compact)) &&     //  and 7 groups, or less and compact
      groups.last.contains('.')) isIPv4(groups.last)                //  and last group is IPv4
    else false
  }

  private inline def between(c: Int, floor: Int, ceil: Int): Boolean = c >= floor && c <= ceil
  private inline def isHex(c: Int): Boolean = isNumeric(c) || (c >= 0x61 && c <= 0x66)
  private inline def isNumeric(c: Int): Boolean = c >= 0x30 && c <= 0x39
  private inline def in(c: Int, arr: Array[Int]): Boolean = arr.contains(c)
  private inline def isAscii(c: Int): Boolean = c <= 0x7F

  private val UriTemplate_r = "^([^\\p{Cntrl}\"'%<>\\\\^`{|}]|%\\p{XDigit}{2}|\\{[+#./;?&=,!@|]?((\\w|%\\p{XDigit}{2})(\\.?(\\w|%\\p{XDigit}{2}))*(:[1-9]\\d{0,3}|\\*)?)(,((\\w|%\\p{XDigit}{2})(\\.?(\\w|%\\p{XDigit}{2}))*(:[1-9]\\d{0,3}|\\*)?))*})*$".r // https://stackoverflow.com/a/61645285/4814697
  private val Hostname_r = "^[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$".r // https://www.rfc-editor.org/rfc/rfc1123.html https://www.rfc-editor.org/rfc/rfc952 https://stackoverflow.com/a/1418724/4814697

  override def uri: String = "https://json-schema.org/draft/2020-12/meta/format-assertion"
  override def shouldApply(schema: ObjectSchema): Boolean = schema.value.contains(FormatKw)
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): FormatAssertion =
    new FormatAssertion(schema, ctx, path, dynParent)
}
