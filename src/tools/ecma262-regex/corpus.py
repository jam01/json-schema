#!/usr/bin/env python3
#
# Copyright 2023 Jose Montoya
# SPDX-License-Identifier: Apache-2.0
#

"""Generates the ECMA-262 conformance corpus.

Output is tab separated: id, pattern, input, note. Pattern and input are space-separated
UTF-16 code units in hex, so Python, Node and the JVM all decode them identically and no
escaping convention has to survive three languages. A lone `-` is the empty string.

Source is deliberately ASCII: every non-ASCII probe character is built with chr().
"""
import sys

def hx(s):
    # UTF-16 code units, not code points: a supplementary character is a surrogate pair, which
    # is what String.fromCharCode and (char) on the other two sides can reconstruct. surrogatepass
    # keeps the deliberately lone surrogate in the corpus encodable.
    units = s.encode("utf-16-be", "surrogatepass")
    return " ".join("%04X" % int.from_bytes(units[i:i + 2], "big")
                    for i in range(0, len(units), 2)) or "-"

CASES = []

def case(cid, pattern, inp, note=""):
    CASES.append((cid, pattern, inp, note))

# Characters the two dialects treat differently, or that discriminate a Unicode property
# from its ASCII-only POSIX namesake.
NUL, SOH, ETX, BEL, BS, TAB, LF, VT, FF, CR, ESC = (
    chr(0), chr(1), chr(3), chr(7), chr(8), chr(9), chr(10), chr(11), chr(12), chr(13), chr(27))
NEL     = chr(0x0085)   # NEXT LINE: a java.util.regex line terminator, not an ECMA-262 one
NBSP    = chr(0x00A0)
SHY     = chr(0x00AD)
MICRO   = chr(0x00B5)
EACUTE  = chr(0x00E9)
EACUTEU = chr(0x00C9)
DZ      = chr(0x01C5)   # titlecase letter
ALPHA   = chr(0x03B1)
ALEF    = chr(0x05D0)
ARAB0   = chr(0x0660)
NKO0    = chr(0x07C0)
BENG4   = chr(0x09EA)
THAI    = chr(0x0E01)
OGHAM   = chr(0x1680)   # OGHAM SPACE MARK
MVS     = chr(0x180E)   # MONGOLIAN VOWEL SEPARATOR: not whitespace in modern Unicode
RUNIC   = chr(0x16EE)
ZWSP    = chr(0x200B)   # not whitespace
ZWNJ    = chr(0x200C)
ZWJ     = chr(0x200D)
LS      = chr(0x2028)   # LINE SEPARATOR
PS      = chr(0x2029)   # PARAGRAPH SEPARATOR
EM      = chr(0x2003)   # EM SPACE
NNBSP   = chr(0x202F)
MMSP    = chr(0x205F)
IDEOSP  = chr(0x3000)
DASH    = chr(0x2013)   # EN DASH
CIRCLE1 = chr(0x2460)
HAN     = chr(0x4E00)
BOM     = chr(0xFEFF)   # ZWNBSP: in ECMA-262's \s, excluded from Unicode's White_Space
EMOJI   = chr(0x1F600)
REGIONA = chr(0x1F1E6)
MATHA   = chr(0x1D400)
VARSEL  = chr(0xE0100)
BANG2   = chr(0x203C)   # an Emoji that is not Emoji_Presentation

# --------------------------------------------------------------------- the portable subset
# Core 6.4 tells schema authors to keep to these. Both targets must be exact here.
case("sub:literal",   "es",        "expression", "unanchored literal")
case("sub:class",     "[abc]",     "b",          "simple class")
case("sub:range",     "[a-z]",     "q",          "range class")
case("sub:negclass",  "[^abc]",    "d",          "complemented class")
case("sub:negrange",  "[^a-z]",    "Q",          "complemented range")
case("sub:plus",      "a+",        "aaa",        "quantifier +")
case("sub:star",      "a*b",       "b",          "quantifier *")
case("sub:opt",       "ab?c",      "ac",         "quantifier ?")
case("sub:lazy",      "a+?",       "aaa",        "lazy quantifier")
case("sub:exact",     "^a{3}$",    "aaa",        "range quantifier {x}")
case("sub:rangeq",    "^a{2,3}$",  "aaa",        "range quantifier {x,y}")
case("sub:openrange", "^a{2,}$",   "aaaa",       "range quantifier {x,}")
case("sub:groupalt",  "^(ab|cd)$", "cd",         "grouping and alternation")
case("sub:anchors",   "^abc$",     "abc",        "anchors, exact input")

# --------------------------------------------------------------------- anchors
for cid, tail in [("lf", LF), ("crlf", CR + LF), ("cr", CR), ("ls", LS), ("ps", PS), ("2lf", LF + LF)]:
    case("anchor:dollar:" + cid, "^abc$", "abc" + tail, "$ before a trailing line terminator")
case("anchor:dollar:bare",  "c$",   "abc" + LF, "unanchored $ before a trailing LF")
case("anchor:dollar:empty", "^$",   LF,         "^$ against a lone LF")
case("anchor:dollar:mid",   "abc$", "abc" + LF + "x", "$ before a non-final LF")
case("anchor:caret:lf",     "^abc", LF + "abc", "^ after a leading LF")
case("anchor:caret:mid",    "^abc", "x" + LF + "abc", "^ mid-input after an LF")
case("anchor:both",         "^abc$", "x" + LF + "abc" + LF + "y", "^..$ around an interior line")

# --------------------------------------------------------------------- escapes
case("esc:t",        "^\\t$",   TAB,   "\\t")
case("esc:n",        "^\\n$",   LF,    "\\n")
case("esc:r",        "^\\r$",   CR,    "\\r")
case("esc:f",        "^\\f$",   FF,    "\\f")
case("esc:v:vt",     "^\\v$",   VT,    "\\v is the vertical tab")
case("esc:v:lf",     "^\\v$",   LF,    "\\v is not any vertical whitespace")
case("esc:v:cr",     "^\\v$",   CR,    "\\v vs CR")
case("esc:v:ff",     "^\\v$",   FF,    "\\v vs FF")
case("esc:v:ls",     "^\\v$",   LS,    "\\v vs LINE SEPARATOR")
case("esc:v:class",  "^[\\v]$", LF,    "\\v inside a class")
case("esc:v:negcls", "^[^\\v]$", LF,   "\\v inside a negated class")
case("esc:nul",      "^\\0$",   NUL,   "\\0 is NUL")
case("esc:hex",      "^\\x41$", "A",   "\\xNN")
case("esc:uni",      "^\\u0041$", "A", "\\uNNNN")
case("esc:unibrace", "^\\u{1F600}$", EMOJI, "\\u{...} code point escape")
case("esc:cC",       "^\\cC$",  ETX,   "\\c + uppercase")
case("esc:cc",       "^\\cc$",  ETX,   "\\c + lowercase")
case("esc:cJ",       "^\\cJ$",  LF,    "\\cJ is LF")
case("esc:bs:class", "^[\\b]$", BS,    "\\b inside a class is a backspace")
case("esc:wordb",    "\\bfoo\\b", "a foo b", "\\b word boundary")
case("esc:wordb:uni", "\\ba\\b", EACUTE + "a" + EACUTE, "\\b is ASCII-based")

# A literal backslash must not be read as the start of an escape.
case("esc:run:cc", "^\\\\cc$",  "\\cc",   "\\\\cc is a backslash then cc")
case("esc:run:s",  "^\\\\s$",   "\\s",    "\\\\s is a backslash then s")
case("esc:run:ctl", "^\\\\cc$", ETX,      "\\\\cc is not the control escape")

# java.util.regex escapes with no ECMA-262 meaning.
for cid, pat, inp in [
        ("a", "\\a", BEL), ("e", "\\e", ESC), ("A", "\\Aabc", "abc"), ("z", "abc\\z", "abc"),
        ("Z", "abc\\Z", "abc" + LF), ("G", "\\Gabc", "abc"), ("h", "\\h", TAB), ("H", "\\H", "a"),
        ("R", "\\R", CR + LF), ("X", "\\X", "a"), ("QE", "\\Qa+b\\E", "a+b"),
        ("N", "\\N{LATIN SMALL LETTER A}", "a")]:
    case("javaonly:" + cid, pat, inp, "java.util.regex-only escape")
case("javaonly:possessive", "^a*+$",         "aaa", "possessive quantifier")
case("javaonly:inlineflag", "(?i)abc",       "ABC", "inline flags")
case("javaonly:extended",   "(?x)a b",       "ab",  "extended mode")
case("javaonly:atomic",     "(?>a+)b",       "aab", "atomic group")
case("javaonly:isprop",     "^\\p{IsLatin}$", "a",  "\\p{Is...} spelling")

# --------------------------------------------------------------------- classes
case("class:empty",       "^[]$",    "a",  "[] never matches")
case("class:empty:blank", "[]",      "",   "[] never matches, even empty input")
case("class:negempty",    "^[^]$",   "a",  "[^] matches anything")
case("class:negempty:lf", "^[^]$",   LF,   "[^] matches a line terminator")
case("class:bracket",     "^[a[]$",  "[",  "[ inside a class is a literal")
case("class:amp",         "^[a&&b]$", "&", "&& is two literals, not an intersection")
case("class:amp:a",       "^[a&&b]$", "a", "[a&&b] matches a")
case("class:amp:c",       "^[a&&b]$", "c", "[a&&b] does not match c")
case("class:nested",      "^[a-d[x-z]]$", "y", "no nested class union")
case("class:intersect",   "^[a-z&&[^bc]]$", "d", "no class intersection")
case("class:caret:mid",   "^[a^]$",  "^",  "^ not first in a class")
case("class:dollar",      "^[$]$",   "$",  "$ inside a class is a literal")

# --------------------------------------------------------------------- legacy spellings
case("legacy:openbrace", "^a{,3}$", "a{,3}", "{,3} is literal without the u flag")
case("legacy:rbrace",    "^}$",     "}",     "a bare } is literal without u")
case("legacy:rbrack",    "^]$",     "]",     "a bare ] is literal without u")
case("legacy:escdash",   "^\\-$",   "-",     "\\- identity escape")
case("legacy:escslash",  "^\\/$",   "/",     "\\/ identity escape")
case("legacy:escspace",  "^\\ $",   " ",     "escaped space")
case("legacy:nestedq",   "^(a+)+$", "aaa",   "nested quantifier")

# --------------------------------------------------------------------- groups and references
case("group:backref",     "^(a)\\1$",        "aa", "backreference")
case("group:named",       "^(?<x>a)$",       "a",  "named group")
case("group:namedref",    "^(?<x>a)\\k<x>$", "aa", "named backreference")
case("group:namedollar",  "^(?<$x>a)$",      "a",  "a group name java.util.regex rejects")
case("group:lookahead",   "^a(?=b)",         "ab", "lookahead")
case("group:neglookahead", "^a(?!b)",        "ac", "negative lookahead")
case("group:lookbehind",  "(?<=a)b",         "ab", "lookbehind")
case("group:neglookbehind", "(?<!a)b",       "cb", "negative lookbehind")
case("group:forwardref",  "^(\\2)(a)$",      "a",  "forward reference matches empty")
case("group:noref",       "^\\1$",           "",   "reference to a group that does not exist")
case("group:octal",       "^\\101$",         "A",  "octal-looking escape")

# --------------------------------------------------------------------- astral
case("astral:dot",     "^.$",         EMOJI,         ". spans a surrogate pair")
case("astral:dot2",    "^..$",        EMOJI,         ". is one code point, not two units")
case("astral:class",   "^[" + EMOJI + "]$", EMOJI,   "astral char in a class")
case("astral:range",   "^[" + EMOJI + "-" + chr(0x1F64F) + "]$", chr(0x1F607), "astral range")
case("astral:quant",   "^" + EMOJI + "{2}$", EMOJI * 2, "quantifier over an astral char")
case("astral:prop",    "^\\p{L}$",    MATHA,         "astral letter vs \\p{L}")
case("surrogate:lone", "^\\uD83D$",   chr(0xD800),   "lone surrogate")

# --------------------------------------------------------------------- the (?U) trap
# A property name that java.util.regex only knows under UNICODE_CHARACTER_CLASS, next to a
# shorthand that ECMA-262 requires to stay ASCII. Compiling the whole pattern under the flag
# to reach the former silently widens the latter.
case("contam:d:uni",   "^\\p{digit}\\d$",   "4" + BENG4,  "\\d must stay ASCII")
case("contam:d:ascii", "^\\p{digit}\\d$",   "44",         "\\d still matches ASCII")
case("contam:w",       "^\\p{digit}\\w$",   "4" + EACUTE, "\\w must stay ASCII")
case("contam:D",       "^\\p{digit}\\D$",   "4" + BENG4,  "\\D must stay ASCII")
case("contam:W",       "^\\p{digit}\\W$",   "4" + EACUTE, "\\W must stay ASCII")
case("contam:b",       "\\p{digit}\\b",     "4 ",         "\\b must stay ASCII")
case("contam:wcls",    "^\\p{digit}[\\w]$", "4" + EACUTE, "[\\w] must stay ASCII")
case("contam:order",   "^\\d\\p{digit}$",   BENG4 + "4",  "order does not matter")

# --------------------------------------------------------------------- shorthand classes
# Swept over every character where the two dialects could disagree. This is where a bug in
# class-context handling shows up.
WIDE = [
    "a", "Z", "0", "_", " ", TAB, LF, CR, FF, VT, NUL, SOH, "-", ".", "\\", "^", "$", "&", "[",
    NEL, NBSP, SHY, MICRO, EACUTE, EACUTEU, DZ, ALPHA, ALEF, ARAB0, NKO0, BENG4, THAI,
    OGHAM, MVS, RUNIC, ZWSP, ZWNJ, ZWJ, LS, PS, EM, NNBSP, MMSP, IDEOSP, DASH, CIRCLE1,
    HAN, BOM, EMOJI, REGIONA, MATHA, VARSEL, BANG2,
]
SHORTHANDS = [
    ("d", "\\d"), ("D", "\\D"), ("w", "\\w"), ("W", "\\W"), ("s", "\\s"), ("S", "\\S"),
    ("dot", "."), ("v", "\\v"),
    ("s:cls", "[\\s]"), ("S:cls", "[\\S]"), ("s:neg", "[^\\s]"), ("S:neg", "[^\\S]"),
    ("s:mix", "[a\\s]"), ("S:mix", "[a\\S]"), ("s:mixneg", "[^a\\s]"), ("S:mixneg", "[^a\\S]"),
    ("sS", "[\\s\\S]"), ("v:cls", "[\\v]"), ("v:neg", "[^\\v]"),
]
for name, esc in SHORTHANDS:
    for i, ch in enumerate(WIDE):
        case("sh:%s:%d" % (name, i), "^%s$" % esc, ch, "%s vs U+%04X" % (esc, ord(ch)))

# --------------------------------------------------------------------- property escapes
# ECMA-262 22.2.1: binary property names and their aliases.
BINARY = """ASCII ASCII_Hex_Digit AHex Alphabetic Alpha Any Assigned Bidi_Control Bidi_C
Bidi_Mirrored Bidi_M Case_Ignorable CI Cased Changes_When_Casefolded CWCF
Changes_When_Casemapped CWCM Changes_When_Lowercased CWL Changes_When_NFKC_Casefolded CWKCF
Changes_When_Titlecased CWT Changes_When_Uppercased CWU Dash Default_Ignorable_Code_Point DI
Deprecated Dep Diacritic Dia Emoji Emoji_Component EComp Emoji_Modifier EMod
Emoji_Modifier_Base EBase Emoji_Presentation EPres Extended_Pictographic ExtPict Extender Ext
Grapheme_Base Gr_Base Grapheme_Extend Gr_Ext Hex_Digit Hex IDS_Binary_Operator IDSB
IDS_Trinary_Operator IDST ID_Continue IDC ID_Start IDS Ideographic Ideo Join_Control Join_C
Logical_Order_Exception LOE Lowercase Lower Math Noncharacter_Code_Point NChar Pattern_Syntax
Pat_Syn Pattern_White_Space Pat_WS Quotation_Mark QMark Radical Regional_Indicator RI
Sentence_Terminal STerm Soft_Dotted SD Terminal_Punctuation Term Unified_Ideograph UIdeo
Uppercase Upper Variation_Selector VS White_Space space XID_Continue XIDC XID_Start XIDS""".split()

# General_Category: long names, short codes, and the extra value aliases Unicode defines.
GC = """L Letter LC Cased_Letter Lu Uppercase_Letter Ll Lowercase_Letter Lt Titlecase_Letter
Lm Modifier_Letter Lo Other_Letter M Mark Combining_Mark Mn Nonspacing_Mark Mc Spacing_Mark
Me Enclosing_Mark N Number Nd Decimal_Number digit Nl Letter_Number No Other_Number
P Punctuation punct Pc Connector_Punctuation Pd Dash_Punctuation Ps Open_Punctuation
Pe Close_Punctuation Pi Initial_Punctuation Pf Final_Punctuation Po Other_Punctuation
S Symbol Sm Math_Symbol Sc Currency_Symbol Sk Modifier_Symbol So Other_Symbol
Z Separator Zs Space_Separator Zl Line_Separator Zp Paragraph_Separator
C Other Cc Control cntrl Cf Format Cs Surrogate Co Private_Use Cn Unassigned""".split()

SCRIPTS = ["Greek", "Latin", "Han", "Cyrillic", "Arabic", "Hebrew", "Thai", "Common", "Inherited"]

# Enough spread to separate a Unicode property from its ASCII-only POSIX namesake.
NARROW = ["a", "A", "0", EACUTE, EACUTEU, BENG4, " ", NBSP, BOM, EMOJI, BANG2, HAN]

for name in BINARY:
    for i, ch in enumerate(NARROW):
        case("prop:bin:%s:%d" % (name, i), "^\\p{%s}$" % name, ch, "binary property")
for name in GC:
    for i, ch in enumerate(NARROW):
        case("prop:gc:%s:%d" % (name, i), "^\\p{%s}$" % name, ch, "General_Category value")
        case("prop:gceq:%s:%d" % (name, i), "^\\p{General_Category=%s}$" % name, ch,
             "General_Category= long form")
for name in SCRIPTS:
    for i, ch in enumerate(NARROW):
        case("prop:sc:%s:%d" % (name, i), "^\\p{Script=%s}$" % name, ch, "Script=")
        case("prop:scx:%s:%d" % (name, i), "^\\p{Script_Extensions=%s}$" % name, ch,
             "Script_Extensions=")
for name, ch in [("negated", "1"), ("negated:letter", "a")]:
    case("prop:P:%s" % name, "^\\P{L}$", ch, "\\P negates")
case("prop:in:class",    "^[\\p{L}0-9]$", "a", "\\p inside a class")
case("prop:in:negclass", "^[^\\p{L}]$",   "1", "\\p inside a negated class")

def main():
    out = sys.stdout if len(sys.argv) < 2 else open(sys.argv[1], "w", encoding="utf-8")
    seen = set()
    for cid, pattern, inp, note in CASES:
        if cid in seen:
            raise SystemExit("duplicate case id: " + cid)
        seen.add(cid)
        out.write("%s\t%s\t%s\t%s\n" % (cid, hx(pattern), hx(inp), note))
    if out is not sys.stdout:
        out.close()
        sys.stderr.write("%d cases\n" % len(CASES))

if __name__ == "__main__":
    main()
