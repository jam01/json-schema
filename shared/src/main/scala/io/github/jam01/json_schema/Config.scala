package io.github.jam01.json_schema


case class Config(dialect: Dialect = Dialect._2020_12,
                  mode: Mode = Mode.Annotation,
                  struct: OutputStructure = OutputStructure.Detailed,
                  ffast: Boolean = false,
                  allowAnnot: Seq[String] = Nil) {
}

object Config {
  val Default: Config = Config()
}

sealed abstract class OutputStructure {
  def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit
}

object OutputStructure {
  val Flag: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }

  val Basic: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }

  val Detailed: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      val (annots, errs) = units.partition(_.vvalid)
      if (errs.nonEmpty)
        OutputUnit(false, path, None, ctx.currentLoc, errors = errs)
      else
        OutputUnit(true, path, None, ctx.currentLoc, annotations = annots)
    }

  val Verbose: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      Detailed.compose(path, units, ctx)
    }
}

enum Mode {
  case Assertion, Annotation
}

case class Dialect(uri: Uri, vocabularies: Seq[VocabFactory[?]])

object Dialect {
  val _2020_12: Dialect  = Dialect(Uri.of("https://json-schema.org/draft/2020-12/schema"), 
    Seq(vocab.Validation, vocab.Applicator, vocab.Core, vocab.Unevaluated))
}
