package io.github.jam01.json_schema

/**
 * Reusable validation configuration
 *
 * @param dialect to interpret the schema being applied
 * @param output structure to return by validator visitor
 * @param ffast whether to fail fast, i.e.: at first error vs fully validate the structure
 * @param keepAnnotations list of JSON schema annotations to keep in output
 */
case class Config(dialect: Dialect = Dialect._2020_12,
                  mode: Mode = Mode.Annotation,
                  output: OutputStructure = OutputStructure.Detailed,
                  ffast: Boolean = false,
                  keepAnnotations: Seq[String] = Nil) {
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

/**
 * A JSON schema dialect
 *
 * @param uri identifier for this dialect
 * @param vocabularies set of [[VocabFactory]]s in this dialect
 */
case class Dialect(uri: Uri, vocabularies: Seq[VocabFactory[?]])

object Dialect {
  /**
   * A [[Dialect]] implementing the <a href=https://json-schema.org/draft/2020-12/json-schema-core>JSON Schema 2020-12 specification</a>
   * @see <a href=https://json-schema.org/draft/2020-12/schema>JSON Schema 2020-12 meta-schema</a>
   */
  val _2020_12: Dialect  = Dialect(Uri.of("https://json-schema.org/draft/2020-12/schema"), 
    Seq(vocab.Validation, vocab.Applicator, vocab.Core, vocab.Unevaluated))
}
