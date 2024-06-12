package io.github.jam01.json_schema

/**
 * Reusable validation configuration.
 *
 * @param dialect   to interpret the schema being applied
 * @param format    structure to return by validator visitor
 * @param ffast     whether to fail fast, i.e.: at first error vs fully validate the structure
 * @param allowList [[AllowList]] of JSON schema annotations to (dis)allow in the result
 */
case class Config(dialect: Dialect = Dialect._2020_12,
                  format: OutputFormat = OutputFormat.Flag,
                  ffast: Boolean = true,
                  allowList: AllowList = AllowList.DropAll) {
}

object Config {
  val Default: Config = Config()
}

abstract class AllowList {
  def ifAllowed(kw: String, ann: Value | Null): Value | Null
}

final class Keep(val list: Seq[String]) extends AllowList {
  override def ifAllowed(kw: String, ann: Value | Null): Value | Null =
    if (list.contains(kw)) ann
    else null
}

final class Drop(val list: Seq[String]) extends AllowList {
  override def ifAllowed(kw: String, ann: Value | Null): Value | Null =
    if (list.contains(kw)) null
    else ann
}

object AllowList {
  val KeepAll: AllowList = (kw: String, ann: Value | Null) => ann
  val DropAll: AllowList = (kw: String, ann: Value | Null) => null
}
