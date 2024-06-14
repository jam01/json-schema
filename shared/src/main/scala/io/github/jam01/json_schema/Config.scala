package io.github.jam01.json_schema

/**
 * Reusable validation configuration.
 *
 * @param dialect   to interpret the schema being applied
 * @param format    structure to return by validator visitor
 * @param ffast     whether to fail fast, i.e.: at first error vs fully validate the structure
 * @param allowList [[AllowList]] of JSON schema annotations to (dis)allow in the result
 */
case class Config(dialect: Dialect = Dialect._2020_12_Basic,
                  format: OutputFormat = OutputFormat.Flag,
                  ffast: Boolean = true,
                  allowList: AllowList = AllowList.DropAll,
                  maxDepth: Int = 32) {
}

object Config {
  val Default: Config = Config()
}

abstract class AllowList {
  def isAllowed(kw: String): Boolean
  def ifAllowed(kw: String, ann: Value | Null): Value | Null
}

final class Keep(val list: Set[String]) extends AllowList {
  override def isAllowed(kw: String): Boolean = list.contains(kw)
  override def ifAllowed(kw: String, ann: Value | Null): Value | Null =
    if (list.contains(kw)) ann else null
}

final class Drop(val list: Set[String]) extends AllowList {
  override def isAllowed(kw: String): Boolean = !list.contains(kw)
  override def ifAllowed(kw: String, ann: Value | Null): Value | Null =
    if (list.contains(kw)) null else ann
}

object AllowList {
  object KeepAll extends AllowList:
    inline override def isAllowed(kw: String): Boolean = true
    inline override def ifAllowed(kw: String, ann: Value | Null): Value | Null = ann
  val DropAll: AllowList = new AllowList:
    inline override def isAllowed(kw: String): Boolean = false
    inline override def ifAllowed(kw: String, ann: Value | Null): Value | Null = null
}
