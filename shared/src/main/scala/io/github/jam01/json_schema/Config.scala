package io.github.jam01.json_schema

/**
 * Reusable validation configuration.
 *
 * @param dialect   to interpret the schema being applied
 * @param format    structure to return by validator visitor
 * @param ffast     whether to fail fast, i.e.: at first error vs fully validate the structure
 * @param allowList [[AllowList]] of JSON Schema annotations to (dis)allow in the result
 * @param maxDepth the maximum schema validator dynamic scope depth, to guard against infinite recursive schemas
 */
case class Config(dialect: Dialect = Dialect.Basic,
                  format: OutputFormat = OutputFormat.Flag,
                  ffast: Boolean = true,
                  allowList: AllowList = AllowList.DropAll,
                  maxDepth: Int = 32) {
}

object Config {
  /**
   * The default configuration.
   */
  val Default: Config = Config()
}

/**
 * Base AllowList class of JSON Schema annotations to (dis)allow during validation.
 */
abstract class AllowList {
  def isAllowed(kw: String): Boolean
  def ifAllowed(kw: String, ann: Value | Null): Value | Null
}

/**
 * AllowList that keeps annotations based on a given set of annotation keywords.
 * @param list the set of keywords
 */
final class Keep(val list: Set[String]) extends AllowList {
  override def isAllowed(kw: String): Boolean = list.contains(kw)
  override def ifAllowed(kw: String, ann: Value | Null): Value | Null =
    if (list.contains(kw)) ann else null
}

/**
 * AllowList that drops annotations based on a given set of annotation keywords.
 * @param list the set of keywords
 */
final class Drop(val list: Set[String]) extends AllowList {
  override def isAllowed(kw: String): Boolean = !list.contains(kw)
  override def ifAllowed(kw: String, ann: Value | Null): Value | Null =
    if (list.contains(kw)) null else ann
}

object AllowList {
  /**
   * AllowList that keeps all annotations.
   */
  object KeepAll extends AllowList:
    inline override def isAllowed(kw: String): Boolean = true
    inline override def ifAllowed(kw: String, ann: Value | Null): Value | Null = ann

  /**
   * AllowList that drops all annotations.
   */
  val DropAll: AllowList = new AllowList:
    inline override def isAllowed(kw: String): Boolean = false
    inline override def ifAllowed(kw: String, ann: Value | Null): Value | Null = null
}
