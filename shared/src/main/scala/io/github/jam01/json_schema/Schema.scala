package io.github.jam01.json_schema

trait Schema { }

enum BooleanSchema extends Schema {
  case True, False
}

object BooleanSchema {
  def of(bool: Boolean): BooleanSchema = if (bool) True else False
}
