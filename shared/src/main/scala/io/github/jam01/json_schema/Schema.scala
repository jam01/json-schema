package io.github.jam01.json_schema

trait Schema { }

enum BooleanSchema extends Schema {
  case True, False
}
