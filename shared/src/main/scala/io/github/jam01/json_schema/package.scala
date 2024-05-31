package io.github.jam01

import io.github.jam01.json_schema.SchemaValidator.of
import upickle.core.Visitor

package object json_schema {
  def validator(sch: Schema,
                schemaRegistry: collection.Map[Uri, Schema] = Map.empty,
                config: Config = Config.Default): Visitor[?, OutputUnit] = {
    val ctx = SimpleContext(schemaRegistry, Config.Default)
    PointerDelegate(ctx, SchemaValidator.of(sch, ctx, JsonPointer.Root, None))
  }
  // note that when ffast, don't need pointer tracker
}
