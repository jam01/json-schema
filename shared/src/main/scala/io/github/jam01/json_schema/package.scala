/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01

import upickle.core.Visitor

package object json_schema {

  /**
   * Creates a validator visitor for the given schema and configuration.
   *
   * Visiting this validator results in the validation results for the visited structure.
   *
   * The returned visitor holds a mutable [[DefaultContext]] that tracks per-traversal state
   * (instance location, annotation dependencies). It is **not thread-safe** — do not share a
   * single visitor across threads. The same visitor may be reused for repeated sequential
   * `.transform(...)` calls: the context is reset at the end of each top-level scope, so a
   * `ValidationException` from a failed `ffast` transform does not corrupt the next call.
   *
   * @param schema the schema to apply
   * @param config validation configuration
   * @param registry registry to lookup referenced schemas
   * @return the schema validator visitor
   */
  def validator(schema: Schema,
                config: Config = Config.Default,
                registry: Registry = Registry.Empty): Visitor[?, OutputUnit] = {
    val ctx = DefaultContext(registry, config)
    PointerDelegate(ctx, SchemaValidator(schema, ctx))
  }

  /**
   * Creates a [[Schema]] by transforming the given readable using the given [[upickle.core.Transformer]].
   * 
   * @param reader the transformer to use
   * @param readable the input structure object
   * @param docbase the document base for the resulting Schema
   * @param registry the schema [[Registry]] to populate with identifiable schemas
   * @tparam I the readable type
   * @return
   */
  def from[I](reader: upickle.core.Transformer[I], readable: I, 
                docbase: Uri = Uri.random, registry: Registry = new MutableRegistry): Schema = {
    reader.transform(readable, SchemaR(docbase, registry))
  }
}
