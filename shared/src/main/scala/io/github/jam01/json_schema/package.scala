package io.github.jam01

import io.github.jam01.json_schema.SchemaValidator.of
import upickle.core.Visitor

import scala.collection.mutable

package object json_schema {

  /**
   * Creates a validator visitor for the given schema and configuration..
   * 
   * Visiting this validator results in the validation results for the visited structure.
   *
   * @param schema the schema to apply
   * @param config validation configuration
   * @param schemaRegistry registry to lookup referenced schemas
   * @return the schema validator visitor
   */
  def validator(schema: Schema,
                config: Config = Config.Default,
                schemaRegistry: collection.Map[Uri, Schema] = Map.empty): Visitor[?, OutputUnit] = {
    val ctx = DefaultContext(schemaRegistry, config)
    PointerDelegate(ctx, SchemaValidator.of(schema, ctx))
  }
  
  def from[I](reader: upickle.core.Transformer[I], readable: I, 
                docbase: Uri = Uri.random, registry: mutable.Map[Uri, Schema] = mutable.Map()): Schema = {
    reader.transform(readable, SchemaR(docbase, registry))
  }

  /**
   * Exception thrown when validating with ffast.
   * 
   * @param result the validation result
   */
  class ValidationException(val result: OutputUnit) extends RuntimeException
}
