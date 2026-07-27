/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test

/**
 * Regression coverage for `Config.resolveDialect`: a schema using a custom `$schema` whose
 * meta-schema declares `$vocabulary` without the Validation vocabulary must have Validation
 * keywords (e.g. `minimum`) ignored, when resolution is opted into via the public API.
 */
class DialectResolutionTest {
  private val MetaUri = Uri("https://example/meta-no-validation")
  private val MetaSchemaNoValidation =
    """{"$vocabulary": {
      |  "https://json-schema.org/draft/2020-12/vocab/core": true,
      |  "https://json-schema.org/draft/2020-12/vocab/applicator": true
      |}}""".stripMargin

  private def mkRegistry(): Registry = {
    val registry = new MutableRegistry
    ujson.Readable.fromString(MetaSchemaNoValidation).transform(SchemaR(docbase = MetaUri, registry = registry))
    registry
  }

  private val SchemaJson =
    """{"$schema": "https://example/meta-no-validation", "properties": {"numberProperty": {"minimum": 10}}}"""

  private def isValid(v: upickle.core.Visitor[?, OutputUnit], instanceJson: String): Boolean = {
    val res = try ujson.Readable.fromString(instanceJson).transform(v)
    catch { case e: ValidationException => e.result }
    res.vvalid
  }

  @Test def resolveDialect_ignores_validation_keywords_when_vocabulary_omits_it(): Unit = {
    val registry = mkRegistry()
    val schema = ujson.Readable.fromString(SchemaJson).transform(SchemaR())

    val resolvedValidator = validator(schema, Config(resolveDialect = true), registry)
    assertTrue(isValid(resolvedValidator, """{"numberProperty": 1}"""),
      "minimum must be ignored: metaschema's $vocabulary omits Validation")
  }

  @Test def default_config_still_enforces_validation_keywords(): Unit = {
    val registry = mkRegistry()
    val schema = ujson.Readable.fromString(SchemaJson).transform(SchemaR())

    val defaultValidator = validator(schema, Config.Default, registry)
    assertFalse(isValid(defaultValidator, """{"numberProperty": 1}"""),
      "without resolveDialect, minimum should still be enforced (unchanged default behavior)")
  }
}
