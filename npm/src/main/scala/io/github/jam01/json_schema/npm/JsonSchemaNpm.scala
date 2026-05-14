/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.npm

import io.github.jam01.json_schema as ipi
import scala.scalajs.js
import scala.scalajs.js.annotation.*

/**
 * Top-level npm facade. Each `@JSExportTopLevel` def becomes a named export of the linked ESM.
 *
 * JS callers do `JSON.parse` on the returned string — the result is a JSON Schema 2020-12
 * spec-format Output unit (Flag / Basic / Detailed / Verbose, per the configured option).
 */
object JsonSchemaNpm {

  /** One-shot validate. Equivalent to `compile(schemaJson, options)(instanceJson)`. */
  @JSExportTopLevel("validate")
  def validate(schemaJson: String, instanceJson: String, options: js.UndefOr[Options]): String =
    compile(schemaJson, options).apply(instanceJson)

  /**
   * Compile a schema for reuse. Returns a function `(instanceJson) -> resultJson` that may be
   * called repeatedly. The underlying validator is sequentially reusable but not thread-safe;
   * Node is single-threaded, so this is a non-issue in practice.
   */
  @JSExportTopLevel("compile")
  def compile(schemaJson: String, options: js.UndefOr[Options]): js.Function1[String, String] = {
    val cfg = configFor(options)
    val reg = new ipi.MutableRegistry
    val sch = ipi.from(ujson.Readable, ujson.Readable.fromString(schemaJson), registry = reg)
    val v = ipi.validator(sch, cfg, reg)
    (instanceJson: String) => {
      val r = try ujson.read(instanceJson).transform(v)
              catch { case e: ipi.ValidationException => e.result }
      ipi.OutputUnitW.transform(r, ujson.StringRenderer()).toString
    }
  }

  /** Options accepted by `validate` and `compile`. All fields are optional. */
  trait Options extends js.Object {
    /** Output format. One of `"flag"`, `"basic"`, `"detailed"`, `"verbose"`. Default: `"detailed"`. */
    val format: js.UndefOr[String] = js.undefined
    /** Enable the format-assertion vocabulary (validates `format: …`). Default: `false`. */
    val formatAssertion: js.UndefOr[Boolean] = js.undefined
    /** Stop at the first invalid keyword. Default: `false`. */
    val ffast: js.UndefOr[Boolean] = js.undefined
  }

  private def configFor(opt: js.UndefOr[Options]): ipi.Config = {
    val o = opt.getOrElse(new Options {})
    val format = o.format.getOrElse("detailed") match
      case "flag" => ipi.OutputFormat.Flag
      case "basic" => ipi.OutputFormat.Basic
      case "detailed" => ipi.OutputFormat.Detailed
      case "verbose" => ipi.OutputFormat.Verbose
      case x => throw new IllegalArgumentException(s"unknown format: $x (expected flag|basic|detailed|verbose)")
    val dialect =
      if (o.formatAssertion.getOrElse(false)) ipi.Dialect.FormatAssertion
      else ipi.Dialect.FullSpec
    ipi.Config(dialect = dialect, format = format, ffast = o.ffast.getOrElse(false))
  }
}
