/**
 * JSON Schema 2020-12 validator.
 *
 * The same validator that ships as `io.github.jam01:json-schema_3` (JVM) and
 * `io.github.jam01:json-schema_sjs1_3` (Scala.js), compiled to ES module for npm.
 *
 * Both `validate` and `compile` return the spec-format Output as a JSON string —
 * the caller is expected to `JSON.parse` it. The shape depends on the `format`
 * option (default `"detailed"`).
 */

export type OutputFormat = "flag" | "basic" | "detailed" | "verbose";

export interface Options {
  /**
   * Output format. See JSON Schema 2020-12 §12.4.
   * - `"flag"`: single root unit with `valid` only. Cheapest.
   * - `"basic"`: single root unit; `details` is a flat list of keyword-level units.
   * - `"detailed"`: hierarchical, errors + annotated successes. **Default.**
   * - `"verbose"`: hierarchical, every unit retained.
   */
  format?: OutputFormat;

  /** Enable the format-assertion vocabulary (validates `format: …`). Default: `false`. */
  formatAssertion?: boolean;

  /** Stop at the first invalid keyword. Default: `false`. */
  ffast?: boolean;
}

/**
 * One-shot validation. Returns the spec-format Output as a JSON string.
 *
 * Equivalent to `compile(schemaJson, options)(instanceJson)` but constructs a
 * fresh validator each call — use `compile` for repeated validation.
 */
export function validate(
  schemaJson: string,
  instanceJson: string,
  options?: Options
): string;

/**
 * Compile a schema for repeated validation. Returns a function that accepts an
 * instance JSON string and returns the spec-format Output as a JSON string.
 *
 * The returned function is safe to call repeatedly. Not thread-safe (Node is
 * single-threaded; this matters only in worker threads).
 */
export function compile(
  schemaJson: string,
  options?: Options
): (instanceJson: string) => string;
