# @jam01/json-schema

JSON Schema 2020-12 validator. The same code that ships as
[`io.github.jam01:json-schema_3`](https://central.sonatype.com/artifact/io.github.jam01/json-schema_3)
on the JVM, compiled to an ES module via Scala.js.

The point: validate on the client and on a Scala backend with **the same
validator** — same bytes of source code — so verdicts and output shape agree
by construction.

## Install

```bash
npm install @jam01/json-schema
```

ES module. Requires Node 14+ or any modern bundler (esbuild, rollup, vite,
webpack).

## Usage

```js
import { validate, compile } from "@jam01/json-schema";

// One-shot.
const out = validate(
  JSON.stringify({ type: "string" }),
  JSON.stringify("hello")
);
const r = JSON.parse(out);
console.log(r.valid);  // true

// Reusable: compile once, call many times.
const v = compile(JSON.stringify({ type: "integer", minimum: 10 }));
console.log(JSON.parse(v("3")).valid);   // false
console.log(JSON.parse(v("42")).valid);  // true
```

Both functions take optional options as the last argument:

```js
const v = compile(schemaJson, {
  format: "detailed",       // "flag" | "basic" | "detailed" | "verbose"
  formatAssertion: true,    // assert format: date, email, etc.
  ffast: false              // stop at first invalid keyword
});
```

The returned string is the JSON Schema 2020-12 spec-format Output (see
[§12.4](https://json-schema.org/draft/2020-12/json-schema-core#section-12.4)).
Call `JSON.parse` on it.

## Size

~315 KiB gzipped after `terser`/`esbuild` minification (~2.1 MiB raw). Much
larger than [ajv](https://ajv.js.org) and friends because the bundle ships
Scala stdlib + collections + boxing infrastructure + `scala-java-time`. The
trade-off buys exact behavior parity with the JVM validator.

## Differences from JVM build

- `format: idn-hostname` and `format: idn-email` (under `formatAssertion: true`)
  are validated structurally only — no IDNA 2008 / RFC 5892 conformance, no
  Punycode decoding, no Bidi rules. The JVM build is fully conformant. If you
  need that, validate on the backend.

## License

Apache-2.0.
