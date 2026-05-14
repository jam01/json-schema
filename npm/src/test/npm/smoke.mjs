// Production smoke: imports the linked npm bundle and exercises validate +
// compile + each output format + format assertion. The path to `main.js` is
// passed as argv so the script can live outside the linked output directory.
import { pathToFileURL } from "node:url";

if (!process.argv[2]) {
  console.error("usage: node smoke.mjs <path/to/main.js>");
  process.exit(2);
}
const { validate, compile } = await import(pathToFileURL(process.argv[2]).href);

let checks = 0, failures = 0;
const check = (cond, msg) => {
  checks++;
  if (!cond) { console.error(`FAIL: ${msg}`); failures++; }
};

// validate (one-shot)
{
  const r = JSON.parse(validate('{"type":"string"}', '"hello"'));
  check(r.valid === true, "type:string accepts string");
}
{
  const r = JSON.parse(validate('{"type":"integer","minimum":10}', "3"));
  check(r.valid === false, "minimum:10 rejects 3");
  // detailed output (default) names the failing keyword somewhere in the tree
  const seen = (function walk(u) {
    if (String(u.keywordLocation || "").includes("/minimum")) return true;
    return (u.details || []).some(walk);
  })(r);
  check(seen, "detailed output names the failing keyword");
}

// compile (reusable)
{
  const v = compile('{"type":"integer"}');
  check(JSON.parse(v("42")).valid === true,  "reused validator accepts 42");
  check(JSON.parse(v('"x"')).valid === false, "reused validator rejects 'x'");
}

// options.format
{
  const r = JSON.parse(validate('{"type":"integer"}', '"x"', { format: "flag" }));
  check(r.valid === false && !r.details, "format:flag has no details");
}
{
  const r = JSON.parse(validate(
    '{"properties":{"name":{"type":"string","minLength":3}}}',
    '{"name":"ab"}',
    { format: "basic" }
  ));
  check(r.valid === false, "format:basic invalid root");
  const flat = r.details.every(u => !u.details || u.details.length === 0);
  check(flat, "format:basic details are flat");
}

// options.formatAssertion
{
  const r1 = JSON.parse(validate('{"format":"date"}', '"not-a-date"'));
  check(r1.valid === true, "format off by default: 'not-a-date' passes");
  const r2 = JSON.parse(validate('{"format":"date"}', '"not-a-date"', { formatAssertion: true }));
  check(r2.valid === false, "formatAssertion=true: 'not-a-date' fails");
}

// options.ffast (no throw out)
{
  const r = JSON.parse(validate('{"type":"integer"}', '"x"', { ffast: true }));
  check(r.valid === false, "ffast=true returns a result (no throw)");
}

console.log(`\n${checks} checks, ${failures} failed`);
if (failures > 0) throw new Error(`${failures} of ${checks} smoke checks failed`);
