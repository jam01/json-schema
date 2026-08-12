/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */

// Runs the corpus through Node's own RegExp under the `u` flag — the ECMA-262 reference the
// JVM target is measured against. Writes "id<TAB>result" to stdout, where result is one of
// true / false / SYNTAX.
//
//   node reference.mjs corpus.tsv > reference.tsv
//
// Node is already a build prerequisite: the js module links with sjsld and runs its smoke test
// under Node.

import { readFileSync } from "node:fs";

const decode = (hex) =>
  hex === "-" ? "" : hex.split(" ").map((u) => String.fromCharCode(parseInt(u, 16))).join("");

const run = (pattern, input) => {
  let re;
  try {
    re = new RegExp(pattern, "u");
  } catch {
    return "SYNTAX";
  }
  return re.test(input) ? "true" : "false";
};

const path = process.argv[2];
if (!path) {
  console.error("usage: node reference.mjs <corpus.tsv>");
  process.exit(2);
}

const out = [];
for (const line of readFileSync(path, "utf8").split("\n")) {
  if (!line.trim()) continue;
  const [id, pattern, input] = line.split("\t");
  out.push(`${id}\t${run(decode(pattern), decode(input))}`);
}
process.stdout.write(out.join("\n") + "\n");
