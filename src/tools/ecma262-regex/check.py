#!/usr/bin/env python3
#
# Copyright 2023 Jose Montoya
# SPDX-License-Identifier: Apache-2.0
#

"""Checks the JVM target's regex translation against ECMA-262.

    mvn -pl jvm compile          # RunShipped reads the compiled classes
    python3 src/tools/ecma262-regex/check.py

Compares the compiled `RegexSupport` against `reference.tsv` — Node's own RegExp under the `u`
flag, frozen — and against `divergences.tsv`, the set of cases where this target is knowingly
not ECMA-262. Exits non-zero if either the results or the divergence set moved, printing what
changed. Node is only needed to refresh the reference:

    python3 src/tools/ecma262-regex/check.py --refresh

Refresh after changing corpus.py, or to re-measure against a newer Node. Refreshing rewrites
both reference.tsv and divergences.tsv, so the diff is the review.
"""
import argparse
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
REFERENCE = os.path.join(HERE, "reference.tsv")
DIVERGENCES = os.path.join(HERE, "divergences.tsv")
CLASSES = os.path.join(ROOT, "jvm", "target", "classes")


def die(msg):
    sys.stderr.write("error: %s\n" % msg)
    raise SystemExit(2)


def decode(hex_units):
    if hex_units == "-":
        return ""
    return "".join(chr(int(u, 16)) for u in hex_units.split(" "))


def show(s):
    """A readable one-line form: printable ASCII as-is, everything else escaped."""
    return "".join(c if 0x20 < ord(c) < 0x7F else "\\u%04X" % ord(c) for c in s)


def scala_jars():
    m2 = os.path.join(os.path.expanduser("~"), ".m2", "repository", "org", "scala-lang")
    if not os.path.isdir(m2):
        die("no local Maven repository at %s; run a build first" % m2)
    found = {}
    for dirpath, _, filenames in os.walk(m2):
        for f in filenames:
            for key, prefix in (("scala3", "scala3-library_3-"), ("scala2", "scala-library-2.")):
                if f.startswith(prefix) and f.endswith(".jar"):
                    found.setdefault(key, []).append(os.path.join(dirpath, f))
    for key in ("scala3", "scala2"):
        if key not in found:
            die("%s library jar not found under %s; run a build first" % (key, m2))
    return [sorted(found[key])[-1] for key in ("scala3", "scala2")]


def run_corpus(tmpdir):
    corpus = os.path.join(tmpdir, "corpus.tsv")
    subprocess.run([sys.executable, os.path.join(HERE, "corpus.py"), corpus],
                   check=True, stderr=subprocess.DEVNULL)
    return corpus


def run_shipped(corpus):
    if not os.path.isdir(CLASSES):
        die("%s does not exist; run `mvn -pl jvm compile` first" % CLASSES)
    cp = os.pathsep.join([CLASSES] + scala_jars())
    proc = subprocess.run(["java", "-cp", cp, os.path.join(HERE, "RunShipped.java"), corpus],
                          check=True, capture_output=True, text=True)
    results = {}
    for line in proc.stdout.splitlines():
        cid, match, valid = line.split("\t")
        results[cid] = (match, valid)
    return results


def run_reference(corpus):
    proc = subprocess.run(["node", os.path.join(HERE, "reference.mjs"), corpus],
                          check=True, capture_output=True, text=True)
    return dict(line.split("\t") for line in proc.stdout.splitlines())


def load_corpus(corpus):
    cases = {}
    order = []
    for line in open(corpus, encoding="utf-8"):
        if not line.strip():
            continue
        cid, pattern, inp, note = line.rstrip("\n").split("\t")
        cases[cid] = (decode(pattern), decode(inp), note)
        order.append(cid)
    return cases, order


def divergence_lines(cases, order, reference, shipped):
    """One line per knowingly-divergent pattern, or per case where the two actually disagree.

    A pattern this target rejects outright collapses to a single line: which input it was
    tried against says nothing extra. A pattern that compiles but matches differently is
    listed per case, since exactly which input flips is the interesting part.
    """
    by_pattern = {}
    pattern_order = []
    for cid in order:
        pattern = cases[cid][0]
        if pattern not in by_pattern:
            by_pattern[pattern] = []
            pattern_order.append(pattern)
        by_pattern[pattern].append(cid)

    lines = []
    for pattern in pattern_order:
        ids = by_pattern[pattern]
        diverging = [c for c in ids if shipped[c][0] != reference[c]]
        if not diverging:
            continue
        ours = {shipped[c][0] for c in ids}
        theirs = {reference[c] for c in ids}
        note = cases[ids[0]][2]
        if ours == {"SYNTAX"} and theirs != {"SYNTAX"}:
            lines.append("%s\trejected\t\t\t\t%s" % (show(pattern), note))
        elif theirs == {"SYNTAX"} and "SYNTAX" not in ours:
            lines.append("%s\taccepted\t\t\t\t%s" % (show(pattern), note))
        else:
            for c in diverging:
                lines.append("%s\tmismatch\t%s\t%s\t%s\t%s" % (
                    show(pattern), show(cases[c][1]), reference[c], shipped[c][0], cases[c][2]))
    return lines


HEADER = [
    "# Cases where this target is knowingly not ECMA-262. Regenerate with check.py --refresh.",
    "# pattern\tkind\tinput\tECMA-262\tours\tnote",
    "#",
    "# rejected  a valid ECMA-262 pattern java.util.regex cannot express: an error, not a wrong",
    "#           answer, and `format: regex` reports it invalid too",
    "# accepted  a pattern ECMA-262 rejects under `u` that this target takes, with the meaning",
    "#           ECMA-262 gives it without `u`",
    "# mismatch  both compile it and they match differently -- the only silent kind",
]


def read_baseline(path):
    if not os.path.exists(path):
        return None
    return [l.rstrip("\n") for l in open(path, encoding="utf-8") if not l.startswith("#")
            and l.strip()]


def write_baseline(path, lines):
    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(HEADER) + "\n")
        f.write("\n".join(lines) + "\n")


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--refresh", action="store_true",
                    help="re-measure the ECMA-262 reference with Node and rewrite both baselines")
    args = ap.parse_args()

    with tempfile.TemporaryDirectory() as tmpdir:
        corpus = run_corpus(tmpdir)
        cases, order = load_corpus(corpus)

        if args.refresh:
            reference = run_reference(corpus)
            with open(REFERENCE, "w", encoding="utf-8") as f:
                f.write("# ECMA-262 truth: node's RegExp under the `u` flag. Regenerate with"
                        " check.py --refresh.\n")
                for cid in order:
                    f.write("%s\t%s\n" % (cid, reference[cid]))
        else:
            if not os.path.exists(REFERENCE):
                die("%s missing; run with --refresh (needs node)" % REFERENCE)
            reference = dict(l.rstrip("\n").split("\t") for l in open(REFERENCE, encoding="utf-8")
                             if not l.startswith("#") and l.strip())

        missing = [c for c in order if c not in reference]
        if missing:
            die("%d case(s) have no reference result, e.g. %s; run with --refresh"
                % (len(missing), missing[0]))

        shipped = run_shipped(corpus)

    total = len(order)
    agree = sum(1 for c in order if shipped[c][0] == reference[c])
    disagree_valid = [c for c in order if (shipped[c][0] == "SYNTAX") != (shipped[c][1] == "invalid")]
    errored = [c for c in order if shipped[c][0].startswith("ERROR")]

    lines = divergence_lines(cases, order, reference, shipped)
    baseline = read_baseline(DIVERGENCES)

    print("%d cases, %d match ECMA-262, %d divergent pattern%s"
          % (total, agree, len(lines), "" if len(lines) == 1 else "s"))

    failed = False
    if errored:
        failed = True
        print("\n%d case(s) raised something other than PatternSyntaxException:" % len(errored))
        for c in errored[:10]:
            print("  %-28s %s" % (c, shipped[c][0]))
    if disagree_valid:
        failed = True
        print("\n%d case(s) where `format: regex` and `pattern` disagree:" % len(disagree_valid))
        for c in disagree_valid[:10]:
            print("  %-28s pattern=%s format:regex=%s" % (c, shipped[c][0], shipped[c][1]))

    if args.refresh:
        write_baseline(DIVERGENCES, lines)
        print("\nwrote %s and %s -- review the diff" % (os.path.relpath(REFERENCE, ROOT),
                                                        os.path.relpath(DIVERGENCES, ROOT)))
        return 1 if failed else 0
    if baseline is None:
        die("%s missing; run with --refresh" % DIVERGENCES)
    elif baseline != lines:
        failed = True
        gone = [l for l in baseline if l not in lines]
        new = [l for l in lines if l not in baseline]
        print("\nthe divergence set moved:")
        for l in gone:
            print("  -%s" % l)
        for l in new:
            print("  +%s" % l)
        print("\nIf this is intended, rerun with --refresh and review the diff. Every line here"
              "\nshould be reflected in README section Regular expressions.")

    if failed:
        return 1
    print("divergence set unchanged")
    return 0


if __name__ == "__main__":
    sys.exit(main())
