/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.PatternSyntaxException;

/**
 * Runs the corpus through the compiled {@code RegexSupport} — the shipped translation, not a
 * copy of it — and writes {@code id<TAB>match<TAB>formatRegex} to stdout.
 *
 * <p>{@code private[vocab]} is a Scala compile-time check with no bytecode counterpart, so the
 * members are reachable by reflection without opening anything up.
 *
 * <pre>java -cp jvm/target/classes:scala3-library.jar:scala-library.jar RunShipped corpus.tsv</pre>
 */
public final class RunShipped {

    private static String decode(String hex) {
        if (hex.equals("-")) return "";
        StringBuilder sb = new StringBuilder();
        for (String unit : hex.split(" ")) sb.append((char) Integer.parseInt(unit, 16));
        return sb.toString();
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
            System.err.println("usage: RunShipped <corpus.tsv>");
            System.exit(2);
        }

        Class<?> support = Class.forName("io.github.jam01.json_schema.vocab.RegexSupport$");
        Object module = support.getField("MODULE$").get(null);
        Method compilePattern = support.getMethod("compilePattern", String.class);
        Method isValidPattern = support.getMethod("isValidPattern", String.class);
        Method matches = Class.forName("io.github.jam01.json_schema.vocab.CompiledPattern")
                .getMethod("matches", CharSequence.class);

        StringBuilder out = new StringBuilder();
        for (String line : Files.readAllLines(Path.of(args[0]))) {
            if (line.isBlank()) continue;
            String[] fields = line.split("\t");
            String pattern = decode(fields[1]), input = decode(fields[2]);

            String match;
            try {
                Object compiled = compilePattern.invoke(module, pattern);
                match = ((Boolean) matches.invoke(compiled, input)) ? "true" : "false";
            } catch (InvocationTargetException e) {
                match = e.getCause() instanceof PatternSyntaxException
                        ? "SYNTAX"
                        : "ERROR:" + e.getCause().getClass().getSimpleName();
            }

            // format: regex must call a pattern invalid exactly when pattern refuses to compile it.
            String valid;
            try {
                valid = ((Boolean) isValidPattern.invoke(module, pattern)) ? "valid" : "invalid";
            } catch (InvocationTargetException e) {
                valid = "THREW:" + e.getCause().getClass().getSimpleName();
            }

            out.append(fields[0]).append('\t').append(match).append('\t').append(valid).append('\n');
        }
        System.out.print(out);
    }
}
