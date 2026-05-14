/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.build

import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.interface.unstable.IRContainerImpl
import org.scalajs.logging.ScalaConsoleLogger

import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Minimal Scala.js linker driver.
 *
 * Two modes:
 *   - `Sjsld <outDir> <mainClass> <ir-path>...` — links a `NoModule` `main.js` with a main-method
 *     initializer (used by the smoke test).
 *   - `Sjsld --esm [--full] <outDir> <ir-path>...` — links an `ESModule` `main.js` with no
 *     initializer; `@JSExportTopLevel` symbols drive what survives. Add `--full` for `fullLink`
 *     (optimized + Closure pass).
 *
 * Each `<ir-path>` may be a directory or jar to scan for `.sjsir`. An entry of the form `@<file>`
 * is read as a path-separator-delimited classpath string (matches `maven-dependency-plugin:build-classpath`
 * output). Non-existent or non-IR-bearing paths are silently skipped.
 */
object Sjsld {
  def main(args: Array[String]): Unit = {
    val (esm, full, rest) = parseFlags(args.toList)

    val minArgs = if (esm) 2 else 3
    if (rest.length < minArgs) {
      System.err.println("Usage: Sjsld <outDir> <mainClass> <ir-path|@classpath-file>...")
      System.err.println("       Sjsld --esm [--full] <outDir> <ir-path|@classpath-file>...")
      sys.exit(2)
    }

    val outDir = Paths.get(rest(0))
    val (mainClass, irStart) = if (esm) (None, 1) else (Some(rest(1)), 2)
    val irPaths = expand(rest.drop(irStart))
      .map(Paths.get(_))
      .filter(Files.exists(_))
      .distinct

    if (irPaths.isEmpty) {
      System.err.println("sjsld: no existing IR paths supplied")
      sys.exit(2)
    }

    given ExecutionContext = ExecutionContext.global

    val base = StandardConfig()
      .withModuleKind(if (esm) ModuleKind.ESModule else ModuleKind.NoModule)
      .withCheckIR(false)
    val config = if (full) base.withSemantics(_.optimized).withClosureCompiler(true) else base
    val linker = StandardImpl.linker(config)
    val initializers = mainClass.map(c => ModuleInitializer.mainMethodWithArgs(c, "main")).toList
    val logger = new ScalaConsoleLogger
    Files.createDirectories(outDir)
    val output = PathOutputDirectory(outDir)

    val task: Future[Any] = for {
      pair    <- PathIRContainer.fromClasspath(irPaths)
      irFiles <- Future.sequence(pair._1.map(c => IRContainerImpl.fromIRContainer(c).sjsirFiles)).map(_.flatten)
      report  <- linker.link(irFiles, initializers, output, logger)
    } yield report

    val report = Await.result(task, Duration.Inf)
    val mode = if (esm) (if (full) "esm/full" else "esm/fast") else "no-module"
    println(s"sjsld[$mode]: linked ${mainClass.getOrElse("(no main)")} -> $outDir")
    println(report)
  }

  private def parseFlags(args: List[String]): (Boolean, Boolean, List[String]) = {
    var esm = false; var full = false
    var rest = args
    var changed = true
    while (changed) {
      changed = false
      rest match {
        case "--esm" :: tail  => esm = true;  rest = tail; changed = true
        case "--full" :: tail => full = true; rest = tail; changed = true
        case _ => ()
      }
    }
    (esm, full, rest)
  }

  private def expand(args: Seq[String]): Seq[String] = {
    val out = mutable.ListBuffer.empty[String]
    args.foreach {
      case s if s.startsWith("@") =>
        val file = Paths.get(s.substring(1))
        Files.readString(file).trim
          .split(File.pathSeparatorChar)
          .foreach(p => if (p.nonEmpty) out += p)
      case s => out += s
    }
    out.toSeq
  }
}
