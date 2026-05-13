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
 * Args: `<outDir> <mainClass> <ir-path>...`
 *
 * Each `<ir-path>` may be a directory or jar to scan for `.sjsir`. An entry of the form `@<file>` is
 * read as a path-separator-delimited classpath string (matches `maven-dependency-plugin:build-classpath`
 * output). Non-existent or non-IR-bearing paths are silently skipped.
 *
 * Output: a single `main.js` (and optional source map) in `<outDir>` using `ModuleKind.NoModule`,
 * so Node can execute it directly without an `--input-type=module` flag.
 */
object Sjsld {
  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      System.err.println("Usage: Sjsld <outDir> <mainClass> <ir-path|@classpath-file>...")
      sys.exit(2)
    }

    val outDir = Paths.get(args(0))
    val mainClass = args(1)
    val irPaths = expand(args.drop(2).toIndexedSeq)
      .map(Paths.get(_))
      .filter(Files.exists(_))
      .distinct

    if (irPaths.isEmpty) {
      System.err.println("sjsld: no existing IR paths supplied")
      sys.exit(2)
    }

    given ExecutionContext = ExecutionContext.global

    val config = StandardConfig()
      .withModuleKind(ModuleKind.NoModule)
      .withCheckIR(false)
    val linker = StandardImpl.linker(config)
    val initializers = List(ModuleInitializer.mainMethodWithArgs(mainClass, "main"))
    val logger = new ScalaConsoleLogger
    Files.createDirectories(outDir)
    val output = PathOutputDirectory(outDir)

    val task: Future[Any] = for {
      pair    <- PathIRContainer.fromClasspath(irPaths)
      irFiles <- Future.sequence(pair._1.map(c => IRContainerImpl.fromIRContainer(c).sjsirFiles)).map(_.flatten)
      report  <- linker.link(irFiles, initializers, output, logger)
    } yield report

    val report = Await.result(task, Duration.Inf)
    println(s"sjsld: linked $mainClass -> $outDir")
    println(report)
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
