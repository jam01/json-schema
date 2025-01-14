/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable

class PointerVisitor(insloc: mutable.Stack[String] = mutable.Stack("")) extends JsonVisitor[Unit, Unit] {

  private def path = {
    insloc.reverseIterator.mkString("/")
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Unit, Unit] = new ObjVisitor[Unit, Unit] {
    override def visitKey(index: Int): Visitor[?, ?] = StringVisitor

    override def visitKeyValue(v: Any): Unit = insloc.push(v.asInstanceOf[String])

    override def subVisitor: Visitor[?, ?] = new PointerVisitor(insloc)

    override def visitValue(v: Unit, index: Int): Unit = insloc.pop

    override def visitEnd(index: Int): Unit = { insloc.pop; println(path) }
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Unit, Unit] = new ArrVisitor[Unit, Unit] {
    private var count: Int = 0
    insloc.push("0")

    override def subVisitor: Visitor[?, ?] = new PointerVisitor(insloc)

    override def visitValue(v: Unit, index: Int): Unit = {
      insloc.pop
      count += 1
      insloc.push(String.valueOf(count))
    }

    override def visitEnd(index: Int): Unit = { insloc.pop; println(path) }
  }

  override def visitNull(index: Int): Unit = println(path)

  override def visitFalse(index: Int): Unit = println(path)

  override def visitTrue(index: Int): Unit = println(path)

  override def visitFloat64(d: Double, index: Int): Unit = println(path)

  override def visitInt64(i: Long, index: Int): Unit = println(path)

  override def visitString(s: CharSequence, index: Int): Unit = println(path)

  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Unit = println(s)
}
