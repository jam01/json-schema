package io.github.jam01.json_schema

import org.junit.jupiter.api.{Assertions, Disabled, Test}
import ujson.StringParser
import upickle.core.{ArrVisitor, ObjVisitor}

import scala.collection.mutable
import scala.language.implicitConversions

class PointerVisitorTest {

  @Test @Disabled
  def test(): Unit = {
    val jsonStr = """[0, 1, "bloop", 3, null, 5.5, ["a", "b", "c"], {"foo7": "bar", "arr": [null, null]}]"""
    StringParser.transform(jsonStr, new PointerVisitor())
  }

  @Test
  def main(): Unit = {
    val jsonStr = """[0, 1, "bloop", 3, null, 5.5, ["a", "b", "c"], {"foo7": "bar", "arr": [null, null]}]"""
    val ctx = SimpleContext(Map.empty, Config.Default)

    val res = StringParser.transform(jsonStr, new PointerDelegate(ctx, new CtxPointerVisitor(ctx)))
    Assertions.assertEquals(
      Arr("/0", "/1", "/2", "/3", "/4", "/5",
        Arr("/6/0", "/6/1", "/6/2"),
        Obj("foo7" -> "/7/foo7",
          "arr" -> Arr("/7/arr/0", "/7/arr/1"))
      ), res)
  }
}

class CtxPointerVisitor(ctx: Context) extends JsonVisitor[Value, Value] {
  override def visitNull(index: Int): Value = ctx.instanceLoc.toString
  override def visitFalse(index: Int): Value = ctx.instanceLoc.toString
  override def visitTrue(index: Int): Value = ctx.instanceLoc.toString
  override def visitFloat64(d: Double, index: Int): Value = ctx.instanceLoc.toString
  override def visitInt64(i: Long, index: Int): Value = ctx.instanceLoc.toString
  override def visitString(s: CharSequence, index: Int): Value = ctx.instanceLoc.toString
  override def visitObject(length: Int, index: Int): ObjVisitor[Value, Obj] = new CollectObjVisitor(this)
  override def visitArray(length: Int, index: Int): ArrVisitor[Value, Arr] = new CollectArrVisitor(this)
}
