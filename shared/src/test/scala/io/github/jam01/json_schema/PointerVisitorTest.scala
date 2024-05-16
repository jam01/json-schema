package io.github.jam01.json_schema

import org.junit.jupiter.api.{Assertions, Disabled, Test}
import ujson.StringParser
import upickle.core.{ArrVisitor, ObjVisitor}

import scala.collection.mutable.ArrayBuffer

class PointerVisitorTest {

  @Test @Disabled
  def test(): Unit = {
    val jsonStr = """[0, 1, "bloop", 3, null, 5.5, ["a", "b", "c"], {"foo7": "bar", "arr": [null, null]}]"""
    StringParser.transform(jsonStr, new PointerVisitor())
  }

  @Test
  def main(): Unit = {
    val jsonStr = """[0, 1, "bloop", 3, null, 5.5, ["a", "b", "c"], {"foo7": "bar", "arr": [null, null]}]"""
    val ctx = Context.empty

    val res = StringParser.transform(jsonStr, new PointerDelegate(new CtxPointerVisitor(ctx), ctx))
    Assertions.assertEquals(
      ArrayBuffer("/0", "/1", "/2", "/3", "/4", "/5",
        ArrayBuffer("/6/0", "/6/1", "/6/2"),
        Map("foo7" -> "/7/foo7",
          "arr" -> ArrayBuffer("/7/arr/0", "/7/arr/1"))
      ), res)
  }
}

class CtxPointerVisitor(ctx: Context) extends JsonVisitor[_, Any] {
  override def visitNull(index: Int): Any = ctx.insPtr
  override def visitFalse(index: Int): Any = ctx.insPtr
  override def visitTrue(index: Int): Any = ctx.insPtr
  override def visitFloat64(d: Double, index: Int): Any = ctx.insPtr
  override def visitInt64(i: Long, index: Int): Any = ctx.insPtr
  override def visitString(s: CharSequence, index: Int): Any = ctx.insPtr
  override def visitObject(length: Int, index: Int): ObjVisitor[_, collection.Map[String, Any]] = ???//new CollectObjVisitor(this)
  override def visitArray(length: Int, index: Int): ArrVisitor[_, collection.Seq[Any]] = ???//new CollectArrVisitor(this)
}

