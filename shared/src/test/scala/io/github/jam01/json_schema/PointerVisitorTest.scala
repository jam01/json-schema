package io.github.jam01.json_schema

import org.junit.jupiter.api.Test
import ujson.StringParser

class PointerVisitorTest {

  @Test
  def test(): Unit = {
    val jsonStr = """[0, 1, "bloop", 3, null, 5.5, ["a", "b", "c"], {"foo7": "bar", "arr": [null, null]}]"""
    StringParser.transform(jsonStr, new PointerVisitor())
  }
}
