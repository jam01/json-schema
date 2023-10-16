package upickle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonWriter, writeToString}
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import ujson.StringParser

class JsonVisitorTest {

  @Test
  def simple(): Unit = {
    val jsonStr = """[0, 1, "bloop", 3, null, 5.5, ["a", "b", "c"], {"foo7": "bar", "arr": [null, null]}]"""
//    writeToString()
//    val res = StringParser.transform(jsonStr, new JsonWriterVisitor(JsonWriter()))
//    assertEquals(jsonStr, res)
  }
}
