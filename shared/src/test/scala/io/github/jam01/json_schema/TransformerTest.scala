package io.github.jam01.json_schema

import org.junit.jupiter.api.Test

class TransformerTest {
  val lhm: LinkedHashMap[Any, Any] = LinkedHashMap.empty
//  val strSch: ObjectSchema = ObjectSchema(lhm)

  lhm.put("type", "string")
  lhm.put("pattern", ".*")
  lhm.put("maxLength", 16)
  lhm.put(123, 4)

  @Test
  def simple(): Unit = {
   Transformer.transform(lhm, new PointerVisitor())
//    java.net.URI("about:")
  }
}
