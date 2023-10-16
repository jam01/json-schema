package io.github.jam01.json_schema

import org.junit.jupiter.api.Test

class ReaderTest {
  val lhm: LinkedHashMap[String, Any] = LinkedHashMap.empty
  val strSch: ObjectSchema = ObjectSchema(lhm)

  lhm.put("type", "string")
  lhm.put("pattern", ".*")
  lhm.put("maxLength", 16)
  lhm.put("minLength", 4)

  @Test
  def simple(): Unit = {
   Reader.transform(lhm, new PointerVisitor())
  }
}
