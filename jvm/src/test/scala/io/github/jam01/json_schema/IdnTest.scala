package io.github.jam01.json_schema

import io.github.jam01.json_schema
import io.github.jam01.json_schema.TestSuiteTest.*
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}
import ujson.StringRenderer
import upickle.core.Visitor

class IdnTest {
  @ParameterizedTest
  @MethodSource(value = Array("args_provider_idn"))
  def idn(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, vis: Visitor[?, OutputUnit]): Unit = {
    val res = try { data.transform(vis) } catch
      case exc: ValidationException => exc.result
    //println(OutputUnitW.transform(res, StringRenderer()).toString)
    Assertions.assertEquals(valid, res.vvalid, path + ": " + desc + ": " + tdesc)
  }
}

object IdnTest {
  def args_provider_idn: java.util.List[Arguments] = {
    val args = new java.util.ArrayList[Arguments]()
    args.addAll(args_provider(resource("test-suite/tests/draft2020-12/optional/format/idn-hostname.json"), Dialect._2020_12_FormatAssertion))
    args.addAll(args_provider(resource("test-suite/tests/draft2020-12/optional/format/idn-email.json"), Dialect._2020_12_FormatAssertion))

    args
  }
}
