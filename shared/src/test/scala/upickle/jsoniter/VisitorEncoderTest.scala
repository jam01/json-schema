package upickle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import io.github.jam01.json_schema.Transformer
import org.junit.jupiter.api.Assertions.{assertThrows, assertTrue}
import org.junit.jupiter.api.Test
import org.skyscreamer.jsonassert.JSONAssert
import upickle.core.NoOpVisitor

import scala.collection.immutable.Seq

class VisitorEncoderTest {
  val obj: Seq[Any] =
    Seq(0, 1, "bloop",
      3, null, 5.5d,
      Seq("a", "b", "c"),
      Map(
        "foo7" -> "bar",
        "arr" -> Seq(null, null)
      )
    )

  @Test
  def main(): Unit = {
    val jsonStr = """[0, 1, "bloop", 3, null, 5.5, ["a", "b", "c"], {"foo7": "bar", "arr": [null, null]}]"""

    val res = writeToString(obj)(VisitorEncoder)
    JSONAssert.assertEquals(jsonStr, res, true)
  }
}
