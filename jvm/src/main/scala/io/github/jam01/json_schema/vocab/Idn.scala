package io.github.jam01.json_schema.vocab

import com.networknt.schema.utils.RFC5892

object Idn {
  def isHostname(s: String): Boolean = {
    RFC5892.isValid(s)
  }
}
