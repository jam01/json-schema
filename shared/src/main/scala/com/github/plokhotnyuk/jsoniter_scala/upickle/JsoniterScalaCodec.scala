package com.github.plokhotnyuk.jsoniter_scala.upickle

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec}
import upickle.core.Visitor

object JsoniterScalaCodec {

  /**
   * Creates a JSON value codec that parses and serialize to/from circe's JSON AST.
   *
   * @param maxDepth the maximum depth for decoding
   * @param numberParser a function that parses JSON numbers
   * @param visitor ???
   * @return The JSON codec
   */
  def visitorCodec[J](
                       maxDepth: Int = 128,
                       numberParser: (JsonReader, Visitor[_, J]) => J = upickle.jsoniter.JsoniterScalaCodec.defaultNumberParser.asInstanceOf,
                       visitor: Visitor[_, J]): JsonValueCodec[J] =
    new upickle.jsoniter.JsoniterScalaCodec(maxDepth, numberParser, visitor)
}
