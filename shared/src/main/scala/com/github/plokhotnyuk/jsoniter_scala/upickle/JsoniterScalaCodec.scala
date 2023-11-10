package com.github.plokhotnyuk.jsoniter_scala.upickle

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec}
import upickle.core.Visitor

object JsoniterScalaCodec {

  /**
   * Creates a JSON value decoder that parses and composes a given type through a Visitor.
   *
   * @param maxDepth the maximum depth for decoding
   * @param numberParser a function that parses JSON numbers
   * @param visitor the visitor to compose the given type
   * @return A JSON codec that supports decoding only
   */
  def visitorDecoder[J](
                       maxDepth: Int = 128,
                       numberParser: (JsonReader, Visitor[_, _]) => Any = upickle.jsoniter.VisitorDecoder.defaultNumberParser,
                       visitor: Visitor[_, J]): JsonValueCodec[J] =
    new upickle.jsoniter.VisitorDecoder(maxDepth, numberParser, visitor)
}
