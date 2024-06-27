/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package upickle.jsoniter

import ujson.Value

object ValueEncoder extends VisitorEncoder[Value](ujson.Value) {
  override def nullValue: Value = ujson.Null
}
