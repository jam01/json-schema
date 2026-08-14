/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

private[json_schema] object CharSupport {
  // `Char.isDigit` would take non-ASCII decimal digits too; RFC 6901 and JSON's number
  // grammar both mean ASCII 0-9.
  def isAsciiDigit(c: Char): Boolean = c >= '0' && c <= '9'
}
