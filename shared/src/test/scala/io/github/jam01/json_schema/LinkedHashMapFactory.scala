/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import upickle.core.LinkedHashMap

object LinkedHashMapFactory {
  def apply[K, V](elems: (K, V)*): LinkedHashMap[K, V] = from(elems)

  def from[K, V](it: IterableOnce[(K, V)]): LinkedHashMap[K, V] =
    it match {
      case lhm: LinkedHashMap[K, V] => lhm // IDE complains but check works
      case _ => scala.collection.mutable.Growable.from(LinkedHashMap[K, V](), it)
    }
}
