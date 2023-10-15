package io.github.jam01.json_schema

import scala.collection.mutable.GrowableBuilder
import scala.collection.{MapFactory, mutable}

// TODO: license from upickle
class LinkedHashMap[K, V](underlying: java.util.LinkedHashMap[K, V]) extends mutable.Map[K, V] {
  override def mapFactory: MapFactory[mutable.Map] = LinkedHashMap
  override def addOne(elem: (K, V)): this.type = {
    _put(elem._1, elem._2)
    this
  }

  override def subtractOne(elem: K): this.type = {
    underlying.remove(elem)
    this
  }

  override def get(key: K): Option[V] = Option(underlying.get(key))

  def getValue(key: K): V = underlying.get(key)

  override def put(key: K, value: V): Option[V] = Option(_put(key, value))

  private def _put(key: K, value: V): V = {
    if (key == null)
      throw new NullPointerException("null keys are not allowed")
    underlying.put(key, value)
  }

  override def iterator: Iterator[(K, V)] = {
    val it = underlying.keySet().iterator()
    new Iterator[(K, V)] {
      def hasNext: Boolean = it.hasNext

      def next(): (K, V) = {
        val key = it.next()
        key -> underlying.get(key)
      }
    }
  }
}

object LinkedHashMap extends MapFactory[LinkedHashMap] {
  def apply[K, V](): LinkedHashMap[K, V] =
    new LinkedHashMap[K, V](new java.util.LinkedHashMap[K, V])

  def apply[K, V](items: IterableOnce[(K, V)]): LinkedHashMap[K, V] = {
    items match {
      case lhm: LinkedHashMap[K, V] => lhm // IDE complains but check works
      case _ => mutable.Growable.from(empty[K, V], items)
    }
  }

  override def empty[K, V]: LinkedHashMap[K, V] = LinkedHashMap()

  override def from[K, V](it: IterableOnce[(K, V)]): LinkedHashMap[K, V] = LinkedHashMap(it)

  override def newBuilder[K, V]: mutable.Builder[(K, V), LinkedHashMap[K, V]] =
    new mutable.GrowableBuilder(empty[K, V])
}
