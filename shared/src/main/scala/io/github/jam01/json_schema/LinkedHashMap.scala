package io.github.jam01.json_schema

import java.util
import scala.collection.mutable.GrowableBuilder
import scala.collection.{MapFactory, MapFactoryDefaults, mutable}

// TODO: ref to upickle
// TODO: consier re-using upickle one 
class LinkedHashMap[K, V](underlying: java.util.LinkedHashMap[K, V]) extends mutable.AbstractMap[K, V]
  with mutable.MapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
  with MapFactoryDefaults[K, V, LinkedHashMap, Iterable] {
  override def mapFactory: MapFactory[LinkedHashMap] = LinkedHashMap
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

  def containsKey(key: K): Boolean = underlying.containsKey(key)

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
  override def empty[K, V]: LinkedHashMap[K, V] = new LinkedHashMap[K, V](new util.LinkedHashMap[K, V]())

  override def from[K, V](it: IterableOnce[(K, V)]): LinkedHashMap[K, V] =
    it match {
      case lhm: LinkedHashMap[K, V] => lhm // IDE complains but check works
      case _ => mutable.Growable.from(empty[K, V], it)
    }

  override def newBuilder[K, V]: mutable.Builder[(K, V), LinkedHashMap[K, V]] =
    new mutable.GrowableBuilder(empty[K, V])
}
