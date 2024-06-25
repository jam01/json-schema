package io.github.jam01.json_schema

import scala.collection.mutable

trait Registry {
  def contains(key: Uri): Boolean

  def get(key: Uri): Option[Schema]

  @throws[NoSuchElementException]
  def apply(key: Uri): Schema = get(key) match {
    case None => throw new NoSuchElementException("Schema not found: " + key)
    case Some(value) => value
  }

  def addOne(elem: (Uri, Schema)): this.type
}

object Registry {
  val Empty: Registry = new ImmutableRegistry(Nil)
  def from(it: IterableOnce[(Uri, Schema)]): ImmutableRegistry = new ImmutableRegistry(it)
  def apply(elems: (Uri, Schema)*): ImmutableRegistry = from(elems)
}

final class ImmutableRegistry(it: IterableOnce[(Uri, Schema)]) extends Registry {
  private val m = Map.from(it)
  override def contains(key: Uri): Boolean = m.contains(key)
  override def get(key: Uri): Option[Schema] = m.get(key)
  override def addOne(elem: (Uri, Schema)): this.type =
    throw new UnsupportedOperationException("Immutable registry")
}

final class MutableRegistry extends Registry {
  private val m: mutable.Map[Uri, Schema] = mutable.Map()
  override def contains(key: Uri): Boolean = m.contains(key)
  override def get(key: Uri): Option[Schema] = m.get(key)
  override def addOne(elem: (Uri, Schema)): this.type =
    m.addOne(elem); this
}
