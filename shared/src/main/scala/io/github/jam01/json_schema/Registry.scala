/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import scala.collection.mutable

/**
 * A schema registry.
 */
trait Registry {
  /**
   * Whether this registry can provide the identified schema.
   * 
   * @param schemaUri the schema Uri
   * @return true if it contains the schema, false otherwise
   */
  def contains(schemaUri: Uri): Boolean

  /**
   * Optionally retrieve the identified schema.
   *
   * @param schemaUri the schema Uri
   * @return an Option of the schema, or None if not retrievable.
   */
  def get(schemaUri: Uri): Option[Schema]

  /**
   * Optionally retrieve the identified schema, throwing if not available.
   * 
   * @param schemaUri the schema Uri
   * @throws NoSuchElementException if the schema is not available
   * @return the identified schema
   */
  @throws[NoSuchElementException]
  def apply(schemaUri: Uri): Schema = get(schemaUri) match {
    case None => throw new NoSuchElementException("Schema not found: " + schemaUri)
    case Some(value) => value
  }

  /**
   * Add a schema to this registry.
   * 
   * @param elem the identifier and schema tuple
   * @return this registry
   */
  def addOne(elem: (Uri, Schema)): this.type
}

object Registry {
  /**
   * An empty immutable registry.
   */
  val Empty: Registry = new ImmutableRegistry(Nil)
  def from(it: IterableOnce[(Uri, Schema)]): ImmutableRegistry = new ImmutableRegistry(it)
  def apply(elems: (Uri, Schema)*): ImmutableRegistry = from(elems)
}

/**
 * An immutable registry with the given identifier and schema tuples.
 * 
 * @param it tuples iterator
 */
final class ImmutableRegistry(it: IterableOnce[(Uri, Schema)]) extends Registry {
  private val m = Map.from(it)
  override def contains(key: Uri): Boolean = m.contains(key)
  override def get(key: Uri): Option[Schema] = m.get(key)
  override def addOne(elem: (Uri, Schema)): this.type =
    throw new UnsupportedOperationException("Immutable registry")
}

/**
 * A mutable registry.
 */
final class MutableRegistry extends Registry {
  private val m: mutable.Map[Uri, Schema] = mutable.Map()
  override def contains(key: Uri): Boolean = m.contains(key)
  override def get(key: Uri): Option[Schema] = m.get(key)
  override def addOne(elem: (Uri, Schema)): this.type =
    m.addOne(elem); this
}
