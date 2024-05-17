package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, LinkedHashMap, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable

/**
 * A Schema reader.
 *
 * Note this is not a thread-safe implementation as <code>prel</code> is re-used to avoid instantiating
 * multiple readers for each child schema.
 *
 * @param docbase the initial base for the schema
 * @param parent  the parent schema, if any
 * @param prel    the relative path to the schema from the parent, if any
 * @param reg     the schema registry to populate when traversing schemas
 */
class SchemaR(docbase: String,
              reg: mutable.Map[Uri, Schema] = mutable.Map.empty,
              ids: mutable.Buffer[(String, ObjectSchema)] = mutable.ArrayBuffer.empty,
              anchors: mutable.Buffer[(String, Boolean, ObjectSchema)] = mutable.ArrayBuffer.empty,
              parent: Option[ObjectSchema] = None,
              private var prel: Option[String] = None) extends SimpleVisitor[Value, Schema] {
  override def expectedMsg: String = "expected boolean or object"

  override def visitTrue(index: Int): Schema = {
    if (parent.isEmpty) reg.addOne(Uri.of(docbase), TrueSchema); TrueSchema
  }

  override def visitFalse(index: Int): Schema = {
    if (parent.isEmpty) reg.addOne(Uri.of(docbase), FalseSchema); FalseSchema
  }

  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Value, Schema] = new ObjVisitor[Value, ObjectSchema] {
    val lhm: LinkedHashMap[String, Value] = LinkedHashMap()
    var key: String = "?"
    val sch: ObjectSchema = ObjectSchema(lhm, Uri.of(docbase), prel, parent)
    if (parent.isEmpty)
      reg.addOne(Uri.of(docbase), sch)

    override def visitKey(index: Int): Visitor[_, _] = StringVisitor

    override def visitKeyValue(v: Any): Unit = key = v.asInstanceOf[String]

    override def subVisitor: Visitor[_, _] = key match
      // kws with schema
      case "items" | "contains" | "additionalProperties" | "propertyNames" | "if" | "then" | "else" | "not" =>
        new SchemaR(docbase, reg, ids, anchors, Some(sch), Some(s"/$key"))
      // kws with map(key -> schema)
      case "$defs" | "properties" | "patternProperties" | "dependentSchemas" => new SimpleVisitor[Schema, Obj] {
        override def expectedMsg: String = "expected object"

        override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Schema, Obj] =
          new CollectObjVisitor(new SchemaR(docbase, reg, ids, anchors, Some(sch), None)) { // TODO: consider anon implementation
            override def subVisitor: Visitor[_, _] = {
              vis.asInstanceOf[SchemaR].prel = Some(s"/$key/$k");
              super.subVisitor
            }
          }
      }
      // kws with seq(schema)
      case "prefixItems" | "allOf" | "anyOf" | "oneOf" => new SimpleVisitor[Schema, Arr] {
        override def expectedMsg: String = "expected array"

        override def visitArray(length: Int, index: Int): ArrVisitor[Schema, Arr] =
          new CollectArrVisitor(new SchemaR(docbase, reg, ids, anchors, Some(sch), None)) { // TODO: consider anon implementation
            private var nextIdx = 0

            override def subVisitor: Visitor[_, _] = {
              vis.asInstanceOf[SchemaR].prel = Some(s"/$key/$nextIdx");
              super.subVisitor
            }

            override def visitValue(v: Value, index: Int): Unit = {
              super.visitValue(v, index);
              nextIdx += 1
            }
          }
      }
      // non-schemas
      case _ => LiteralVisitor

    override def visitValue(v: Value, index: Int): Unit = {
      lhm.addOne(key, v)
      if ("$id".equals(key)) ids.addOne(v.str, sch)
      else if ("$anchor".equals(key)) anchors.addOne(v.str, false, sch)
      else if ("$dynamicAnchor".equals(key)) anchors.addOne(v.str, true, sch)
    }

    override def visitEnd(index: Int): ObjectSchema = {
      if (parent.isEmpty) {
        ids.foreach {
          case (id, os) => reg.addOne(os.getBase.resolve(id), os)
        }
        anchors.foreach {
          case (anchor, isDyn, os) => reg.addOne(Uri.of(os.getBase.toString + "#" + anchor, isDyn), os)
        }
      }
      sch
    }
  }
}
