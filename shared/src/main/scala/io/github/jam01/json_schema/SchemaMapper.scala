package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

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
              private var prel: Option[String] = None) extends SimpleVisitor[Any, Schema] {
  override def expectedMsg: String = "expected boolean or object"

  override def visitTrue(index: Int): Schema = {
    if (parent.isEmpty) reg.addOne(Uri.of(docbase), True); True
  }

  override def visitFalse(index: Int): Schema = {
    if (parent.isEmpty) reg.addOne(Uri.of(docbase), False); False
  }

  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Any, Schema] = new ObjVisitor[Any, ObjectSchema] {
    val lhm: LinkedHashMap[String, Any] = LinkedHashMap.empty
    var key: String = "?"
    val sch: ObjectSchema = ObjectSchema(lhm, Uri.of(docbase), prel, parent)
    if (parent.isEmpty) reg.addOne(Uri.of(docbase), sch)

    override def visitKey(index: Int): Visitor[_, _] = StringVisitor

    override def visitKeyValue(v: Any): Unit = key = v.asInstanceOf[String]

    override def subVisitor: Visitor[_, _] = key match
      // kws with schema
      case "items" | "contains" | "additionalProperties" | "propertyNames" | "if" | "then" | "else" | "not" =>
        new SchemaR(docbase, reg, ids, anchors, Some(sch), Some(s"/$key"))
      // kws with map(key -> schema)
      case "$defs" | "properties" | "patternProperties" | "dependentSchemas" => new SimpleVisitor[Schema, collection.Map[String, Schema]] {
        override def expectedMsg: String = "expected object"

        override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Schema, collection.Map[String, Schema]] =
          new CollectObjVisitor[Schema](new SchemaR(docbase, reg, ids, anchors, Some(sch), None)) { // TODO: consider anon implementation
            override def subVisitor: Visitor[_, _] = {
              vis.asInstanceOf[SchemaR].prel = Some(s"/$key/$k");
              super.subVisitor
            }
          }
      }
      // kws with seq(schema)
      case "prefixItems" | "allOf" | "anyOf" | "oneOf" => new SimpleVisitor[Schema, collection.Seq[Schema]] {
        override def expectedMsg: String = "expected array"

        override def visitArray(length: Int, index: Int): ArrVisitor[Schema, collection.Seq[Schema]] =
          new CollectArrVisitor[Schema](new SchemaR(docbase, reg, ids, anchors, Some(sch), None)) { // TODO: consider anon implementation
            private var nextIdx = 0

            override def subVisitor: Visitor[_, _] = {
              vis.asInstanceOf[SchemaR].prel = Some(s"/$key/$nextIdx");
              super.subVisitor
            }

            override def visitValue(v: Schema, index: Int): Unit = {
              super.visitValue(v, index);
              nextIdx += 1
            }
          }
      }
      // non-schemas
      case _ => LiteralVisitor

    override def visitValue(v: Any, index: Int): Unit = {
      lhm.addOne(key, v)
      if ("$id".equals(key)) ids.addOne(v.asInstanceOf[String], sch)
      else if ("$anchor".equals(key)) anchors.addOne(v.asInstanceOf[String], false, sch)
      else if ("$dynamicAnchor".equals(key)) anchors.addOne(v.asInstanceOf[String], true, sch)
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
