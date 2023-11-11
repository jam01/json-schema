package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable

object LiteralVisitor extends JsonVisitor[_, Any] {
  override def visitNull(index: Int): Any = null
  override def visitFalse(index: Int): Any = false
  override def visitTrue(index: Int): Any = true
  override def visitFloat64(d: Double, index: Int): Any = d
  override def visitInt64(i: Long, index: Int): Any = i
  override def visitString(s: CharSequence, index: Int): Any = s.toString
  override def visitObject(length: Int, index: Int): ObjVisitor[_, collection.Map[String, Any]] = new CollectObjVisitor(LiteralVisitor)
  override def visitArray(length: Int, index: Int): ArrVisitor[_, collection.Seq[Any]] = new CollectArrVisitor(LiteralVisitor)
}

class SchemaR(base: String,
              parent: Option[ObjectSchema] = None,
              reg: mutable.Map[String, Schema] = mutable.Map.empty) extends SimpleVisitor[Any, Schema] {
  override def expectedMsg: String = "expected boolean or object"

  override def visitTrue(index: Int): Schema = True

  override def visitFalse(index: Int): Schema = False

  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Any, Schema] = new ObjVisitor[Any, ObjectSchema] {
    val lhm: LinkedHashMap[String, Any] = LinkedHashMap.empty
    var k: String = "?"
    val sch: ObjectSchema = ObjectSchema(lhm, base, parent)

    override def visitKey(index: Int): Visitor[_, _] = StringVisitor

    override def visitKeyValue(v: Any): Unit = k = v.asInstanceOf[String]

    override def subVisitor: Visitor[_, _] = k match
      // kws with schema
      case "items" | "contains" | "additionalProperties" | "propertyNames" | "if" | "then" | "else" | "not" =>
        new SchemaR(base, Some(sch))
      // kws with map(key -> schema)
      case "$defs" | "properties" | "patternProperties" | "dependentSchemas" => new SimpleVisitor[Schema, collection.Map[String, Schema]] {
        override def expectedMsg: String = "expected object"

        override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Schema, collection.Map[String, Schema]] =
          new CollectObjVisitor(new SchemaR(base, Some(sch)))
      }
      // kws with seq(schema)
      case "prefixItems" | "allOf" | "anyOf" | "oneOf" => new SimpleVisitor[Schema, collection.Seq[Schema]] {
        override def expectedMsg: String = "expected array"

        override def visitArray(length: Int, index: Int): ArrVisitor[Schema, collection.Seq[Schema]] =
          new CollectArrVisitor(new SchemaR(base, Some(sch)))
      }
      // non-schemas
      case _ => LiteralVisitor

    override def visitValue(v: Any, index: Int): Unit = {
      lhm.addOne(k, v)
      if ("$id".equals(k)) reg.addOne(v.asInstanceOf[String], sch) // TODO: probably should conform the uri?
    }

    override def visitEnd(index: Int): ObjectSchema = sch
  }
}
