package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable

class CompositeVisitor[-T, +J](delegates: Visitor[T, J]*) extends JsonVisitor[Seq[_], Seq[J]] {
  override def visitNull(index: Int): Seq[J] = delegates.map(_.visitNull(index))
  override def visitFalse(index: Int): Seq[J] = delegates.map(_.visitFalse(index))
  override def visitTrue(index: Int): Seq[J] = delegates.map(_.visitTrue(index))
  override def visitFloat64(d: Double, index: Int): Seq[J] = delegates.map(_.visitFloat64(d, index))
  override def visitInt64(i: Long, index: Int): Seq[J] = delegates.map(_.visitInt64(i, index))
  override def visitString(s: CharSequence, index: Int): Seq[J] = delegates.map(_.visitString(s, index))
  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[_], Seq[J]] =
    new CompositeArrVisitor[T, J](delegates.map(_.visitArray(length, index)): _*)
  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[_], Seq[J]] =
    new CompositeObjVisitor[T, J](delegates.map(_.visitObject(length, true, index)): _*)
}

class CompositeArrVisitor[-T, +J](delArrVis: ArrVisitor[T, J]*) extends ArrVisitor[Seq[_], Seq[J]] {
  // unsure why ArrVisitor[Seq[T], Seq[J]] doesn't work when delArrVis if Arr[_, J]
  //  .../json_schema/ObjectSchemaValidator.scala:99:78

  //  Found:    (delegArrVis : Seq[upickle.core.ArrVisitor[?, Boolean]])
  //  Required: Seq[upickle.core.ArrVisitor[Any, Boolean]] |
  //    Array[? <: upickle.core.ArrVisitor[Any, Boolean]]
  //  new MapArrContext[Seq[_], Seq[Boolean], Boolean](new CompositeArrVisitor(delegArrVis: _*), _.forall(identity)) {

  override def subVisitor: Visitor[_, _] =
    new CompositeVisitor(delArrVis.map(_.subVisitor): _*)

  override def visitValue(v: Seq[_], index: Int): Unit = {
    var i = 0
    while (i < delArrVis.length) {
      delArrVis(i).visitValue(v(i).asInstanceOf[T], index)
      i = i + 1
    }
  }

  override def visitEnd(index: Int): Seq[J] = delArrVis.map(_.visitEnd(index))
}

class CompositeObjVisitor[-T, +J](delObjVis: ObjVisitor[T, J]*) extends ObjVisitor[Seq[_], Seq[J]] {
  override def visitKey(index: Int): Visitor[_, _] =
    new CompositeVisitor(delObjVis.map(_.visitKey(index)): _*)

  override def visitKeyValue(v: Any): Unit = {
    var i = 0
    while (i < delObjVis.length) {
      delObjVis(i).visitKeyValue(v.asInstanceOf[Seq[_]](i).asInstanceOf[T])
      i = i + 1
    }
  }

  override def subVisitor: Visitor[_, _] =
    new CompositeVisitor(delObjVis.map(_.subVisitor): _*)

  override def visitValue(v: Seq[_], index: Int): Unit = {
    var i = 0
    while (i < delObjVis.length) {
      delObjVis(i).visitValue(v(i).asInstanceOf[T], index)
      i = i + 1
    }
  }

  override def visitEnd(index: Int): Seq[J] = delObjVis.map(_.visitEnd(index))
}



class CompositeArrVisitorReducer[-T, +J](reducer: Seq[J] => J, delArrVis: ArrVisitor[T, J]*) extends ArrVisitor[Seq[T], J] {
  override def subVisitor: Visitor[_, _] = new CompositeVisitor(delArrVis.map(_.subVisitor): _*)

  override def visitValue(v: Seq[T], index: Int): Unit = {
    var i = 0
    while (i < delArrVis.length) {
      delArrVis(i).visitValue(v(i), index)
      i = i + 1
    }
  }

  override def visitEnd(index: Int): J = reducer(delArrVis.map(_.visitEnd(index)))
}

class CompositeObjVisitorReducer[-T, +J](reducer: Seq[J] => J, delObjVis: ObjVisitor[T, J]*) extends ObjVisitor[Seq[T], J] {

  override def visitKey(index: Int): Visitor[_, _] =
    new CompositeVisitor(delObjVis.map(_.visitKey(index)): _*)

  override def visitKeyValue(v: Any): Unit = {
    var i = 0
    while (i < delObjVis.length) {
      delObjVis(i).visitKeyValue(v.asInstanceOf[Seq[_]](i).asInstanceOf[T])
      i = i + 1
    }
  }

  override def subVisitor: Visitor[_, _] = new CompositeVisitor(delObjVis.map(_.subVisitor): _*)

  override def visitValue(v: Seq[T], index: Int): Unit = {
    var i = 0
    while (i < delObjVis.length) {
      delObjVis(i).visitValue(v(i), index)
      i = i + 1
    }
  }

  override def visitEnd(index: Int): J = reducer(delObjVis.map(_.visitEnd(index)))
}



class CollectObjVisitor[J](v: Visitor[_, _]) extends ObjVisitor[J, collection.Map[String, J]] {
  val lhm: LinkedHashMap[String, J] = LinkedHashMap.empty
  var k: String = "?"

  override def visitKey(index: Int): Visitor[_, _] = StringVisitor
  override def visitKeyValue(v: Any): Unit = k = v.asInstanceOf[String]
  override def subVisitor: Visitor[_, _] = v
  override def visitValue(v: J, index: Int): Unit = lhm.addOne(k, v)
  override def visitEnd(index: Int): collection.Map[String, J] = lhm
}

class CollectArrVisitor[J](v: Visitor[_, _]) extends ArrVisitor[J, collection.Seq[J]] {
  val arr: mutable.Buffer[J] = mutable.Buffer.empty[J]

  override def subVisitor: Visitor[_, _] = v
  override def visitValue(v: J, index: Int): Unit = arr.append(v)
  override def visitEnd(index: Int): collection.Seq[J] = arr
}



object StringVisitor extends SimpleVisitor[Nothing, String] {
  def expectedMsg = "expected string"

  override def visitString(s: CharSequence, index: Int): String = s.toString
}
