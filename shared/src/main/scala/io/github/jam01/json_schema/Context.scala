package io.github.jam01.json_schema

import scala.collection.mutable

/**
 * Schema validation context.
 */
trait Context {
  /**
   * @return the effective validation configuration
   */
  def config: Config

  /**
   * @return the current location being visited within instance under validation
   */
  def instanceLoc: JsonPointer

  /**
   * Retrieve the referenced schema.
   *
   * @param schemaUri identifier of the schema
   * @return optionally the schema referenced
   */
  def getSch(schemaUri: Uri): Option[Schema]

  /**
   * Retrieve the referenced schema using the given dynamic validation scope.
   *
   * @see <a href="https://json-schema.org/draft/2020-12/json-schema-core#section-7.1">JSON Schema § Lexical Scope and Dynamic Scope </a>
   *
   * @param schemaUri identifier of the dynamic schema
   * @param origin [[Vocab]] from where the dynamic schema is requested
   * @return optionally the schema referenced
   */
  def getDynSch(schemaUri: Uri, origin: Vocab[?]): Option[Schema]

  /**
   * Retrieve the referenced schema or throw an exception if not retrievable.
   *
   * @param schemaUri identifier of the schema
   * @return the referenced schema
   * @throws IllegalArgumentException if not retrievable
   */
  def getSchOrThrow(schemaUri: Uri): Schema =
    getSch(schemaUri) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"Unavailable schema $schemaUri")

  /**
   * Retrieve the referenced schema using the dynamic validation scope or throw an exception if not retrievable.
   *
   * @see <a href="https://json-schema.org/draft/2020-12/json-schema-core#section-7.1">JSON Schema § Lexical Scope and Dynamic Scope </a>
   *
   * @param schemaUri identifier of the schema
   * @param origin [[Vocab]] from where the dynamic schema is requested
   * @return the referenced schema
   * @throws IllegalArgumentException if not retrievable
   */
  def getDynSchOrThrow(schemaUri: Uri, origin: Vocab[?]): Schema =
    getDynSch(schemaUri, origin) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"Unavailable schema $schemaUri")

  /**
   * Register an annotation dependant keyword located within the given schema location.
   *
   * This method will register that some keyword within the schema at the given location is dependent on annotation
   * [[OutputUnit]]s to be identified by the provided predicate function.
   *
   * For example, an <i>Unevaluated</i> vocabulary implementation would register a dependant providing the schema
   * location where such keywords are found and a predicate that matches annotations of keywords <i>items</i>,
   * <i>properties</i>, <i>paternProperties</i>, etc.
   *
   * The predicate is guaranteed to be applied to [[OutputUnit]]s that have an annotation and that are relative to the
   * given schema location, i.e.: immediately within that schema or deeper in its keyword subschemas.
   *
   * @see <a href="https://github.com/orgs/json-schema-org/discussions/491">Annotations as inter-keyword communication</a>
   * @see <a href="https://github.com/orgs/json-schema-org/discussions/236">Annotation collection</a>
   *
   * @param schLocation location of the schema where the dependant keyword is found
   * @param predicate function to filter candidate annotation [[OutputUnit]]s
   */
  def registerDependant(schLocation: JsonPointer, kwLocation: JsonPointer, predicate: JsonPointer => Boolean): Unit 

  /**
   * Retrieve annotations that satisfy the dependant at the given keyword location.
   *
   * @param kwLocation location of the keyword dependant
   * @return the collection of annotation [[OutputUnit]]s that satisfy the dependant
   */
  def getDependenciesFor(kwLocation: JsonPointer): Seq[(JsonPointer, Value)]

  /**
   * Publish output units produced for the given schema location.
   *
   * This method must be called with the results of every vocabulary in order to satisfy annotation dependencies from
   * other vocabularies in the validation scope.
   *
   * @param schLocation of the schema where the units were produced
   * @param units that were produced
   */
  def onVocabResults(schLocation: JsonPointer, units: Seq[OutputUnit]): Unit

  /**
   * Offer a computed annotation to be used by any registered dependant based on location.
   * 
   * @param location the location of the candidate annotation
   * @param value the annotation value
   */
  def offerAnnotation(location: JsonPointer, value: Value): Unit

  /**
   * Notify of output units that were found to be invalid.
   *
   * Validators must call this method with the [[OutputUnit]]s that were computed contingently for unpredictable
   * validation paths that were then found to not have been taken.
   *
   * For example, an <i>Applicator</i> vocabulary implementation might always compute <i>if</i>,<i>then</i> and
   * <i>else</i> as the instance is being visited, in order to assert which results should be included in the result or
   * discarded. But if the validation for their subschemas called [[onVocabResults]], then the discarded branch result
   * must be invalidated.
   *
   * @param invalid units
   */
  def notifyInvalid(invalid: Seq[OutputUnit]): Unit

  /**
   * Signal the end of a schema scope.
   *
   * @param schLocation the location of ending schema scope
   * @param result of the validation
   */
  def onScopeEnd(schLocation: JsonPointer, result: OutputUnit): OutputUnit
}

final case class DefaultContext(private val registry: collection.Map[Uri, Schema],
                          config: Config = Config.Default) extends Context with Tracker {

  private val insloc = mutable.Stack[String]("")
  private var _pointer = JsonPointer.Root
  override def push(ref: String): Unit = {
    insloc.push(ref)
    _pointer = JsonPointer(insloc.reverseIterator.toSeq)
  }
  override def pop: String = {
    val ref = insloc.pop
    _pointer = JsonPointer(insloc.reverseIterator.toSeq)
    ref
  }
  override def instanceLoc: JsonPointer = _pointer

  override def getSch(schemaUri: Uri): Option[Schema] = {
    val frag = schemaUri.getFragment // using decoded fragment as map keys would be unencoded
    if (frag == null) return registry.get(schemaUri)

    if (frag.startsWith("/")) registry.get(schemaUri.withoutFragment)
      .map(sch => sch.schBy(JsonPointer(schemaUri.getFragment)))
    else registry.get(schemaUri).orElse(registry.get(schemaUri.asDyn))
  }

  override def getDynSch(schemaUri: Uri, origin: Vocab[?]): Option[Schema] = {
    val frag = schemaUri.getFragment
    if (frag == null) return None // expecting a fragment
    if (schemaUri.toString.contains("#/")) return getSch(schemaUri.asNonDyn) // anchor expected, try w/o dynamic

    val sch0 = getSch(schemaUri)
    if (sch0.isEmpty) return getSch(schemaUri.asNonDyn) // try w/o dynamic

    val dynScope = mutable.ArrayBuffer(origin.schema)
    var head = origin
    while (head.dynParent.nonEmpty) {
      dynScope.addOne(head.dynParent.get.schema)
      head = head.dynParent.get
    }

    dynScope.reverseIterator
      .map(osch => osch.base.withFragment(frag, true))
      .find(dref => registry.contains(dref))
      .flatMap(dref => registry.get(dref))
  }

  private val dependents: mutable.Map[JsonPointer, mutable.ArrayBuffer[(JsonPointer, JsonPointer => Boolean)]] = mutable.Map.empty // schLoc -> [(kwLoc, pred)]
  private val dependencies: mutable.Map[JsonPointer, mutable.ArrayBuffer[(JsonPointer, Value)]] = mutable.Map.empty // kwLoc -> [(annKwLoc, value)]

  override def registerDependant(schLocation: JsonPointer, kwLocation: JsonPointer, predicate: JsonPointer => Boolean): Unit =
    dependents.getOrElseUpdate(schLocation, new mutable.ArrayBuffer(1)).addOne((kwLocation, predicate)) // perf: chances of there being 2 dependant kws in one sch

  override def getDependenciesFor(kwLocation: JsonPointer): Seq[(JsonPointer, Value)] =
    dependencies.getOrElse(kwLocation, Nil).toSeq

  override def offerAnnotation(location: JsonPointer, value: Value): Unit = {
    if (dependents.isEmpty) return
    
    val it = dependents.iterator
    while (it.hasNext) {
      val (schLoc, schdeps) = it.next()
      val it0 = schdeps.iterator
      while (it0.hasNext) {
        val (depKwLoc, predicate) = it0.next()
        if (schLoc.isRelativeTo(location) && predicate(location))
          dependencies.getOrElseUpdate(depKwLoc, new mutable.ArrayBuffer(5)).addOne((location, value))  // perf: chances of a dependent kw requiring 5+ annotations
      }
    }
  }

  override def onVocabResults(schLocation: JsonPointer, units: Seq[OutputUnit]): Unit = {
    if (dependencies.nonEmpty && units.nonEmpty) 
      notifyInvalid(units.filter(u => !u.vvalid)) // discard dependencies relative to an error
  }

  override def notifyInvalid(invalid: Seq[OutputUnit]): Unit = {
    dependencies.values.foreach(deps => 
      deps.filterInPlace((kwLoc, _) => !invalid.exists(inv => inv.kwLoc.isRelativeTo(kwLoc))))
  }

  override def onScopeEnd(schLocation: JsonPointer, result: OutputUnit): OutputUnit = {
    dependents.remove(schLocation)
    dependencies.remove(schLocation)

    if (!result.vvalid) notifyInvalid(Seq(result))
    result
  }
}

object DefaultContext {
  val Empty: DefaultContext = DefaultContext(Map.empty[Uri, Schema], Config.Default)
}
