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
   * @return whether verbose output is required
   *
   * @see <a href=""></a>
   */
  def isVerbose: Boolean = config.output == OutputStructure.Verbose

  /**
   * @return the current location being visited within instance under validation
   */
  def currentLoc: JsonPointer

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
   * For example, the <i>Unevaluated</i> vocabulary would register a dependant providing the schema location where such
   * keywords are found and a predicate that matches annotations of keywords <i>items</i>, <i>properties</i>,
   * <i>paternProperties</i>, etc.
   *
   * The predicate is guaranteed to be applied to [[OutputUnit]]s that have an annotation and that are relative to the
   * given schema location, i.e.: immediately within that location or deeper in subschemas.
   *
   * @see <a href="https://github.com/orgs/json-schema-org/discussions/491">Annotations as inter-keyword communication</a>
   * @see <a href="https://github.com/orgs/json-schema-org/discussions/236">Annotation collection</a>
   *
   * @param schLocation location of the schema where the dependant keyword is found
   * @param predicate function to filter candidate annotation [[OutputUnit]]s
   */
  def registerDependant(schLocation: JsonPointer, predicate: OutputUnit => Boolean): Unit

  /**
   * Retrieve annotations that satisfy one or more dependants located within the given schema location.
   *
   * @param schLocation location of the schema where dependants may be found
   * @return the collection of annotation [[OutputUnit]]s that satisfy some dependant in the given schema location
   */
  def getDependenciesFor(schLocation: JsonPointer): collection.Seq[OutputUnit]

  /**
   * Publish output units produced for the given schema location.
   *
   * @param schLocation of the schema where the units were produced
   * @param units that were produced
   */
  def publish(schLocation: JsonPointer, units: collection.Seq[OutputUnit]): Unit

  /**
   * Discard collected annotation dependencies that are related to the given output units.
   *
   * @param invalid units
   */
  def discardRelatives(invalid: collection.Seq[OutputUnit]): Unit

  /**
   * Signal the end of a schema scope.
   *
   * @param schLocation the location of ending schema scope
   * @param result of the validation
   */
  def endScope(schLocation: JsonPointer, result: OutputUnit): Unit // should be internal?
}

case class SimpleContext(private val reg: collection.Map[Uri, Schema],
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
  override def currentLoc: JsonPointer = _pointer
  override def pointer: JsonPointer = _pointer

  def getSch(schemaUri: Uri): Option[Schema] = {
    val ptr = schemaUri.toString.lastIndexOf("#/")
    if (ptr == -1)
      reg.get(schemaUri).orElse(reg.get(schemaUri.asDyn))
    else
      reg.get(schemaUri.withoutFragment)
        .map(sch => sch.schBy(JsonPointer(schemaUri.uri.getFragment))) // using decoded fragment as map values would be unencoded
  }

  def getDynSch(schemaUri: Uri, origin: Vocab[?]): Option[Schema] = {
    if (schemaUri.toString.contains("#/")) return getSch(schemaUri.asStatic)

    val sch0 = getSch(schemaUri)
    if (sch0.isEmpty) return getSch(schemaUri.asStatic) // trying w/o dynamic

    val dynScope = mutable.ArrayBuffer(origin.schema)
    var head = origin
    while (head.dynParent.nonEmpty) {
      dynScope.addOne(head.dynParent.get.schema)
      head = head.dynParent.get
    }

    dynScope.reverseIterator
      .map(osch => osch.base.withFragment(schemaUri.uri.getFragment, true))
      .find(dref => reg.contains(dref))
      .flatMap(dref => reg.get(dref))
  }

  private val dependents: mutable.Map[JsonPointer, mutable.Buffer[OutputUnit => Boolean]] = mutable.Map()
  private val dependencies: mutable.Map[JsonPointer, mutable.ArrayBuffer[OutputUnit]] = mutable.Map()

  override def registerDependant(schLocation: JsonPointer, predicate: OutputUnit => Boolean): Unit = {
    dependents.getOrElseUpdate(schLocation, mutable.ArrayBuffer()).addOne(predicate)
  }

  override def getDependenciesFor(schLocation: JsonPointer): collection.Seq[OutputUnit] =
    dependencies.getOrElse(schLocation, Nil)

  override def publish(schLocation: JsonPointer, units: collection.Seq[OutputUnit]): Unit = {
    if (units.isEmpty) return

    // this checks if any unit satisfies dependants and registers the dependency
    dependents.withFilter((depPath, _) => depPath.isRelative(schLocation)) // filtering filters relative to given path
      .flatMap((depPath, preds) => preds.map(p => (depPath, p)))        // flattening (path, filters) to (path, filter)
      .foreach((dependent, fltr) => units
        .filter(u => u.vvalid && u.annotation.nonEmpty && fltr(u))      // applying filter to every unit
        .foreach(u => addDependencyFor(dependent, u)))                  // registering dependency

    // this discards registered dependencies that are found to be relative to an error
    if (dependencies.nonEmpty) {
      val inv = units.filter(u => !u.vvalid)
      discardRelatives(inv)
    }
  }

  override def discardRelatives(invalid: collection.Seq[OutputUnit]): Unit = {
    dependencies.values.foreach(deps => {
      deps.filterInPlace(dep => !invalid.exists(i => i.kwLoc.isRelative(dep.kwLoc)))
    })
  }

  def addDependencyFor(path: JsonPointer, unit: OutputUnit): Unit = {
    dependencies.getOrElseUpdate(path, mutable.ArrayBuffer()).addOne(unit)
  }

  override def endScope(schLocation: JsonPointer, result: OutputUnit): Unit = {
    dependents.remove(schLocation)
    dependencies.remove(schLocation)

    if (!result.vvalid) discardRelatives(Seq(result))
  }
}

object SimpleContext {
  val Empty: SimpleContext = SimpleContext(Map.empty[Uri, Schema], Config.Default)
}
