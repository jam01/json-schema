package io.github.jam01.json_schema

import io.github.jam01.json_schema.Uri.{conformUri, resolve}

import java.net.URI
import java.util.UUID

/**
 * A JSON Schema URI.
 */
final class Uri private(val uri: URI, val isDyn: Boolean, private val str: String) {
  // perf: consider tracking fragment
  private def this(uri: URI, isDyn: Boolean) = {
    this(uri, isDyn, conformUri(uri.toString))
  }

  /**
   * Resolves the given Uri against this Uri.
   *
   * @param ref the reference to be resolved
   * @param dyn whether the resulting Uri should be a dynamic reference
   * @return the resulting Uri
   */
  def resolve(ref: String, dyn: Boolean = isDyn): Uri = {
    new Uri(Uri.resolve(uri, ref), dyn)
  }

  /**
   * Returns the decoded fragment component of this Uri.
   *
   * @see [[java.net.URI.getFragment]]
   * @return the decoded fragment component of this URI
   */
  def fragment: String | Null = uri.getFragment

  /**
   * Appends the given fragment to the existing fragment in this Uri, if any. Otherwise sets the given fragment.
   *
   * @param frag the fragment to append or set
   * @return the resulting Uri
   */
  def appendedFragment(frag: String): Uri = {
    val rfrag = URI(null, null, null, frag).getRawFragment
    if (uri.getRawFragment == null) Uri(str + "#" + rfrag, isDyn)
    else Uri(this.withoutFragment.str + "#" + uri.getRawFragment + rfrag, isDyn)
  }

  /**
   * Sets the given fragment to this Uri, discarding any existing fragment.
   *
   * @param frag the fragment to set
   * @return the resulting Uri
   */
  def withFragment(frag: String, resIsDyn: Boolean = isDyn): Uri = {
    val rfrag = URI(null, null, null, frag).getRawFragment
    val str0 = if (uri.getRawFragment == null) str + "#" + rfrag
      else str.substring(0, str.indexOf('#') + 1) + rfrag
    new Uri(new URI(str0), resIsDyn, str0)
  }

  /**
   * Removes this Uri's fragment, if any.
   * @return the resulting fragment-less Uri
   */
  def withoutFragment: Uri = {
    if (uri.getRawFragment == null) this
    else Uri(str.substring(0, str.indexOf('#')), isDyn)
  }

  /**
   * This Uri as dynamic
   * @return the resulting Uri
   */
  def asDyn: Uri = {
    if (isDyn) this
    else new Uri(uri, true)
  }

  /**
   * This Uri as non-dynamic
   * @return the resulting Uri
   */
  def asNonDyn: Uri = {
    if (!isDyn) this
    else new Uri(uri, false)
  }

  override def toString: String = str
  override def hashCode(): Int = str.## + isDyn.##
  override def equals(obj: Any): Boolean = {
    obj != null &&
      obj.getClass == this.getClass &&
      str == obj.asInstanceOf[Uri].str && isDyn == obj.asInstanceOf[Uri].isDyn
  }
}

object Uri {
  /**
   * A JSON Schema URI.
   *
   * @param str the string to be parsed as an URI
   * @param isDyn whether the Uri is signaled to be used for dynamic references
   */
  def apply(str: String, isDyn: Boolean = false): Uri = {
    new Uri(new URI(str), isDyn)
  }

  private def conformUri(str: String): String = {
    val nofrag = if (str.endsWith("#")) str.substring(0, str.length - 1)
    else str
    if (nofrag.startsWith("file:///")) "file:/" + nofrag.substring(8) // see: https://superuser.com/a/479262
    else nofrag
  }

  private def resolve(base: URI, ref: String): URI = {
    val refURI = URI(ref) // this constructor will decode any escaped chars
    if (refURI.isAbsolute) refURI
    else if ("urn" == base.getScheme && ref.startsWith("#")) URI(base.getScheme, base.getSchemeSpecificPart, refURI.getFragment)
    else base.resolve(refURI)
  }

  /**
   * A random Uri.
   */
  def random: Uri = {
    Uri("urn:uuid:" + UUID.randomUUID().toString)
  }
}
