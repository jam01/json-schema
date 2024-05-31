package io.github.jam01.json_schema

import io.github.jam01.json_schema.Uri.conformUri

import java.net.URI
import java.util.UUID

final case class Uri(uri: URI, isDyn: Boolean = false) {
  // TODO: consider tracking fragment
  private lazy val str = conformUri(uri.toString)

  def resolve(ref: String, resIsDyn: Boolean = isDyn): Uri = {
    Uri.resolve(uri, ref, resIsDyn)
  }

  def appendedFragment(frag: String): Uri = {
    val rfrag = java.net.URI(null, null, null, frag).getRawFragment
    if (uri.getRawFragment eq null) Uri.of(str + "#" + rfrag, isDyn)
    else Uri.of(withoutFragment.toString + "#" + uri.getRawFragment + rfrag, isDyn)
  }

  def withFragment(frag: String, resIsDyn: Boolean = isDyn): Uri = {
    val rfrag = java.net.URI(null, null, null, frag).getRawFragment
    if (uri.getRawFragment eq null) Uri.of(str + "#" + rfrag, resIsDyn)
    else Uri.of(withoutFragment.toString + "#" + rfrag, resIsDyn)
  }

  def withoutFragment: Uri = {
    if (uri.getRawFragment == null) this
    else Uri.of(str.substring(0, str.indexOf('#')), isDyn)
  }

  def asDyn: Uri = {
    if (isDyn) this
    else new Uri(uri, true)
  }

  def asStatic: Uri = {
    if (!isDyn) this
    else new Uri(uri, false)
  }

  override def toString: String = str
  override def hashCode(): Int = str.## + isDyn.##
  override def equals(obj: Any): Boolean = {
    obj != null &&
      obj.getClass == this.getClass &&
      str.equals(obj.asInstanceOf[Uri].str) && isDyn.equals(obj.asInstanceOf[Uri].isDyn)
  }
}

object Uri {
  def of(s: String): Uri = {
    new Uri(new URI(conformUri(s)), false)
  }

  def of(s: String, isDyn: Boolean): Uri = {
    new Uri(new URI(conformUri(s)), isDyn)
  }

  private def conformUri(s: String): String = {
    val nofrag = if (s.endsWith("#")) s.substring(0, s.length - 1)
    else s

    if (nofrag.startsWith("file:///")) "file:/" + nofrag.substring(8) // see: https://superuser.com/a/479262
    else nofrag
  }

  private def resolve(base: URI, ref: String, isDyn: Boolean): Uri =
    new Uri(resolve(base, ref), isDyn)

  private def resolve(base: URI, ref: String): URI = { // assumes string is correctly encoded
    val refURI = java.net.URI(ref) // this constructor will decode any escaped chars
    if (refURI.isAbsolute) refURI
    else if ("urn" == base.getScheme && ref.startsWith("#")) java.net.URI(base.getScheme, base.getSchemeSpecificPart, refURI.getFragment)
    else base.resolve(refURI)
  }
  
  def random: Uri = {
    Uri.of("urn:uuid:" + UUID.randomUUID().toString)
  }
}
