package ch.epfl.scala.decoder.binary

trait BinaryClass extends Type:
  def name: String
  def isInterface: Boolean
  def superclass: Option[BinaryClass]
  def interfaces: Seq[BinaryClass]
  def method(name: String, descriptor: String): Option[Method]
  def declaredField(name: String): Option[Field]
  def declaredMethod(name: String, descriptor: String): Option[Method]
  def declaredMethods: Seq[Method]
  def declaredFields: Seq[Field]
  def classLoader: BinaryClassLoader

  private[decoder] def isObject = name.endsWith("$")
  private[decoder] def isPackageObject = name.endsWith(".package$") || name.endsWith("$package$")
  private[decoder] def isPartialFunction = superclass.exists(_.name == "scala.runtime.AbstractPartialFunction")
  private[decoder] def isJavaLangEnum = superclass.exists(_.name == "java.lang.Enum")
