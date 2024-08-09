package ch.epfl.scala.decoder.jdi

import ch.epfl.scala.decoder.binary.*

import java.util as ju
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/* A class or interface */
class JdiClass(ref: com.sun.jdi.ReferenceType) extends JdiType(ref) with BinaryClass:
  override def classLoader: BinaryClassLoader = JdiClassLoader(ref.classLoader)

  override def superclass: Option[BinaryClass] = ref match
    case cls: com.sun.jdi.ClassType => Some(JdiClass(cls.superclass))
    case _ => None

  override def interfaces: Seq[BinaryClass] = ref match
    case cls: com.sun.jdi.ClassType => cls.interfaces.asScala.toSeq.map(JdiClass.apply)
    case interface: com.sun.jdi.InterfaceType => interface.superinterfaces.asScala.toSeq.map(JdiClass.apply)

  override def isInterface = ref.isInstanceOf[com.sun.jdi.InterfaceType]

  override def sourceLines: Option[SourceLines] = sourceName.map(SourceLines(_, allLineLocations.map(_.lineNumber)))

  override def method(name: String, sig: String): Option[Method] =
    visibleMethods.find(_.signedName == SignedName(name, sig))

  override def declaredMethod(name: String, sig: String): Option[Method] =
    declaredMethods.find(_.signedName == SignedName(name, sig))

  override def declaredMethods: Seq[Method] = ref.methods.asScala.map(JdiMethod(_)).toSeq

  override def declaredField(name: String): Option[Field] = declaredFields.find(_.name == name)

  override def declaredFields: Seq[Field] = ref.fields.asScala.map(JdiField(_)).toSeq

  private[jdi] def constantPool: ConstantPool = ConstantPool(ref.constantPool)

  private def allLineLocations: Seq[com.sun.jdi.Location] =
    try ref.allLineLocations.asScala.toSeq
    catch case e: com.sun.jdi.AbsentInformationException => Seq.empty

  override def sourceName: Option[String] = Option(ref.sourceName)

  private def visibleMethods: Seq[JdiMethod] = ref.visibleMethods.asScala.map(JdiMethod(_)).toSeq
