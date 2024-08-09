package ch.epfl.scala.decoder.jdi

import ch.epfl.scala.decoder.binary.*

import java.lang.reflect.InvocationTargetException
import scala.jdk.CollectionConverters.*
import java.util as ju

class JdiMethod(method: com.sun.jdi.Method) extends Method:
  override def name: String = method.name

  override def declaringClass: JdiClass = JdiClass(method.declaringType)

  override def parameters: Seq[Parameter] =
    method.arguments.asScala.toSeq.map(JdiParameter.apply(_, method))

  override def variables: Seq[Variable] =
    method.variables.asScala.toSeq.map(JdiVariable.apply(_, method))

  override def returnType: Option[Type] =
    try Some(JdiType(method.returnType))
    catch case e: com.sun.jdi.ClassNotLoadedException => None

  override def isBridge: Boolean = method.isBridge

  override def isStatic: Boolean = method.isStatic

  override def isFinal: Boolean = method.isFinal

  override def isConstructor: Boolean = method.isConstructor

  override def sourceLines: Option[SourceLines] =
    declaringClass.sourceName.map(SourceLines(_, allLineLocations.map(_.lineNumber)))

  override def signedName: SignedName = SignedName(name, signature)

  override def instructions: Seq[Instruction] = ByteCodes.parse(bytecodes, declaringClass.constantPool)

  private def allLineLocations: Seq[com.sun.jdi.Location] = method.allLineLocations.asScala.toSeq

  private def signature: String = method.signature

  private def bytecodes: Array[Byte] = method.bytecodes
