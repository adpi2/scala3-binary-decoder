package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary

import java.lang.reflect.Field
import java.lang.reflect.Modifier

class JavaReflectField(field: Field, loader: JavaReflectLoader) extends binary.Field:
  override def name: String = field.getName

  override def sourceLines: Option[binary.SourceLines] = None

  override def declaringClass: binary.BinaryClass = loader.loadClass(field.getDeclaringClass)

  override def isStatic: Boolean = Modifier.isStatic(field.getModifiers)

  override def `type`: binary.Type =
    loader.loadClass(field.getType)

  override def toString: String = field.toString
