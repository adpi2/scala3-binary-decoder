package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary

import java.lang.reflect.Field

class JavaReflectField(field: Field, loader: JavaReflectLoader) extends binary.Field:
  override def name: String = field.getName

  override def sourceLines: Option[binary.SourceLines] = None

  override def declaringClass: binary.ClassType = loader.loadClass(field.getDeclaringClass)

  override def `type`: binary.Type =
    loader.loadClass(field.getType)
