package ch.epfl.scala.decoder.jdi

import ch.epfl.scala.decoder.binary.*

class JdiField(field: com.sun.jdi.Field) extends Field:

  override def declaringClass: BinaryClass = JdiClass(field.declaringType())

  override def isStatic: Boolean = field.isStatic()

  override def name: String = field.name
  override def sourceLines: Option[SourceLines] = None
  override def `type`: Type = JdiType(field.`type`)
