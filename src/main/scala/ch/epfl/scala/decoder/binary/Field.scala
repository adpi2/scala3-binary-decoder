package ch.epfl.scala.decoder.binary

trait Field extends Symbol:
  def declaringClass: BinaryClass
  def `type`: Type
  def isStatic: Boolean
