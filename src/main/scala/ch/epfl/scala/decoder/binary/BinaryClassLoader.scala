package ch.epfl.scala.decoder.binary

trait BinaryClassLoader:
  def loadClass(name: String): BinaryClass
