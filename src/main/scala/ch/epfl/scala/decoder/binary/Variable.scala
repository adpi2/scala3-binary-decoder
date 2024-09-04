package ch.epfl.scala.decoder.binary

trait Variable extends Symbol:
  def `type`: Type
  def declaringMethod: Method
  def isParameter: Boolean = isInstanceOf[Parameter]
