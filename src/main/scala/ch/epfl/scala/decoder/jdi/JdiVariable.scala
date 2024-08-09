package ch.epfl.scala.decoder.jdi

import ch.epfl.scala.decoder.binary.*

class JdiVariable(variable: com.sun.jdi.LocalVariable, val declaringMethod: JdiMethod) extends Variable:

  override def name: String = variable.name
  override def sourceLines: Option[SourceLines] = None
  override def `type`: Type = JdiType(variable.`type`)

object JdiVariable:
  def apply(variable: com.sun.jdi.LocalVariable, method: com.sun.jdi.Method): JdiVariable =
    if variable.isArgument then new JdiParameter(variable, JdiMethod(method))
    else new JdiVariable(variable, JdiMethod(method))
