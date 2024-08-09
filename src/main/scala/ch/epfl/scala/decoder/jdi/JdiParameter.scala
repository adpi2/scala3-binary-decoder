package ch.epfl.scala.decoder.jdi

import ch.epfl.scala.decoder.binary.*

class JdiParameter(variable: com.sun.jdi.LocalVariable, declaringMethod: JdiMethod) extends JdiVariable(variable, declaringMethod) with Parameter

object JdiParameter:
  def apply(variable: com.sun.jdi.LocalVariable, method: com.sun.jdi.Method): JdiParameter =
    new JdiParameter(variable, JdiMethod(method))
