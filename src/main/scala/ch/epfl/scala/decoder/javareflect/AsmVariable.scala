package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.binary.Type
import ch.epfl.scala.decoder.binary.SourceLines
import ch.epfl.scala.decoder.binary.Method
import ch.epfl.scala.decoder.binary.Variable

class AsmVariable(val name: String, val `type`: Type, val declaringMethod: Method, val sourceLines: Option[SourceLines])
    extends Variable:

  override def toString: String = s"${`type`.name} $name"
