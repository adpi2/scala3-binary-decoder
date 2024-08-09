package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary
import org.objectweb.asm
import scala.collection.SeqMap

private case class ExtraMethodInfo(
    sourceLines: Option[binary.SourceLines],
    instructions: Seq[binary.Instruction],
    variables: Seq[ExtraMethodInfo.Variable],
    labels: SeqMap[asm.Label, Int]
):
  // not parameters
  def localVariables: Seq[ExtraMethodInfo.Variable] =
    variables.filter(v => v.start != labels.head(0) || v.name == "this")

private object ExtraMethodInfo:
  def empty: ExtraMethodInfo = ExtraMethodInfo(None, Seq.empty, Seq.empty, SeqMap.empty)
  case class Variable(name: String, descriptor: String, signature: String, start: asm.Label, end: asm.Label)
