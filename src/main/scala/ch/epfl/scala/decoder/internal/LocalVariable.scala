package ch.epfl.scala.decoder.internal

import tastyquery.Types.Type
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Contexts.Context

sealed trait LocalVariable:
  def sym: Symbol
  def scope: Scope
  def tpe: Type

  def positions: Set[SourcePosition] = scope.allPositions

object LocalVariable:
  case class This(sym: ClassSymbol, scope: Scope) extends LocalVariable:
    def tpe: Type = sym.thisType

  case class ValDef(sym: TermSymbol, scope: Scope) extends LocalVariable:
    def tpe: Type = sym.declaredType.requireType

  case class InlinedFromDef(underlying: LocalVariable, inlineCall: InlineCall, scope: Scope)(using Context)
      extends LocalVariable:
    def sym: Symbol = underlying.sym
    def tpe: Type = inlineCall.substTypeParams(underlying.tpe)
