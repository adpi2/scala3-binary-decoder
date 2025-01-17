package ch.epfl.scala.decoder.internal

import tastyquery.SourcePosition
import tastyquery.Symbols.TermSymbol
import tastyquery.SourceFile
import tastyquery.Contexts.Context
import ch.epfl.scala.decoder.ThrowOrWarn

/**
 * The description of a scope in the code.
 *
 * @param position         the main position of the scope
 * @param inlinedPositions the other inlined positions in the scope
 * @param inlinedParams    the inlined params, that should later be replaced
 */
case class Scope(position: SourcePosition, inlinedPositions: Set[SourcePosition], inlinedParams: Set[TermSymbol]):
  def allPositions: Set[SourcePosition] = Set(position) ++ inlinedPositions
  def sourceFile: SourceFile = position.sourceFile

object Scope:
  def empty: Scope = Scope(SourcePosition.NoPosition, Set.empty, Set.empty)
