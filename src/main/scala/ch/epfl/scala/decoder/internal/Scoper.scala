package ch.epfl.scala.decoder.internal

import tastyquery.Contexts.Context
import ch.epfl.scala.decoder.ThrowOrWarn
import tastyquery.SourcePosition
import scala.collection.mutable
import tastyquery.Trees.*
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Modifiers.TermSymbolKind

// computes and caches the scopes of symbols or trees
class Scoper(using Context, ThrowOrWarn):
  private val cache = mutable.Map.empty[Symbol, Scope]

  /**
   * Compute the scope inlined from an inline call:
   *   - compute the scope of the inlined arguments
   *   - use the pos of the inlineCall as main position
   */
  def inlinedScope(scope: Scope, inlineCall: InlineCall): Scope =
    val argScopes =
      for
        param <- scope.inlinedParams
        arg <- inlineCall.paramsMap.get(param).toSeq
      yield getScope(arg)
    val newInlinedParams = argScopes.flatMap(_.inlinedParams)
    if scope.sourceFile == inlineCall.pos.sourceFile then
      Scope(scope.position, scope.inlinedPositions ++ argScopes.flatMap(_.allPositions), newInlinedParams)
    else Scope(inlineCall.pos, scope.allPositions ++ argScopes.flatMap(_.inlinedPositions), newInlinedParams)

  def getScope(sym: Symbol): Scope =
    sym.tree match
      case None => Scope.empty
      case Some(tree) =>
        cache.getOrElseUpdate(
          sym, {
            cache += sym -> Scope.empty // break recursion
            getScope(tree)
          }
        )

  def getScope(tree: Tree): Scope =
    val inlinedPositions = mutable.Set.empty[SourcePosition]
    val inlinedParams = mutable.Set.empty[TermSymbol]
    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        tree match
          case _: TypeTree => ()
          case tree: TermReferenceTree =>
            for sym <- tree.safeSymbol.collect { case s: TermSymbol => s } do
              // todo should it be inline?
              if sym.isParamInInlineMethod then inlinedParams += sym
              if sym.isInline then inlinedPositions ++= getScope(sym).allPositions
          case _ => ()
        super.traverse(tree)
    Traverser.traverse(tree)
    Scope(tree.pos, inlinedPositions.toSet, inlinedParams.toSet)
