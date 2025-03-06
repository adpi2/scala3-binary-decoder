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
  private val cache = mutable.Map.empty[Symbol, LocalScope]

  private case class LocalScope(
      position: SourcePosition,
      capturedSyms: Set[TermSymbol],
      inlineSyms: Set[TermSymbol]
  ):
    def capturedInlineParams: Set[TermSymbol] = capturedSyms.filter(_.isParamInInlineMethod)
    def capturedMethods: Set[TermSymbol] =
      capturedSyms.filter(s => s.isLocal && (s.isMethod || s.isModuleVal || s.isLazyVal))
    def capturedVariables: Set[TermSymbol] = capturedSyms.filter(s => !s.isMethod)

  /**
   * Compute the scope inlined from an inline call:
   *   - compute the scope of the inlined arguments
   *   - use the pos of the inlineCall as main position
   */
  def inlinedScope(scope: Scope, inlineCall: InlineCall): Scope =
    val argScopes =
      for
        param <- scope.inlineParams
        arg <- inlineCall.paramsMap.get(param).toSeq
      yield getScope(arg)

    val (position, inlinedPositions) =
      if scope.sourceFile == inlineCall.pos.sourceFile then
        (scope.position, scope.inlinedPositions ++ argScopes.flatMap(_.allPositions))
      else (inlineCall.pos, scope.allPositions ++ argScopes.flatMap(_.inlinedPositions))
    val capturedVariables = scope.capturedVariables ++ argScopes.flatMap(_.capturedVariables)
    Scope(position, inlinedPositions, capturedVariables)

  def inlinedFromLambdaArg(scope: Scope, inlinedArgsByLambdaParam: Map[TermSymbol, Seq[TermTree]]): Scope =
    val argScopes =
      for
        (param, args) <- inlinedArgsByLambdaParam
        if scope.capturedVariables.contains(param)
        arg <- args
      yield getScope(arg)
    val capturedVariables = scope.capturedVariables ++ argScopes.flatMap(_.capturedVariables)
    scope.copy(capturedVariables = capturedVariables)

  def getScope(tree: Tree): Scope = buildScope(getLocalScope(tree))
  def getScope(sym: Symbol): Scope = buildScope(getLocalScope(sym))

  private def buildScope(localScope: LocalScope): Scope =
    def loopInline(acc: Map[TermSymbol, LocalScope]): Iterable[LocalScope] =
      val remaining = acc.values.flatMap(_.inlineSyms).toSet.filter(!acc.contains(_))
      if remaining.isEmpty then acc.values
      else loopInline(acc ++ getLocalScopes(remaining))
    def loopCapture(acc: Map[TermSymbol, LocalScope]): Iterable[LocalScope] =
      val remaining = acc.values.flatMap(_.capturedMethods).toSet.filter(!acc.contains(_))
      if remaining.isEmpty then acc.values
      else loopCapture(acc ++ getLocalScopes(remaining))
    val allInlined = loopInline(getLocalScopes(localScope.inlineSyms))
    val allCaptured = loopCapture(getLocalScopes(localScope.capturedMethods))
    Scope(
      localScope.position,
      allInlined.map(_.position).toSet,
      localScope.capturedVariables ++ allCaptured.flatMap(_.capturedVariables)
    )

  private def getLocalScopes(syms: Set[TermSymbol]): Map[TermSymbol, LocalScope] =
    syms.map(s => s -> getLocalScope(s)).toMap

  private def getLocalScope(sym: Symbol): LocalScope =
    sym match
      case sym: TermSymbol if sym.isModuleVal => getLocalScope(sym.moduleClass.get)
      case _ =>
        sym.tree match
          case None => LocalScope(SourcePosition.NoPosition, Set.empty, Set.empty)
          case Some(tree) => cache.getOrElseUpdate(sym, getLocalScope(tree))

  private def getLocalScope(tree: Tree): LocalScope =
    val inlineSyms = mutable.Set.empty[TermSymbol]
    val capturedSyms = mutable.Set.empty[TermSymbol]
    val localSyms = mutable.Set.empty[TermSymbol]
    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        tree match
          case tree: ValOrDefDef =>
            localSyms += tree.symbol
          case bind: Bind =>
            localSyms += bind.symbol
          case ident: Ident =>
            for
              sym <- ident.safeTermSymbol
              // sym.isLocal is not enough because of primary ctor params
              if !localSyms.contains(sym) && (sym.isLocal || sym.isVal)
            do capturedSyms += sym
          case _ => ()

        // inline call
        tree match
          case tree: TermReferenceTree =>
            for sym <- tree.safeTermSymbol if sym.isInline do inlineSyms += sym
          case _ => ()

        tree match
          case _: TypeTree => ()
          case _ => super.traverse(tree)
    Traverser.traverse(tree)
    LocalScope(tree.pos, capturedSyms.toSet, inlineSyms.toSet)
