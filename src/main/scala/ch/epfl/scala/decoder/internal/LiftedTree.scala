package ch.epfl.scala.decoder.internal

import ch.epfl.scala.decoder.*
import tastyquery.Contexts.*
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Trees.*
import tastyquery.Types.*

import scala.collection.mutable

sealed trait LiftedTree[S]:
  def tree: Tree
  def symbol: S
  def tpe: TermType
  def owner: Symbol

  def scope(scoper: Scoper): Scope = scoper.getScope(tree)
end LiftedTree

sealed trait LocalTermDef(val symbol: TermSymbol) extends LiftedTree[TermSymbol]:
  def tpe: TypeOrMethodic = symbol.declaredType
  override def scope(scoper: Scoper): Scope = scoper.getScope(symbol)

final case class LocalDef(tree: DefDef) extends LocalTermDef(tree.symbol):
  def owner: Symbol = tree.symbol.owner

final case class LocalLazyVal(tree: ValDef) extends LocalTermDef(tree.symbol):
  def owner: Symbol = tree.symbol.owner

final case class LambdaTree(lambda: Lambda)(using Context) extends LiftedTree[(TermSymbol, ClassSymbol)]:
  def symbol: (TermSymbol, ClassSymbol) = (lambda.meth.symbol.asTerm, lambda.samClassSymbol)
  def owner: Symbol = lambda.meth.symbol.owner
  def tree: Tree = lambda.meth.symbol.tree.get
  def tpe: TermType = lambda.tpe

final case class LocalClass(tree: ClassDef) extends LiftedTree[ClassSymbol]:
  def symbol: ClassSymbol = tree.symbol
  def owner: Symbol = tree.symbol.owner
  def tpe = symbol.thisType

final case class LiftedTry(owner: Symbol, tree: Try)(using Context) extends LiftedTree[Nothing]:
  def tpe: TermType = tree.tpe
  def symbol: Nothing = unexpected("no symbol for lifted try")

final case class ByNameArg(owner: Symbol, tree: TermTree, paramTpe: TermType, isInline: Boolean)(using Context)
    extends LiftedTree[Nothing]:
  def tpe: TermType = if isInline then tree.tpe.widenTermRef else paramTpe
  def symbol: Nothing = unexpected("no symbol for by name arg")

final case class ConstructorArg(owner: ClassSymbol, tree: TermTree, paramTpe: TermType)(using
    ctx: Context
) extends LiftedTree[Nothing]:
  def tpe: TermType =
    paramTpe match
      case _: ByNameType => Definitions.Function0Type.appliedTo(tree.tpe.asInstanceOf[Type])
      case _ => tree.tpe

  def symbol: Nothing = unexpected("no symbol for constructor arg")

final case class InlinedFromDef[S](underlying: LiftedTree[S], inlineCall: InlineCall)(using Context)
    extends LiftedTree[S]:
  def tree: Tree = underlying.tree
  def symbol: S = underlying.symbol
  def owner: Symbol = underlying.owner
  def tpe: TermType = inlineCall.substTypeParams(underlying.tpe)

  override def scope(scoper: Scoper): Scope =
    scoper.inlinedScope(underlying.scope(scoper), inlineCall)

/**
 * A lambda in an inline lambda can capture a val passed as argument to the inline call
 * Example:
 *   inline def withContext(ctx: Context)(inline f: Context ?=> T): T = f(using ctx)
 *   withContext(someCtx)(list.map(<anon fun>))
 * <anon fun> can capture someCtx
 *
 * @param params the params of the inline lambda
 * @param inlineArgs the other args of the inline call
 */
final case class InlinedFromArg[S](underlying: LiftedTree[S], lambdaParams: Seq[TermSymbol], inlineCall: InlineCall)
    extends LiftedTree[S]:
  def tree: Tree = underlying.tree
  def symbol: S = underlying.symbol
  def owner: Symbol = underlying.owner
  def tpe: TermType = underlying.tpe

  override def scope(scoper: Scoper): Scope =
    scoper.inlinedFromLambdaArg(underlying.scope(scoper), lambdaParams, inlineCall)
