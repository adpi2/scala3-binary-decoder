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

  def inlinedFrom: List[InlineCall] = Nil
  def inlinedArgs: Map[Symbol, Seq[TermTree]] = Map.empty
  def scope(scoper: Scoper): Scope = scoper.getScope(tree)
  def positions(scoper: Scoper): Seq[SourcePosition] = scope(scoper).allPositions.toSeq
  def capture(using Context, ThrowOrWarn): Seq[String] = LiftedTree.collectCapture(this)
end LiftedTree

sealed trait LocalTermDef extends LiftedTree[TermSymbol]:
  def symbol: TermSymbol
  def tpe: TypeOrMethodic = symbol.declaredType

final case class LocalDef(tree: DefDef) extends LocalTermDef:
  def symbol: TermSymbol = tree.symbol
  def owner: Symbol = tree.symbol.owner

final case class LocalLazyVal(tree: ValDef) extends LocalTermDef:
  def symbol: TermSymbol = tree.symbol
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

  override def scope(scoper: Scoper): Scope = scoper.inlinedScope(underlying.scope(scoper), inlineCall)

  override def inlinedFrom: List[InlineCall] = inlineCall :: underlying.inlinedFrom
  override def inlinedArgs: Map[Symbol, Seq[TermTree]] = underlying.inlinedArgs

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
final case class InlinedFromArg[S](underlying: LiftedTree[S], params: Seq[TermSymbol], inlineArgs: Seq[TermTree])
    extends LiftedTree[S]:
  def tree: Tree = underlying.tree
  def symbol: S = underlying.symbol
  def owner: Symbol = underlying.owner
  def tpe: TermType = underlying.tpe
  override def inlinedFrom: List[InlineCall] = underlying.inlinedFrom
  override def inlinedArgs: Map[Symbol, Seq[TermTree]] = underlying.inlinedArgs ++ params.map(_ -> inlineArgs)

object LiftedTree:
  def collectCapture(liftedTree: LiftedTree[?])(using Context, ThrowOrWarn): Seq[String] =
    val capture = mutable.Set.empty[String]
    val alreadySeen = mutable.Set.empty[Symbol]

    def loopCollect(symbol: Symbol)(collect: => Unit): Unit =
      if !alreadySeen.contains(symbol) then
        alreadySeen += symbol
        collect

    class Traverser(inlinedFrom: List[InlineCall], inlinedArgs: Map[Symbol, Seq[TermTree]])(using Context)
        extends TreeTraverser:
      private val inlineMapping: Map[Symbol, TermTree] = inlinedFrom.headOption.toSeq.flatMap(_.paramsMap).toMap
      override def traverse(tree: Tree): Unit =
        tree match
          case tree: TermReferenceTree =>
            for symbol <- tree.safeSymbol do
              for arg <- inlineMapping.get(symbol) do
                loopCollect(symbol)(Traverser(inlinedFrom.tail, inlinedArgs).traverse(arg))
              for args <- inlinedArgs.get(symbol) do loopCollect(symbol)(args.foreach(traverse))
          case _ => ()

        tree match
          case _: TypeTree => ()
          case ident: Ident =>
            for sym <- ident.safeSymbol.collect { case sym: TermSymbol => sym } do
              capture += sym.nameStr
              if sym.isLocal then
                if sym.isMethod || sym.isLazyVal then loopCollect(sym)(sym.tree.foreach(traverse))
                else if sym.isModuleVal then loopCollect(sym)(sym.moduleClass.flatMap(_.tree).foreach(traverse))
          case _ => super.traverse(tree)
    end Traverser

    val traverser = Traverser(liftedTree.inlinedFrom, liftedTree.inlinedArgs)
    def traverse(tree: LiftedTree[?]): Unit =
      tree match
        case term: LocalTermDef if term.symbol.isModuleVal =>
          loopCollect(term.symbol)(term.symbol.moduleClass.flatMap(_.tree).foreach(traverser.traverse))
        case term: LocalTermDef =>
          loopCollect(term.symbol)(traverser.traverse(term.tree))
        case lambda: LambdaTree => loopCollect(lambda.symbol(0))(lambda.tree)
        case InlinedFromDef(underlying, inlineCall) => traverse(underlying)
        case InlinedFromArg(underlying, params, inlineArgs) => traverse(underlying)
        case tree => traverser.traverse(tree.tree)
    traverse(liftedTree)
    capture.toSeq
  end collectCapture
