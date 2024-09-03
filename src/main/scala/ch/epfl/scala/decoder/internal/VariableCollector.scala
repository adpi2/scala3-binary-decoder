package ch.epfl.scala.decoder.internal

import ch.epfl.scala.decoder.ThrowOrWarn
import ch.epfl.scala.decoder.binary
import tastyquery.Contexts.*
import tastyquery.SourceFile
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Trees.*
import tastyquery.Types.*

import scala.collection.mutable
import scala.languageFeature.postfixOps

object VariableCollector:
  def collectVariables(tree: Tree, sym: Option[TermSymbol] = None)(using Context, ThrowOrWarn): Set[LocalVariable] =
    val collector = VariableCollector()
    collector.collect(tree, sym)

sealed trait LocalVariable:
  def sym: Symbol
  def sourceLines: Option[binary.SourceLines]
  def tpe: Type

object LocalVariable:
  case class This(sym: ClassSymbol, sourceLines: Option[binary.SourceLines]) extends LocalVariable:
    def tpe: Type = sym.thisType

  case class ValDef(sym: TermSymbol, sourceLines: Option[binary.SourceLines]) extends LocalVariable:
    def tpe: Type = sym.declaredType.requireType

  case class InlinedFromDef(underlying: LocalVariable, inlineCall: InlineCall)(using Context) extends LocalVariable:
    def sym: Symbol = underlying.sym
    def sourceLines: Option[binary.SourceLines] = underlying.sourceLines
    def tpe: Type = inlineCall.substTypeParams(underlying.tpe)

end LocalVariable

class VariableCollector()(using Context, ThrowOrWarn) extends TreeTraverser:
  private val inlinedVariables = mutable.Map.empty[TermSymbol, Set[LocalVariable]]

  def collect(tree: Tree, sym: Option[TermSymbol] = None): Set[LocalVariable] =
    val variables: mutable.Set[LocalVariable] = mutable.Set.empty
    type ScopeTree = DefDef | Block | CaseDef
    var scopeTrees: mutable.Stack[ScopeTree] = mutable.Stack.empty

    object Traverser extends TreeTraverser:
      def traverseDef(tree: Tree): Unit =
        // traverse even if it's a DefDef or ClassDef
        tree match
          case tree: DefDef =>
            scoped(tree)(tree.paramLists.foreach(_.left.foreach(Traverser.traverse)))
            val isContextFun = tree.symbol.declaredType.returnType.safeDealias.exists(_.isContextFunction)
            tree.rhs match
              case Some(body @ Block(List(lambda: DefDef), expr)) if isContextFun =>
                // if the method returns a context function, we traverse the internal anonfun
                traverseDef(lambda)
                Traverser.traverse(expr)
              case Some(tree) => Traverser.traverse(tree)
              case None => ()
          case tree: ClassDef => tree.rhs.body.foreach(Traverser.traverse)
          case tree: ValDef => tree.rhs.foreach(Traverser.traverse)
          case _ => Traverser.traverse(tree)

      override def traverse(tree: Tree): Unit =
        tree match
          case tree: (ValDef | Bind) => addValDefOrBind(tree)
          case _ => ()

        tree match
          case _: TypeTree => ()
          case _: DefDef | ClassDef => ()
          case InlineCall(inlineCall) =>
            val localVariables =
              inlinedVariables.getOrElseUpdate(inlineCall.symbol, collectInlineDef(inlineCall.symbol))
            variables ++= localVariables.map(LocalVariable.InlinedFromDef(_, inlineCall))
            inlineCall.args.foreach(traverseDef)
          case tree: (Block | CaseDef) => scoped(tree)(super.traverse(tree))
          case _ => super.traverse(tree)

      private def scoped(scopeTree: ScopeTree)(f: => Unit): Unit =
        scopeTrees.push(scopeTree)
        f
        scopeTrees.pop()

      private def addValDefOrBind(valDef: ValDef | Bind): Unit =
        val sym = valDef.symbol.asInstanceOf[TermSymbol]
        val sourceLines = scopeTrees.headOption.map: scope =>
          binary.SourceLines(scope.pos.sourceFile.name, Seq(valDef.pos.startLine + 1, scope.pos.endLine + 1))
        variables += LocalVariable.ValDef(sym, sourceLines)
    end Traverser

    sym
      .flatMap(allOuterClasses)
      .foreach: cls =>
        val sourceLines =
          if cls.pos.isUnknown then None
          else Some(binary.SourceLines(cls.pos.sourceFile.name, Seq(cls.pos.startLine + 1, cls.pos.endLine + 1)))
        variables += LocalVariable.This(cls, sourceLines)
    Traverser.traverseDef(tree)
    variables.toSet
  end collect

  private def collectInlineDef(symbol: TermSymbol): Set[LocalVariable] =
    inlinedVariables(symbol) = Set.empty // break recursion
    symbol.tree.toSet.flatMap(tree => collect(tree, Some(symbol)))

  private def allOuterClasses(sym: Symbol): List[ClassSymbol] =
    def loop(sym: Symbol, acc: List[ClassSymbol]): List[ClassSymbol] =
      sym.outerClass match
        case Some(cls) => loop(cls, cls :: acc)
        case None => acc
    loop(sym, Nil)
