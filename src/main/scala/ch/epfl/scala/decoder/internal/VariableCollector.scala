package ch.epfl.scala.decoder.internal

import tastyquery.Trees.*
import scala.collection.mutable
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Contexts.*
import tastyquery.SourcePosition
import tastyquery.Types.*
import tastyquery.Traversers
import ch.epfl.scala.decoder.ThrowOrWarn
import scala.languageFeature.postfixOps
import tastyquery.SourceFile

object VariableCollector:
  def collectVariables(tree: Tree, sym: Option[TermSymbol] = None)(using Context, ThrowOrWarn): Set[LocalVariable] =
    val collector = VariableCollector()
    collector.collect(tree, sym)

trait LocalVariable:
  def sym: Symbol
  def sourceFile: SourceFile
  def startLine: Int
  def endLine: Int

object LocalVariable:
  case class This(sym: ClassSymbol, sourceFile: SourceFile, startLine: Int, endLine: Int) extends LocalVariable
  case class ValDef(sym: TermSymbol, sourceFile: SourceFile, startLine: Int, endLine: Int) extends LocalVariable
  case class InlinedFromDef(underlying: LocalVariable, inlineCall: InlineCall) extends LocalVariable:
    def sym: Symbol = underlying.sym
    def startLine: Int = underlying.startLine
    def endLine: Int = underlying.endLine
    def sourceFile: SourceFile = underlying.sourceFile

end LocalVariable

class VariableCollector()(using Context, ThrowOrWarn) extends TreeTraverser:
  private val inlinedVariables = mutable.Map.empty[TermSymbol, Set[LocalVariable]]

  def collect(tree: Tree, sym: Option[TermSymbol] = None): Set[LocalVariable] =
    val variables: mutable.Set[LocalVariable] = mutable.Set.empty
    var previousTree: mutable.Stack[Tree] = mutable.Stack.empty

    object Traverser extends TreeTraverser:
      def traverseDef(tree: Tree): Unit =
        // traverse even if it's a DefDef or ClassDef
        tree match
          case tree: DefDef =>
            previousTree.push(tree)
            tree.paramLists.foreach(_.left.foreach(Traverser.traverse))
            tree.rhs.foreach(Traverser.traverse)
            previousTree.pop()
          case tree: ClassDef =>
            previousTree.push(tree)
            Traverser.traverse(tree.rhs)
            previousTree.pop()
          case _ => Traverser.traverse(tree)
      override def traverse(tree: Tree): Unit =
        tree match
          case valDefOrBind: (ValDef | Bind) => addValDefOrBind(valDefOrBind)
          case _ => ()

        tree match
          case _: TypeTree => ()
          case _: DefDef | ClassDef => ()
          case valDef: ValDef => traverse(valDef.rhs)
          case bind: Bind => traverse(bind.body)
          case InlineCall(inlineCall) =>
            val localVariables =
              inlinedVariables.getOrElseUpdate(inlineCall.symbol, collectInlineDef(inlineCall.symbol))
            variables ++= localVariables.map(LocalVariable.InlinedFromDef(_, inlineCall))
            previousTree.push(inlineCall.termRefTree)
            inlineCall.args.foreach(traverseDef)
            previousTree.pop()
          case _ =>
            previousTree.push(tree)
            super.traverse(tree)
            previousTree.pop()

      private def addValDefOrBind(valDef: ValDef | Bind): Unit =
        val sym = valDef.symbol.asInstanceOf[TermSymbol]
        previousTree
          .collectFirst { case tree: (Block | CaseDef | DefDef | Template) => tree }
          .foreach { parentTree =>
            variables +=
              LocalVariable.ValDef(
                sym,
                parentTree.pos.sourceFile,
                valDef.pos.startLine + 1,
                parentTree.pos.endLine + 1
              )
          }

    sym
      .flatMap(allOuterClasses)
      .foreach(cls =>
        variables += LocalVariable.This(
          cls,
          cls.pos.sourceFile,
          if cls.pos.isUnknown then -1 else cls.pos.startLine + 1,
          if cls.pos.isUnknown then -1 else cls.pos.endLine + 1
        )
      )
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
