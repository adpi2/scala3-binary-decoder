package ch.epfl.scala.decoder.internal

import ch.epfl.scala.decoder.ThrowOrWarn
import ch.epfl.scala.decoder.binary
import tastyquery.Contexts.*
import tastyquery.SourceFile
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.SourcePosition

import scala.collection.mutable
import scala.languageFeature.postfixOps

object VariableCollector:
  def collectVariables(scoper: Scoper, tree: Tree, sym: Option[TermSymbol] = None)(using
      Context,
      ThrowOrWarn
  ): Set[LocalVariable] =
    val collector = VariableCollector(scoper)
    collector.collect(tree, sym)

class VariableCollector(scoper: Scoper)(using Context, ThrowOrWarn) extends TreeTraverser:
  private val inlinedVariables = mutable.Map.empty[TermSymbol, Set[LocalVariable]]

  def collect(tree: Tree, sym: Option[TermSymbol] = None): Set[LocalVariable] =
    val variables: mutable.Set[LocalVariable] = mutable.Set.empty
    type ScopeTree = ClassDef | DefDef | Block | CaseDef | Inlined
    var scopes: mutable.Stack[Scope] = mutable.Stack(scoper.getScope(tree))

    object Traverser extends TreeTraverser:
      def traverseDef(tree: Tree): Unit =
        // traverse even if it's a DefDef or ClassDef
        tree match
          case tree: DefDef =>
            // TODO comment this line
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
            variables ++= localVariables.map { v =>
              val scope = scoper.inlinedScope(v.scope, inlineCall)
              LocalVariable.InlinedFromDef(v, inlineCall, scope)
            }
            inlineCall.args.foreach(traverseDef)
          case tree: (Block | CaseDef | Inlined) => scoped(tree)(super.traverse(tree))
          case _ => super.traverse(tree)

      private def scoped(tree: ScopeTree)(f: => Unit): Unit =
        val scope = scoper.getScope(tree)
        if scope.position.isFullyDefined then
          scopes.push(scope)
          f
          scopes.pop()
        else f

      private def addValDefOrBind(valDef: ValDef | Bind): Unit =
        val sym = valDef.symbol.asInstanceOf[TermSymbol]
        variables += LocalVariable.ValDef(sym, scopes.head)
    end Traverser

    sym
      .flatMap(allOuterClasses)
      .foreach: cls =>
        val scope = scoper.getScope(cls)
        variables += LocalVariable.This(cls, scope)
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
