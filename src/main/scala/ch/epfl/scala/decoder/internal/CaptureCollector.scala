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

object CaptureCollector:
  def collectCaptures(tree: Tree)(using Context, ThrowOrWarn): Set[TermSymbol] =
    val collector = CaptureCollector()
    collector.collect(tree)

class CaptureCollector(using Context, ThrowOrWarn) extends TreeTraverser:
  private val alreadySeen: mutable.Set[Symbol] = mutable.Set.empty

  def collect(tree: Tree): Set[TermSymbol] =
    val localVariables: mutable.Set[TermSymbol] = mutable.Set.empty
    val capture: mutable.Set[TermSymbol] = mutable.Set.empty
    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        tree match
          case _: TypeTree => ()
          case valDef: ValDef =>
            localVariables += valDef.symbol
            traverse(valDef.rhs)
          case bind: Bind =>
            localVariables += bind.symbol
            traverse(bind.body)
          case ident: Ident =>
            for sym <- ident.safeSymbol.collect { case sym: TermSymbol => sym } do
              if !localVariables.contains(sym) && sym.isLocal then
                if !sym.isMethod then capture += sym
                if sym.isMethod || sym.isLazyVal || sym.isModuleVal then capture ++= loopCollect(sym)
          case _ => super.traverse(tree)
    Traverser.traverse(tree)
    capture.toSet

  def loopCollect(sym: TermSymbol): Set[TermSymbol] =
    if !alreadySeen.contains(sym) then
      alreadySeen += sym
      if sym.isModuleVal then sym.moduleClass.toSet.flatMap(_.tree).flatMap(collect)
      else sym.tree.toSet.flatMap(collect)
    else Set.empty
