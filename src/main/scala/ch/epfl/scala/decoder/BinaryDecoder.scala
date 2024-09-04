package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.internal.*
import tastyquery.Contexts.*
import tastyquery.Symbols.Symbol
import tastyquery.jdk.ClasspathLoaders

import java.nio.file.Path
import scala.collection.concurrent.TrieMap

object BinaryDecoder:
  def apply(classEntries: Seq[Path])(using ThrowOrWarn): BinaryDecoder =
    val classpath = CustomClasspath(ClasspathLoaders.read(classEntries.toList))
    val ctx = Context.initialize(classpath)
    new BinaryDecoder(using ctx)

  def cached(classEntries: Seq[Path])(using ThrowOrWarn): BinaryDecoder =
    val classpath = CustomClasspath(ClasspathLoaders.read(classEntries.toList))
    val ctx = Context.initialize(classpath)
    new BinaryDecoder(using ctx):
      private val classCache: TrieMap[String, DecodedClass] = TrieMap.empty
      private val methodCache: TrieMap[(String, binary.SignedName), DecodedMethod] = TrieMap.empty
      private val liftedTreesCache: TrieMap[Symbol, Seq[LiftedTree[?]]] = TrieMap.empty

      override def decode(cls: binary.BinaryClass): DecodedClass =
        classCache.getOrElseUpdate(cls.name, super.decode(cls))

      override def decode(method: binary.Method): DecodedMethod =
        methodCache.getOrElseUpdate((method.declaringClass.name, method.signedName), super.decode(method))

      override protected def collectAllLiftedTrees(owner: Symbol): Seq[LiftedTree[?]] =
        liftedTreesCache.getOrElseUpdate(owner, super.collectAllLiftedTrees(owner))
    end new

class BinaryDecoder(using Context, ThrowOrWarn)
    extends BinaryClassDecoder,
      BinaryMethodDecoder,
      BinaryFieldDecoder,
      BinaryVariableDecoder:
  def context = ctx
