package ch.epfl.scala.decoder.testutils

import ch.epfl.scala.decoder.*
import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.binary.BinaryClassLoader
import ch.epfl.scala.decoder.javareflect.JavaReflectLoader

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import java.nio.file.FileSystem

object TestingDecoder:
  def javaRuntime = JavaRuntime(Properties.jdkHome).get

  def apply(source: String, scalaVersion: ScalaVersion)(using ThrowOrWarn): TestingDecoder =
    val module = Module.fromSource(source, scalaVersion)
    TestingDecoder(module.mainEntry, module.classpath)

  def apply(source: String, scalaVersion: ScalaVersion, dependencies: Seq[ClasspathEntry])(using
      ThrowOrWarn
  ): TestingDecoder =
    val module = Module.fromSource(source, scalaVersion, dependencies)
    TestingDecoder(module.mainEntry, module.classpath)

  def apply(mainEntry: ClasspathEntry, classEntries: Seq[ClasspathEntry])(using ThrowOrWarn): TestingDecoder =
    val classPath = classEntries.map(_.absolutePath)
    val javaRuntimeJars = javaRuntime match
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.flatMap { fs =>
          Files.list(fs.getPath("/modules")).iterator.asScala.toSeq
        }
    val decoder = BinaryDecoder(classPath ++ javaRuntimeJars)
    val classLoader = JavaReflectLoader(classPath)
    new TestingDecoder(mainEntry, classLoader, decoder)

class TestingDecoder(mainEntry: ClasspathEntry, val classLoader: BinaryClassLoader, val decoder: BinaryDecoder):
  export decoder.*

  def decode(cls: String): DecodedClass =
    val binaryClass = classLoader.loadClass(cls)
    decode(binaryClass)
  def name: String = mainEntry.name
  def allClasses: Seq[binary.BinaryClass] =
    def listClassNames(root: Path): Seq[String] =
      val classMatcher = root.getFileSystem.getPathMatcher("glob:**.class")
      Files
        .walk(root)
        .filter(classMatcher.matches)
        .iterator
        .asScala
        .map(path => root.relativize(path).toString.stripPrefix("/").stripSuffix(".class").replace('/', '.'))
        .filterNot(_.endsWith("module-info"))
        .toSeq
    val classNames =
      if mainEntry.isJar then IO.withinJarFile(mainEntry.absolutePath)(fs => listClassNames(fs.getPath("/"))).get
      else listClassNames(mainEntry.absolutePath)
    classNames.map(classLoader.loadClass)
