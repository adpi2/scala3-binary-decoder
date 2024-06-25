package ch.epfl.scala.decoder.testutils

import ch.epfl.scala.decoder.*
import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.internal.isGenerated
import ch.epfl.scala.decoder.javareflect.*

import java.nio.file.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

trait BinaryDecoderSuite extends CommonFunSuite:
  protected def defaultThrowOrWarn: ThrowOrWarn = ThrowOrWarn.printAndThrow
  private def formatter(using ThrowOrWarn) = StackTraceFormatter()

  given ThrowOrWarn = defaultThrowOrWarn

  def println(x: Any): Unit = Predef.println(x)

  def initDecoder(
      groupId: String,
      artifactId: String,
      version: String,
      fetchOptions: FetchOptions = FetchOptions.default
  )(using ThrowOrWarn): TestingDecoder =
    val libraries = Resolver.fetch(groupId, artifactId, version, fetchOptions)
    initDecoder(libraries, artifactId)

  def initDecoder(libraries: Seq[ClasspathEntry], artifactId: String)(using ThrowOrWarn): TestingDecoder =
    val library = libraries.find(l => l.name.startsWith(artifactId)).get
    TestingDecoder(library, libraries)

  extension (decoder: TestingDecoder)
    def showFields(className: String): Unit =
      val fields = decoder.classLoader.loadClass(className).declaredFields
      println(s"Available binary fields in $className are:\n" + fields.map(f => s"  " + formatField(f)).mkString("\n"))

    def assertDecode(className: String, expected: String)(using munit.Location): Unit =
      val cls = decoder.classLoader.loadClass(className)
      val decodedClass = decoder.decode(cls)
      assertEquals(formatter.format(decodedClass), expected)

    def assertDecode(className: String, method: String, expected: String, generated: Boolean = false)(using
        munit.Location
    ): Unit =
      val binaryMethod = loadBinaryMethod(className, method)
      val decodedMethod = decoder.decode(binaryMethod)
      assertEquals(formatter.format(decodedMethod), expected)
      assertEquals(decodedMethod.isGenerated, generated)

    def assertDecodeField(className: String, field: String, expected: String, generated: Boolean = false)(using
        munit.Location
    ): Unit =
      val binaryField: binary.Field = loadBinaryField(className, field)
      val decodedField = decoder.decode(binaryField)
      assertEquals(formatter.format(decodedField), expected)
      assertEquals(decodedField.isGenerated, generated)

    def assertAmbiguousField(className: String, field: String)(using munit.Location): Unit =
      val binaryField: binary.Field = loadBinaryField(className, field)
      intercept[AmbiguousException](decoder.decode(binaryField))

    def assertNotFoundField(className: String, field: String)(using munit.Location): Unit =
      val binaryField = loadBinaryField(className, field)
      intercept[NotFoundException](decoder.decode(binaryField))

    def assertNotFound(declaringType: String, javaSig: String)(using munit.Location): Unit =
      val method = loadBinaryMethod(declaringType, javaSig)
      intercept[NotFoundException](decoder.decode(method))

    def assertDecodeAllInClass(
        className: String
    )(expectedMethods: ExpectedCount = ExpectedCount(0), printProgress: Boolean = false)(using munit.Location): Unit =
      val binaryClass = decoder.classLoader.loadClass(className)
      val decodedClass = decoder.decode(binaryClass)
      val methodCounter = Counter(className + " methods")
      val binaryMethods = binaryClass.declaredMethods
      var reported = 0
      for (binaryMethod, i) <- binaryMethods.zipWithIndex do
        val percent = i * 100 / binaryMethods.size
        if percent > reported && printProgress then
          println(percent + "%")
          reported = percent
        decoder.tryDecode(decodedClass, binaryMethod, methodCounter)
      methodCounter.check(expectedMethods)

    def assertDecodeAll(
        expectedClasses: ExpectedCount = ExpectedCount(0),
        expectedMethods: ExpectedCount = ExpectedCount(0),
        expectedFields: ExpectedCount = ExpectedCount(0),
        printProgress: Boolean = false
    )(using munit.Location): Unit =
      val (classCounter, methodCounter, fieldCounter) = decodeAll(printProgress)
      if classCounter.throwables.nonEmpty then
        classCounter.printThrowables()
        classCounter.printThrowable(0)
      else if methodCounter.throwables.nonEmpty then
        methodCounter.printThrowables()
        methodCounter.printThrowable(0)
      fieldCounter.printNotFound()
      classCounter.check(expectedClasses)
      methodCounter.check(expectedMethods)
      fieldCounter.check(expectedFields)

    def decodeAll(printProgress: Boolean = false): (Counter, Counter, Counter) =
      val classCounter = Counter(decoder.name + " classes")
      val methodCounter = Counter(decoder.name + " methods")
      val fieldCounter = Counter(decoder.name + " fields")
      for
        binaryClass <- decoder.allClasses
        _ = if printProgress then println(s"\"${binaryClass.name}\"")
        decodedClass <- decoder.tryDecode(binaryClass, classCounter)
        binaryMethodOrField <- binaryClass.declaredMethods ++ binaryClass.declaredFields
      do
        if printProgress then println(formatDebug(binaryMethodOrField))
        binaryMethodOrField match
          case m: binary.Method =>
            decoder.tryDecode(decodedClass, m, methodCounter)
          case f: binary.Field =>
            decoder.tryDecode(decodedClass, f, fieldCounter)
      classCounter.printReport()
      methodCounter.printReport()
      fieldCounter.printReport()
      (classCounter, methodCounter, fieldCounter)

    private def loadBinaryMethod(declaringType: String, method: String)(using
        munit.Location
    ): binary.Method =
      val binaryMethods = decoder.classLoader.loadClass(declaringType).declaredMethods
      def notFoundMessage: String =
        s"""|$method
            |    Available binary methods in $declaringType are:
            |""".stripMargin + binaryMethods.map(m => s"        " + formatMethod(m)).mkString("\n")
      binaryMethods.find(m => formatMethod(m) == method).getOrElse(throw new NoSuchElementException(notFoundMessage))

    private def loadBinaryField(declaringType: String, field: String)(using
        munit.Location
    ): binary.Field =
      val binaryFields = decoder.classLoader.loadClass(declaringType).declaredFields
      def notFoundMessage: String =
        s"""|$field
            |    Available binary fields in $declaringType are:
            |""".stripMargin + binaryFields.map(f => s"        " + formatField(f)).mkString("\n")
      binaryFields.find(f => formatField(f) == field).getOrElse(throw new NoSuchElementException(notFoundMessage))

    private def tryDecode(cls: binary.ClassType, counter: Counter): Option[DecodedClass] =
      try
        val sym = decoder.decode(cls)
        counter.success += (cls -> sym)
        Some(sym)
      catch
        case ambiguious: AmbiguousException =>
          counter.ambiguous += ambiguious
          None
        case notFound: NotFoundException =>
          counter.notFound += (cls -> notFound)
          None
        case e =>
          counter.throwables += (cls -> e)
          None

    private def tryDecode(cls: DecodedClass, mthd: binary.Method, counter: Counter): Unit =
      try
        val decoded = decoder.decode(cls, mthd)
        counter.success += (mthd -> decoded)
      catch
        case notFound: NotFoundException => counter.notFound += (mthd -> notFound)
        case ambiguous: AmbiguousException => counter.ambiguous += ambiguous
        case ignored: IgnoredException => counter.ignored += ignored
        case e => counter.throwables += (mthd -> e)

    private def tryDecode(cls: DecodedClass, field: binary.Field, counter: Counter): Unit =
      try
        val decoded = decoder.decode(cls, field)
        counter.success += (field -> decoded)
      catch
        case notFound: NotFoundException => counter.notFound += (field -> notFound)
        case ambiguous: AmbiguousException => counter.ambiguous += ambiguous
        case ignored: IgnoredException => counter.ignored += ignored
        case e => counter.throwables += (field -> e)
  end extension

  private def formatDebug(m: binary.Symbol): String =
    m match
      case f: binary.Field => s"\"${f.declaringClass}\", \"${formatField(f)}\""
      case m: binary.Method => s"\"${m.declaringClass.name}\", \"${formatMethod(m)}\""
      case cls => s"\"${cls.name}\""

  private def formatMethod(m: binary.Method): String =
    val returnType = m.returnType.map(_.name).get
    val parameters = m.allParameters.map(p => p.`type`.name + " " + p.name).mkString(", ")
    s"$returnType ${m.name}($parameters)"

  private def formatField(f: binary.Field): String =
    s"${f.`type`.name} ${f.name}"

  case class ExpectedCount(success: Int, ambiguous: Int = 0, notFound: Int = 0, throwables: Int = 0)

  case class Count(name: String, success: Int = 0, ambiguous: Int = 0, notFound: Int = 0, throwables: Int = 0):
    def size: Int = success + notFound + ambiguous + throwables
    def successPercent: Float = percent(success)

    def check(expected: ExpectedCount)(using munit.Location): Unit =
      assertEquals(success, expected.success)
      assertEquals(ambiguous, expected.ambiguous)
      assertEquals(notFound, expected.notFound)
      assertEquals(throwables, expected.throwables)

    def merge(count: Count): Count =
      Count(
        name,
        count.success + success,
        count.ambiguous + ambiguous,
        count.notFound + notFound,
        count.throwables + throwables
      )

    def printReport() =
      def format(kind: String, count: Int): Option[String] =
        Option.when(count > 0)(s"$kind: $count (${percent(count)}%)")
      if size > 0 then
        val stats =
          Seq("success" -> success, "ambiguous" -> ambiguous, "not found" -> notFound, "throwables" -> throwables)
            .flatMap(format)
            .map("\n  - " + _)
            .mkString
        println(s"$name ($size): $stats")

    private def percent(count: Int): Float = count.toFloat * 100 / size
  end Count

  class Counter(val name: String):
    val success = mutable.Buffer.empty[(binary.Symbol, DecodedSymbol)]
    val notFound = mutable.Buffer.empty[(binary.Symbol, NotFoundException)]
    val ambiguous = mutable.Buffer.empty[AmbiguousException]
    val ignored = mutable.Buffer.empty[IgnoredException]
    val throwables = mutable.Buffer.empty[(binary.Symbol, Throwable)]

    def count: Count = Count(name, success.size, ambiguous.size, notFound.size, throwables.size)

    def printReport() = count.printReport()

    def printComparisionWithJavaFormatting(): Unit =
      def formatJavaStyle(m: binary.Method): String =
        s"${m.declaringClass.name}.${m.name}(${m.allParameters.map(_.`type`.name).mkString(",")})"

      val formatted = success
        .collect { case (m: binary.Method, d: DecodedMethod) => (formatJavaStyle(m), formatter.format(d)) }
        .sortBy((j, s) => s.size - j.size)

      println(formatted.take(10).mkString("min:\n  ", "\n  ", ""))
      println(formatted.takeRight(10).mkString("max:\n  ", "\n  ", ""))
      println(s"mean: ${formatted.map((j, s) => s.size - j.size).sum / formatted.size}")
    end printComparisionWithJavaFormatting

    def printNotFound() =
      notFound.foreach { case (s1, NotFoundException(s2)) =>
        if s1 != s2 then println(s"${formatDebug(s1)} not found because of ${formatDebug(s2)}")
        else println(s"${formatDebug(s1)} not found")
      }

    def printAmbiguous() =
      ambiguous.foreach { case AmbiguousException(s, candidates) =>
        println(s"${formatDebug(s)} is ambiguous:" + candidates.map(s"\n  - " + _).mkString)
      }

    def printThrowable(i: Int) =
      if throwables.size > i then
        val (sym, t) = throwables(i)
        println(s"${formatDebug(sym)} $t")
        t.printStackTrace()

    def printThrowables() = throwables.foreach { (sym, t) =>
      println(s"${formatDebug(sym)} $t")
    }

    def check(expected: ExpectedCount)(using munit.Location): Unit =
      assertEquals(success.size, expected.success)
      assertEquals(ambiguous.size, expected.ambiguous)
      assertEquals(notFound.size, expected.notFound)
      assertEquals(throwables.size, expected.throwables)
