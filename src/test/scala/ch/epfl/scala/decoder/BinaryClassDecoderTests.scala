package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.testutils.*
import tastyquery.Exceptions.*

import scala.util.Properties

class Scala3LtsBinaryClassDecoderTests extends BinaryClassDecoderTests(ScalaVersion.`3.lts`)
class Scala3NextBinaryClassDecoderTests extends BinaryClassDecoderTests(ScalaVersion.`3.next`)

abstract class BinaryClassDecoderTests(scalaVersion: ScalaVersion) extends BinaryDecoderSuite:
  def isScala33 = scalaVersion.isScala33
  def isScala34 = scalaVersion.isScala34

  test("local class, trait and object by parents") {
    val source =
      """|package example
         |object Main :
         |  class A
         |  def main(args: Array[String]): Unit = 
         |    trait D extends A
         |    class C extends D
         |    object F extends D
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeClass("example.Main$D$1", "Main.main.D")
    decoder.assertDecodeClass("example.Main$C$1", "Main.main.C")
    decoder.assertDecodeClass("example.Main$F$2$", "Main.main.F")
    decoder.assertDecodeMethod(
      "example.Main$",
      "example.Main$F$2$ F$1(scala.runtime.LazyRef F$lzy1$2)",
      "Main.main.F: F",
      generated = true
    )
    decoder.assertDecodeMethod(
      "example.Main$",
      "example.Main$F$2$ F$lzyINIT1$1(scala.runtime.LazyRef F$lzy1$1)",
      "Main.main.F.<lazy init>: F"
    )
  }

  test("find local classes") {
    val source =
      """|package example
         |class A 
         |trait B         
         |object Main:
         |  def m() = 
         |    class C extends A,B 
         |    ()
         |    class E :
         |      class F 
         |    class G extends A
         |  def l () = 
         |    class C extends A 
         |    class G extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeClass("example.Main$C$1", "Main.m.C")
    decoder.assertDecodeClass("example.Main$E$1$F", "Main.m.E.F")
    decoder.assertDecodeClass("example.Main$G$1", "Main.m.G")
  }

  test("anonymous class") {
    val source =
      """|package example
         |class B :
         |  def n = 42
         |class A :
         |  def m(t: => Any): Int = 
         |    val b = new B {
         |      def m = ()
         |    }
         |    b.n
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeClass("example.A$$anon$1", "A.m.<anon class>")
  }

  test("local enum") {
    val source =
      """|package example
         |object Main :
         |  def m =
         |    enum A:
         |      case B
         |    ()
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeClass("example.Main$A$1", "Main.m.A")
  }

  test("tasty-query#412"):
    val decoder = initDecoder("dev.zio", "zio-interop-cats_3", "23.1.0.0")(using ThrowOrWarn.ignore)
    decoder.assertDecodeClass("zio.BuildInfoInteropCats", "BuildInfoInteropCats")
    decoder.assertDecodeClass("zio.interop.ZioMonadErrorE$$anon$4", "ZioMonadErrorE.adaptError.<anon PartialFunction>")

  test("tasty-query#414"):
    val decoder = initDecoder("io.github.dieproht", "matr-dflt-data_3", "0.0.3")
    decoder.assertDecodeClass(
      "matr.dflt.DefaultMatrixFactory$$anon$1",
      "DefaultMatrixFactory.defaultMatrixFactory.builder.<anon class>"
    )

  test("tasty-query#415"):
    val decoder = initDecoder("com.github.mkroli", "dns4s-fs2_3", "0.21.0")
    decoder.assertDecodeClass("com.github.mkroli.dns4s.fs2.DnsClientOps", "DnsClientOps")

  test("tasty-query#423"):
    val decoder = initDecoder("com.typesafe.akka", "akka-stream_3", "2.8.5")
    decoder.assertDecodeClass("akka.stream.scaladsl.FlowOps$passedEnd$2$", "FlowOps.zipAllFlow.passedEnd")

  test("tasty-query#424"):
    val decoder = initDecoder("edu.gemini", "lucuma-itc-core_3", "0.10.0")
    decoder.assertDecodeClass("lucuma.itc.ItcImpl", "ItcImpl")

  test("specialized class"):
    val decoder = initDecoder("org.scala-lang", "scala-library", "2.13.12")
    decoder.assertDecodeClass("scala.runtime.java8.JFunction1$mcII$sp", "JFunction1$mcII$sp")

  test("local class in value class"):
    val source =
      """|package example
         |
         |class A(self: String) extends AnyVal:
         |  def m(size: Int): String =
         |    class B:
         |      def m(): String =
         |        self.take(size)
         |    val b = new B
         |    b.m()
         |""".stripMargin
    // tasty-query#428
    val decoder = TestingDecoder(source, scalaVersion)(using ThrowOrWarn.ignore)
    decoder.assertDecodeClass("example.A$B$1", "A.m.B")
