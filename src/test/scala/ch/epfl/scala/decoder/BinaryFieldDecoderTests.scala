package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.testutils.*
import tastyquery.Exceptions.*

import scala.util.Properties

class Scala3LtsBinaryFieldDecoderTests extends BinaryFieldDecoderTests(ScalaVersion.`3.lts`)
class Scala3NextBinaryFieldDecoderTests extends BinaryFieldDecoderTests(ScalaVersion.`3.next`)

abstract class BinaryFieldDecoderTests(scalaVersion: ScalaVersion) extends BinaryDecoderSuite:
  def isScala33 = scalaVersion.isScala33
  def isScala34 = scalaVersion.isScala34

  test("public and private fields") {
    val source =
      """|package example
         |
         |class A {
         |  var x: Int = 1
         |  var `val`: Int = 1
         |  private val y: String = "y"
         |  lazy val z: Int = 2
         |
         |  def foo: String = y
         |}
         |
         |object A {
         |  val z: Int = 2
         |  private var w: String = "w"
         |  private lazy val v: Int = 3
         |
         |  def bar: String = w + v
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "int x", "A.x: Int")
    decoder.assertDecodeField("example.A", "int val", "A.val: Int")
    decoder.assertDecodeField("example.A", "java.lang.String y", "A.y: String")
    decoder.assertDecodeField("example.A", "java.lang.Object z$lzy1", "A.z: Int")
    decoder.assertDecodeField("example.A$", "int z", "A.z: Int")
    decoder.assertDecodeField("example.A$", "java.lang.String w", "A.w: String")
    decoder.assertDecodeField("example.A$", "java.lang.Object v$lzy1", "A.v: Int")
  }

  test("public and private objects") {
    val source =
      """|package example
         |
         |class A {
         |  object B
         |  private object C
         |}
         |
         |object A {
         |  object D
         |  private object E
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "java.lang.Object B$lzy1", "A.B: B")
    decoder.assertDecodeField("example.A", "java.lang.Object C$lzy1", "A.C: C")
    decoder.assertDecodeField("example.A$", "example.A$ MODULE$", "A: A")
    decoder.assertDecodeField("example.A", "long OFFSET$1", "A.<offset 1>: Long")
    decoder.assertDecodeField("example.A", "long OFFSET$0", "A.<offset 0>: Long")
    decoder.assertDecodeField("example.A$", "example.A$D$ D", "A.D: D")
    decoder.assertDecodeField("example.A$D$", "example.A$D$ MODULE$", "A.D: D")
    decoder.assertDecodeField("example.A$", "example.A$E$ E", "A.E: E")
    decoder.assertDecodeField("example.A$E$", "example.A$E$ MODULE$", "A.E: E")
  }

  test("fields in extended trait") {
    val source =
      """|package example
         |
         |trait A {
         |  private val x: Int = 1
         |  private val y: Int = 2
         |  val z: Int = 3
         |}
         |
         |class B extends A {
         |  val y: Int = 2
         |}
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.B", "int example$A$$x", "B.x: Int")
    decoder.assertDecodeField("example.B", "int z", "B.z: Int")
    // TODO fix
    // decoder.assertDecodeField("example.B", "int y", "B.y: Int")
    // decoder.assertDecodeField("example.B", "int example$A$$y", "B.y: Int")
  }

  test("given field in extended trait") {
    val source =
      """|package example
         |
         |trait A:
         |  given x: Int = 1
         |
         |class C extends A     
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.C", "java.lang.Object x$lzy1", "C.x: Int")
  }

  test("expanded names") {
    val source =
      """|package example
         |
         |trait A {
         |  def foo = 
         |    enum B:
         |      case C, D
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField(
      "example.A$B$3$",
      "scala.runtime.LazyRef example$A$B$3$$$B$lzy1$3",
      "A.foo.B.B.<capture>: B"
    )
  }

  test("static fields in Java") {
    val source =
      """|package example;
         |
         |final class A {
         |  public static final int x = 1;
         |}
         |""".stripMargin
    val javaModule = Module.fromJavaSource(source, scalaVersion)
    val decoder = TestingDecoder(javaModule.mainEntry, javaModule.classpath)
    decoder.assertDecodeField("example.A", "int x", "A.x: Int")
  }

  test("case field in JavaLangEnum") {
    val source =
      """|package example
         |
         |enum A extends java.lang.Enum[A] :
         |  case B
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "example.A B", "A.B: A")
  }

  test("anonymous using parameter") {
    val source =
      """|package example
         |
         |trait C
         |
         |class B (using C):
         |  def foo = summon[C]
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.B", "example.C x$1", "B.x$1: C")
  }

  test("lazy val bitmap") {
    val source =
      """|package example
         |import scala.annotation.threadUnsafe
         |
         |class A:
         |  @threadUnsafe lazy val x: Int = 1
         |  @threadUnsafe lazy val y: Int = 1
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "boolean xbitmap$1", "A.x.<lazy val bitmap>: Boolean")
    decoder.assertDecodeField("example.A", "boolean ybitmap$1", "A.y.<lazy val bitmap>: Boolean")
  }

  test("serialVersionUID fields") {
    val source =
      """|package example
         |
         |@SerialVersionUID(1L)
         |class A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "long serialVersionUID", "A.<serialVersionUID>: Long")
  }

  test("offset_m field") {
    val source =
      """|package example
         |
         |trait A {
         |  def foo: Int
         |}
         |class C:
         |  object B extends A {
         |    lazy val foo: Int = 42
         |  }
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.C$B$", "long OFFSET$_m_0", "C.B.<offset 0>: Long")
  }

  test("ambiguous module val and implicit def fields") {
    val source =
      """|package example
         |
         |object A {
         |  object B
         |
         |  implicit class B (val x: Int) 
         |
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A$", "example.A$B$ B", "A.B: B")
  }

  test("anon lazy val") {
    val source =
      """|package example
         |
         |class A:
         |  lazy val (a, b) = (1, 2)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "java.lang.Object $1$$lzy1", "A.<anon>: (Int, Int)")
  }

  test("outer field and param") {
    val source =
      """|package example
         |
         |class A[T](x: T){
         |  class B {
         |    def foo: T = x
         |  }
         |
         |  def bar: T = {
         |    class C {
         |      def foo: T = x
         |    }
         |    (new C).foo
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "java.lang.Object example$A$$x", "A.x: T")
    decoder.assertDecodeField("example.A$B", "example.A $outer", "A.B.<outer>: A[T]")
    decoder.assertDecodeVariable("example.A$B", "void <init>(example.A $outer)", "example.A $outer", 5, "<outer>: A[T]")
    decoder.assertDecodeField("example.A$C$1", "example.A $outer", "A.bar.C.<outer>: A[T]")
    decoder.assertDecodeVariable(
      "example.A$C$1",
      "void <init>(example.A $outer)",
      "example.A $outer",
      10,
      "<outer>: A[T]"
    )
  }

  test("intricated outer fields") {
    val source =
      """|package example
         |
         |trait A {
         |  class X
         |}
         |
         |trait B extends A {
         |  class Y {
         |    class Z extends X
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A$X", "example.A $outer", "A.X.<outer>: A")
    decoder.assertDecodeField("example.B$Y$Z", "example.B$Y $outer", "B.Y.Z.<outer>: Y")
  }

  test("indirect capture") {
    val source =
      """|package example
         |
         |class A():
         |  def foo = 
         |    val x: Int = 1
         |    def met = x
         |    class B:
         |      def bar = met
         |      
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A$B$1", "int x$2", "A.foo.B.x.<capture>: Int")
    decoder.assertDecodeVariable(
      "example.A$B$1",
      "void <init>(int x$3, example.A $outer)",
      "int x$3",
      8,
      "x.<capture>: Int"
    )
  }

  test("ambiguous indirect captures") {
    val source =
      """|package example
         |
         |class A():
         |  def bar =
         |    val x: Int = 12
         |    def getX = x
         |    def foo = 
         |      val x: Int = 1
         |      def met = x
         |      class B:
         |        def bar2 = met
         |        def bar3: Int = getX
         |      
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertAmbiguousField("example.A$B$1", "int x$3")
    decoder.assertAmbiguousField("example.A$B$1", "int x$4")
  }

  test("captured lazy ref") {
    val source =
      """|package example
         |trait C
         |
         |class A {
         |  def foo =
         |    lazy val c: C = new C {}
         |    class B:
         |      def ct = c
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A$B$1", "scala.runtime.LazyRef c$lzy1$3", "A.foo.B.c.<capture>: C")
    decoder.assertDecodeVariable(
      "example.A$B$1",
      "void <init>(scala.runtime.LazyRef c$lzy1$4, example.A $outer)",
      "scala.runtime.LazyRef c$lzy1$4",
      8,
      "c.<capture>: C"
    )
  }

  test("local class capture") {
    val source =
      """|package example
         |
         |class Foo {
         |  def foo = 
         |    val x = " "
         |    class A:
         |      def bar = x
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.Foo$A$1", "java.lang.String x$1", "Foo.foo.A.x.<capture>: String")
    decoder.assertDecodeVariable(
      "example.Foo$A$1",
      "void <init>(java.lang.String x$2)",
      "java.lang.String x$2",
      7,
      "x.<capture>: String"
    )
  }

  test("captured value class") {
    val source =
      """|package example
         |
         |class A(val x: Int) extends AnyVal:
         |  def foo =
         |    class B:
         |      def bar = x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A$B$1", "int $this$1", "A.foo.B.x.<capture>: Int")
  }

  test("captured through inline method") {
    val source =
      """|package example
         |
         |trait C
         |
         |object A:
         |  inline def withMode(inline op: C ?=> Unit)(using C): Unit = op
         |
         |  def foo(using C) = withMode {
         |    class B:
         |      def bar = summon[C]
         |  }
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertNotFoundField("example.A$B$1", "example.C x$1$1")
  }
