package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.testutils.*
import tastyquery.Exceptions.*

import scala.util.Properties

class Scala3LtsBinaryDecoderTests extends BinaryDecoderTests(ScalaVersion.`3.lts`)
class Scala3NextBinaryDecoderTests extends BinaryDecoderTests(ScalaVersion.`3.next`)

abstract class BinaryDecoderTests(scalaVersion: ScalaVersion) extends BinaryDecoderSuite:
  def isScala33 = scalaVersion.isScala33
  def isScala34 = scalaVersion.isScala34

  test("scala3-compiler:3.3.1"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.3.1")
    decoder.assertDecodeVariable(
      "scala.quoted.runtime.impl.QuoteMatcher$",
      "scala.Option treeMatch(dotty.tools.dotc.ast.Trees$Tree scrutineeTree, dotty.tools.dotc.ast.Trees$Tree patternTree, dotty.tools.dotc.core.Contexts$Context x$3)",
      "scala.util.boundary$Break ex",
      "ex: Break[T]",
      128
    )

  test("tailLocal variables") {
    val source =
      """|package example
         |
         |class A {
         |  @annotation.tailrec
         |  private def factAcc(x: Int, acc: Int): Int =
         |    if x <= 1 then List(1, 2).map(_ * acc).sum
         |    else factAcc(x - 1, x * acc)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int factAcc$$anonfun$1(int acc$tailLocal1$1, int _$1)")
    // decoder.assertDecodeVariable("example.A", "int factAcc$$anonfun$1(int acc$tailLocal1$1, int _$1)", "int acc$tailLocal1$1", "acc.<capture>: Int", 6)
  }

  test("SAMOrPartialFunctionImpl") {
    val source =
      """|package example
         |
         |class A:
         |  def foo(x: Int) = 
         |    val xs = List(x, x + 1, x + 2)
         |    xs.collect { case z if z % 2 == 0 => z }
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A$$anon$1", "boolean isDefinedAt(int x)")
    decoder.assertDecodeVariable(
      "example.A$$anon$1",
      "boolean isDefinedAt(int x)",
      "int x",
      "x: A",
      6
    )
    decoder.assertDecodeVariable(
      "example.A$$anon$1",
      "boolean isDefinedAt(int x)",
      "int z",
      "z: Int",
      6
    )

    decoder.showVariables("example.A$$anon$1", "java.lang.Object applyOrElse(int x, scala.Function1 default)")
    decoder.assertDecodeVariable(
      "example.A$$anon$1",
      "java.lang.Object applyOrElse(int x, scala.Function1 default)",
      "scala.Function1 default",
      "default: A1 => B1",
      6
    )
  }

  test("inlined this") {
    val source =
      """|package example
         |
         |class A(x: Int):
         |  inline def foo: Int = x + x
         |
         |class B:
         |  def bar(a: A) = a.foo
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.B", "int bar(example.A a)")
    decoder.assertDecodeVariable(
      "example.B",
      "int bar(example.A a)",
      "example.A A_this",
      "this: A.this.type",
      7,
      generated = true
    )
  }

  test("inlined param") {
    val source =
      """|package example
         |
         |class A {
         |  inline def foo(x: Int): Int = x + x
         |
         |  def bar(y: Int) = foo(y + 2)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int bar(int y)")
    decoder.assertDecodeVariable("example.A", "int bar(int y)", "int x$proxy1", "x: Int", 4)
  }

  test("bridge parameter") {
    val source =
      """|package example
         |class A
         |class B extends A
         |
         |class C:
         |  def foo(x: Int): A = new A
         |
         |class D extends C:
         |  override def foo(y: Int): B = new B
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.D", "example.A foo(int x)")
    decoder.assertIgnoredVariable("example.D", "example.A foo(int x)", "int x", "Bridge")
  }

  test("lazy val capture") {
    val source =
      """|package example
         |
         |class A {
         |  def foo =
         |    val y = 4
         |    lazy val z = y + 1
         |    def bar = z
         |    z
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int bar$1(scala.runtime.LazyInt z$lzy1$3, int y$3)")
    decoder.assertDecodeVariable(
      "example.A",
      "int bar$1(scala.runtime.LazyInt z$lzy1$3, int y$3)",
      "scala.runtime.LazyInt z$lzy1$3",
      "z.<capture>: Int",
      7,
      generated = true
    )

    decoder.showVariables("example.A", "int z$lzyINIT1$1(scala.runtime.LazyInt z$lzy1$1, int y$1)")
    decoder.assertDecodeVariable(
      "example.A",
      "int z$lzyINIT1$1(scala.runtime.LazyInt z$lzy1$1, int y$1)",
      "scala.runtime.LazyInt z$lzy1$1",
      "z.<capture>: Int",
      7,
      generated = true
    )

    decoder.showVariables("example.A", "int z$1(scala.runtime.LazyInt z$lzy1$2, int y$2)")
    decoder.assertDecodeVariable(
      "example.A",
      "int z$1(scala.runtime.LazyInt z$lzy1$2, int y$2)",
      "scala.runtime.LazyInt z$lzy1$2",
      "z.<capture>: Int",
      7,
      generated = true
    )
  }

  test("by-name arg capture") {
    val source =
      """|package example
         |
         |class A {
         |  def foo(x: => Int) = ???
         |
         |  def bar(x: Int) = 
         |    foo(x)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int bar$$anonfun$1(int x$1)")
    decoder.assertDecodeVariable(
      "example.A",
      "int bar$$anonfun$1(int x$1)",
      "int x$1",
      "x.<capture>: Int",
      7,
      generated = true
    )
  }

  test("binds") {
    val source =
      """|package example
         |
         |class B
         |case class C(x: Int, y: String) extends B
         |case class D(z: String) extends B
         |case class E(v: Int) extends B
         |case class F(w: Int) extends B
         |
         |class A:
         |  private def bar(a: B) =
         |    a match
         |      case F(w) => w
         |      case C(x, y) =>
         |        x
         |      case D(z) => 0
         |      case E(v) => 1
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    // decoder.showVariables("example.A", "int bar(example.B a)")
    // decoder.assertDecodeAll(
    //   ExpectedCount(2),
    //   ExpectedCount(37),
    //   expectedFields = ExpectedCount(5)
    // )
    decoder.assertDecodeVariable("example.A", "int bar(example.B a)", "int w", "w: Int", 12)
    decoder.assertDecodeVariable("example.A", "int bar(example.B a)", "int x", "x: Int", 14)

  }

  test("mixin and trait static forwarders") {
    val source =
      """|package example
         |
         |trait A {
         |  def foo(x: Int): Int = x
         |}
         |
         |class B extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.B", "int foo(int x)")
    decoder.showVariables("example.A", "int foo$(example.A $this, int x)")
    decoder.assertDecodeVariable("example.B", "int foo(int x)", "int x", "x: Int", 7)
    decoder.assertDecodeVariable(
      "example.A",
      "int foo$(example.A $this, int x)",
      "example.A $this",
      "this: A.this.type",
      4,
      generated = true
    )
  }

  test("this AnyVal") {
    val source =
      """|package example
         |
         |class A(x: Int) extends AnyVal {
         |  def foo: Int = 
         |    x
         |    
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A$", "int foo$extension(int $this)")
    decoder.assertDecodeVariable(
      "example.A$",
      "int foo$extension(int $this)",
      "int $this",
      "x: Int",
      5,
      generated = true
    )
  }

  test("this variable") {
    val source =
      """|package example
         |
         |class A:
         |  def foo: Int = 
         |    4
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int foo()")
    decoder.assertDecodeVariable("example.A", "int foo()", "example.A this", "this: A.this.type", 5, generated = true)
  }

  test("binds tuple and pattern matching") {
    val source =
      """|package example
         |
         |class A {
         |  def foo: Int = 
         |    val x = (1, 2)
         |    val (c, d) = (3, 4)
         |    x match
         |      case (a, b) => a + b
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int foo()")
    decoder.assertDecodeVariable("example.A", "int foo()", "scala.Tuple2 x", "x: (Int, Int)", 5)
    decoder.assertDecodeVariable("example.A", "int foo()", "int c", "c: Int", 6)
    decoder.assertDecodeVariable("example.A", "int foo()", "int d", "d: Int", 6)
    decoder.assertDecodeVariable("example.A", "int foo()", "int a", "a: Int", 8)
    decoder.assertDecodeVariable("example.A", "int foo()", "int b", "b: Int", 8)
  }

  test("ambiguous impossible") {
    val source =
      """|package example
         |
         |class A:
         |  def foo(a: Boolean) =
         |    if (a) {val x = 1} else {val x = 2}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "void foo(boolean a)")
    decoder.assertAmbiguousVariable("example.A", "void foo(boolean a)", "int x", 5)
  }

  test("ambiguous variables 2") {
    val source =
      """|package example
         |
         |class A:
         |  def foo() =
         |    var i = 0
         |    while i < 10 do
         |      val x = i
         |      i += 1
         |    val x = 17
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "void foo()")
    decoder.assertDecodeVariable("example.A", "void foo()", "int x", "x: Int", line = 7)
    decoder.assertDecodeVariable("example.A", "void foo()", "int x", "x: Int", line = 9)
  }

  test("ambiguous variables") {
    val source =
      """|package example
         |
         |class A :
         |  def foo(a: Boolean) =
         |    if a then
         |      val x = 1
         |      x
         |    else
         |      val x = "2"
         |      x
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "java.lang.Object foo(boolean a)")
    decoder.assertDecodeVariable("example.A", "java.lang.Object foo(boolean a)", "int x", "x: Int", line = 7)
    decoder.assertDecodeVariable(
      "example.A",
      "java.lang.Object foo(boolean a)",
      "java.lang.String x",
      "x: String",
      line = 9
    )
  }

  test("local object") {
    val source =
      """|package example
         |
         |class A {
         |  def foo() =
         |    object B
         |    B
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "java.lang.Object foo()")
    decoder.assertNoSuchElementVariable("example.A", "java.lang.Object foo()", "example.A$B$2$ B$1")
  }

  test("local lazy val") {
    val source =
      """|package example
         |
         |class A:
         |  def foo() =
         |    lazy val x: Int = 1
         |    x
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int foo()")
    decoder.assertNoSuchElementVariable("example.A", "int foo()", "int x$1$lzyVal")
  }

  test("array") {
    val source =
      """|package example
         |
         |class A {
         |  def foo() = 
         |    val x = Array(1, 2, 3)
         |    x
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int[] foo()")
    decoder.assertDecodeVariable("example.A", "int[] foo()", "int[] x", "x: Array[Int]", 6)
  }

  test("captured param in a local def") {
    val source =
      """|package example
         |
         |class A {
         |  def foo(x: Int) = {
         |    def bar() = x
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int bar$1(int x$1)")
    decoder.assertDecodeVariable(
      "example.A",
      "int bar$1(int x$1)",
      "int x$1",
      "x.<capture>: Int",
      5,
      generated = true
    )
  }

  test("method parameter") {
    val source =
      """|package example
         |
         |class A:
         |  def foo(y: String) =
         |    println(y)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "void foo(java.lang.String y)")
    decoder.assertDecodeVariable(
      "example.A",
      "void foo(java.lang.String y)",
      "java.lang.String y",
      "y: String",
      5
    )
  }

  test("local variable") {
    val source =
      """|package example
         |
         |class A:
         |  def foo =
         |    val x: Int = 1
         |    x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int foo()")
    decoder.assertDecodeVariable("example.A", "int foo()", "int x", "x: Int", 6)
  }

  test("capture value class") {
    val source =
      """|package example
         |
         |class A(val x: Int) extends AnyVal:
         |  def foo =
         |    class B:
         |      def bar = x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A$B$1", "int $this$1", "A.foo.B.x.<capture>: Int", generated = true)
  }

  test("capture inline method") {
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

  test("expanded names fields") {
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
      "A.foo.B.B.<capture>: B",
      generated = true
    )
  }

  test("lazy ref") {
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
    decoder.assertDecodeField(
      "example.A$B$1",
      "scala.runtime.LazyRef c$lzy1$3",
      "A.foo.B.c.<capture>: C",
      generated = true
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
    decoder.assertDecodeField("example.A$B$1", "int x$2", "A.foo.B.x.<capture>: Int", generated = true)
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
    decoder.assertDecodeField("example.A", "boolean xbitmap$1", "A.x.<lazy val bitmap>: Boolean", generated = true)
    decoder.assertDecodeField("example.A", "boolean ybitmap$1", "A.y.<lazy val bitmap>: Boolean", generated = true)
  }

  test("class defined in a method fields") {
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
    decoder.assertDecodeField(
      "example.Foo$A$1",
      "java.lang.String x$1",
      "Foo.foo.A.x.<capture>: String",
      generated = true
    )
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

  test("serialVersionUID fields") {
    val source =
      """|package example
         |
         |@SerialVersionUID(1L)
         |class A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeField("example.A", "long serialVersionUID", "A.<serialVersionUID>: Long", generated = true)
  }

  test("static fields in static classes Java") {
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

  test("extend trait with given fields") {
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

  test("extend traits with val fields") {
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

  test("notFound offset_m field") {
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
    decoder.assertDecodeField("example.C$B$", "long OFFSET$_m_0", "C.B.<offset 0>: Long", generated = true)
  }

  test("ambiguous Object/ImplicitClass fields") {
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
    decoder.assertDecodeField("example.A$", "example.A$ MODULE$", "A: A", true)
    decoder.assertDecodeField("example.A", "long OFFSET$1", "A.<offset 1>: Long", true)
    decoder.assertDecodeField("example.A", "long OFFSET$0", "A.<offset 0>: Long", true)
    decoder.assertDecodeField("example.A$", "example.A$D$ D", "A.D: D")
    decoder.assertDecodeField("example.A$D$", "example.A$D$ MODULE$", "A.D: D", true)
    decoder.assertDecodeField("example.A$", "example.A$E$ E", "A.E: E")
    decoder.assertDecodeField("example.A$E$", "example.A$E$ MODULE$", "A.E: E", true)
  }

  test("outer field") {
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
    decoder.assertDecodeField("example.A$B", "example.A $outer", "A.B.<outer>: A[T]", generated = true)
    decoder.assertDecodeField("example.A$C$1", "example.A $outer", "A.bar.C.<outer>: A[T]", generated = true)
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
    decoder.assertDecodeField("example.A$X", "example.A $outer", "A.X.<outer>: A", generated = true)
    decoder.assertDecodeField("example.B$Y$Z", "example.B$Y $outer", "B.Y.Z.<outer>: Y", generated = true)
  }

  test("mixin and static forwarders") {
    val source =
      """|package example
         |
         |trait A {
         |  def m(): String = "A.m()"
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a1 = new A {}
         |    val a2 = new A {
         |      override def m(): String = "g.m()"
         |    }
         |  }
         |
         |  private class G extends A {
         |    override def m(): String = "G.m()"
         |  }
         |
         |  class H extends A
         |}
         |
         |class B extends A
         |
         |class C extends A {
         |  def m(x: Int): String = s"C.m($x)"
         |}
         |
         |class D extends A {
         |  override def m(): String = "D.m()"
         |}
         |
         |class E extends D
         |
         |object F extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    val javaSig = "java.lang.String m()"
    val staticTraitAccessor = "java.lang.String m$(example.A $this)"

    decoder.assertDecode("example.A", javaSig, "A.m(): String")
    decoder.assertDecode("example.A", staticTraitAccessor, "A.m.<static forwarder>(): String", generated = true)
    decoder.assertDecode("example.B", javaSig, "B.m.<mixin forwarder>(): String", generated = true)
    decoder.assertDecode("example.C", javaSig, "C.m.<mixin forwarder>(): String", generated = true)
    decoder.assertDecode("example.D", javaSig, "D.m(): String")
    decoder.assertDecode("example.F$", javaSig, "F.m.<mixin forwarder>(): String", generated = true)
    decoder.assertDecode("example.F", javaSig, "F.m.<static forwarder>(): String", generated = true)
    decoder.assertDecode("example.Main$G", javaSig, "Main.G.m(): String")
    decoder.assertDecode("example.Main$H", javaSig, "Main.H.m.<mixin forwarder>(): String", generated = true)
    decoder.assertDecode(
      "example.Main$$anon$1",
      javaSig,
      "Main.main.<anon class>.m.<mixin forwarder>(): String",
      generated = true
    )
    decoder.assertDecode("example.Main$$anon$2", javaSig, "Main.main.<anon class>.m(): String")
  }

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
    decoder.assertDecode("example.Main$D$1", "Main.main.D")
    decoder.assertDecode("example.Main$C$1", "Main.main.C")
    decoder.assertDecode("example.Main$F$2$", "Main.main.F")
    decoder.assertDecode(
      "example.Main$",
      "example.Main$F$2$ F$1(scala.runtime.LazyRef F$lzy1$2)",
      "Main.main.F: F",
      generated = true
    )
    decoder.assertDecode(
      "example.Main$",
      "example.Main$F$2$ F$lzyINIT1$1(scala.runtime.LazyRef F$lzy1$1)",
      "Main.main.F.<lazy init>: F"
    )
  }

  test("local class and local method in a local class") {
    val source =
      """|package example
         |object Main {
         |  def m =
         |    class A // Main$A$1
         |    class Bar :
         |      def A = ()
         |      def m =
         |        class A  // Main$A$2
         |        def A() = () // Main.example$Main$Bar$1$$_$A$3()
         |        A()
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "void example$Main$Bar$1$$_$A$3()", "Main.m.….m.A(): Unit")
  }

  test("local methods with same name") {
    val source =
      """|package example
         |
         |class A {
         |  val x = "x"
         |  def m1: Unit = {
         |    val y = "y"
         |    def m: Unit = {
         |      def m(z: String): Unit =
         |        println(x + y + z)
         |      m("z")
         |    }
         |    m
         |  }
         |
         |  def m2: Unit = {
         |    def m(i: Int): Unit = println(i)
         |    m(1)
         |  }
         |
         |  def m3: Unit = {
         |    def m(i: Int): Unit = println(i)
         |    m(2)
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "void m$1(java.lang.String y$1)", "A.m1.m: Unit")
    decoder.assertDecode(
      "example.A",
      "void m$2(java.lang.String y$2, java.lang.String z)",
      "A.m1.m.m(z: String): Unit"
    )
    decoder.assertDecode("example.A", "void m$3(int i)", "A.m2.m(i: Int): Unit")
  }

  test("getters and setters") {
    val source =
      """|package example
         |
         |object Main {
         |  val x1 = "x1"
         |  var x2 = "x2"
         |}
         |
         |trait A {
         |  val a1: String
         |  def a2: String
         |}
         |
         |abstract class B {
         |  val b1: String = "b1"
         |  protected val b2: String = "b2"
         |}
         |
         |class C(val c1: String) extends B with A {
         |  override val a1: String = "a1"
         |  override val a2: String = "a2"
         |}
         |
         |case class D(d1: String)
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    def getter(field: String): String = s"java.lang.String $field()"
    def setter(field: String, param: String = "x$1"): String = s"void ${field}_$$eq(java.lang.String $param)"

    decoder.assertDecode("example.Main$", getter("x1"), "Main.x1: String", generated = true)
    decoder.assertDecode("example.Main$", getter("x2"), "Main.x2: String", generated = true)
    decoder.assertDecode("example.Main$", setter("x2"), "Main.x2_=(String): Unit", generated = true)

    // static forwarders
    decoder.assertDecode("example.Main", getter("x1"), "Main.x1.<static forwarder>: String", generated = true)
    decoder.assertDecode("example.Main", getter("x2"), "Main.x2.<static forwarder>: String", generated = true)
    decoder.assertDecode(
      "example.Main",
      setter("x2", param = "arg0"),
      "Main.x2_=.<static forwarder>(String): Unit",
      generated = true
    )

    decoder.assertDecode("example.A", getter("a1"), "A.a1: String", generated = true)
    decoder.assertDecode("example.A", getter("a2"), "A.a2: String")
    decoder.assertDecode("example.B", getter("b1"), "B.b1: String", generated = true)
    decoder.assertDecode("example.B", getter("b2"), "B.b2: String", generated = true)
    decoder.assertDecode("example.C", getter("c1"), "C.c1: String", generated = true)
    decoder.assertDecode("example.D", getter("d1"), "D.d1: String", generated = true)
  }

  test("bridges") {
    val source =
      """|package example
         |
         |class A {
         |  def m(): Object = "object"
         |}
         |
         |class B extends A {
         |  override def m(): String = "string"
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    def javaSig(returnType: String): String = s"java.lang.Object m()"

    decoder.assertDecode("example.A", "java.lang.Object m()", "A.m(): Object")
    decoder.assertDecode("example.B", "java.lang.Object m()", "B.m.<bridge>(): String", generated = true)
    decoder.assertDecode("example.B", "java.lang.String m()", "B.m(): String")
  }

  test("outer accessors") {
    val source =
      """|package example
         |class A:
         |  private val x =2 
         |  class B[T]: 
         |    class C:
         |      private val y = x+2
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode(
      "example.A$B$C",
      "example.A$B example$A$B$C$$$outer()",
      "A.B.C.<outer>: B.this.type",
      generated = true
    )
  }

  test("using and implicit parameters") {
    val source =
      """|package example
         |object Main{
         |  def m1(using x : Int , y : Int) = x+y
         |  def m2(implicit x : Int) = x+1
         |  def m3(using String , Int): Unit = ()
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "int m1(int x, int y)", "Main.m1(using x: Int, y: Int): Int")
    decoder.assertDecode("example.Main$", "int m2(int x)", "Main.m2(implicit x: Int): Int")
    decoder.assertDecode(
      "example.Main$",
      "void m3(java.lang.String x$1, int x$2)",
      "Main.m3(using String, Int): Unit"
    )

    // static forwarders
    decoder.assertDecode(
      "example.Main",
      "int m1(int arg0, int arg1)",
      "Main.m1.<static forwarder>(using x: Int, y: Int): Int",
      generated = true
    )
    decoder.assertDecode(
      "example.Main",
      "int m2(int arg0)",
      "Main.m2.<static forwarder>(implicit x: Int): Int",
      generated = true
    )
    decoder.assertDecode(
      "example.Main",
      "void m3(java.lang.String arg0, int arg1)",
      "Main.m3.<static forwarder>(using String, Int): Unit",
      generated = true
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
    decoder.assertDecode("example.Main$C$1", "Main.m.C")
    decoder.assertDecode("example.Main$E$1$F", "Main.m.E.F")
    decoder.assertDecode("example.Main$G$1", "Main.m.G")
  }

  test("local class in signature") {
    val source =
      """|package example
         |object Main :
         |  class A :
         |    def m =
         |      class B :
         |        println("B")
         |        class C :
         |          def m =
         |            class D 
         |            def m(t : D) : D = 
         |              t
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "example.Main$D$1 m$1(example.Main$D$1 t)", "Main.A.….m.m(t: D): D")
    decoder.assertDecode("example.Main$A$B$1", "void <init>()", "Main.A.….B.<init>(): Unit")
  }

  test("operator-like names") {
    val source =
      """|package example 
         |class ++ :
         |  def m = 
         |    def ++ = 1
         |    class ++
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.$plus$plus", "int $plus$plus$1()", "++.m.++: Int")
    decoder.assertDecode("example.$plus$plus$$plus$plus$2", "++.m.++")
  }

  test("extension method of value classes") {
    val source =
      """|package example
         |
         |class A(val x: String) extends AnyVal {
         |  def m(): String = {
         |    x + x
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A$", "java.lang.String m$extension(java.lang.String $this)", "A.m(): String")
    decoder.assertDecode(
      "example.A",
      "java.lang.String m$extension(java.lang.String arg0)",
      "A.m.<static forwarder>(): String",
      generated = true
    )
    decoder.assertDecode("example.A", "void <init>(java.lang.String x)", "A.<init>(x: String): Unit")
  }

  test("local method inside a value class") {
    val source =
      """|package example
         |
         |class A(val x: String) extends AnyVal {
         |  def m: String = {
         |    def m(t : String) : String = {
         |      t
         |    }
         |    m("")
         |  }
         |}
         |
         |object A {
         |  def m: String = {
         |    def m : String = "m"
         |    m
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A$", "java.lang.String m$2(java.lang.String t)", "A.m.m(t: String): String")
    decoder.assertDecode("example.A$", "java.lang.String m$1()", "A.m.m: String")
  }

  test("multi parameter lists") {
    val source =
      """|package example
         |
         |object Main {
         |  def m()(a: A): String = {
         |    a.toString
         |  }
         |}
         |
         |class A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "java.lang.String m(example.A a)", "Main.m()(a: A): String")
  }

  test("lazy initializer") {
    val source =
      """|package example
         |
         |object A extends B {
         |  lazy val a = {
         |    "a".toString
         |  }
         |}
         |
         |trait B {
         |  lazy val b = {
         |    "b".toString
         |  }
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A$", "java.lang.String a()", "A.a: String", generated = true)
    decoder.assertDecode("example.A$", "java.lang.String b()", "A.b: String", generated = true)
    decoder.assertDecode("example.B", "java.lang.String b()", "B.b: String")
    decoder.assertDecode(
      "example.B",
      "java.lang.String b$(example.B $this)",
      "B.b.<static forwarder>: String",
      generated = true
    )

    // new in Scala 3.3.0
    decoder.assertDecode("example.A$", "java.lang.Object a$lzyINIT1()", "A.a.<lazy init>: String")
    decoder.assertDecode("example.A$", "java.lang.Object b$lzyINIT1()", "A.b.<lazy init>: String", generated = true)

    // static forwarders
    decoder.assertDecode("example.A", "java.lang.String a()", "A.a.<static forwarder>: String", generated = true)
    decoder.assertDecode("example.A", "java.lang.String b()", "A.b.<static forwarder>: String", generated = true)
  }

  test("synthetic methods of case class") {
    val source =
      """|package example
         |
         |case class A(a: String)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A", "java.lang.String toString()", "A.toString(): String", generated = true)
    decoder.assertDecode("example.A", "example.A copy(java.lang.String a)", "A.copy(a: String): A", generated = true)
    decoder.assertDecode("example.A", "int hashCode()", "A.hashCode(): Int", generated = true)
    decoder.assertDecode(
      "example.A",
      "boolean equals(java.lang.Object x$0)",
      "A.equals(Any): Boolean",
      generated = true
    )
    decoder.assertDecode("example.A", "int productArity()", "A.productArity: Int", generated = true)
    decoder.assertDecode("example.A", "java.lang.String productPrefix()", "A.productPrefix: String", generated = true)
    decoder.assertDecode(
      "example.A",
      "java.lang.Object productElement(int n)",
      "A.productElement(n: Int): Any",
      generated = true
    )
    decoder.assertDecode(
      "example.A",
      "scala.collection.Iterator productIterator()",
      "A.productIterator.<mixin forwarder>: Iterator[Any]",
      generated = true
    )

    decoder.assertDecode("example.A$", "example.A apply(java.lang.String a)", "A.apply(a: String): A", generated = true)
    decoder.assertDecode("example.A$", "example.A unapply(example.A x$1)", "A.unapply(A): A", generated = true)
  }

  test("anonymous functions") {
    val source =
      """|package example
         |class A :
         |  class B : 
         |      def m =
         |        List(true).map(x => x.toString + 1)
         |        val f: Int => String = x => ""
         |  def m =
         |    List("").map(x => x + 1)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String m$$anonfun$1(boolean x)",
      "A.B.m.<anon fun>(x: Boolean): String"
    )
    decoder.assertDecode("example.A", "java.lang.String $anonfun$1(int x)", "A.B.m.<anon fun>(x: Int): String")
    decoder.assertDecode(
      "example.A",
      "java.lang.String m$$anonfun$2(java.lang.String x)",
      "A.m.<anon fun>(x: String): String"
    )
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
    decoder.assertDecode("example.A$$anon$1", "A.m.<anon class>")
  }

  test("this.type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(): this.type = this
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "example.A m()", "A.m(): A.this.type")
  }

  test("inline def with anonymous class and method") {
    val source =
      """|package example
         |class A 
         |
         |inline def m: Unit = 
         |  val f = (x : Int) => x + 1
         |  val a = new A {
         |    println("")
         |  }
         |  if true then () else m
         |
         |class B : 
         |  def n = 
         |    m
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "int $anonfun$1(int x)", "example.m.<anon fun>(x: Int): Int")
    decoder.assertDecode("example.B$$anon$1", "example.m.<anon class>")
  }

  test("SAM and partial functions") {
    val source =
      """|package example
         |object Main {
         |    val foo: Ordering[String] = (x, y) => x.size - y.size
         |    val f: PartialFunction[(String), Int] = {
         |      case ("1") => 1
         |      case ("2") => 2
         |      case ("3") => 3
         |    }    
         |  }
         |
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$$anon$1", "Main.<anon Ordering>")
    decoder.assertDecode(
      "example.Main$$anon$1",
      "int compare(java.lang.String x, java.lang.String y)",
      "Main.<anon Ordering>.compare(x: String, y: String): Int"
    )
    decoder.assertDecode(
      "example.Main$$anon$1",
      "int compare(java.lang.Object x, java.lang.Object y)",
      "Main.<anon Ordering>.compare.<bridge>(x: String, y: String): Int",
      generated = true
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "Main.<anon PartialFunction>"
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "boolean isDefinedAt(java.lang.String x)",
      "Main.<anon PartialFunction>.isDefinedAt(x: String): Boolean"
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "boolean isDefinedAt(java.lang.Object x)",
      "Main.<anon PartialFunction>.isDefinedAt.<bridge>(x: String): Boolean",
      generated = true
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "java.lang.Object applyOrElse(java.lang.String x, scala.Function1 default)",
      "Main.<anon PartialFunction>.applyOrElse[A1, B1](x: A1, default: A1 => B1): B1"
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "java.lang.Object applyOrElse(java.lang.Object x, scala.Function1 default)",
      "Main.<anon PartialFunction>.applyOrElse.<bridge>[A1, B1](x: A1, default: A1 => B1): B1",
      generated = true
    )
  }

  test("default values") {
    val source =
      """|package example
         |
         |case class A(a1: String = "", y: Int = 2) {
         |  def m(x: String = "", y: Int = 2): String = {
         |    x * 2
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A", "java.lang.String m$default$1()", "A.m.<default 1>: String")
    decoder.assertDecode("example.A", "int m$default$2()", "A.m.<default 2>: Int")
    decoder.assertDecode(
      "example.A$",
      "java.lang.String $lessinit$greater$default$1()",
      "A.<init>.<default 1>: String"
    )
    decoder.assertDecode("example.A$", "int $lessinit$greater$default$2()", "A.<init>.<default 2>: Int")
  }

  test("matches on return types") {
    val source =
      """|package example
         |
         |trait A {
         |  def m(xs: List[Int]): Int = xs.sum
         |}
         |
         |class B extends A {
         |  def m(xs: List[String]): String = xs.mkString(", ")
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A", "int m(scala.collection.immutable.List xs)", "A.m(xs: List[Int]): Int")
    decoder.assertDecode(
      "example.B",
      "int m(scala.collection.immutable.List xs)",
      "B.m.<mixin forwarder>(xs: List[Int]): Int",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "java.lang.String m(scala.collection.immutable.List xs)",
      "B.m(xs: List[String]): String"
    )
  }

  test("all kinds of types") {
    val source: String =
      """|package example
         |
         |class annot
         |  extends scala.annotation.Annotation
         |  with scala.annotation.StaticAnnotation
         |
         |trait A {
         |  class B
         |}
         |
         |case class !:[A, B](left: A, right: B)
         |
         |object Main extends A {
         |  def m(a : example.A): example.A = a
         |  def mbis(b: A#B): A#B = b
         |  def mbis(a: A)(b: a.B): a.B = b
         |  def m(a: this.type): this.type = a
         |  def mbis(a: A { def b: B }): A { def b: B } = a
         |  def m(x: String @annot): String @annot = x
         |  def m[T](x: T): T = x
         |  def mbis(a: Main.type): Main.type = a
         |  def m(x: => Int ): Int = 1
         |  def m(x : Int => Int): Int = 1
         |  def m(x : (Int,Int)) : Int = 1
         |  def m(x: 1 & 1): 1 | 1 = 1
         |  def m(x: Int): Option[?] = Some(x)
         |  def m(a: A { type B })(b: a.type): b.B = new a.B
         |  val x: A = new A {}
         |  def m(a: x.type)(b: x.B): A = a
         |  def m(t: Int !: Int) = 1
         |  def m() : [T] => List[T] => Option[T] = ???
         |  def mbis() : [T] => (List[T],List[T]) => Option[T] = ???
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)

    def assertDecode(javaSig: String, expected: String)(using munit.Location): Unit =
      decoder.assertDecode("example.Main$", javaSig, expected)

    assertDecode("example.A m(example.A a)", "Main.m(a: A): A")
    assertDecode("example.A$B mbis(example.A$B b)", "Main.mbis(b: A.B): A.B")
    assertDecode("example.A$B mbis(example.A a, example.A$B b)", "Main.mbis(a: A)(b: a.B): a.B")
    assertDecode("example.Main$ m(example.Main$ a)", "Main.m(a: Main.this.type): Main.this.type")
    assertDecode("example.A mbis(example.A a)", "Main.mbis(a: A {...}): A {...}")
    assertDecode("java.lang.String m(java.lang.String x)", "Main.m(x: String): String")
    assertDecode("java.lang.Object m(java.lang.Object x)", "Main.m[T](x: T): T")
    assertDecode("example.Main$ mbis(example.Main$ a)", "Main.mbis(a: Main.type): Main.type")
    assertDecode("int m(scala.Function0 x)", "Main.m(x: => Int): Int")
    assertDecode("int m(scala.Function1 x)", "Main.m(x: Int => Int): Int")
    assertDecode("int m(scala.Tuple2 x)", "Main.m(x: (Int, Int)): Int")
    assertDecode("int m(example.$bang$colon t)", "Main.m(t: Int !: Int): Int")
    assertDecode("int m(int x)", "Main.m(x: 1 & 1): 1 | 1")
    assertDecode("scala.Option m(int x)", "Main.m(x: Int): Option[?]")
    assertDecode("example.A$B m(example.A a, example.A b)", "Main.m(a: A {...})(b: a.type): b.B")
    assertDecode("example.A m(example.A a, example.A$B b)", "Main.m(a: x.type)(b: x.B): A")
    assertDecode("scala.Function1 m()", "Main.m(): [T] => List[T] => Option[T]")
    assertDecode("scala.Function2 mbis()", "Main.mbis(): [T] => (List[T], List[T]) => Option[T]")
  }

  test("constant type") {
    val source =
      """|package example
         |
         |class A {
         |  def m1(x: "a"): 1 = 1
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "int m1(java.lang.String x)", "A.m1(x: \"a\"): 1")
  }

  test("type aliases") {
    val source =
      """|package example
         |
         |class A
         |
         |object Main {
         |  type Foo = A
         |  type Bar = String
         |  def m(x: Foo): Bar  = x.toString
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "java.lang.String m(example.A x)", "Main.m(x: Foo): Bar")
  }

  test("refined types") {
    val source =
      """|package example
         |
         |trait A
         |trait B extends A
         |
         |object Main {
         |  def m1(): A with B { def foo: String }  = {
         |    new A with B { def foo: String = toString }
         |  }
         |  
         |  def m2(): { def foo: String } = {
         |    new { def foo: String = toString }
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "example.B m1()", "Main.m1(): A & B {...}")
    decoder.assertDecode("example.Main$", "java.lang.Object m2()", "Main.m2(): Object {...}")
  }

  test("type parameters") {
    val source =
      """|package example
         |
         |class A
         |
         |trait B {
         |  type X <: A
         |  
         |  def m1(x: X): X = x
         |  def m2[T <: X](x: T) = x  
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "example.A m1(example.A x)", "B.m1(x: X): X")
    decoder.assertDecode("example.B", "example.A m2(example.A x)", "B.m2[T](x: T): T")
  }

  test("nested classes") {
    val source =
      """|package example
         |
         |object WeekDay extends Enumeration {
         |  type WeekDay = Value
         |  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
         |}
         |
         |object Main {
         |  def today(): Enumeration#Value = WeekDay.Mon
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "scala.Enumeration$Value today()", "Main.today(): Enumeration.Value")
  }

  test("matches Null and Nothing") {
    val source =
      """|package example
         |
         |object Main {
         |  def m(xs: Array[Int]): Nothing = ???
         |  def m(xs: Array[String]): Null = null
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "scala.runtime.Nothing$ m(int[] xs)", "Main.m(xs: Array[Int]): Nothing")
    decoder.assertDecode(
      "example.Main$",
      "scala.runtime.Null$ m(java.lang.String[] xs)",
      "Main.m(xs: Array[String]): Null"
    )
  }

  test("matches Array whose erasure is Object") {
    val source =
      """|package example
         |
         |object Main {
         |  def m[T](xs: Array[T]): Array[T] = xs
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.Main$",
      "java.lang.Object m(java.lang.Object xs)",
      "Main.m[T](xs: Array[T]): Array[T]"
    )
  }

  test("matches PolyType") {
    val source =
      """|package example
         |
         |class A[B[_]] {
         |  def m[T](x: B[T]): B[T] = x
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.Object m(java.lang.Object x)", "A.m[T](x: B[T]): B[T]")
  }

  test("constructors and trait constructors") {
    val source =
      """|package example
         |
         |trait A {
         |  val a: String = "a"
         |}
         |
         |class B extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "void $init$(example.A $this)", "A.<init>(): Unit")
    decoder.assertDecode("example.B", "void <init>()", "B.<init>(): Unit")
  }

  test("vararg type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(as: String*): String = as.mkString(", ")
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String m(scala.collection.immutable.Seq as)",
      "A.m(as: String*): String"
    )
  }

  test("encoded symbols") {
    val source =
      """|package example
         |
         |object Main {
         |  def &(x: <>): String = x.toString
         |}
         |
         |class <> {
         |  def m: <> = this
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "java.lang.String $amp(example.$less$greater x)", "Main.&(x: <>): String")
    decoder.assertDecode("example.$less$greater", "example.$less$greater m()", "<>.m: <>")
  }

  test("local recursive method") {
    val source =
      """|package example
         |
         |object Main {
         |  def fac(x: Int): Int = {
         |    def rec(x: Int, acc: Int): Int = {
         |      if x <= 0 then acc
         |      else rec(x - 1, acc * x)
         |    }
         |    rec(x, 1)
         |  }
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "int rec$1(int x, int acc)", "Main.fac.rec(x: Int, acc: Int): Int")
  }

  test("local lazy initializer") {
    val source =
      """|package example
         |
         |class A {
         |  def m: Unit = {
         |    val x: String = "x"
         |    lazy val y = {
         |      x + "y"
         |    }
         |    println(y)
         |  }
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String y$1(scala.runtime.LazyRef y$lzy1$2, java.lang.String x$2)",
      "A.m.y: String",
      generated = true
    )
    decoder.assertDecode(
      "example.A",
      "java.lang.String y$lzyINIT1$1(scala.runtime.LazyRef y$lzy1$1, java.lang.String x$1)",
      "A.m.y.<lazy init>: String"
    )
  }

  test("private methods made public") {
    val source =
      """|package example
         |
         |class Outer {
         |  private def foo: String = "foo"
         |  class Inner {
         |    def bar: String = {
         |      foo
         |    }
         |  }
         |}
         |
         |class A {
         |  def m: Int = A.m
         |}
         |
         |object A {
         |  private def m: Int = 1
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Outer", "java.lang.String example$Outer$$foo()", "Outer.foo: String")
    decoder.assertDecode("example.A$", "int example$A$$$m()", "A.m: Int")
  }

  test("type lambda") {
    val source =
      """|package example
         |
         |trait Foo[F[_]]
         |
         |object Main:
         |  def foo : Foo[[X] =>> Either[X, Int]] = ???
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "example.Foo foo()", "Main.foo: Foo[[X] =>> Either[X, Int]]")
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
    decoder.assertDecode("example.Main$A$1", "Main.m.A")
  }

  test("package object") {
    val source =
      """|package object example {
         |  def foo: String = ???
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.package$", "java.lang.String foo()", "example.foo: String")
    decoder.assertDecode(
      "example.package",
      "java.lang.String foo()",
      "example.foo.<static forwarder>: String",
      generated = true
    )
  }

  test("top-level definition") {
    val source =
      """|package example
         |
         |def foo: String = ???
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Test$package$", "java.lang.String foo()", "example.foo: String")
    decoder.assertDecode(
      "example.Test$package",
      "java.lang.String foo()",
      "example.foo.<static forwarder>: String",
      generated = true
    )
  }

  test("i491") {
    val source =
      """|package example
         |
         |class A {
         |  val m: String = ""
         |  def m(x: String): String = ""
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.String m()", "A.m: String", generated = true)
    decoder.assertDecode("example.A", "java.lang.String m(java.lang.String x)", "A.m(x: String): String")
  }

  test("adapted anon fun") {
    val source =
      """|package example
         |
         |class A {
         |  def m(x: String): String = x.takeWhile(_ != '.')
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "boolean m$$anonfun$adapted$1(java.lang.Object _$1)",
      "A.m.<anon fun>.<adapted>(Char): Boolean",
      generated = true
    )
  }

  test("super args") {
    val source =
      """|package example
         |
         |class A1(x: => String)
         |class A2(x: Int)(y: String => String)
         |
         |object B1 extends A1("") {
         |  object B2 extends A2(5)(x => x)
         |}
         |
         |class C1 extends A1(
         |  ""
         |) {
         |  object C2 extends A2(5)(x => x)
         |  
         |  def m = {
         |    class C3 extends A1(
         |      ""
         |    ) {
         |      class C4 extends A2(5)({ x =>
         |          x + x
         |      })
         |    }
         |
         |    new A1("") {
         |      override def toString: String = ""
         |    }
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B1$", "scala.Function0 B1$$superArg$1()", "B1.<init>.<super arg>: () => \"\"")
    decoder.assertDecode(
      "example.B1$",
      "scala.Function1 example$B1$$$B2$$superArg$1()",
      "B1.B2.<init>.<super arg>: String => String"
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function0 C1$superArg$1()",
      "C1.<init>.<super arg>: () => \"\""
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function1 example$C1$$C2$$superArg$1()",
      "C1.C2.<init>.<super arg>: String => String"
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function0 example$C1$$_$C3$superArg$1$1()",
      "C1.m.C3.<init>.<super arg>: () => \"\""
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function0 example$C1$$_$$anon$superArg$1$1()",
      "C1.m.<anon class>.<init>.<super arg>: () => \"\""
    )
    decoder.assertDecode(
      "example.C1$C3$1",
      "scala.Function1 example$C1$C3$1$$C4$superArg$1()",
      "C1.m.….C4.<init>.<super arg>: String => String"
    )
  }

  test("method returning a context function") {
    val source =
      """|package example
         |
         |class A:
         |  def m(x: Int): String ?=> String = ???
         |  def m(): (Int, String) ?=> Int = ???
         |  def m(x: String): Int ?=> String ?=> String = ???
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      if isScala33 then "java.lang.String m(int x, java.lang.String evidence$1)"
      else "java.lang.String m(int x, java.lang.String contextual$1)",
      "A.m(x: Int): String ?=> String"
    )
    decoder.assertDecode(
      "example.A",
      if isScala33 then "int m(int evidence$2, java.lang.String evidence$3)"
      else "int m(int contextual$2, java.lang.String contextual$3)",
      "A.m(): (Int, String) ?=> Int"
    )
    decoder.assertDecode(
      "example.A",
      if isScala33 then "java.lang.String m(java.lang.String x, int evidence$4, java.lang.String evidence$5)"
      else "java.lang.String m(java.lang.String x, int contextual$4, java.lang.String contextual$5)",
      "A.m(x: String): Int ?=> String ?=> String"
    )
    if isScala34 then
      val source =
        """|package example
           |
           |class A:
           |  def mbis: ? ?=> String = ???
           |""".stripMargin
      val decoder = TestingDecoder(source, scalaVersion)
      decoder.assertDecode("example.A", "java.lang.String mbis(java.lang.Object contextual$1)", "A.mbis: ? ?=> String")
  }

  test("trait param") {
    val source =
      """|package example
         |
         |trait A(val x: Int, var y: Int, z: Int)(using String)
         |
         |class B(x: Int)(using String) extends A(1, 2, 3)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    // todo fix: should be a BinaryTraitParamGetter
    decoder.assertDecode("example.B", "int x()", "B.x: Int", generated = true)
    decoder.assertDecode("example.B", "int y()", "B.y: Int", generated = true)
    decoder.assertDecode("example.B", "void y_$eq(int x$1)", "B.y_=(Int): Unit", generated = true)
    decoder.assertDecode("example.B", "int example$A$$z()", "B.z: Int", generated = true)
    decoder.assertDecode("example.B", "java.lang.String example$A$$x$4()", "B.x$4: String", generated = true)
  }

  test("lifted try") {
    assume(isScala33)
    val source =
      """|package example
         |
         |class A:
         |  println("" + (try "" catch case e: Exception => ""))
         |
         |  val x = "" + 
         |    (try "" catch case e: Exception => "")
         |
         |  def m1 = 
         |    val x = "" + (try "" catch case e: Exception => "")
         |    def m2 = 1 + (try 2 catch case e: Exception => 3)
         |    x * m2
         |
         |  inline def m3 = try "" catch case e: Exception => ""
         |
         |   def m4 = "" + m3
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.String liftedTree1$1()", "A.<try>: \"\" | \"\"")
    decoder.assertDecode("example.A", "java.lang.String liftedTree2$1()", "A.<try>: \"\" | \"\"")
    decoder.assertDecode("example.A", "java.lang.String liftedTree3$1()", "A.m1.<try>: \"\" | \"\"")
    decoder.assertDecode("example.A", "int liftedTree4$1()", "A.m1.m2.<try>: 2 | 3")
    decoder.assertDecode("example.A", "java.lang.String liftedTree5$1()", "A.m4.<try>: \"\" | \"\"")
  }

  test("by-name args") {
    val source =
      """|package example
         |
         |class A {
         |  def foo[T](x: => T): T = x
         |
         |  foo("Hello")
         |  
         |  def m =
         |    foo(1 + 1)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.Object foo(scala.Function0 x)", "A.foo[T](x: => T): T")
    decoder.assertDecode("example.A", "java.lang.String $init$$$anonfun$1()", "A.<by-name arg>: String")
    decoder.assertDecode("example.A", "int m$$anonfun$1()", "A.m.<by-name arg>: Int")
  }

  test("inner object") {
    val source =
      """|package example
         |
         |trait A:
         |  object B
         |
         |object C extends A:
         |  object D
         |
         |class E extends A:
         |  object F
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "example.A$B$ B()", "A.B: B")
    decoder.assertDecode("example.A", "example.A$B$ B$(example.A $this)", "A.B.<static forwarder>: B", generated = true)
    decoder.assertDecode("example.C$", "example.A$B$ B()", "C.B: B", generated = true)
    decoder.assertDecode("example.E", "example.E$F$ F()", "E.F: F", generated = true)
    decoder.assertDecode("example.E", "example.A$B$ B()", "E.B: B", generated = true)
    decoder.assertDecode("example.C$", "java.lang.Object B$lzyINIT1()", "C.B.<lazy init>: B", generated = true)
    decoder.assertDecode("example.E", "java.lang.Object F$lzyINIT1()", "E.F.<lazy init>: F")
    decoder.assertDecode("example.E", "java.lang.Object B$lzyINIT2()", "E.B.<lazy init>: B", generated = true)
  }

  test("static forwarder") {
    val source =
      """|package example
         |
         |class A[T] {
         |  def foo(x: T): String = "foo"
         |}
         |
         |object B extends A[String]
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "java.lang.String foo(java.lang.Object arg0)",
      "B.foo.<static forwarder>(x: String): String",
      generated = true
    )
  }

  test("param forwarders") {
    val source =
      """|package example
         |
         |class A[T](val foo: T)
         |
         |class B(foo: String) extends A(foo)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "java.lang.String foo$accessor()", "B.foo: String", generated = true)
  }

  test("trait setters") {
    val source =
      """|package example
         |
         |trait A:
         |  private val foo = "foo"
         |
         |class B extends A
         |
         |object C extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "void example$A$_setter_$example$A$$foo_$eq(java.lang.String x$0)",
      "B.foo.<setter>(String): Unit",
      generated = true
    )
    decoder.assertDecode(
      "example.C$",
      "void example$A$_setter_$example$A$$foo_$eq(java.lang.String x$0)",
      "C.foo.<setter>(String): Unit",
      generated = true
    )
    decoder.assertDecode(
      "example.C",
      "void example$A$_setter_$example$A$$foo_$eq(java.lang.String arg0)",
      "C.foo.<setter>.<static forwarder>(String): Unit",
      generated = true
    )
  }

  test("super accessors") {
    val source =
      """|package example
         |
         |class A[T]:
         |  def foo(x: T): String = "foo"
         |
         |trait B[T] extends A[T]:
         |  override def foo(x: T): String = super.foo(x) + "bar"
         |
         |class C extends B[String]
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "java.lang.String example$B$$super$foo(java.lang.Object x)",
      "B.foo.<super>(x: T): String",
      generated = true
    )
    decoder.assertDecode(
      "example.C",
      "java.lang.String example$B$$super$foo(java.lang.String x)",
      "C.foo.<super>(x: String): String",
      generated = true
    )
    decoder.assertDecode(
      "example.C",
      "java.lang.String example$B$$super$foo(java.lang.Object x)",
      "C.foo.<super>.<bridge>(x: String): String",
      generated = true
    )
  }

  test("java arg bridges") {
    val javaSource =
      """|package example;
         |
         |class A {
         |  public String m(Object... args) {
         |    return "";
         |  }
         |}
         |""".stripMargin
    val source =
      """|package example
         |
         |class B extends A:
         |  override def m(args: Any*): String = super.m(args)
         |
         |  @scala.annotation.varargs
         |  def m(args: String*): Int = args.size
         |""".stripMargin
    val javaModule = Module.fromJavaSource(javaSource, scalaVersion)
    val decoder = TestingDecoder(source, scalaVersion, javaModule.classpath)
    decoder.assertDecode(
      "example.B",
      "java.lang.String m(java.lang.Object[] args)",
      "B.m.<bridge>(args: Any*): String",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "java.lang.String m(scala.collection.immutable.Seq args)",
      "B.m(args: Any*): String"
    )
    decoder.assertDecode(
      "example.B",
      "int m(java.lang.String[] args)",
      "B.m.<bridge>(args: String*): Int",
      generated = true
    )
    decoder.assertDecode("example.B", "int m(scala.collection.immutable.Seq args)", "B.m(args: String*): Int")
  }

  test("specialized methods") {
    val source =
      """|package example
         |
         |class A extends (Double => Boolean):
         |  def apply(x: Double): Boolean = x > 0
         |
         |object B extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "boolean apply(double x)", "A.apply(x: Double): Boolean")
    decoder.assertDecode(
      "example.A",
      "java.lang.Object apply(java.lang.Object v1)",
      "A.apply.<bridge>(x: Double): Boolean",
      generated = true
    )
    decoder.assertDecode(
      "example.A",
      "boolean apply$mcZD$sp(double x)",
      "A.apply.<specialized>(x: Double): Boolean",
      generated = true
    )
    decoder.assertDecode(
      "example.A",
      "int apply$mcII$sp(int x$0)",
      "A.apply.<specialized>(x: Double): Boolean",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "boolean apply(double arg0)",
      "B.apply.<static forwarder>(x: Double): Boolean",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "boolean apply$mcZD$sp(double arg0)",
      "B.apply.<specialized>.<static forwarder>(x: Double): Boolean",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "int apply$mcII$sp(int arg0)",
      "B.apply.<specialized>.<static forwarder>(x: Double): Boolean",
      generated = true
    )
  }

  test("by-name arg proxy") {
    val source =
      """|package example
         |
         |trait A:
         |  def m[T](x: => T): T
         |
         |class B:
         |  def m(s: String): String =
         |    B.m(s * 2)
         |
         |object B extends A:
         |  inline override def m[T](x: => T): T = x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "java.lang.String x$proxy2$1(java.lang.String s$1)", "B.m.<by-name arg>: String")
    decoder.assertDecode("example.B$", "java.lang.Object x$proxy1$1(scala.Function0 x$1)", "B.m.<by-name arg>: T")
  }

  test("inline accessor") {
    val source =
      """|package example
         |
         |trait A:
         |  class AA:
         |    private[A] var x: String = "foo"
         |  inline def m(aa: AA): Unit = if aa.x == "foo" then aa.x = "bar"
         |
         |class B extends A
         |
         |object B:
         |  private var y: String = "foo"
         |  inline def m: Unit = if y == "foo" then y = "bar"
         |
         |class C(x: String) extends AnyVal:
         |  inline def m: String = x + x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String inline$x$i2(example.A$AA x$0)",
      "A.<inline A.AA.x>: String",
      generated = true
    )
    decoder.assertDecode(
      "example.A",
      "java.lang.String inline$x$i2$(example.A $this, example.A$AA x$0)",
      "A.<inline A.AA.x>.<static forwarder>: String",
      generated = true
    )
    decoder.assertDecode(
      "example.A",
      "void inline$x_$eq$i2(example.A$AA x$0, java.lang.String x$0)",
      "A.<inline A.AA.x_=>(String): Unit",
      generated = true
    )
    decoder.assertDecode(
      "example.A",
      "void inline$x_$eq$i2$(example.A $this, example.A$AA x$0, java.lang.String x$0)",
      "A.<inline A.AA.x_=>.<static forwarder>(String): Unit",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "java.lang.String inline$x$i2(example.A$AA x$0)",
      "B.<inline A.AA.x>.<mixin forwarder>: String",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "void inline$x_$eq$i2(example.A$AA x$0, java.lang.String x$0)",
      "B.<inline A.AA.x_=>.<mixin forwarder>(String): Unit",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "java.lang.String inline$y()",
      "B.<inline B.y>.<static forwarder>: String",
      generated = true
    )
    decoder.assertDecode(
      "example.B",
      "void inline$y_$eq(java.lang.String arg0)",
      "B.<inline B.y_=>.<static forwarder>(String): Unit",
      generated = true
    )
    decoder.assertDecode(
      "example.C$",
      "java.lang.String inline$x$extension(java.lang.String $this)",
      "C.<inline C.x>: String",
      generated = true
    )
    decoder.assertDecode(
      "example.C",
      "java.lang.String inline$x$extension(java.lang.String arg0)",
      "C.<inline C.x>.<static forwarder>: String",
      generated = true
    )
  }

  test("deserializeLambda") {
    val source =
      """|package example
         |
         |object A:
         |  val x: String => String = identity
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A$",
      "java.lang.Object $deserializeLambda$(java.lang.invoke.SerializedLambda arg0)",
      "A.$deserializeLambda$(arg0: SerializedLambda): Object"
    )
  }

  test("java.lang.Enum constructor") {
    val source =
      """|package example
         |
         |enum A(x: String) extends java.lang.Enum[A]:
         |  case B extends A("b")
         |  case C extends A("c")
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "void <init>(java.lang.String x, java.lang.String _$name, int _$ordinal)",
      "A.<init>(x: String): Unit"
    )
  }

  test("anon lazy inits") {
    val source =
      """|package example
         |
         |class A:
         |  lazy val (x, y) = m
         |  def m =
         |    lazy val (x, y) = ("x", "y")
         |    (x, y)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.Object $1$$lzyINIT1()", "A.<anon>.<lazy init>: (String, String)")
    decoder.assertDecode("example.A", "scala.Tuple2 $1$()", "A.<anon>: (String, String)", generated = true)
    decoder.assertDecode(
      "example.A",
      "scala.Tuple2 $2$$lzyINIT1$1(scala.runtime.LazyRef $2$$lzy1$1)",
      "A.m.<anon>.<lazy init>: (String, String)"
    )
    decoder.assertDecode(
      "example.A",
      "scala.Tuple2 $2$$1(scala.runtime.LazyRef $2$$lzy1$2)",
      "A.m.<anon>: (String, String)",
      generated = true
    )
  }

  test("trait local static forwarder") {
    val source =
      """|package example
         |
         |trait A:
         |  val x: String
         |  private def m1 =
         |    class B:
         |      def m2 = m3
         |    def m3: String = x + x
         |    () 
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String example$A$$_$m3$1$(example.A $this)",
      "A.m1.m3.<static forwarder>: String",
      generated = true
    )
  }

  test("reduce ambiguity of anon funs by finding clashing methods") {
    val source =
      """|package example
         |
         |class A:
         |  def m =
         |    for {
         |      (tag, formatter) <- scala.collection.immutable.ListMap.empty[String, String]
         |      boundss <- Some(List.empty[(String, String)])
         |      texts = List.empty[String]
         |      formatted <- Some("")
         |    } yield formatted
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      if isScala33 then "scala.Option m$$anonfun$2(scala.Tuple2 x$1)"
      else "scala.Option m$$anonfun$1(scala.Tuple2 x$1)",
      "A.m.<anon fun>((String, String)): Option[String]"
    )
    decoder.assertDecode(
      "example.A",
      if isScala33 then "scala.Option m$$anonfun$2$$anonfun$2(scala.Tuple2 x$1)"
      else "scala.Option m$$anonfun$1$$anonfun$2(scala.Tuple2 x$1)",
      "A.m.<anon fun>.<anon fun>((List[(String, String)], List[String])): Option[String]"
    )
  }

  test("inline defs") {
    val source =
      """|package example
         |
         |class Logger:
         |  def m1(x: String): String = ???
         |
         |  inline def m2(f: String => String): String => String =
         |    x => m1(f(x))
         |
         |  inline def trace1(a: String)(inline f: String => String): Unit =
         |    println(f(a))
         |
         |  inline def rec(b: Boolean)(inline f: String => String): String =
         |    inline if b then f(rec(false)(f))
         |    else f("")
         |
         |class Test:
         |  def test(name: String): Unit =
         |    val logger = new Logger
         |    val xs = List.empty[String]
         |    val f = logger.m2(x => xs.map(y => x + y).mkString)
         |    logger.trace1(name)(x => xs.map(y => x + y).mkString)
         |    logger.trace1(name + name)(x => xs.map(y => x + y).mkString)
         |    logger.rec(true)(x => xs.map(y => x + y).mkString)
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.Test",
      "java.lang.String $anonfun$1(scala.collection.immutable.List xs$1, java.lang.String x)",
      "Test.test.<anon fun>(x: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String $anonfun$2(example.Logger Logger_this$1, scala.Function1 f$proxy1$1, java.lang.String x)",
      "Logger.m2.<anon fun>(x: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$1(java.lang.String name$1, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$2(java.lang.String a$proxy1$1, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$3(java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$4(java.lang.String x$2, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String $anonfun$1$$anonfun$1(java.lang.String x$1, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
  }

  test("tastyquery#395"):
    assume(!isJava8)
    val decoder = initDecoder("de.sciss", "desktop-core_3", "0.11.4")
    decoder.assertDecode(
      "de.sciss.desktop.impl.LogPaneImpl$textPane$",
      "boolean apply$mcZD$sp(double x$0)",
      "LogPaneImpl.textPane.apply.<specialized>(str: String): Unit",
      generated = true
    )

  test("tasty-query#397 and tasty-query#413"):
    val decoder = initDecoder("com.github.xuwei-k", "httpz_3", "0.8.0")
    decoder.assertDecode("httpz.package$$anon$1", "httpz.ActionZipAp.<anon class>")
    decoder.assertDecode("httpz.InterpretersTemplate$$anon$5", "InterpretersTemplate.times.….apply.<anon class>")
    decoder.assertDecode(
      "httpz.package$",
      "java.lang.Object httpz$package$$anon$1$$_$ap$$anonfun$1(scala.Function1 _$2, java.lang.Object _$3)",
      "httpz.ActionZipAp.….ap.<anon fun>(A => B, A): B"
    )
    decoder.assertDecode(
      "httpz.Response",
      "scalaz.Equal responseEqual(scalaz.Equal arg0)",
      "Response.responseEqual.<static forwarder>[A](implicit Equal[A]): Equal[Response[A]]",
      generated = true
    )
    decoder.assertDecode(
      "httpz.Core",
      "scalaz.$bslash$div jsonResponse$$anonfun$3$$anonfun$1(argonaut.DecodeJson A$2, httpz.Request request$1, httpz.Response json)",
      "Core.jsonResponse.<anon fun>.<anon fun>(json: Response[Json]): \\/[Error, Response[A]]"
    )

  test("tasty-query#398"):
    val decoder = initDecoder("io.github.ashwinbhaskar", "sight-client_3", "0.1.2")
    decoder.assertDecode(
      "sight.client.SightClientImpl",
      "java.lang.String b$1(scala.Tuple2 x$1$2)",
      "SightClientImpl.constructPayload.….<anon fun>.b: String"
    )

  test("tasty-query#401".ignore) {
    val source =
      """|package example
         |
         |type Rec[A <: Tuple] <: Tuple = A match
         |  case hd *: tl => hd *: Rec[tl]
         |  case EmptyTuple => EmptyTuple
         |
         |object Rec:
         |  inline def rec[A <: Tuple](a: A): Rec[A] = 
         |    inline a match
         |      case b: (hd *: tl) => b.head *: rec(b.tail)
         |      case _: EmptyTuple => EmptyTuple
         |
         |trait Codec[A]:
         |  def map[B](x: A => B): Codec[B]
         |
         |object Test:
         |  inline def rec[A <: Tuple](c: Codec[A]): Codec[Rec[A]] =
         |    c.map(x => Rec.rec(x))
         |  def foo(c: Codec[(Int, Int)]): Codec[(Int, Int)] =
         |    rec(c)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.Test$",
      "scala.Product foo$$anonfun$1(scala.Tuple2 x)",
      "Foo.drop1[A](a: A): DropUnits[A]"
    )
  }

  test("tasty-query#402"):
    val decoder = initDecoder("com.softwaremill.sttp.client3", "opentelemetry_3", "3.6.1")
    decoder.assertDecode(
      "sttp.client3.opentelemetry.OpenTelemetryTracingBackend",
      "java.lang.Object send$$anonfun$2$$anonfun$1$$anonfun$1(sttp.client3.RequestT request$5, scala.collection.mutable.Map carrier$5)",
      "OpenTelemetryTracingBackend.send.<anon fun>.<by-name arg>: F[Response[T]]"
    )

  test("tasty-query#403") {
    val source =
      """|package example
         |
         |trait A[T]:
         |  opaque type Type <: T = T
         |
         |class B extends A[String]:
         |  def m(x: Type): Unit = ???
         |
         |class Value[T](v: T) extends AnyVal
         |
         |class C[T <: Int]:
         |  def m(x: Value[T]): Unit = ???
         |  
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "void m(java.lang.String x)", "B.m(x: Type): Unit")
    decoder.assertDecode("example.C", "void m(java.lang.Integer x)", "C.m(x: Value[T]): Unit")
  }

  test("tasty-query#407") {
    val source =
      """|package example
         |
         |import java.util.function.Consumer
         |
         |class A:
         |  def m(f: String => Unit): Consumer[String] =
         |    new Consumer[String]():
         |      def accept(line: String): Unit = f(line)
         |  
         |  def test: Consumer[String] = m(println)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "void test$$anonfun$1(java.lang.String x)", "A.test.<anon fun>(x: String): Unit")
  }

  test("scala3-compiler:3.3.1"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.3.1")
    decoder.assertDecode(
      "scala.quoted.runtime.impl.QuotesImpl",
      "boolean scala$quoted$runtime$impl$QuotesImpl$$inline$xCheckMacro()",
      "QuotesImpl.<inline QuotesImpl.xCheckMacro>: Boolean",
      generated = true
    )
    decoder.assertDecode(
      "dotty.tools.dotc.printing.RefinedPrinter",
      "void dotty$tools$dotc$printing$RefinedPrinter$$inline$myCtx_$eq(dotty.tools.dotc.core.Contexts$Context x$0)",
      "RefinedPrinter.<inline RefinedPrinter.myCtx_=>(Contexts.Context): Unit",
      generated = true
    )
    decoder.assertDecode(
      "dotty.tools.dotc.transform.sjs.PrepJSInterop$OwnerKind",
      "int inline$baseKinds$extension(int arg0)",
      "PrepJSInterop.OwnerKind.<inline PrepJSInterop.OwnerKind.baseKinds>.<static forwarder>: Int",
      generated = true
    )
    decoder.assertDecode(
      "org.scalajs.ir.Trees$OptimizerHints",
      "boolean inline$extension(int arg0)",
      "Trees.OptimizerHints.inline.<static forwarder>: Boolean",
      generated = true
    )
    decoder.assertDecode(
      "dotty.tools.package",
      "java.lang.Object unreachable$default$1()",
      "tools.unreachable.<default 1>.<static forwarder>: Any",
      generated = true
    )
    // decoder.assertDecode(
    //   "dotty.tools.dotc.printing.Formatting$StringFormatter",
    //   "java.lang.String assemble$$anonfun$1(java.lang.String str)",
    //   ""
    // )
    decoder.assertDecode(
      "dotty.tools.dotc.core.tasty.TreeUnpickler",
      "dotty.tools.dotc.ast.Trees$Tree dotty$tools$dotc$core$tasty$TreeUnpickler$TreeReader$$_$_$$anonfun$18(dotty.tools.dotc.core.Contexts$Context x$1$19, dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader $this$tailLocal1$1)",
      "TreeUnpickler.readTpt.<static forwarder>()(using Contexts.Context): tpd.Tree",
      generated = true
    )
    decoder.assertNotFoundVariable(
      "scala.quoted.runtime.impl.QuotesImpl$reflect$defn$",
      "dotty.tools.dotc.core.Symbols$Symbol TupleClass(int arity)",
      "dotty.tools.dotc.core.Types$TypeRef x$proxy1",
      2816
    )

  test("tasty-query#412"):
    val decoder = initDecoder("dev.zio", "zio-interop-cats_3", "23.1.0.0")(using ThrowOrWarn.ignore)
    decoder.assertDecode("zio.BuildInfoInteropCats", "BuildInfoInteropCats")
    decoder.assertDecode("zio.interop.ZioMonadErrorE$$anon$4", "ZioMonadErrorE.adaptError.<anon PartialFunction>")

  test("tasty-query#414"):
    val decoder = initDecoder("io.github.dieproht", "matr-dflt-data_3", "0.0.3")
    decoder.assertDecode(
      "matr.dflt.DefaultMatrixFactory$$anon$1",
      "DefaultMatrixFactory.defaultMatrixFactory.builder.<anon class>"
    )

  test("tasty-query#415"):
    val decoder = initDecoder("com.github.mkroli", "dns4s-fs2_3", "0.21.0")
    decoder.assertDecode("com.github.mkroli.dns4s.fs2.DnsClientOps", "DnsClientOps")

  test("bug: Type.of creates capture".ignore):
    val source =
      """|package example
         |
         |import scala.quoted.*
         |
         |trait A[T]
         |
         |class B(using q: Quotes):
         |  import q.reflect.*
         |
         |  private def m[T](using tpe: Type[T]): String => TypeRepr =
         |    (x: String) => TypeRepr.of[A[T]]
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "java.lang.Object m$$anonfun$1(scala.quoted.Type tpe$1, java.lang.String x)",
      "B.m.<anon fun>(x: String): q.reflect.TypeRepr"
    )

  test("bug: not an outer".ignore):
    val decoder = initDecoder("com.disneystreaming", "weaver-monix-core_3", "0.6.15")(using ThrowOrWarn.ignore)
    decoder.assertDecode(
      "weaver.monixcompat.PureTaskSuite",
      "weaver.SourceLocation$ weaver$SourceLocationMacro$Here$$$outer()",
      ""
    )

  test("bug: $ as a name".ignore) {
    val source =
      """|package example
         |
         |class $:
         |  def $: String = ???
         |
         |object $
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.$", "java.lang.String $()", "$.$: String")
    decoder.assertDecode("example.$$", "$")
  }

  test("tasty-query#423"):
    val decoder = initDecoder("com.typesafe.akka", "akka-stream_3", "2.8.5")
    decoder.assertDecode("akka.stream.scaladsl.FlowOps$passedEnd$2$", "FlowOps.zipAllFlow.passedEnd")

  test("tasty-query#424"):
    val decoder = initDecoder("edu.gemini", "lucuma-itc-core_3", "0.10.0")
    decoder.assertDecode("lucuma.itc.ItcImpl", "ItcImpl")

  test("specialized class"):
    val decoder = initDecoder("org.scala-lang", "scala-library", "2.13.12")
    decoder.assertDecode("scala.runtime.java8.JFunction1$mcII$sp", "JFunction1$mcII$sp")

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
    decoder.assertDecode("example.A$B$1", "A.m.B")
