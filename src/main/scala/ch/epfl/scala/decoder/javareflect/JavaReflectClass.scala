package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary
import scala.util.matching.Regex
import scala.jdk.CollectionConverters.*
import java.lang.reflect.Method
import java.lang.reflect.Constructor

class JavaReflectClass(val cls: Class[?], extraInfo: ExtraClassInfo, override val classLoader: JavaReflectLoader)
    extends binary.BinaryClass:
  override def name: String = cls.getTypeName
  override def superclass = Option(cls.getSuperclass).map(classLoader.loadClass)
  override def interfaces = cls.getInterfaces.toList.map(classLoader.loadClass)
  override def isInterface: Boolean = cls.isInterface
  override def sourceLines: Option[binary.SourceLines] = extraInfo.sourceLines

  override def declaredMethod(name: String, sig: String): Option[binary.Method] =
    declaredMethods.find(m => m.signedName == binary.SignedName(name, sig))

  override def method(name: String, sig: String): Option[binary.Method] =
    declaredMethod(name, sig).orElse {
      for
        method <- cls.getMethods.find(m => JavaReflectUtils.signature(m) == binary.SignedName(name, sig))
        declaringClass = classLoader.loadClass(method.getDeclaringClass)
        declaredMethod <- declaringClass.declaredMethod(name, sig)
      yield declaredMethod
    }

  override def declaredField(name: String): Option[binary.Field] =
    try Some(JavaReflectField(cls.getDeclaredField(name), classLoader))
    catch case _: NoSuchFieldException => None

  override def toString: String =
    if showSpan.isEmpty then cls.toString else s"$cls $showSpan"

  override def declaredMethods: Seq[binary.Method] =
    cls.getDeclaredMethods
      .++[Method | Constructor[?]](cls.getDeclaredConstructors)
      .map { m =>
        val sig = JavaReflectUtils.signature(m)
        val methodInfo = extraInfo.getMethodInfo(sig)
        JavaReflectMethod(m, sig, methodInfo, classLoader)
      }

  override def declaredFields: Seq[binary.Field] =
    cls.getDeclaredFields().map(f => JavaReflectField(f, classLoader))

object JavaReflectClass:
  val boolean: JavaReflectClass = JavaReflectClass(classOf[Boolean], ExtraClassInfo.empty, null)
  val int: JavaReflectClass = JavaReflectClass(classOf[Int], ExtraClassInfo.empty, null)
  val long: JavaReflectClass = JavaReflectClass(classOf[Long], ExtraClassInfo.empty, null)
  val float: JavaReflectClass = JavaReflectClass(classOf[Float], ExtraClassInfo.empty, null)
  val double: JavaReflectClass = JavaReflectClass(classOf[Double], ExtraClassInfo.empty, null)
  val byte: JavaReflectClass = JavaReflectClass(classOf[Byte], ExtraClassInfo.empty, null)
  val char: JavaReflectClass = JavaReflectClass(classOf[Char], ExtraClassInfo.empty, null)
  val short: JavaReflectClass = JavaReflectClass(classOf[Short], ExtraClassInfo.empty, null)
  val void: JavaReflectClass = JavaReflectClass(classOf[Unit], ExtraClassInfo.empty, null)

  val primitives: Map[String, JavaReflectClass] = Map(
    "boolean" -> boolean,
    "int" -> int,
    "long" -> long,
    "float" -> float,
    "double" -> double,
    "byte" -> byte,
    "char" -> char,
    "short" -> short,
    "void" -> void
  )

  def array(componentType: JavaReflectClass): JavaReflectClass =
    JavaReflectClass(
      java.lang.reflect.Array.newInstance(componentType.cls, 0).getClass,
      ExtraClassInfo.empty,
      componentType.classLoader
    )
