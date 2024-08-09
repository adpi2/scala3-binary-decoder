package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary.SignedName

import java.lang.reflect.Constructor
import java.lang.reflect.Method

object JavaReflectUtils:
  val primitiveSigs = Map[Class[?], String](
    classOf[Byte] -> "B",
    classOf[Char] -> "C",
    classOf[Short] -> "S",
    classOf[Int] -> "I",
    classOf[Long] -> "J",
    classOf[Float] -> "F",
    classOf[Double] -> "D",
    classOf[Boolean] -> "Z",
    classOf[Unit] -> "V"
  )

  def signature(method: Method | Constructor[?]): SignedName =
    val name = method match
      case _: Constructor[?] => "<init>"
      case m: Method => m.getName
    val params = method.getParameterTypes.map(signature)
    val returnType = method match
      case _: Constructor[?] => "V"
      case m: Method => signature(m.getReturnType)
    SignedName(name, s"(${params.mkString})$returnType")

  def signature(cls: Class[?]): String =
    if cls.isPrimitive then primitiveSigs(cls)
    else if cls.isArray then s"[" + signature(cls.getComponentType)
    else "L" + cls.getName.replace('.', '/') + ";"
