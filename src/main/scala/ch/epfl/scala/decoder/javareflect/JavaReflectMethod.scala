package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary

import java.lang.reflect.Method
import java.lang.reflect.Constructor
import java.lang.reflect.Modifier
import org.objectweb.asm

class JavaReflectMethod(
    method:  Method | Constructor[?],
    val signedName: binary.SignedName,
    extraInfos: ExtraMethodInfo,
    loader: JavaReflectLoader
) extends binary.Method:
  override def returnType: Option[binary.Type] =
    method match
      case _: Constructor[?] => Some(loader.loadClass(classOf[Unit]))
      case m: Method => Option(m.getReturnType).map(loader.loadClass)

  override def declaringClass: binary.BinaryClass =
    loader.loadClass(method.getDeclaringClass)

  override def parameters: Seq[binary.Parameter] =
    method.getParameters.map(JavaReflectParameter.apply(_, this, loader))

  override def variables: Seq[binary.Variable] =
    val localVariables =
      for variable <- extraInfos.localVariables yield
        val typeName = asm.Type.getType(variable.descriptor).getClassName
        val sourceLines =
          for
            sourceName <- sourceName
            line <- extraInfos.labels.get(variable.start)
          yield binary.SourceLines(sourceName, Seq(line))
        AsmVariable(variable.name, loader.loadClass(typeName), this, sourceLines)
    parameters ++ localVariables

  override def name: String = method match
    case _: Constructor[?] => "<init>"
    case m: Method => m.getName

  override def isStatic: Boolean = Modifier.isStatic(method.getModifiers)

  override def isFinal: Boolean = Modifier.isFinal(method.getModifiers)

  override def toString: String =
    if showSpan.isEmpty then method.toString else s"$method $showSpan"

  override def isBridge: Boolean = method match
    case _: Constructor[?] => false
    case m: Method => m.isBridge

  override def isConstructor: Boolean = method.isInstanceOf[Constructor[?]]

  override def sourceLines: Option[binary.SourceLines] = extraInfos.sourceLines

  override def instructions: Seq[binary.Instruction] = extraInfos.instructions
