package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.binary.Variable
import java.lang.reflect.Constructor
import java.lang.reflect.Method
import ch.epfl.scala.decoder.binary.SignedName

class JavaReflectConstructor(
    constructor: Constructor[?],
    val signedName: SignedName,
    extraInfos: ExtraMethodInfo,
    loader: JavaReflectLoader
) extends binary.Method:

  override def variables: Seq[Variable] = Seq.empty

  override def returnType: Option[binary.Type] = Some(loader.loadClass(classOf[Unit]))

  override def returnTypeName: String = "void"

  override def declaringClass: binary.ClassType =
    loader.loadClass(constructor.getDeclaringClass)

  override def allParameters: Seq[binary.Parameter] =
    constructor.getParameters.map(JavaReflectParameter.apply(_, loader))

  override def name: String = "<init>"

  override def isBridge: Boolean = false

  override def isStatic: Boolean = false

  override def isFinal: Boolean = true

  override def isConstructor: Boolean = true

  override def toString: String = constructor.toString

  override def sourceLines: Option[binary.SourceLines] = extraInfos.sourceLines

  override def instructions: Seq[binary.Instruction] = extraInfos.instructions
