package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary.*

import scala.collection.mutable
import org.objectweb.asm
import java.io.IOException
import ch.epfl.scala.decoder.binary.SignedName
import java.net.URLClassLoader
import java.nio.file.Path
import org.objectweb.asm.Type

class JavaReflectLoader(classLoader: ClassLoader, loadExtraInfo: Boolean) extends BinaryClassLoader:
  private val loadedClasses: mutable.Map[Class[?], JavaReflectClass] = mutable.Map.empty

  def loadClass(cls: Class[?]): JavaReflectClass =
    loadedClasses.getOrElseUpdate(cls, doLoadClass(cls))

  override def loadClass(name: String): JavaReflectClass =
    val cls = classLoader.loadClass(name)
    loadClass(cls)

  private def doLoadClass(cls: Class[?]): JavaReflectClass =
    val extraInfo =
      if loadExtraInfo && !cls.isPrimitive && !cls.isArray then
        try
          val name = cls.getName
          val inputStream = classLoader.getResourceAsStream(name.replace('.', '/') + ".class")
          val asmReader = new asm.ClassReader(inputStream)
          getExtraInfo(asmReader)
        catch case _: IOException => ExtraClassInfo.empty
      else ExtraClassInfo.empty
    JavaReflectClass(cls, extraInfo, this)

  private def getExtraInfo(reader: asm.ClassReader): ExtraClassInfo =
    assert(loadExtraInfo)
    var sourceName: String = ""
    var allLines = mutable.Set.empty[Int]
    val extraInfos = mutable.Map.empty[SignedName, ExtraMethodInfo]
    val visitor =
      new asm.ClassVisitor(asm.Opcodes.ASM9):
        override def visitSource(source: String, debug: String): Unit = sourceName = source
        override def visitMethod(
            access: Int,
            name: String,
            descriptor: String,
            signature: String,
            exceptions: Array[String]
        ): asm.MethodVisitor =
          new asm.MethodVisitor(asm.Opcodes.ASM9):
            val lines = mutable.Set.empty[Int]
            val instructions = mutable.Buffer.empty[Instruction]
            val variables = mutable.Buffer.empty[Instruction.Variable]
            override def visitLineNumber(line: Int, start: asm.Label): Unit =
              lines += line
            override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit =
              instructions += Instruction.Field(opcode, owner.replace('/', '.'), name, descriptor)
            override def visitMethodInsn(
                opcode: Int,
                owner: String,
                name: String,
                descriptor: String,
                isInterface: Boolean
            ): Unit =
              instructions += Instruction.Method(opcode, owner.replace('/', '.'), name, descriptor, isInterface)
              if descriptor.startsWith("(Lscala/runtime/Lazy") then
                variables += Instruction.Variable(name + "$lzyVal", descriptor.substring(descriptor.indexOf(')') + 1), null)
            override def visitLocalVariable(
                name: String,
                descriptor: String,
                signature: String,
                start: asm.Label,
                end: asm.Label,
                index: Int
            ): Unit =
              variables += Instruction.Variable(name, descriptor, signature)
            override def visitEnd(): Unit =
              allLines ++= lines
              val sourceLines = Option.when(sourceName.nonEmpty)(SourceLines(sourceName, lines.toSeq))
              extraInfos += SignedName(name, descriptor) -> ExtraMethodInfo(
                sourceLines,
                instructions.toSeq,
                variables.toSeq
              )
    reader.accept(visitor, asm.Opcodes.ASM9)
    val sourceLines = Option.when(sourceName.nonEmpty)(SourceLines(sourceName, allLines.toSeq))
    ExtraClassInfo(sourceLines, extraInfos.toMap)

object JavaReflectLoader:
  def apply(classPath: Seq[Path], loadExtraInfo: Boolean = true): JavaReflectLoader =
    val classLoader = URLClassLoader(classPath.map(_.toUri.toURL).toArray)
    new JavaReflectLoader(classLoader, loadExtraInfo)
