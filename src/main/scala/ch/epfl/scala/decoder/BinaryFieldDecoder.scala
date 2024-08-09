package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.internal.*
import tastyquery.Contexts.*
import tastyquery.Symbols.*

trait BinaryFieldDecoder(using Context, ThrowOrWarn):
  self: BinaryClassDecoder =>

  def decode(field: binary.Field): DecodedField =
    val decodedClass = decode(field.declaringClass)
    decode(decodedClass, field)

  def decode(decodedClass: DecodedClass, field: binary.Field): DecodedField =
    def tryDecode(f: PartialFunction[binary.Field, Seq[DecodedField]]): Seq[DecodedField] =
      f.applyOrElse(field, _ => Seq.empty[DecodedField])

    extension (xs: Seq[DecodedField])
      def orTryDecode(f: PartialFunction[binary.Field, Seq[DecodedField]]): Seq[DecodedField] =
        if xs.nonEmpty then xs else f.applyOrElse(field, _ => Seq.empty[DecodedField])
    val decodedFields =
      tryDecode {
        case Patterns.LazyVal(name) =>
          for
            owner <- decodedClass.classSymbol.toSeq ++ decodedClass.linearization.filter(_.isTrait)
            sym <- owner.declarations.collect {
              case sym: TermSymbol if sym.nameStr == name && sym.isModuleOrLazyVal => sym
            }
          yield DecodedField.ValDef(decodedClass, sym)
        case Patterns.Module() =>
          decodedClass.classSymbol.flatMap(_.moduleValue).map(DecodedField.ModuleVal(decodedClass, _)).toSeq
        case Patterns.Offset(nbr) =>
          Seq(DecodedField.LazyValOffset(decodedClass, nbr, defn.LongType))
        case Patterns.OuterField() =>
          decodedClass.symbolOpt
            .flatMap(_.outerClass)
            .map(outerClass => DecodedField.Outer(decodedClass, outerClass.selfType))
            .toSeq
        case Patterns.SerialVersionUID() =>
          Seq(DecodedField.SerialVersionUID(decodedClass, defn.LongType))
        case Patterns.LazyValBitmap(name) =>
          Seq(DecodedField.LazyValBitmap(decodedClass, defn.BooleanType, name))
        case Patterns.AnyValCapture() =>
          for
            classSym <- decodedClass.symbolOpt.toSeq
            outerClass <- classSym.outerClass.toSeq
            if outerClass.isSubClass(defn.AnyValClass)
            sym <- outerClass.declarations.collect {
              case sym: TermSymbol if sym.isVal && !sym.isMethod => sym
            }
          yield DecodedField.Capture(decodedClass, sym)
        case Patterns.Capture(names) =>
          decodedClass.treeOpt.toSeq
            .flatMap(CaptureCollector.collectCaptures)
            .filter { captureSym =>
              names.exists {
                case Patterns.LazyVal(name) => name == captureSym.nameStr
                case name => name == captureSym.nameStr
              }
            }
            .map(DecodedField.Capture(decodedClass, _))

        case _ if field.isStatic && decodedClass.isJava =>
          for
            owner <- decodedClass.companionClassSymbol.toSeq
            sym <- owner.declarations.collect { case sym: TermSymbol if sym.nameStr == field.name => sym }
          yield DecodedField.ValDef(decodedClass, sym)
      }.orTryDecode { case _ =>
        for
          owner <- withCompanionIfExtendsJavaLangEnum(decodedClass) ++ decodedClass.linearization.filter(_.isTrait)
          sym <- owner.declarations.collect {
            case sym: TermSymbol if matchTargetName(field, sym) && !sym.isMethod => sym
          }
        yield DecodedField.ValDef(decodedClass, sym)
      }
    decodedFields.singleOrThrow(field)
  end decode

  private def withCompanionIfExtendsJavaLangEnum(decodedClass: DecodedClass): Seq[ClassSymbol] =
    decodedClass.classSymbol.toSeq.flatMap { cls =>
      if cls.isSubClass(Definitions.javaLangEnumClass) then Seq(cls) ++ cls.companionClass
      else Seq(cls)
    }

  private def matchTargetName(field: binary.Field, symbol: TermSymbol): Boolean =
    field.unexpandedDecodedNames.map(_.stripSuffix("$")).contains(symbol.targetNameStr)
