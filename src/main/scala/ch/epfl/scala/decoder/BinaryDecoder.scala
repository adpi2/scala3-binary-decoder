package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.internal.*
import ch.epfl.scala.decoder.javareflect.JavaReflectLoader
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders

import java.nio.file.Path
import scala.util.matching.Regex
import tastyquery.Exceptions.NonMethodReferenceException

object BinaryDecoder:
  def apply(classEntries: Seq[Path])(using ThrowOrWarn): BinaryDecoder =
    val classpath = CustomClasspath(ClasspathLoaders.read(classEntries.toList))
    val ctx = Context.initialize(classpath)
    new BinaryDecoder(using ctx)

  def cached(classEntries: Seq[Path])(using ThrowOrWarn): BinaryDecoder =
    val classpath = CustomClasspath(ClasspathLoaders.read(classEntries.toList))
    val ctx = Context.initialize(classpath)
    new CachedBinaryDecoder(using ctx)

class BinaryDecoder(using Context, ThrowOrWarn):
  private given defn: Definitions = Definitions()

  def decode(cls: binary.ClassType): DecodedClass =
    val javaParts = cls.name.split('.')
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else defn.EmptyPackage
    val decodedClassName = NameTransformer.decode(javaParts.last)
    val allSymbols = decodedClassName match
      case Patterns.AnonClass(declaringClassName, remaining) =>
        val WithLocalPart = "(.+)\\$(.+)\\$\\d+".r
        val topLevelClassName = declaringClassName match
          case WithLocalPart(topLevelClassName, _) => topLevelClassName.stripSuffix("$")
          case topLevelClassName => topLevelClassName
        reduceAmbiguityOnClasses(decodeLocalClasses(cls, packageSym, topLevelClassName, "$anon", remaining))
      case Patterns.LocalClass(declaringClassName, localClassName, remaining) =>
        decodeLocalClasses(cls, packageSym, declaringClassName, localClassName, remaining)
      case _ => decodeClassFromPackage(packageSym, decodedClassName)

    val candidates =
      if cls.isObject then allSymbols.filter(_.isModuleClass)
      else if cls.sourceLines.forall(_.isEmpty) && allSymbols.forall(_.isModuleClass) then
        allSymbols.collect { case cls: DecodedClass.ClassDef => DecodedClass.SyntheticCompanionClass(cls.symbol) }
      else allSymbols.filter(!_.isModuleClass)
    candidates.singleOrThrow(cls)
  end decode

  def decode(method: binary.Method): DecodedMethod =
    val decodedClass = decode(method.declaringClass)
    decode(decodedClass, method)

  def decode(decodedClass: DecodedClass, method: binary.Method): DecodedMethod =
    def tryDecode(f: PartialFunction[binary.Method, Seq[DecodedMethod]]): Seq[DecodedMethod] =
      f.applyOrElse(method, _ => Seq.empty[DecodedMethod])

    extension (xs: Seq[DecodedMethod])
      def orTryDecode(f: PartialFunction[binary.Method, Seq[DecodedMethod]]): Seq[DecodedMethod] =
        if xs.nonEmpty then xs else f.applyOrElse(method, _ => Seq.empty[DecodedMethod])
    val candidates =
      tryDecode {
        // static and/or bridge
        case Patterns.AdaptedAnonFun() => decodeAdaptedAnonFun(decodedClass, method)
        // bridge or standard
        case Patterns.SpecializedMethod(names) => decodeSpecializedMethod(decodedClass, method, names)
        // bridge only
        case m if m.isBridge => decodeBridgesAndMixinForwarders(decodedClass, method).toSeq
        // static or standard
        case Patterns.AnonFun() => decodeAnonFunsAndReduceAmbiguity(decodedClass, method)
        case Patterns.ByNameArgProxy() => decodeByNameArgsProxy(decodedClass, method)
        case Patterns.SuperArg() => decodeSuperArgs(decodedClass, method)
        case Patterns.LiftedTree() => decodeLiftedTries(decodedClass, method)
        case Patterns.LocalLazyInit(names) => decodeLocalLazyInit(decodedClass, method, names)
        // static only
        case Patterns.TraitInitializer() => decodeTraitInitializer(decodedClass, method)
        case Patterns.DeserializeLambda() =>
          Seq(DecodedMethod.DeserializeLambda(decodedClass, defn.DeserializeLambdaType))
        case Patterns.TraitStaticForwarder() => decodeTraitStaticForwarder(decodedClass, method).toSeq
        case m if m.isStatic && decodedClass.isJava => decodeStaticJavaMethods(decodedClass, method)
        // cannot be static anymore
        case Patterns.LazyInit(name) => decodeLazyInit(decodedClass, name)
        case Patterns.Outer() => decodeOuter(decodedClass).toSeq
        case Patterns.ParamForwarder(names) => decodeParamForwarder(decodedClass, method, names)
        case Patterns.TraitSetter(name) => decodeTraitSetter(decodedClass, method, name)
        case Patterns.Setter(names) =>
          decodeStandardMethods(decodedClass, method).orIfEmpty(decodeSetter(decodedClass, method, names))
        case Patterns.SuperAccessor(names) => decodeSuperAccessor(decodedClass, method, names)
      }
        .orTryDecode { case Patterns.ValueClassExtension() => decodeValueClassExtension(decodedClass, method) }
        .orTryDecode { case Patterns.InlineAccessor(names) => decodeInlineAccessor(decodedClass, method, names).toSeq }
        .orTryDecode { case Patterns.LocalMethod(names) => decodeLocalMethods(decodedClass, method, names) }
        .orTryDecode {
          case m if m.isStatic => decodeStaticForwarder(decodedClass, method)
          case _ => decodeStandardMethods(decodedClass, method)
        }

    candidates.singleOrThrow(method)
  end decode

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
          decodedClass.symbolOpt.toSeq
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

  private def reduceAmbiguityOnClasses(syms: Seq[DecodedClass]): Seq[DecodedClass] =
    if syms.size > 1 then
      val reduced = syms.filterNot(sym => syms.exists(enclose(sym, _)))
      if reduced.size != 0 then reduced else syms
    else syms

  private def enclose(enclosing: DecodedClass, enclosed: DecodedClass): Boolean =
    (enclosing, enclosed) match
      case (enclosing: DecodedClass.InlinedClass, enclosed: DecodedClass.InlinedClass) =>
        enclosing.callPos.enclose(enclosed.callPos) || (
          !enclosed.callPos.enclose(enclosing.callPos) &&
            enclose(enclosing.underlying, enclosed.underlying)
        )
      case (enclosing: DecodedClass.InlinedClass, enclosed) =>
        enclosing.callPos.enclose(enclosed.pos)
      case (enclosing, enclosed: DecodedClass.InlinedClass) =>
        enclosing.pos.enclose(enclosed.callPos)
      case (enclosing, enclosed) =>
        enclosing.pos.enclose(enclosed.pos)

  private def decodeLocalClasses(
      javaClass: binary.ClassType,
      packageSym: PackageSymbol,
      declaringClassName: String,
      localClassName: String,
      remaining: Option[String]
  ): Seq[DecodedClass] =
    val classOwners = decodeClassFromPackage(packageSym, declaringClassName).map(_.symbol)
    remaining match
      case None =>
        val parents = (javaClass.superclass.toSet ++ javaClass.interfaces)
          .map(decode)
          .collect { case cls: DecodedClass.ClassDef => cls.symbol }
        classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, javaClass.sourceLines))
          .filter(matchParents(_, parents, javaClass.isInterface))
      case Some(remaining) =>
        val localClasses = classOwners
          .flatMap(cls => collectLocalClasses(cls, localClassName, None))
          .flatMap(_.classSymbol)
        localClasses.flatMap(s => decodeClassRecursively(s, remaining))

  private def decodeClassFromPackage(owner: PackageSymbol, decodedName: String): Seq[DecodedClass.ClassDef] =
    val packageObject = "([^\\$]+\\$package)(\\$.*)?".r
    val specializedClass = "([^\\$]+\\$mc.+\\$sp)(\\$.*)?".r
    val standardClass = "([^\\$]+)(\\$.*)?".r
    val topLevelName = decodedName match
      case packageObject(name, _) => name
      case specializedClass(name, _) => name
      case standardClass(name, _) => name
    val remaining = decodedName.stripPrefix(topLevelName).stripPrefix("$")
    val typeNames = Seq(typeName(topLevelName), moduleClassName(topLevelName))
    typeNames
      .flatMap(owner.getDecl)
      .collect { case sym: ClassSymbol => sym }
      .flatMap { sym =>
        if remaining.isEmpty then Seq(DecodedClass.ClassDef(sym))
        else decodeClassRecursively(sym, remaining)
      }

  private def decodeClassRecursively(owner: ClassSymbol, decodedName: String): Seq[DecodedClass.ClassDef] =
    owner.declarations
      .collect { case sym: ClassSymbol => sym }
      .flatMap { sym =>
        val Symbol = s"${Regex.quote(sym.sourceName)}\\$$?(.*)".r
        decodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(DecodedClass.ClassDef(sym))
            else decodeClassRecursively(sym, remaining)
          case _ => None
      }

  private def collectLocalClasses(
      classSymbol: ClassSymbol,
      name: String,
      sourceLines: Option[binary.SourceLines]
  ): Seq[DecodedClass] =
    val localClasses = collectLiftedTrees(classSymbol, sourceLines) {
      case cls: LocalClass if cls.symbol.sourceName == name => cls
    }
      .map(cls => wrapIfInline(cls, DecodedClass.ClassDef(cls.symbol)))
    val samAndPartialFunctions = collectLiftedTrees(classSymbol, sourceLines) { case lambda: LambdaTree => lambda }
      .map { lambda =>
        val (term, samClass) = lambda.symbol
        wrapIfInline(lambda, DecodedClass.SAMOrPartialFunction(term, samClass, lambda.tpe.asInstanceOf))
      }
    localClasses ++ samAndPartialFunctions

  private def matchParents(
      decodedClass: DecodedClass,
      expectedParents: Set[ClassSymbol],
      isInterface: Boolean
  ): Boolean =
    decodedClass match
      case cls: DecodedClass.ClassDef =>
        if cls.symbol.isEnum then expectedParents == cls.symbol.parentClasses.toSet + defn.ProductClass
        else if isInterface then expectedParents == cls.symbol.parentClasses.filter(_.isTrait).toSet
        else if cls.symbol.isAnonClass then cls.symbol.parentClasses.forall(expectedParents.contains)
        else expectedParents == cls.symbol.parentClasses.toSet
      case _: DecodedClass.SyntheticCompanionClass => false
      case anonFun: DecodedClass.SAMOrPartialFunction =>
        if anonFun.parentClass == defn.PartialFunctionClass then
          expectedParents == Set(defn.AbstractPartialFunctionClass, defn.SerializableClass)
        else expectedParents.contains(anonFun.parentClass)
      case inlined: DecodedClass.InlinedClass => matchParents(inlined.underlying, expectedParents, isInterface)

  private def matchParents(classSymbol: ClassSymbol, expectedParents: Set[ClassSymbol], isInterface: Boolean): Boolean =
    if classSymbol.isEnum then expectedParents == classSymbol.parentClasses.toSet + defn.ProductClass
    else if isInterface then expectedParents == classSymbol.parentClasses.filter(_.isTrait).toSet
    else if classSymbol.isAnonClass then classSymbol.parentClasses.forall(expectedParents.contains)
    else expectedParents == classSymbol.parentClasses.toSet

  private def wrapIfInline(liftedTree: LiftedTree[?], decodedClass: DecodedClass): DecodedClass =
    liftedTree match
      case InlinedFromDef(underlying, inlineCall) =>
        DecodedClass.InlinedClass(wrapIfInline(underlying, decodedClass), inlineCall.callTree)
      case _ => decodedClass

  private def decodeStaticJavaMethods(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    decodedClass.companionClassSymbol.toSeq
      .flatMap(_.declarations)
      .collect {
        case sym: TermSymbol
            if matchTargetName(method, sym) && matchSignature(method, sym.declaredType, checkParamNames = false) =>
          DecodedMethod.ValOrDefDef(decodedClass, sym)
      }

  private def decodeStandardMethods(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    def rec(underlying: DecodedClass): Seq[DecodedMethod] =
      underlying match
        case anonFun: DecodedClass.SAMOrPartialFunction =>
          if method.isConstructor then Seq(DecodedMethod.SAMOrPartialFunctionConstructor(decodedClass, anonFun.tpe))
          else if anonFun.parentClass == defn.PartialFunctionClass then
            decodePartialFunctionImpl(decodedClass, anonFun.tpe, method).toSeq
          else decodeSAMFunctionImpl(decodedClass, anonFun.symbol, anonFun.parentClass, method).toSeq
        case underlying: DecodedClass.ClassDef => decodeInstanceMethods(decodedClass, underlying.symbol, method)
        case _: DecodedClass.SyntheticCompanionClass => Seq.empty
        case inlined: DecodedClass.InlinedClass => rec(inlined.underlying)
    rec(decodedClass)

  private def decodeParamForwarder(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.ValOrDefDef] =
    decodedClass.declarations.collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym.declaredType) =>
        DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def decodeTraitSetter(
      decodedClass: DecodedClass,
      method: binary.Method,
      name: String
  ): Seq[DecodedMethod.SetterAccessor] =
    for
      traitSym <- decodedClass.linearization.filter(_.isTrait)
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if sym.targetNameStr == name && !sym.isMethod && !sym.isAbstractMember => sym
      }
      paramType <- decodedClass.thisType.map(sym.typeAsSeenFrom).collect { case tpe: Type => tpe }
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(paramType), defn.UnitType)
      DecodedMethod.SetterAccessor(decodedClass, sym, tpe)

  private def decodeSetter(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.SetterAccessor] =
    for
      param <- method.allParameters.lastOption.toSeq
      sym <- decodeFields(decodedClass, param.`type`, names)
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
      DecodedMethod.SetterAccessor(decodedClass, sym, tpe)

  private def decodeFields(
      decodedClass: DecodedClass,
      binaryType: binary.Type,
      names: Seq[String]
  ): Seq[TermSymbol] =
    def matchType0(sym: TermSymbol): Boolean = matchSetterArgType(sym.declaredType, binaryType)
    decodedClass.declarations.collect {
      case sym: TermSymbol if !sym.isMethod && names.exists(sym.targetNameStr == _) && matchType0(sym) =>
        sym
    }

  private def decodeSuperAccessor(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    for
      traitSym <- decodedClass.linearization.filter(_.isTrait)
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if names.contains(sym.targetNameStr) && !sym.isAbstractMember => sym
      }
      expectedTpe <- decodedClass.thisType.map(sym.typeAsSeenFrom(_))
      if matchSignature(method, expectedTpe)
    yield DecodedMethod.SuperAccessor(decodedClass, sym, expectedTpe)

  private def decodeSpecializedMethod(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.SpecializedMethod] =
    decodedClass.declarations.collect {
      case sym: TermSymbol
          if names.contains(sym.targetNameStr) &&
            matchSignature(
              method,
              sym.declaredType,
              captureAllowed = false,
              checkParamNames = false,
              checkTypeErasure = false
            ) &&
            // hack: in Scala 3 only overriding symbols can be specialized (Function and Tuple)
            sym.allOverriddenSymbols.nonEmpty =>
        DecodedMethod.SpecializedMethod(decodedClass, sym)
    }

  private def decodeInlineAccessor(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    val classLoader = method.declaringClass.classLoader
    val methodAccessors = method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map { binaryTarget =>
        val target = decode(binaryTarget)
        // val tpe = target.declaredType.asSeenFrom(fromType, fromClass)
        DecodedMethod.InlineAccessor(decodedClass, target)
      }
    def singleFieldInstruction(f: binary.Instruction.Field => Boolean) = method.instructions
      .collect { case instr: binary.Instruction.Field => instr }
      .singleOpt
      .filter(f)
      .toSeq
    def fieldSetters =
      val expectedNames = names.map(_.stripSuffix("_=")).distinct
      for
        instr <- singleFieldInstruction(f => f.isPut && f.unexpandedDecodedNames.exists(expectedNames.contains))
        binaryField <- classLoader.loadClass(instr.owner).declaredField(instr.name).toSeq
        fieldOwner = decode(binaryField.declaringClass)
        sym <- decodeFields(fieldOwner, binaryField.`type`, instr.unexpandedDecodedNames)
      yield
        val tpe = MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
        val decodedTarget = DecodedMethod.SetterAccessor(fieldOwner, sym, tpe)
        DecodedMethod.InlineAccessor(decodedClass, decodedTarget)
    def fieldGetters =
      for
        instr <- singleFieldInstruction(f => !f.isPut && f.unexpandedDecodedNames.exists(names.contains))
        binaryField <- classLoader.loadClass(instr.owner).declaredField(instr.name).toSeq
        fieldOwner = decode(binaryField.declaringClass)
        sym <- decodeFields(fieldOwner, binaryField.`type`, instr.unexpandedDecodedNames)
      yield DecodedMethod.InlineAccessor(decodedClass, DecodedMethod.ValOrDefDef(fieldOwner, sym))
    def moduleAccessors =
      for
        instr <- singleFieldInstruction(_.name == "MODULE$")
        targetClass = decode(classLoader.loadClass(instr.owner))
        targetClassSym <- targetClass.classSymbol
        targetTermSym <- targetClassSym.moduleValue
      yield DecodedMethod.InlineAccessor(decodedClass, DecodedMethod.ValOrDefDef(targetClass, targetTermSym))
    def valueClassAccessors =
      if method.instructions.isEmpty && method.isExtensionMethod then
        for
          companionClass <- decodedClass.companionClass.toSeq
          param <- method.allParameters.lastOption.toSeq
          sym <- decodeFields(companionClass, param.`type`, names.map(_.stripSuffix("$extension")))
        yield
          val decodedTarget = DecodedMethod.ValOrDefDef(decodedClass, sym)
          DecodedMethod.InlineAccessor(decodedClass, decodedTarget)
      else Seq.empty
    methodAccessors.toSeq
      .orIfEmpty(fieldSetters)
      .orIfEmpty(fieldGetters)
      .orIfEmpty(moduleAccessors.toSeq)
      .orIfEmpty(valueClassAccessors)

  private def decodeInstanceMethods(
      decodedClass: DecodedClass,
      classSymbol: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod] =
    if method.isConstructor && classSymbol.isSubClass(defn.AnyValClass) then
      classSymbol.getAllOverloadedDecls(SimpleName("<init>")).map(DecodedMethod.ValOrDefDef(decodedClass, _))
    else
      val isJava = decodedClass.isJava
      val fromClass = classSymbol.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) => sym }
        .collect {
          case sym
              if matchSignature(
                method,
                sym.declaredType,
                asJavaVarargs = isJava,
                captureAllowed = !isJava,
                checkParamNames = !isJava
              ) =>
            DecodedMethod.ValOrDefDef(decodedClass, sym)
          case sym if !isJava && matchSignature(method, sym.declaredType, asJavaVarargs = true) =>
            DecodedMethod.Bridge(DecodedMethod.ValOrDefDef(decodedClass, sym), sym.declaredType)
        }
      fromClass.orIfEmpty(decodeAccessorsFromTraits(decodedClass, classSymbol, method))

  private def decodeAccessorsFromTraits(
      decodedClass: DecodedClass,
      classSymbol: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod] =
    if classSymbol.isTrait then Seq.empty
    else decodeAccessorsFromTraits(decodedClass, classSymbol, classSymbol.thisType, method)

  private def decodeAccessorsFromTraits(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Seq[DecodedMethod] =
    for
      traitSym <- fromClass.linearization.filter(_.isTrait)
      if !method.isExpanded || method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations
        .collect {
          case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym.declaredType) => sym
        }
      if method.isExpanded == sym.isPrivate
      if sym.isParamAccessor || sym.isSetter || !sym.isMethod
      if sym.isOverridingSymbol(fromClass)
    yield
      val tpe = sym.typeAsSeenFrom(fromType)
      if sym.isParamAccessor then DecodedMethod.TraitParamAccessor(decodedClass, sym)
      else if sym.isSetter then DecodedMethod.SetterAccessor(decodedClass, sym, tpe)
      else DecodedMethod.GetterAccessor(decodedClass, sym, tpe)

  private def decodeLazyInit(decodedClass: DecodedClass, name: String): Seq[DecodedMethod] =
    val matcher: PartialFunction[Symbol, TermSymbol] =
      case sym: TermSymbol if sym.isModuleOrLazyVal && sym.nameStr == name => sym
    val fromClass = decodedClass.declarations.collect(matcher).map(DecodedMethod.LazyInit(decodedClass, _))
    def fromTraits =
      for
        traitSym <- decodedClass.linearization.filter(_.isTrait)
        term <- traitSym.declarations.collect(matcher)
        if term.isOverridingSymbol(decodedClass)
      yield DecodedMethod.LazyInit(decodedClass, term)
    fromClass.orIfEmpty(fromTraits)

  private def decodeTraitStaticForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod.TraitStaticForwarder] =
    method.instructions
      .collect {
        case binary.Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
          method.declaringClass.method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map(target => DecodedMethod.TraitStaticForwarder(decode(decodedClass, target)))

  private def decodeOuter(decodedClass: DecodedClass): Option[DecodedMethod.OuterAccessor] =
    decodedClass.symbolOpt
      .flatMap(_.outerClass)
      .map(outerClass => DecodedMethod.OuterAccessor(decodedClass, outerClass.thisType))

  private def decodeTraitInitializer(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.ValOrDefDef] =
    decodedClass.declarations.collect {
      case sym: TermSymbol if sym.name == nme.Constructor => DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def decodeValueClassExtension(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.ValOrDefDef] =
    val names = method.unexpandedDecodedNames.map(_.stripSuffix("$extension"))
    decodedClass.companionClassSymbol.toSeq.flatMap(_.declarations).collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym.declaredType) =>
        DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def decodeStaticForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.StaticForwarder] =
    decodedClass.companionClassSymbol.toSeq.flatMap(decodeStaticForwarder(decodedClass, _, method))

  private def decodeStaticForwarder(
      decodedClass: DecodedClass,
      companionObject: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod.StaticForwarder] =
    method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        method.declaringClass.classLoader.loadClass(owner).method(name, descriptor)
      }
      .flatten
      .singleOpt
      .toSeq
      .map(decode)
      .collect {
        case mixin: DecodedMethod.MixinForwarder => mixin.target
        case target => target
      }
      .map { target =>
        val declaredType = target.symbolOpt
          .map(_.typeAsSeenFrom(companionObject.thisType))
          .getOrElse(target.declaredType)
        DecodedMethod.StaticForwarder(decodedClass, target, declaredType)
      }

  private def decodeSAMFunctionImpl(
      decodedClass: DecodedClass,
      symbol: TermSymbol,
      parentClass: ClassSymbol,
      method: binary.Method
  ): Option[DecodedMethod] =
    val types =
      for
        parentCls <- parentClass.linearization.iterator
        overridden <- parentCls.declarations.collect { case term: TermSymbol if matchTargetName(method, term) => term }
        if overridden.overridingSymbol(parentClass).exists(_.isAbstractMember)
      yield DecodedMethod.SAMOrPartialFunctionImpl(decodedClass, overridden, symbol.declaredType)
    types.nextOption

  private def decodePartialFunctionImpl(
      decodedClass: DecodedClass,
      tpe: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    for sym <- defn.PartialFunctionClass.getNonOverloadedDecl(SimpleName(method.name)) yield
      val implTpe = sym.typeAsSeenFrom(SkolemType(tpe))
      DecodedMethod.SAMOrPartialFunctionImpl(decodedClass, sym, implTpe)

  private def decodeBridgesAndMixinForwarders(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod] =
    def rec(underlying: DecodedClass): Option[DecodedMethod] =
      underlying match
        case underlying: DecodedClass.ClassDef =>
          if !underlying.symbol.isTrait then
            decodeBridgesAndMixinForwarders(decodedClass, underlying.symbol, underlying.symbol.thisType, method)
          else None
        case underlying: DecodedClass.SAMOrPartialFunction =>
          decodeBridgesAndMixinForwarders(decodedClass, underlying.parentClass, SkolemType(underlying.tpe), method)
        case underlying: DecodedClass.InlinedClass => rec(underlying.underlying)
        case _: DecodedClass.SyntheticCompanionClass => None
    rec(decodedClass)

  private def decodeBridgesAndMixinForwarders(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    decodeBridges(decodedClass, fromClass, fromType, method)
      .orIfEmpty(decodeMixinForwarder(decodedClass, method))

  private def decodeBridges(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    method.instructions
      .collect {
        case binary.Instruction.Method(_, owner, name, descriptor, _) if name == method.name =>
          method.declaringClass.classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map { binaryTarget =>
        val target = decode(binaryTarget)
        val tpe = target.declaredType.asSeenFrom(fromType, fromClass)
        DecodedMethod.Bridge(target, tpe)
      }

  private def decodeMixinForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod.MixinForwarder] =
    method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        method.declaringClass.classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .filter(target => target.isStatic && target.declaringClass.isInterface)
      .map(decode)
      .collect { case staticForwarder: DecodedMethod.TraitStaticForwarder =>
        DecodedMethod.MixinForwarder(decodedClass, staticForwarder.target)
      }

  private def withCompanionIfExtendsAnyVal(decodedClass: DecodedClass): Seq[Symbol] = decodedClass match
    case classDef: DecodedClass.ClassDef =>
      Seq(classDef.symbol) ++ classDef.symbol.companionClass.filter(_.isSubClass(defn.AnyValClass))
    case _: DecodedClass.SyntheticCompanionClass => Seq.empty
    case anonFun: DecodedClass.SAMOrPartialFunction => Seq(anonFun.symbol)
    case inlined: DecodedClass.InlinedClass => withCompanionIfExtendsAnyVal(inlined.underlying)

  private def withCompanionIfExtendsJavaLangEnum(decodedClass: DecodedClass): Seq[ClassSymbol] =
    decodedClass.classSymbol.toSeq.flatMap { cls =>
      if cls.isSubClass(defn.javaLangEnumClass) then Seq(cls) ++ cls.companionClass
      else Seq(cls)
    }

  private def decodeAdaptedAnonFun(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    if method.instructions.nonEmpty then
      val underlying = method.instructions
        .collect {
          case binary.Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
            method.declaringClass.declaredMethod(name, descriptor)
        }
        .flatten
        .singleOrElse(unexpected(s"$method is not an adapted method: cannot find underlying invocation"))
      decodeAnonFunsAndByNameArgs(decodedClass, underlying).map(DecodedMethod.AdaptedFun(_))
    else Seq.empty

  private def decodeAnonFunsAndReduceAmbiguity(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod] =
    val candidates = decodeAnonFunsAndByNameArgs(decodedClass, method)
    if candidates.size > 1 then
      val clashingMethods = method.declaringClass.declaredMethods
        .filter(m => m.returnType.zip(method.returnType).forall(_ == _) && m.signedName.name != method.signedName.name)
        .collect { case m @ Patterns.AnonFun() if m.name != method.name => m }
        .map(m => m -> decodeAnonFunsAndByNameArgs(decodedClass, m).toSet)
        .toMap
      def reduceAmbiguity(
          methods: Map[binary.Method, Set[DecodedMethod]]
      ): Map[binary.Method, Set[DecodedMethod]] =
        val found = methods.collect { case (m, syms) if syms.size == 1 => syms.head }
        val reduced = methods.map { case (m, candidates) =>
          if candidates.size > 1 then m -> (candidates -- found)
          else m -> candidates
        }
        if reduced.count { case (_, s) => s.size == 1 } == found.size then methods
        else reduceAmbiguity(reduced)
      reduceAmbiguity(clashingMethods + (method -> candidates.toSet))(method).toSeq
    else candidates

  private def decodeAnonFunsAndByNameArgs(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod] =
    val anonFuns = decodeLocalMethods(decodedClass, method, Seq(CommonNames.anonFun.toString))
    val byNameArgs =
      if method.allParameters.forall(_.isCapture) then decodeByNameArgs(decodedClass, method)
      else Seq.empty
    reduceAmbiguityOnMethods(anonFuns ++ byNameArgs)

  private def decodeLocalMethods(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    collectLocalMethods(decodedClass, method) {
      case fun if names.contains(fun.symbol.name.toString) && matchLiftedFunSignature(method, fun) =>
        wrapIfInline(fun, DecodedMethod.ValOrDefDef(decodedClass, fun.symbol.asTerm))
    }

  private def reduceAmbiguityOnMethods(syms: Seq[DecodedMethod]): Seq[DecodedMethod] =
    if syms.size > 1 then
      val reduced = syms.filterNot(sym => syms.exists(enclose(sym, _)))
      if reduced.size != 0 then reduced else syms
    else syms

  private def enclose(enclosing: DecodedMethod, enclosed: DecodedMethod): Boolean =
    (enclosing, enclosed) match
      case (enclosing: DecodedMethod.InlinedMethod, enclosed: DecodedMethod.InlinedMethod) =>
        enclosing.callPos.enclose(enclosed.callPos) || (
          !enclosed.callPos.enclose(enclosing.callPos) &&
            enclose(enclosing.underlying, enclosed.underlying)
        )
      case (enclosing: DecodedMethod.InlinedMethod, enclosed) =>
        enclosing.callPos.enclose(enclosed.pos)
      case (enclosing, enclosed: DecodedMethod.InlinedMethod) =>
        enclosing.pos.enclose(enclosed.callPos)
      case (enclosing, enclosed) =>
        enclosing.pos.enclose(enclosed.pos)

  private def decodeByNameArgs(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case arg: ByNameArg if !arg.isInline => arg }
      .collect {
        case arg if matchReturnType(arg.tpe, method.returnType) && matchCapture(arg.capture, method.allParameters) =>
          wrapIfInline(arg, DecodedMethod.ByNameArg(decodedClass, arg.owner, arg.tree, arg.tpe.asInstanceOf))
      }

  private def decodeByNameArgsProxy(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    val explicitByNameArgs =
      collectLiftedTrees(decodedClass, method) { case arg: ByNameArg if arg.isInline => arg }
        .collect {
          case arg if matchReturnType(arg.tpe, method.returnType) && matchCapture(arg.capture, method.allParameters) =>
            wrapIfInline(arg, DecodedMethod.ByNameArg(decodedClass, arg.owner, arg.tree, arg.tpe.asInstanceOf))
        }
    val inlineOverrides =
      for
        classSym <- decodedClass.classSymbol.toSeq
        sym <- classSym.declarations.collect {
          case sym: TermSymbol if sym.allOverriddenSymbols.nonEmpty && sym.isInline => sym
        }
        if method.sourceLines.forall(sym.pos.matchLines)
        paramSym <- sym.paramSymbols
        resultType <- Seq(paramSym.declaredType).collect { case tpe: ByNameType => tpe.resultType }
        if matchReturnType(resultType, method.returnType)
      yield
        val argTree = Ident(paramSym.name)(paramSym.localRef)(SourcePosition.NoPosition)
        DecodedMethod.ByNameArg(decodedClass, sym, argTree, resultType)
    explicitByNameArgs ++ inlineOverrides

  private def collectLocalMethods(
      decodedClass: DecodedClass,
      method: binary.Method
  )(
      matcher: PartialFunction[LiftedTree[TermSymbol], DecodedMethod]
  ): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case term: LocalTermDef => term }
      .collect(matcher)

  private def decodeSuperArgs(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.SuperConstructorArg] =
    def matchSuperArg(liftedArg: LiftedTree[Nothing]): Boolean =
      val primaryConstructor = liftedArg.owner.asClass.getAllOverloadedDecls(nme.Constructor).head
      // a super arg takes the same parameters as its constructor
      val sourceParams = extractSourceParams(method, primaryConstructor.declaredType)
      val binaryParams = splitBinaryParams(method, sourceParams)
      matchReturnType(liftedArg.tpe, method.returnType) && matchCapture(liftedArg.capture, binaryParams.capturedParams)
    collectLiftedTrees(decodedClass, method) { case arg: ConstructorArg => arg }
      .collect {
        case liftedArg if matchSuperArg(liftedArg) =>
          DecodedMethod.SuperConstructorArg(
            decodedClass,
            liftedArg.owner.asClass,
            liftedArg.tree,
            liftedArg.tpe.asInstanceOf
          )
      }

  private def decodeLiftedTries(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case tree: LiftedTry => tree }
      .collect {
        case liftedTry if matchReturnType(liftedTry.tpe, method.returnType) =>
          wrapIfInline(
            liftedTry,
            DecodedMethod.LiftedTry(decodedClass, liftedTry.owner, liftedTry.tree, liftedTry.tpe.asInstanceOf)
          )
      }

  private def decodeLocalLazyInit(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    collectLocalMethods(decodedClass, method) {
      case term if term.symbol.isModuleOrLazyVal && names.contains(term.symbol.nameStr) =>
        wrapIfInline(term, DecodedMethod.LazyInit(decodedClass, term.symbol))
    }

  private def wrapIfInline(liftedTree: LiftedTree[?], decodedMethod: DecodedMethod): DecodedMethod =
    liftedTree match
      case InlinedFromDef(liftedTree, inlineCall) =>
        DecodedMethod.InlinedMethod(wrapIfInline(liftedTree, decodedMethod), inlineCall.callTree)
      case _ => decodedMethod

  private def collectLiftedTrees[S](decodedClass: DecodedClass, method: binary.Method)(
      matcher: PartialFunction[LiftedTree[?], LiftedTree[S]]
  ): Seq[LiftedTree[S]] =
    val owners = withCompanionIfExtendsAnyVal(decodedClass)
    val sourceLines =
      if owners.size == 2 && method.allParameters.exists(p => p.name.matches("\\$this\\$\\d+")) then
        // workaround of https://github.com/lampepfl/dotty/issues/18816
        method.sourceLines.map(_.last)
      else method.sourceLines
    owners.flatMap(collectLiftedTrees(_, sourceLines)(matcher))

  private def collectLiftedTrees[S](owner: Symbol, sourceLines: Option[binary.SourceLines])(
      matcher: PartialFunction[LiftedTree[?], LiftedTree[S]]
  ): Seq[LiftedTree[S]] =
    val recursiveMatcher = new PartialFunction[LiftedTree[?], LiftedTree[S]]:
      override def apply(tree: LiftedTree[?]): LiftedTree[S] = tree.asInstanceOf[LiftedTree[S]]
      override def isDefinedAt(tree: LiftedTree[?]): Boolean = tree match
        case InlinedFromArg(underlying, _, _) => isDefinedAt(underlying)
        case InlinedFromDef(underlying, _) => isDefinedAt(underlying)
        case _ => matcher.isDefinedAt(tree)
    collectAllLiftedTrees(owner).collect(recursiveMatcher).filter(tree => sourceLines.forall(matchLines(tree, _)))

  protected def collectAllLiftedTrees(owner: Symbol): Seq[LiftedTree[?]] =
    LiftedTreeCollector.collect(owner)

  private def matchLines(liftedFun: LiftedTree[?], sourceLines: binary.SourceLines): Boolean =
    // we use endsWith instead of == because of tasty-query#434
    val positions = liftedFun.positions.filter(pos => pos.sourceFile.name.endsWith(sourceLines.sourceName))
    sourceLines.tastyLines.forall(line => positions.exists(_.containsLine(line)))

  private def matchTargetName(method: binary.Method, symbol: TermSymbol): Boolean =
    method.unexpandedDecodedNames.map(_.stripSuffix("$")).contains(symbol.targetNameStr)

  private def matchTargetName(field: binary.Field, symbol: TermSymbol): Boolean =
    field.unexpandedDecodedNames.map(_.stripSuffix("$")).contains(symbol.targetNameStr)

  private case class SourceParams(
      declaredParamNames: Seq[UnsignedTermName],
      declaredParamTypes: Seq[Type],
      expandedParamTypes: Seq[Type],
      returnType: Type
  ):
    def regularParamTypes: Seq[Type] = declaredParamTypes ++ expandedParamTypes

  private case class BinaryParams(
      capturedParams: Seq[binary.Parameter],
      declaredParams: Seq[binary.Parameter],
      expandedParams: Seq[binary.Parameter],
      returnType: Option[binary.Type]
  ):
    def regularParams = declaredParams ++ expandedParams

  private def matchLiftedFunSignature(method: binary.Method, tree: LiftedTree[TermSymbol]): Boolean =
    val sourceParams = extractSourceParams(method, tree.tpe)
    val binaryParams = splitBinaryParams(method, sourceParams)

    def matchParamNames: Boolean =
      sourceParams.declaredParamNames
        .corresponds(binaryParams.declaredParams)((name, javaParam) => name.toString == javaParam.name)

    def matchTypeErasure: Boolean =
      sourceParams.regularParamTypes
        .corresponds(binaryParams.regularParams)((tpe, javaParam) => matchArgType(tpe, javaParam.`type`, false)) &&
        matchReturnType(sourceParams.returnType, binaryParams.returnType)

    matchParamNames && matchTypeErasure && matchCapture(tree.capture, binaryParams.capturedParams)
  end matchLiftedFunSignature

  private def matchCapture(capture: Seq[String], capturedParams: Seq[binary.Parameter]): Boolean =
    val anonymousPattern = "\\$\\d+".r
    val evidencePattern = "evidence\\$\\d+".r
    def toPattern(variable: String): Regex =
      variable match
        case anonymousPattern() => "\\$\\d+\\$\\$\\d+".r
        case evidencePattern() => "evidence\\$\\d+\\$\\d+".r
        case _ =>
          val encoded = NameTransformer.encode(variable)
          s"${Regex.quote(encoded)}(\\$$tailLocal\\d+)?(\\$$lzy\\d+)?\\$$\\d+".r
    val patterns = capture.map(toPattern)
    def isCapture(param: String) =
      patterns.exists(_.unapplySeq(param).nonEmpty)
    def isProxy(param: String) = "(.+)\\$proxy\\d+\\$\\d+".r.unapplySeq(param).nonEmpty
    def isThisOrOuter(param: String) = "(.+_|\\$)(this|outer)\\$\\d+".r.unapplySeq(param).nonEmpty
    def isLazy(param: String) = "(.+)\\$lzy\\d+\\$\\d+".r.unapplySeq(param).nonEmpty
    capturedParams.forall(p => isProxy(p.name) || isCapture(p.name) || isThisOrOuter(p.name) || isLazy(p.name))

  private def matchSignature(
      method: binary.Method,
      declaredType: TypeOrMethodic,
      expandContextFunction: Boolean = true,
      captureAllowed: Boolean = true,
      asJavaVarargs: Boolean = false,
      checkParamNames: Boolean = true,
      checkTypeErasure: Boolean = true
  ): Boolean =
    val sourceParams = extractSourceParams(method, declaredType)
    val binaryParams = splitBinaryParams(method, sourceParams)

    def matchParamNames: Boolean =
      sourceParams.declaredParamNames
        .corresponds(binaryParams.declaredParams)((name, javaParam) => name.toString == javaParam.name)

    def matchTypeErasure: Boolean =
      sourceParams.regularParamTypes
        .corresponds(binaryParams.regularParams)((tpe, javaParam) =>
          matchArgType(tpe, javaParam.`type`, asJavaVarargs)
        ) && matchReturnType(sourceParams.returnType, binaryParams.returnType)

    (captureAllowed || binaryParams.capturedParams.isEmpty) &&
    binaryParams.capturedParams.forall(_.isGenerated) &&
    binaryParams.expandedParams.forall(_.isGenerated) &&
    sourceParams.regularParamTypes.size == binaryParams.regularParams.size &&
    (!checkParamNames || matchParamNames) &&
    (!checkTypeErasure || matchTypeErasure)
  end matchSignature

  private def extractSourceParams(method: binary.Method, tpe: TermType): SourceParams =
    val (expandedParamTypes, returnType) =
      if method.isConstructor && method.declaringClass.isJavaLangEnum then
        (List(defn.StringType, defn.IntType), tpe.returnType)
      else if !method.isAnonFun then expandContextFunctions(tpe.returnType, acc = Nil)
      else (List.empty, tpe.returnType)
    SourceParams(tpe.allParamNames, tpe.allParamTypes, expandedParamTypes, returnType)

  /* After code generation, a method ends up with more than its declared parameters.
   *
   * It has, in order:
   * - captured params,
   * - declared params,
   * - "expanded" params (from java.lang.Enum constructors and uncurried context function types).
   *
   * We can only check the names of declared params.
   * We can check the (erased) type of declared and expanded params; together we call them "regular" params.
   */
  private def splitBinaryParams(method: binary.Method, sourceParams: SourceParams): BinaryParams =
    val (capturedParams, regularParams) =
      method.allParameters.splitAt(method.allParameters.size - sourceParams.regularParamTypes.size)
    val (declaredParams, expandedParams) = regularParams.splitAt(sourceParams.declaredParamTypes.size)
    BinaryParams(capturedParams, declaredParams, expandedParams, method.returnType)

  private def expandContextFunctions(tpe: Type, acc: List[Type]): (List[Type], Type) =
    tpe.safeDealias match
      case Some(tpe: AppliedType) if tpe.tycon.isContextFunction =>
        val argsAsTypes = tpe.args.map(_.highIfWildcard)
        expandContextFunctions(argsAsTypes.last, acc ::: argsAsTypes.init)
      case _ => (acc, tpe)

  private lazy val scalaPrimitivesToJava: Map[ClassSymbol, String] = Map(
    defn.BooleanClass -> "boolean",
    defn.ByteClass -> "byte",
    defn.CharClass -> "char",
    defn.DoubleClass -> "double",
    defn.FloatClass -> "float",
    defn.IntClass -> "int",
    defn.LongClass -> "long",
    defn.ShortClass -> "short",
    defn.UnitClass -> "void",
    defn.NullClass -> "scala.runtime.Null$"
  )

  private def matchSetterArgType(scalaVarType: TypeOrMethodic, javaSetterParamType: binary.Type): Boolean =
    scalaVarType match
      case scalaVarType: Type =>
        scalaVarType.erasedAsArgType(asJavaVarargs = false).exists(matchType(_, javaSetterParamType))
      case _: MethodicType => false

  private def matchArgType(scalaType: Type, javaType: binary.Type, asJavaVarargs: Boolean): Boolean =
    scalaType.erasedAsArgType(asJavaVarargs).exists(matchType(_, javaType))

  private def matchReturnType(scalaType: TermType, javaType: Option[binary.Type]): Boolean =
    scalaType match
      case scalaType: Type => javaType.forall(jt => scalaType.erasedAsReturnType.exists(matchType(_, jt)))
      case _: MethodicType | _: PackageRef => false

  private lazy val dollarDigitsMaybeDollarAtEndRegex = "\\$\\d+\\$?$".r

  private def matchType(
      scalaType: ErasedTypeRef,
      javaType: binary.Type
  ): Boolean =
    def rec(scalaType: ErasedTypeRef, javaType: String): Boolean =
      scalaType match
        case ErasedTypeRef.ArrayTypeRef(base, dimensions) =>
          javaType.endsWith("[]" * dimensions) &&
          rec(base, javaType.dropRight(2 * dimensions))
        case ErasedTypeRef.ClassRef(scalaClass) =>
          scalaPrimitivesToJava.get(scalaClass) match
            case Some(javaPrimitive) => javaPrimitive == javaType
            case None => matchClassType(scalaClass, javaType, nested = false)
    rec(scalaType, javaType.name)

  private def matchClassType(scalaClass: ClassSymbol, javaType: String, nested: Boolean): Boolean =
    def encodedName(nested: Boolean): String = scalaClass.name match
      case ObjectClassTypeName(underlying) if nested => NameTransformer.encode(underlying.toString())
      case name => NameTransformer.encode(name.toString())
    scalaClass.owner match
      case owner: PackageSymbol =>
        javaType == owner.fullName.toString() + "." + encodedName(nested)
      case owner: ClassSymbol =>
        val encodedName1 = encodedName(nested)
        javaType.endsWith("$" + encodedName1) &&
        matchClassType(owner, javaType.dropRight(1 + encodedName1.length()), nested = true)
      case owner: TermOrTypeSymbol =>
        dollarDigitsMaybeDollarAtEndRegex.findFirstIn(javaType).exists { suffix =>
          val prefix = javaType.stripSuffix(suffix)
          val encodedName1 = encodedName(nested = true)
          prefix.endsWith("$" + encodedName1) && {
            val ownerJavaType = prefix.dropRight(1 + encodedName1.length())
            enclosingClassOwners(owner).exists(matchClassType(_, ownerJavaType, nested = true))
          }
        }

  private def enclosingClassOwners(sym: TermOrTypeSymbol): List[ClassSymbol] =
    sym.owner match
      case owner: ClassSymbol => owner :: enclosingClassOwners(owner)
      case owner: TermOrTypeSymbol => enclosingClassOwners(owner)
      case owner: PackageSymbol => Nil
