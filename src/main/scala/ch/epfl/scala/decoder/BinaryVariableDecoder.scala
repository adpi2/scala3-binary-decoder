package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.internal.*
import tastyquery.Contexts.*
import tastyquery.SourceLanguage
import tastyquery.Symbols.*
import tastyquery.Types.*

trait BinaryVariableDecoder(using Context, ThrowOrWarn):
  self: BinaryClassDecoder & BinaryMethodDecoder =>

  def decode(variable: binary.Variable, sourceLine: Int): DecodedVariable =
    val decodedMethod = decode(variable.declaringMethod)
    decode(decodedMethod, variable, sourceLine)

  def decode(decodedMethod: DecodedMethod, variable: binary.Variable, sourceLine: Int): DecodedVariable =
    def tryDecode(f: PartialFunction[binary.Variable, Seq[DecodedVariable]]): Seq[DecodedVariable] =
      f.applyOrElse(variable, _ => Seq.empty[DecodedVariable])

    extension (xs: Seq[DecodedVariable])
      def orTryDecode(f: PartialFunction[binary.Variable, Seq[DecodedVariable]]): Seq[DecodedVariable] =
        if xs.nonEmpty then xs else f.applyOrElse(variable, _ => Seq.empty[DecodedVariable])
    val decodedVariables = tryDecode {
      case Patterns.CapturedLzyVariable(name) =>
        if variable.declaringMethod.isConstructor then decodeClassCapture(decodedMethod, variable, name)
        else decodeCapturedLzyVariable(decodedMethod, variable, name)
      case Patterns.CapturedTailLocalVariable(name) => decodeMethodCapture(decodedMethod, variable, name)
      case Patterns.AnyValCapture() => decodeAnyValCapture(decodedMethod)
      case Patterns.Capture(name) =>
        if variable.declaringMethod.isConstructor then decodeClassCapture(decodedMethod, variable, name)
        else decodeMethodCapture(decodedMethod, variable, name)
      case Patterns.This() => decodedMethod.owner.thisType.toSeq.map(DecodedVariable.This(decodedMethod, _))
      case Patterns.DollarThis() => decodeDollarThis(decodedMethod)
      case Patterns.Proxy(name) => decodeProxy(decodedMethod, name)
      case Patterns.InlinedThis() => decodeInlinedThis(decodedMethod, variable)
      case Patterns.Outer() => decodeOuterParam(decodedMethod)
    }.orTryDecode { case _ =>
      decodedMethod match
        case decodedMethod: DecodedMethod.SAMOrPartialFunctionImpl =>
          decodeValDef(decodedMethod, variable, sourceLine)
            .orIfEmpty(decodeSAMOrPartialFun(decodedMethod, decodedMethod.implementedSymbol, variable, sourceLine))
        case _: DecodedMethod.Bridge => ignore(variable, "Bridge method")
        case _ =>
          if variable.isParameter then
            decodeParameter(decodedMethod, variable)
              .orIfEmpty(
                decodeValDef(decodedMethod, variable, sourceLine)
              ) // if the method returns a contextual function
          else if variable.declaringMethod.isConstructor then
            decodeValDef(decodedMethod, variable, sourceLine)
              .orIfEmpty(decodeLocalValDefInConstructor(decodedMethod, variable, sourceLine))
          else decodeValDef(decodedMethod, variable, sourceLine)
    }
    decodedVariables.singleOrThrow(variable, decodedMethod)

  private def decodeCapturedLzyVariable(
      decodedMethod: DecodedMethod,
      variable: binary.Variable,
      name: String
  ): Seq[DecodedVariable] =
    decodedMethod match
      case m: DecodedMethod.LazyInit if m.symbol.nameStr == name =>
        Seq(DecodedVariable.CapturedVariable(decodedMethod, m.symbol))
      case m: DecodedMethod.ValOrDefDef if m.symbol.nameStr == name =>
        Seq(DecodedVariable.CapturedVariable(decodedMethod, m.symbol))
      case _ => decodeMethodCapture(decodedMethod, variable, name).filter(_.symbol.isModuleOrLazyVal)

  private def decodeMethodCapture(
      decodedMethod: DecodedMethod,
      variable: binary.Variable,
      name: String
  ): Seq[DecodedVariable.CapturedVariable] =
    for
      metTree <- decodedMethod.treeOpt.toSeq
      sym <- CaptureCollector.collectCaptures(metTree)
      if name == sym.nameStr && matchCaptureType(sym, variable.`type`)
    yield DecodedVariable.CapturedVariable(decodedMethod, sym)

  private def decodeParameter(decodedMethod: DecodedMethod, variable: binary.Variable): Seq[DecodedVariable] =
    (decodedMethod, variable) match
      case (m: DecodedMethod.AdaptedFun, Patterns.V(i)) =>
        for
          owner <- decodedMethod.symbolOpt.toSeq.collect { case sym: TermSymbol => sym }
          if owner.paramSymbols.size > i - 1
        yield DecodedVariable.ValDef(m, owner.paramSymbols(i - 1))
      case (m: DecodedMethod.SetterAccessor, Patterns.XDollar(0)) if !m.symbol.isMethod =>
        Seq(DecodedVariable.SetterParam(m, m.symbol.declaredType.asInstanceOf[Type]))
      case (m: DecodedMethod.SpecializedMethod, Patterns.XDollar(i)) =>
        if m.symbol.paramSymbols.size > i then Seq(DecodedVariable.SpecializedParam(m, m.symbol.paramSymbols(i)))
        else Seq.empty
      case _ =>
        for
          owner <- decodedMethod.symbolOpt.toSeq.collect { case sym: TermSymbol => sym }
          params <- owner.paramSymss.collect { case Left(value) => value }
          sym <- params
          if variable.name == sym.nameStr || matchEmptyName(variable, sym)
        yield DecodedVariable.ValDef(decodedMethod, sym)

  private def matchEmptyName(variable: binary.Variable, sym: TermSymbol): Boolean =
    val EmptyName = "(arg|x\\$)(\\d+)".r
    (variable.name, sym.nameStr) match
      case (EmptyName(_, x), EmptyName(_, y)) => x == y
      case _ => false

  private def decodeValDef(
      decodedMethod: DecodedMethod,
      variable: binary.Variable,
      sourceLine: Int
  ): Seq[DecodedVariable] =
    for
      tree <- decodedMethod.treeOpt.toSeq
      localVar <- VariableCollector.collectVariables(scoper, tree).toSeq
      if variable.name == localVar.sym.nameStr && matchSourceLinesAndType(variable, localVar, sourceLine)
    yield DecodedVariable.ValDef(decodedMethod, localVar.sym.asTerm)

  private def decodeLocalValDefInConstructor(
      decodedMethod: DecodedMethod,
      variable: binary.Variable,
      sourceLine: Int
  ): Seq[DecodedVariable] =
    for
      tree <- decodedMethod.owner.treeOpt.toSeq
      localVar <- VariableCollector.collectVariables(scoper, tree).toSeq
      if variable.name == localVar.sym.nameStr && matchSourceLinesAndType(variable, localVar, sourceLine)
    yield DecodedVariable.ValDef(decodedMethod, localVar.sym.asTerm)

  private def decodeSAMOrPartialFun(
      decodedMethod: DecodedMethod,
      owner: TermSymbol,
      variable: binary.Variable,
      sourceLine: Int
  ): Seq[DecodedVariable] =
    if owner.sourceLanguage == SourceLanguage.Scala2 then
      val x =
        for
          paramSym <- owner.paramSymss.collect { case Left(value) => value }.flatten
          if variable.name == paramSym.nameStr
        yield DecodedVariable.ValDef(decodedMethod, paramSym)
      x.toSeq
    else
      for
        localVar <- owner.tree.toSeq.flatMap(t => VariableCollector.collectVariables(scoper, t))
        if variable.name == localVar.sym.nameStr
      yield DecodedVariable.ValDef(decodedMethod, localVar.sym.asTerm)

  private def decodeProxy(decodedMethod: DecodedMethod, name: String): Seq[DecodedVariable] =
    for
      metTree <- decodedMethod.treeOpt.toSeq
      localVar <- VariableCollector.collectVariables(scoper, metTree)
      if name == localVar.sym.nameStr
    yield DecodedVariable.ValDef(decodedMethod, localVar.sym.asTerm)

  private def decodeInlinedThis(decodedMethod: DecodedMethod, variable: binary.Variable): Seq[DecodedVariable] =
    val decodedClassSym = variable.`type` match
      case cls: binary.BinaryClass => decode(cls).classSymbol
      case _ => None
    for
      metTree <- decodedMethod.treeOpt.toSeq
      decodedClassSym <- decodedClassSym.toSeq
      if VariableCollector.collectVariables(scoper, metTree, sym = decodedMethod.symbolOpt).exists { localVar =>
        localVar.sym == decodedClassSym
      }
    yield DecodedVariable.This(decodedMethod, decodedClassSym.thisType)

  private def decodeOuterParam(decodedMethod: DecodedMethod): Seq[DecodedVariable] =
    decodedMethod.owner.symbolOpt
      .flatMap(_.outerClass)
      .map(outerClass => DecodedVariable.OuterParam(decodedMethod, outerClass.selfType))
      .toSeq

  private def decodeClassCapture(
      decodedMethod: DecodedMethod,
      variable: binary.Variable,
      name: String
  ): Seq[DecodedVariable] =
    decodedMethod.owner.treeOpt.toSeq
      .flatMap(CaptureCollector.collectCaptures)
      .filter(sym => name == sym.nameStr && matchCaptureType(sym, variable.`type`))
      .map(DecodedVariable.CapturedVariable(decodedMethod, _))

  private def decodeDollarThis(decodedMethod: DecodedMethod): Seq[DecodedVariable] =
    decodedMethod match
      case _: DecodedMethod.TraitStaticForwarder =>
        decodedMethod.owner.thisType.toSeq.map(DecodedVariable.This(decodedMethod, _))
      case _ =>
        for
          classSym <- decodedMethod.owner.companionClassSymbol.toSeq
          if classSym.isSubClass(defn.AnyValClass)
          sym <- classSym.declarations.collect {
            case sym: TermSymbol if sym.isVal && !sym.isMethod => sym
          }
        yield DecodedVariable.AnyValThis(decodedMethod, sym)

  private def decodeAnyValCapture(decodedMethod: DecodedMethod): Seq[DecodedVariable] =
    for
      classSym <- decodedMethod.owner.companionClassSymbol.toSeq
      if classSym.isSubClass(defn.AnyValClass)
      sym <- classSym.declarations.collect {
        case sym: TermSymbol if sym.isVal && !sym.isMethod => sym
      }
    yield DecodedVariable.CapturedVariable(decodedMethod, sym)

  private def matchCaptureType(sym: TermSymbol, binaryTpe: binary.Type): Boolean =
    if sym.isModuleOrLazyVal then binaryTpe.isLazy
    else if sym.isVar then binaryTpe.isRef
    else matchArgType(sym.declaredType.requireType, binaryTpe, false)

  private def matchSourceLinesAndType(variable: binary.Variable, localVar: LocalVariable, sourceLine: Int): Boolean =
    matchSourceLines(variable, localVar, sourceLine) && matchType(variable.`type`, localVar)

  private def matchSourceLines(variable: binary.Variable, localVar: LocalVariable, sourceLine: Int): Boolean =
    val sourceName = variable.sourceName.getOrElse("")
    // we use endsWith instead of == because of tasty-query#434
    val positions = localVar.positions.filter(pos => pos.sourceFile.name.endsWith(sourceName))
    variable.declaringMethod.isConstructor ||
    positions.isEmpty || // can happen when localVar has been inlined from a transparent inline
    positions.exists(_.containsLine(sourceLine - 1))

  private def matchType(binaryTpe: binary.Type, localVar: LocalVariable): Boolean =
    localVar.sym match
      case sym: ClassSymbol => matchArgType(localVar.tpe, binaryTpe, false)
      case sym: TermSymbol =>
        if sym.isModuleOrLazyVal then binaryTpe.isLazy
        else (sym.isVar && binaryTpe.isRef) || matchArgType(localVar.tpe, binaryTpe, false)
      case _ => false
