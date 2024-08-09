package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.internal.*
import tastyquery.Contexts.*
import tastyquery.SourceLanguage
import tastyquery.Symbols.*

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
      case Patterns.CapturedLzyVariable(name) => decodeCapturedLzyVariable(decodedMethod, name)
      case Patterns.CapturedTailLocalVariable(name) => decodeCapturedVariable(decodedMethod, name)
      case Patterns.CapturedVariable(name) => decodeCapturedVariable(decodedMethod, name)
      case Patterns.This() => decodedMethod.owner.thisType.toSeq.map(DecodedVariable.This(decodedMethod, _))
      case Patterns.DollarThis() => decodeDollarThis(decodedMethod)
      case Patterns.Proxy(name) => decodeProxy(decodedMethod, name)
      case Patterns.InlinedThis() => decodeInlinedThis(decodedMethod, variable)
    }.orTryDecode { case _ =>
      decodedMethod match
        case decodedMethod: DecodedMethod.SAMOrPartialFunctionImpl =>
          decodeValDef(decodedMethod, variable, sourceLine)
            .orIfEmpty(
              decodeSAMOrPartialFun(
                decodedMethod,
                decodedMethod.implementedSymbol,
                variable,
                sourceLine
              )
            )
        case _: DecodedMethod.Bridge => ignore(variable, "Bridge method")
        case _ => decodeValDef(decodedMethod, variable, sourceLine)
    }
    decodedVariables.singleOrThrow(variable, decodedMethod)

  private def decodeCapturedLzyVariable(decodedMethod: DecodedMethod, name: String): Seq[DecodedVariable] =
    decodedMethod match
      case m: DecodedMethod.LazyInit if m.symbol.nameStr == name =>
        Seq(DecodedVariable.CapturedVariable(decodedMethod, m.symbol))
      case m: DecodedMethod.ValOrDefDef if m.symbol.nameStr == name =>
        Seq(DecodedVariable.CapturedVariable(decodedMethod, m.symbol))
      case _ => decodeCapturedVariable(decodedMethod, name)

  private def decodeCapturedVariable(decodedMethod: DecodedMethod, name: String): Seq[DecodedVariable] =
    for
      metTree <- decodedMethod.treeOpt.toSeq
      sym <- CaptureCollector.collectCaptures(metTree)
      if name == sym.nameStr
    yield DecodedVariable.CapturedVariable(decodedMethod, sym)

  private def decodeValDef(
      decodedMethod: DecodedMethod,
      variable: binary.Variable,
      sourceLine: Int
  ): Seq[DecodedVariable] = decodedMethod.symbolOpt match
    case Some(owner: TermSymbol) if owner.sourceLanguage == SourceLanguage.Scala2 =>
      for
        paramSym <- owner.paramSymss.collect { case Left(value) => value }.flatten
        if variable.name == paramSym.nameStr
      yield DecodedVariable.ValDef(decodedMethod, paramSym)
    case _ =>
      for
        tree <- decodedMethod.treeOpt.toSeq
        localVar <- VariableCollector.collectVariables(tree).toSeq
        if variable.name == localVar.sym.nameStr &&
          (decodedMethod.isGenerated || !variable.declaringMethod.sourceLines.exists(x =>
            localVar.sourceFile.name.endsWith(x.sourceName)
          ) || (localVar.startLine <= sourceLine && sourceLine <= localVar.endLine))
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
        localVar <- owner.tree.toSeq.flatMap(t => VariableCollector.collectVariables(t))
        if variable.name == localVar.sym.nameStr
      yield DecodedVariable.ValDef(decodedMethod, localVar.sym.asTerm)

  private def unexpandedSymName(sym: Symbol): String =
    "(.+)\\$\\w+".r.unapplySeq(sym.nameStr).map(xs => xs(0)).getOrElse(sym.nameStr)

  private def decodeProxy(
      decodedMethod: DecodedMethod,
      name: String
  ): Seq[DecodedVariable] =
    for
      metTree <- decodedMethod.treeOpt.toSeq
      localVar <- VariableCollector.collectVariables(metTree)
      if name == localVar.sym.nameStr
    yield DecodedVariable.ValDef(decodedMethod, localVar.sym.asTerm)

  private def decodeInlinedThis(
      decodedMethod: DecodedMethod,
      variable: binary.Variable
  ): Seq[DecodedVariable] =
    val decodedClassSym = variable.`type` match
      case cls: binary.BinaryClass => decode(cls).classSymbol
      case _ => None
    for
      metTree <- decodedMethod.treeOpt.toSeq
      decodedClassSym <- decodedClassSym.toSeq
      if VariableCollector.collectVariables(metTree, sym = decodedMethod.symbolOpt).exists { localVar =>
        localVar.sym == decodedClassSym
      }
    yield DecodedVariable.This(decodedMethod, decodedClassSym.thisType)

  private def decodeDollarThis(
      decodedMethod: DecodedMethod
  ): Seq[DecodedVariable] =
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
