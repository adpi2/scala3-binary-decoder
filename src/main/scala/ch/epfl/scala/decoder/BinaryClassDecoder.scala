package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.internal.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*

import scala.util.matching.Regex

trait BinaryClassDecoder(using Context, ThrowOrWarn):
  self: BinaryDecoder =>

  def decode(cls: binary.BinaryClass): DecodedClass =
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

  private def reduceAmbiguityOnClasses(syms: Seq[DecodedClass]): Seq[DecodedClass] =
    if syms.size > 1 then
      val reduced = syms.filterNot(sym => syms.exists(enclose(sym, _)))
      if reduced.size != 0 then reduced else syms
    else syms

  private def decodeLocalClasses(
      javaClass: binary.BinaryClass,
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
        if anonFun.parentClass == Definitions.PartialFunctionClass then
          expectedParents == Set(Definitions.AbstractPartialFunctionClass, Definitions.SerializableClass)
        else expectedParents.contains(anonFun.parentClass)
      case inlined: DecodedClass.InlinedClass => matchParents(inlined.underlying, expectedParents, isInterface)

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

  protected def collectLiftedTrees[S](owner: Symbol, sourceLines: Option[binary.SourceLines])(
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

  private def wrapIfInline(liftedTree: LiftedTree[?], decodedClass: DecodedClass): DecodedClass =
    liftedTree match
      case InlinedFromDef(underlying, inlineCall) =>
        DecodedClass.InlinedClass(wrapIfInline(underlying, decodedClass), inlineCall.callTree)
      case _ => decodedClass

  private def matchLines(liftedFun: LiftedTree[?], sourceLines: binary.SourceLines): Boolean =
    // we use endsWith instead of == because of tasty-query#434
    val positions = liftedFun.positions.filter(pos => pos.sourceFile.name.endsWith(sourceLines.sourceName))
    sourceLines.tastyLines.forall(line => positions.exists(_.containsLine(line)))
