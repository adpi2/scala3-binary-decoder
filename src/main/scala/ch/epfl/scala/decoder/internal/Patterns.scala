package ch.epfl.scala.decoder.internal

import ch.epfl.scala.decoder.binary
import scala.util.matching.Regex

object Patterns:
  object LocalClass:
    def unapply(cls: binary.ClassType): Option[(String, String, Option[String])] =
      val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
      unapply(decodedClassName)

    def unapply(decodedClassName: String): Option[(String, String, Option[String])] =
      """(.+)\$([^$]+)\$\d+(\$.*)?""".r
        .unapplySeq(decodedClassName)
        .filter(xs => xs(1) != "anon")
        .map(xs => (xs(0), xs(1), Option(xs(2)).map(_.stripPrefix("$")).filter(_.nonEmpty)))

  object AnonClass:
    def unapply(cls: binary.ClassType): Option[(String, Option[String])] =
      val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
      unapply(decodedClassName)

    def unapply(decodedClassName: String): Option[(String, Option[String])] =
      """(.+)\$\$anon\$\d+(\$.*)?""".r
        .unapplySeq(decodedClassName)
        .map(xs => (xs(0), Option(xs(1)).map(_.stripPrefix("$")).filter(_.nonEmpty)))

  object InnerClass:
    def unapply(cls: binary.ClassType): Option[String] =
      val decodedClassName = NameTransformer.decode(cls.name.split('.').last)
      "(.+)\\$(.+)".r
        .unapplySeq(decodedClassName)
        .map(_ => decodedClassName)

  object LazyInit:
    def unapply(method: binary.Method): Option[String] =
      if method.isBridge || method.isStatic then None
      else """(.*)\$lzyINIT\d+""".r.unapplySeq(NameTransformer.decode(method.name)).map(xs => xs(0).stripSuffix("$"))

  object TraitLocalStaticForwarder:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if method.isTraitStaticForwarder then method.extractFromDecodedNames("(.+)\\$\\d+\\$".r)(_(0))
      else None

  object TraitStaticForwarder:
    def unapply(method: binary.Method): Boolean = method.isTraitStaticForwarder

  object Outer:
    def unapply(method: binary.Method): Boolean =
      (!method.isBridge && !method.isStatic) &&
        "(.*)\\$\\$\\$outer".r.unapplySeq(NameTransformer.decode(method.name)).isDefined

  object AnonFun:
    def unapply(method: binary.Method): Boolean =
      !method.isBridge && "(.*)\\$anonfun\\$\\d+".r.unapplySeq(NameTransformer.decode(method.name)).isDefined

  object AdaptedAnonFun:
    def unapply(method: binary.Method): Boolean =
      // adapted anon funs can be bridges or static or both
      (method.isBridge || method.isStatic) &&
        """(.*)\$anonfun\$adapted\$\d+""".r.unapplySeq(NameTransformer.decode(method.name)).isDefined

  object LocalMethod:
    def unapply(method: binary.Method): Option[Seq[String]] =
      method match
        case DefaultArg(_) => None
        case m if m.isBridge => None
        case _ => method.extractFromDecodedNames("(.+)\\$\\d+".r)(_(0).stripSuffix("$"))

  object DefaultArg:
    def unapply(method: binary.Method): Option[Seq[String]] =
      method.extractFromDecodedNames("(.+)\\$default\\$\\d+".r)(_(0))

  object LocalLazyInit:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if method.isBridge || !method.allParameters.forall(_.isGenerated) then None
      else method.extractFromDecodedNames("""(.+)\$lzyINIT\d+\$(\d+)""".r)(_(0).stripSuffix("$"))

  object SuperArg:
    def unapply(method: binary.Method): Boolean =
      !method.isBridge && """(.*)\$superArg\$\d+(\$\d+)?""".r.unapplySeq(method.name).isDefined

  object LiftedTree:
    def unapply(method: binary.Method): Boolean =
      val liftedTree = "liftedTree\\d+\\$\\d+".r
      !method.isBridge && liftedTree.unapplySeq(method.name).isDefined

  object TraitInitializer:
    def unapply(method: binary.Method): Boolean =
      method.isTraitInitializer

  object ValueClassExtension:
    def unapply(method: binary.Method): Boolean =
      method.isExtensionMethod

  object DeserializeLambda:
    def unapply(method: binary.Method): Boolean =
      method.isDeserializeLambda

  object ParamForwarder:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if method.isStatic || method.isBridge then None
      else method.extractFromDecodedNames("(.+)\\$accessor".r)(xs => xs(0))

  object TraitSetter:
    def unapply(method: binary.Method): Option[String] =
      if method.isStatic || method.isBridge then None
      else """.+\$_setter_\$(.+\$\$)?(.+)_=""".r.unapplySeq(method.decodedName).map(xs => xs(1))

  object Setter:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if method.isStatic || method.isBridge then None
      else method.extractFromDecodedNames("(.+)_=".r)(_(0))

  object SuperAccessor:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if method.isStatic || method.isBridge then None
      else method.extractFromDecodedNames("super\\$(.+)".r)(_(0))

  object SpecializedMethod:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if method.isStatic then None
      else method.extractFromDecodedNames("(.+)\\$mc.+\\$sp".r)(_(0))

  object ByNameArgProxy:
    def unapply(method: binary.Method): Boolean =
      !method.isBridge && ".+\\$proxy\\d+\\$\\d+".r.unapplySeq(method.name).isDefined

  object InlineAccessor:
    def unapply(method: binary.Method): Option[Seq[String]] =
      if method.isStatic || method.isBridge then None
      else
        method.extractFromDecodedNames("inline\\$(.+)".r) { xs =>
          // strip $i1 suffix if exists
          "(.+)\\$i\\d+".r.unapplySeq(xs(0)).map(_(0)).getOrElse(xs(0))
        }

  extension (method: binary.Method)
    private def extractFromDecodedNames[T](regex: Regex)(extract: List[String] => T): Option[Seq[T]] =
      val extracted = method.unexpandedDecodedNames
        .flatMap(regex.unapplySeq)
        .map(extract)
        .distinct
      if extracted.nonEmpty then Some(extracted) else None
