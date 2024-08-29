package ch.epfl.scala.decoder.binary

trait Parameter extends Variable:
  def `type`: Type

  private[decoder] def isThisParam: Boolean = name == "$this"
  private[decoder] def isOuterParam: Boolean = name == "$outer"
  private[decoder] def isCapturedParam: Boolean = !name.matches("_\\$\\d+") && name.matches(".+\\$\\d+")
  private[decoder] def isUnknownJavaParam: Boolean = name.matches("arg\\d+")
  private[decoder] def isJavaLangEnumParam: Boolean = name == "_$name" || name == "_$ordinal"
  private[decoder] def isGeneratedParam: Boolean =
    isCapturedParam || isOuterParam || isThisParam || isUnknownJavaParam || isJavaLangEnumParam
