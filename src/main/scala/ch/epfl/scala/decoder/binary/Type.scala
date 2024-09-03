package ch.epfl.scala.decoder.binary

trait Type extends Symbol:
  private[decoder] def isLazy: Boolean =
    "scala\\.runtime\\.Lazy(Boolean|Byte|Char|Short|Int|Long|Float|Double|Unit|Ref)".r.matches(name)
  private[decoder] def isRef: Boolean =
    "scala\\.runtime\\.(Volatile)?(Boolean|Byte|Char|Short|Int|Long|Float|Double|Object)Ref".r.matches(name)
