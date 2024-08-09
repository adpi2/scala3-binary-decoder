package ch.epfl.scala.decoder.binary

final case class SourceLines(sourceName: String, lines: Seq[Int]):
  def span: Seq[Int] =
    if lines.size > 2 then Seq(lines.head, lines.last)
    else lines

  def showSpan: String = span.mkString("(", ", ", ")")

  def last: SourceLines = copy(lines = lines.lastOption.toSeq)
  def isEmpty: Boolean = lines.isEmpty

  private[decoder] def tastyLines = lines.map(_ - 1)
  private[decoder] def tastySpan: Seq[Int] = span.map(_ - 1)

object SourceLines:
  def apply(sourceName: String, lines: Seq[Int]): SourceLines =
    new SourceLines(sourceName, lines.distinct.sorted)
