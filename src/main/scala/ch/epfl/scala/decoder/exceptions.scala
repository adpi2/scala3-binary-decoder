package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.binary

case class AmbiguousException(symbol: binary.Symbol, candidates: Seq[DecodedSymbol])
    extends Exception(s"Found ${candidates.size} matching symbols for ${symbol.name}")

case class NotFoundException(symbol: binary.Symbol, decodedOwner: Option[DecodedSymbol])
    extends Exception(s"Cannot find binary symbol of $symbol")

case class IgnoredException(symbol: binary.Symbol, reason: String)
    extends Exception(s"Ignored $symbol because: $reason")

case class UnexpectedException(message: String) extends Exception(message)

inline def notFound(symbol: binary.Symbol, decodedOwner: Option[DecodedSymbol] = None): Nothing =
  throw new NotFoundException(symbol, decodedOwner)

inline def ambiguous(symbol: binary.Symbol, candidates: Seq[DecodedSymbol]): Nothing =
  throw new AmbiguousException(symbol, candidates)

inline def ignore(symbol: binary.Symbol, reason: String): Nothing = throw new IgnoredException(symbol, reason)

inline def unexpected(message: String): Nothing = throw new UnexpectedException(message)
