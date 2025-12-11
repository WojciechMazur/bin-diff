package analysis

import model.*
import process.*

/** Extracts symbols from binaries using the `nm` tool */
class SymbolExtractor(runner: ProcessRunner):

  /** Extract symbols from a binary file */
  def extract(path: os.Path): Either[String, List[Symbol]] =
    // Try nm with different options depending on the platform
    // -p: don't sort (faster)
    // -n: sort by address
    // -g: only external symbols (we want all, so don't use this)
    val result = runner.run("nm", "-p", path.toString)

    result match
      case ProcessResult.Success(stdout, _) =>
        Right(parseNmOutput(stdout))
      case ProcessResult.Failure(code, _, stderr) =>
        // On macOS, nm might fail for certain binaries, try with -j flag
        val fallback = runner.run("nm", "-j", path.toString)
        fallback match
          case ProcessResult.Success(stdout, _) =>
            Right(parseNmOutputNamesOnly(stdout))
          case ProcessResult.Failure(_, _, fallbackErr) =>
            Left(s"nm failed (exit $code): $stderr\nFallback also failed: $fallbackErr")

  /** Parse standard nm output format:
    * Format: <address> <type> <name>
    * or:     <type> <name> (for undefined symbols)
    */
  private def parseNmOutput(output: String): List[Symbol] =
    output
      .linesIterator
      .flatMap(parseLine)
      .toList

  private def parseLine(line: String): Option[Symbol] =
    val trimmed = line.trim
    if trimmed.isEmpty then None
    else
      // Handle different nm output formats
      // Format 1: "0000000100003f40 T _main"
      // Format 2: "                 U _printf"
      // Format 3 (macOS with -n): "0000000100003f40 T _main" or just "T _main"

      val parts = trimmed.split("\\s+").toList

      parts match
        case addr :: typeChar :: name :: Nil if addr.matches("[0-9a-fA-F]+") =>
          // Full format with address
          val address = parseHexAddress(addr)
          val kind = SymbolKind.fromNmType(typeChar.head)
          val binding = SymbolBinding.fromNmFlag(typeChar.head)
          Some(Symbol(name, kind, binding, address, None, None))

        case typeChar :: name :: Nil if typeChar.length == 1 =>
          // Format without address (undefined symbols)
          val kind = SymbolKind.fromNmType(typeChar.head)
          val binding = SymbolBinding.fromNmFlag(typeChar.head)
          Some(Symbol(name, kind, binding, None, None, None))

        case name :: Nil =>
          // Just a name (shouldn't happen normally)
          Some(Symbol(name, SymbolKind.Other, SymbolBinding.Unknown, None, None, None))

        case _ =>
          // Can't parse this line
          None

  /** Parse nm -j output (names only) */
  private def parseNmOutputNamesOnly(output: String): List[Symbol] =
    output
      .linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(name => Symbol(name, SymbolKind.Other, SymbolBinding.Unknown, None, None, None))
      .toList

  private def parseHexAddress(hex: String): Option[Long] =
    try Some(java.lang.Long.parseUnsignedLong(hex, 16))
    catch case _: NumberFormatException => None

object SymbolExtractor:
  def apply(runner: ProcessRunner): SymbolExtractor = new SymbolExtractor(runner)

