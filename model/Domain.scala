package model

/** Binary file information */
case class BinaryInfo(
    oldPath: os.Path,
    newPath: os.Path,
    oldFileInfo: String,
    newFileInfo: String
)

/** Result of bit-level comparison */
case class BitEquivalenceResult(
    identical: Boolean,
    oldHash: String,
    newHash: String,
    oldSize: Long,
    newSize: Long
)

/** Symbol binding type */
enum SymbolBinding:
  case Global
  case Local
  case Weak
  case Unknown

  override def toString: String = this match
    case Global  => "global"
    case Local   => "local"
    case Weak    => "weak"
    case Unknown => "unknown"

object SymbolBinding:
  def fromNmFlag(flag: Char): SymbolBinding = flag match
    case c if c.isUpper => Global
    case 'w' | 'v'      => Weak
    case c if c.isLower => Local
    case _              => Unknown

/** Symbol type/kind */
enum SymbolKind:
  case Function    // T/t - text (code) section
  case Data        // D/d - initialized data section
  case BSS         // B/b - uninitialized data section
  case ReadOnly    // R/r - read-only data section
  case Common      // C - common symbol
  case Undefined   // U - undefined symbol
  case Weak        // W/w - weak symbol
  case Absolute    // A/a - absolute symbol
  case Other

  override def toString: String = this match
    case Function  => "function"
    case Data      => "data"
    case BSS       => "bss"
    case ReadOnly  => "readonly"
    case Common    => "common"
    case Undefined => "undefined"
    case Weak      => "weak"
    case Absolute  => "absolute"
    case Other     => "other"

object SymbolKind:
  def fromNmType(typeChar: Char): SymbolKind = typeChar.toUpper match
    case 'T' => Function
    case 'D' => Data
    case 'B' => BSS
    case 'R' => ReadOnly
    case 'C' => Common
    case 'U' => Undefined
    case 'W' | 'V' => Weak
    case 'A' => Absolute
    case _   => Other

/** A symbol from the binary's symbol table */
case class Symbol(
    name: String,
    kind: SymbolKind,
    binding: SymbolBinding,
    address: Option[Long],
    size: Option[Long],
    section: Option[String]
):
  def isFunction: Boolean = kind == SymbolKind.Function
  def isData: Boolean = kind == SymbolKind.Data || kind == SymbolKind.BSS || kind == SymbolKind.ReadOnly

/** Change in a symbol between old and new binaries */
case class SymbolChange(
    name: String,
    oldSymbol: Symbol,
    newSymbol: Symbol,
    changes: List[String] // Description of what changed
)

/** Result of symbol comparison */
case class SymbolDiff(
    oldTotal: Int,
    newTotal: Int,
    added: List[Symbol],
    removed: List[Symbol],
    changed: List[SymbolChange],
    unchanged: List[Symbol]
):
  def hasChanges: Boolean = added.nonEmpty || removed.nonEmpty || changed.nonEmpty

/** A single disassembled instruction */
case class Instruction(
    address: Long,
    bytes: String,
    mnemonic: String,
    operands: String,
    rawLine: String
):
  /** Normalized representation for comparison (ignores addresses and raw bytes) */
  def normalized: String = s"$mnemonic $operands".trim

  /** Whether this is a control flow instruction */
  def isControlFlow: Boolean =
    val cf = Set(
      "jmp", "je", "jne", "jz", "jnz", "jg", "jge", "jl", "jle",
      "ja", "jae", "jb", "jbe", "jo", "jno", "js", "jns",
      "call", "ret", "retq", "retn",
      "b", "bl", "blr", "br", "beq", "bne", "blt", "ble", "bgt", "bge",
      "cbz", "cbnz", "tbz", "tbnz"
    )
    cf.contains(mnemonic.toLowerCase)

  def isCall: Boolean =
    val calls = Set("call", "callq", "bl", "blr")
    calls.contains(mnemonic.toLowerCase)

  def isBranch: Boolean = isControlFlow && !isCall && mnemonic.toLowerCase != "ret"

/** A disassembled function */
case class DisassembledFunction(
    name: String,
    startAddress: Long,
    instructions: List[Instruction]
):
  /** Normalized instruction sequence for comparison */
  def normalizedInstructions: List[String] = instructions.map(_.normalized)

  /** Hash of normalized instructions for quick comparison */
  def bodyHash: String =
    val content = normalizedInstructions.mkString("\n")
    java.security.MessageDigest
      .getInstance("SHA-256")
      .digest(content.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString

/** Diff between two instruction sequences */
case class InstructionDiff(
    added: List[Instruction],
    removed: List[Instruction],
    commonCount: Int,
    changedControlFlow: Boolean,
    changedCalls: Boolean
):
  def changedCount: Int = added.size + removed.size

/** Status of a function comparison */
enum FunctionStatus:
  case Identical
  case Modified(diff: InstructionDiff)
  case OnlyInOld
  case OnlyInNew

/** Result of comparing a single function */
case class FunctionComparisonResult(
    name: String,
    status: FunctionStatus,
    oldInstructionCount: Option[Int],
    newInstructionCount: Option[Int]
)

/** Aggregated function comparison results */
case class FunctionDiff(
    identical: List[FunctionComparisonResult],
    modified: List[FunctionComparisonResult],
    added: List[FunctionComparisonResult],
    removed: List[FunctionComparisonResult]
):
  def totalOld: Int = identical.size + modified.size + removed.size
  def totalNew: Int = identical.size + modified.size + added.size

  def identicalPercent: Double =
    if totalOld == 0 then 100.0
    else (identical.size.toDouble / totalOld) * 100

/** Overall comparison result */
case class ComparisonResult(
    binaryInfo: BinaryInfo,
    bitEquivalence: BitEquivalenceResult,
    symbolDiff: Option[SymbolDiff],
    functionDiff: Option[FunctionDiff],
    stringDiff: Option[analysis.StringDiff] = None
):
  def isIdentical: Boolean = bitEquivalence.identical

  def severity: ComparisonSeverity =
    if bitEquivalence.identical then ComparisonSeverity.Identical
    else
      val hasSymbolChanges = symbolDiff.exists(_.hasChanges)
      val modifiedFunctions = functionDiff.map(_.modified.size).getOrElse(0)
      val addedFunctions = functionDiff.map(_.added.size).getOrElse(0)
      val removedFunctions = functionDiff.map(_.removed.size).getOrElse(0)
      val hasStringChanges = stringDiff.exists(_.hasChanges)

      if removedFunctions > 0 then ComparisonSeverity.High
      else if addedFunctions > 5 || modifiedFunctions > 10 then ComparisonSeverity.Medium
      else if hasStringChanges then ComparisonSeverity.Low
      else ComparisonSeverity.Low

/** Severity classification of differences */
enum ComparisonSeverity:
  case Identical
  case Low
  case Medium
  case High

  override def toString: String = this match
    case Identical => "IDENTICAL"
    case Low       => "LOW"
    case Medium    => "MEDIUM"
    case High      => "HIGH"

