package analysis

import model.*

/** Represents a line in a unified diff */
enum DiffLine:
  case Context(instruction: Instruction)      // Unchanged line
  case Removed(instruction: Instruction)      // Line only in old
  case Added(instruction: Instruction)        // Line only in new

  def toColoredString(normalizeAddresses: Boolean, usePltSymbols: Boolean = false): String =
    val Colors = report.Colors
    this match
      case Context(i) =>
        val text = if normalizeAddresses then normalizeOperands(i, usePltSymbols) else i.normalized
        s"  ${Colors.Gray}$text${Colors.Reset}"
      case Removed(i) =>
        val text = if normalizeAddresses then normalizeOperands(i, usePltSymbols) else i.normalized
        s"${Colors.Red}- $text${Colors.Reset}"
      case Added(i) =>
        val text = if normalizeAddresses then normalizeOperands(i, usePltSymbols) else i.normalized
        s"${Colors.Green}+ $text${Colors.Reset}"

  private def normalizeOperands(i: Instruction, usePltSymbols: Boolean): String =
    // Use the same normalization as comparison for display consistency
    FunctionDiffer.normalizeForComparison(i, usePltSymbols)

/** Result of computing a detailed function diff */
case class DetailedFunctionDiff(
    functionName: String,
    oldInstructions: List[Instruction],
    newInstructions: List[Instruction],
    diffLines: List[DiffLine],
    stats: DiffStats
)

case class DiffStats(
    totalOld: Int,
    totalNew: Int,
    added: Int,
    removed: Int,
    unchanged: Int
):
  def changedPercent: Double =
    if totalOld == 0 then 0.0
    else ((added + removed).toDouble / (totalOld + totalNew)) * 100

/** Computes detailed diffs between functions using Myers algorithm */
object FunctionDiffer:

  /** Instructions where symbol annotations are just "nearest symbol" noise, not actual targets */
  private val addressLoadingInstructions = Set(
    "adrp", "adr",           // ARM address page/address load
    "ldr", "ldur", "ldp",    // Load instructions (when computing addresses)
    "str", "stur", "stp",    // Store instructions
    "lea"                    // x86 load effective address
  )

  /** Instructions where symbol annotations indicate actual call/branch targets */
  private val controlFlowInstructions = Set(
    "bl", "blr", "b", "br",  // ARM branch instructions
    "call", "jmp", "je", "jne", "jz", "jnz", "jg", "jl", "jge", "jle", // x86 control flow
    "cbz", "cbnz", "tbz", "tbnz", "b.eq", "b.ne", "b.lt", "b.gt", "b.le", "b.ge" // ARM conditional
  )

  /** Symbols that are dynamic linker/PLT artifacts, not actual call targets.
    * When these appear in call annotations, they're just the "nearest symbol"
    * to a PLT stub, not the actual function being called.
    */
  private val pltStubSymbols = Set(
    "dyld_stub_binder",       // macOS lazy binding stub
    "_dyld_stub_binder",
    "__stub_helper",
    "_dl_runtime_resolve",    // Linux glibc lazy resolver
    "_dl_runtime_resolve_xsave",
    "_dl_runtime_resolve_xsavec"
  )

  /** Check if a symbol annotation looks like a PLT/stub artifact */
  private def isPltStubSymbol(operands: String): Boolean =
    pltStubSymbols.exists(stub => operands.contains(s"<$stub>") || operands.contains(s"<_$stub>"))

  /** Normalize an instruction for comparison by removing address-specific values
    * but preserving semantic information like function names and data references.
    *
    * @param i The instruction to normalize
    * @param usePltSymbols If true, keep PLT/stub symbol annotations; if false, filter them as noise
    */
  def normalizeForComparison(i: Instruction, usePltSymbols: Boolean = false): String =
    var operands = i.operands
      // Normalize hex addresses (e.g., 0x100003f40 -> <addr>)
      .replaceAll("0x[0-9a-fA-F]+", "<addr>")
      // Normalize numeric offsets in symbol references (e.g., <_main+0x40> -> <_main>)
      // This keeps the symbol name but removes the offset
      .replaceAll("\\+<addr>>", ">")
      .replaceAll("\\+[0-9]+>", ">")
      // Normalize ARM immediates used as offsets in brackets (e.g., [sp, #16] -> [sp, #<off>])
      .replaceAll("\\[([^,\\]]+),\\s*#-?[0-9]+\\]", "[$1, #<off>]")
      // Normalize standalone ARM immediates NOT in brackets (e.g., #123 -> #<imm>)
      // but only if they look like pure numeric values
      .replaceAll("(^|\\s)#-?[0-9]+($|\\s|;)", "$1#<imm>$2")

    // Only apply PLT/symbol filtering if not showing PLT symbols
    if !usePltSymbols then
      // For address-loading instructions (adrp, ldr, etc.), the symbol annotation
      // is just the "nearest known symbol" - it's NOT the actual target.
      // Different linkers produce different layouts, so this is noise.
      // Strip these annotations entirely for such instructions.
      val mnemonic = i.mnemonic.toLowerCase
      if addressLoadingInstructions.exists(mnemonic.startsWith) then
        // Strip all <symbol> annotations - they're linker layout artifacts
        operands = operands.replaceAll("<[^>]+>", "").trim
      else if mnemonic == "bl" || mnemonic == "call" then
        // For bl/call to an address (not a register), the target goes through PLT
        // and the symbol annotation is just "nearest known symbol" - unreliable.
        // Keep only calls to actual named functions (symbols starting with __ or _Z for C++).
        // External C functions through PLT get inconsistent annotations.
        val symbolMatch = "<([^>]+)>".r.findFirstMatchIn(operands)
        symbolMatch match
          case Some(m) =>
            val symbol = m.group(1)
            // Keep C++ mangled names and local functions (these are real targets)
            // Strip annotations for external C library calls (PLT noise)
            val isRealTarget = symbol.startsWith("__Z") ||  // C++ mangled
                               symbol.startsWith("_Z") ||   // C++ mangled (Linux)
                               symbol.contains("+") ||       // Offset within function (internal jump)
                               symbol.startsWith("__") && !pltStubSymbols.contains(symbol.stripPrefix("_"))
            if !isRealTarget then
              operands = operands.replaceAll("<[^>]+>", "").trim
          case None => ()
      // For branch instructions (b, jmp), keep the target as it's usually internal

    // Clean up any double spaces
    operands = operands.replaceAll("\\s+", " ").trim

    s"${i.mnemonic} $operands".trim

  /** Compute a detailed diff between two functions
    *
    * @param oldFn Old function to compare
    * @param newFn New function to compare
    * @param showPltSymbols If true, keep PLT/stub symbols; if false, filter them
    */
  def diff(
      oldFn: DisassembledFunction,
      newFn: DisassembledFunction,
      showPltSymbols: Boolean = false
  ): DetailedFunctionDiff =
    val oldInstructions = oldFn.instructions
    val newInstructions = newFn.instructions

    // Use address-normalized instructions for comparison
    val oldNorm = oldInstructions.map(i => normalizeForComparison(i, showPltSymbols))
    val newNorm = newInstructions.map(i => normalizeForComparison(i, showPltSymbols))

    // Compute LCS (Longest Common Subsequence)
    val lcs = computeLCS(oldNorm, newNorm)

    // Build diff lines from LCS
    val diffLines = buildDiffLines(oldInstructions, newInstructions, oldNorm, newNorm, lcs)

    // Compute statistics
    val added = diffLines.count(_.isInstanceOf[DiffLine.Added])
    val removed = diffLines.count(_.isInstanceOf[DiffLine.Removed])
    val unchanged = diffLines.count(_.isInstanceOf[DiffLine.Context])

    DetailedFunctionDiff(
      functionName = oldFn.name,
      oldInstructions = oldInstructions,
      newInstructions = newInstructions,
      diffLines = diffLines,
      stats = DiffStats(
        totalOld = oldInstructions.size,
        totalNew = newInstructions.size,
        added = added,
        removed = removed,
        unchanged = unchanged
      )
    )

  /** Compute Longest Common Subsequence using dynamic programming */
  private def computeLCS(old: List[String], newSeq: List[String]): List[String] =
    val m = old.length
    val n = newSeq.length

    // Build LCS table
    val dp = Array.ofDim[Int](m + 1, n + 1)
    for i <- 1 to m do
      for j <- 1 to n do
        if old(i - 1) == newSeq(j - 1) then
          dp(i)(j) = dp(i - 1)(j - 1) + 1
        else
          dp(i)(j) = math.max(dp(i - 1)(j), dp(i)(j - 1))

    // Backtrack to find the LCS
    val lcs = scala.collection.mutable.ListBuffer.empty[String]
    var i = m
    var j = n
    while i > 0 && j > 0 do
      if old(i - 1) == newSeq(j - 1) then
        lcs.prepend(old(i - 1))
        i -= 1
        j -= 1
      else if dp(i - 1)(j) > dp(i)(j - 1) then
        i -= 1
      else
        j -= 1

    lcs.toList

  /** Build diff lines from LCS */
  private def buildDiffLines(
      oldInstr: List[Instruction],
      newInstr: List[Instruction],
      oldNorm: List[String],
      newNorm: List[String],
      lcs: List[String]
  ): List[DiffLine] =
    val result = scala.collection.mutable.ListBuffer.empty[DiffLine]
    var oi = 0 // old index
    var ni = 0 // new index
    var li = 0 // lcs index

    while oi < oldNorm.length || ni < newNorm.length do
      if li < lcs.length && oi < oldNorm.length && ni < newNorm.length &&
         oldNorm(oi) == lcs(li) && newNorm(ni) == lcs(li) then
        // Common line (context)
        result += DiffLine.Context(newInstr(ni))
        oi += 1
        ni += 1
        li += 1
      else if oi < oldNorm.length && (li >= lcs.length || oldNorm(oi) != lcs(li)) then
        // Line only in old (removed)
        result += DiffLine.Removed(oldInstr(oi))
        oi += 1
      else if ni < newNorm.length && (li >= lcs.length || newNorm(ni) != lcs(li)) then
        // Line only in new (added)
        result += DiffLine.Added(newInstr(ni))
        ni += 1

    result.toList

  /** Format diff as a unified diff string with context */
  def formatUnifiedDiff(
      diff: DetailedFunctionDiff,
      contextLines: Int = 3,
      normalizeAddresses: Boolean = true,
      usePltSymbols: Boolean = false
  ): String =
    val sb = new StringBuilder
    val Colors = report.Colors

    // Header
    sb.append(s"${Colors.Bold}${Colors.Cyan}--- old/${diff.functionName}${Colors.Reset}\n")
    sb.append(s"${Colors.Bold}${Colors.Cyan}+++ new/${diff.functionName}${Colors.Reset}\n")
    sb.append("\n")

    // Find hunks (groups of changes with context)
    val hunks = findHunks(diff.diffLines, contextLines)

    for hunk <- hunks do
      // Hunk header
      sb.append(s"${Colors.Cyan}@@ ... @@${Colors.Reset}\n")
      for line <- hunk do
        sb.append(line.toColoredString(normalizeAddresses, usePltSymbols))
        sb.append("\n")
      sb.append("\n")

    // Statistics
    sb.append(s"${Colors.Bold}Statistics:${Colors.Reset}\n")
    sb.append(s"  Old: ${diff.stats.totalOld} instructions\n")
    sb.append(s"  New: ${diff.stats.totalNew} instructions\n")
    sb.append(s"  ${Colors.Red}Removed: ${diff.stats.removed}${Colors.Reset}\n")
    sb.append(s"  ${Colors.Green}Added: ${diff.stats.added}${Colors.Reset}\n")
    sb.append(s"  Unchanged: ${diff.stats.unchanged}\n")

    sb.toString

  /** Group diff lines into hunks with context */
  private def findHunks(lines: List[DiffLine], contextLines: Int): List[List[DiffLine]] =
    if lines.isEmpty then return Nil

    // Find indices of changed lines
    val changedIndices = lines.zipWithIndex.collect {
      case (DiffLine.Added(_), i) => i
      case (DiffLine.Removed(_), i) => i
    }.toSet

    if changedIndices.isEmpty then return Nil

    // Expand to include context
    val includedIndices = changedIndices.flatMap { i =>
      (math.max(0, i - contextLines) to math.min(lines.length - 1, i + contextLines))
    }.toList.sorted.distinct

    // Group into continuous hunks
    val hunks = scala.collection.mutable.ListBuffer.empty[List[DiffLine]]
    var currentHunk = scala.collection.mutable.ListBuffer.empty[DiffLine]
    var lastIndex = -10

    for i <- includedIndices do
      if i - lastIndex > 1 && currentHunk.nonEmpty then
        hunks += currentHunk.toList
        currentHunk = scala.collection.mutable.ListBuffer.empty[DiffLine]
      currentHunk += lines(i)
      lastIndex = i

    if currentHunk.nonEmpty then
      hunks += currentHunk.toList

    hunks.toList

