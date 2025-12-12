package report

import model.*
import analysis.*
import process.*

/** ANSI color codes for terminal output */
object Colors:
  val Reset = "\u001b[0m"
  val Bold = "\u001b[1m"
  val Red = "\u001b[31m"
  val Green = "\u001b[32m"
  val Yellow = "\u001b[33m"
  val Blue = "\u001b[34m"
  val Cyan = "\u001b[36m"
  val Gray = "\u001b[90m"

/** Generates human-readable reports from comparison results */
object Reporter:
  import Colors.*

  // Shared runner for demangling
  private val defaultRunner: ProcessRunner = process.DefaultProcessRunner()

  /** Print the full comparison report to stdout */
  def printReport(result: ComparisonResult, verbose: Boolean, runner: ProcessRunner = defaultRunner): Unit =
    // Collect all symbol names for batch demangling
    val allSymbolNames = collectAllSymbolNames(result)
    val demangleMap = Demangler.demangleBatch(allSymbolNames, runner)

    printHeader()
    printBinaryInfo(result.binaryInfo)
    printBitEquivalence(result.bitEquivalence)

    if !result.bitEquivalence.identical then
      result.symbolDiff.foreach(printSymbolDiff(_, verbose, demangleMap))
      result.functionDiff.foreach(printFunctionDiff(_, verbose, demangleMap))
      result.stringDiff.foreach(printStringDiff(_, verbose))

    printSummary(result)

  /** Collect all symbol names from the result for batch demangling */
  private def collectAllSymbolNames(result: ComparisonResult): Seq[String] =
    val symbolNames = result.symbolDiff.toSeq.flatMap { sd =>
      sd.added.map(_.name) ++ sd.removed.map(_.name) ++ sd.changed.map(_.name)
    }
    val functionNames = result.functionDiff.toSeq.flatMap { fd =>
      fd.added.map(_.name) ++ fd.removed.map(_.name) ++ fd.modified.map(_.name)
    }
    (symbolNames ++ functionNames).distinct

  private def printHeader(): Unit =
    println()
    println(s"$Bold$Cyan╔══════════════════════════════════════════════════════════════════╗$Reset")
    println(s"$Bold$Cyan║                    Binary Comparison Report                      ║$Reset")
    println(s"$Bold$Cyan╚══════════════════════════════════════════════════════════════════╝$Reset")
    println()

  private def printBinaryInfo(info: BinaryInfo): Unit =
    println(s"${Bold}Binary Information:$Reset")
    println(s"  Old: ${info.oldPath}")
    println(s"       $Gray${info.oldFileInfo}$Reset")
    println(s"  New: ${info.newPath}")
    println(s"       $Gray${info.newFileInfo}$Reset")
    println()

  private def printBitEquivalence(result: BitEquivalenceResult): Unit =
    println(s"${Bold}Step 1: Bit Equivalence$Reset")
    if result.identical then
      println(s"  ${Green}✓ Binaries are bit-identical$Reset")
      println(s"    Hash: ${result.oldHash}")
      println(s"    Size: ${formatSize(result.oldSize)}")
    else
      println(s"  ${Yellow}✗ Binaries differ at byte level$Reset")
      println(s"    Old: ${result.oldHash} (${formatSize(result.oldSize)})")
      println(s"    New: ${result.newHash} (${formatSize(result.newSize)})")
      if result.oldSize != result.newSize then
        val diff = result.newSize - result.oldSize
        val sign = if diff > 0 then "+" else ""
        println(s"    Size difference: $sign${formatSize(diff)}")
    println()

  private def printSymbolDiff(diff: SymbolDiff, verbose: Boolean, demangleMap: Map[String, String]): Unit =
    val stats = SymbolComparator.stats(diff)
    println(s"${Bold}Step 2: Symbol Comparison$Reset")
    println(s"  Total symbols: ${stats.totalOld} (old) → ${stats.totalNew} (new)")
    println(s"  Unchanged: ${stats.unchanged} (${f"${stats.unchangedPercent}%.1f"}%)")

    if diff.added.nonEmpty then
      println(s"  ${Green}Added: ${diff.added.size}$Reset")
      if verbose then
        diff.added.take(20).foreach { s =>
          val displayName = formatSymbolName(s.name, demangleMap)
          println(s"    $Green+ $displayName$Reset ($Gray${s.kind}$Reset)")
        }
        if diff.added.size > 20 then
          println(s"    $Gray... and ${diff.added.size - 20} more$Reset")

    if diff.removed.nonEmpty then
      println(s"  ${Red}Removed: ${diff.removed.size}$Reset")
      if verbose then
        diff.removed.take(20).foreach { s =>
          val displayName = formatSymbolName(s.name, demangleMap)
          println(s"    $Red- $displayName$Reset ($Gray${s.kind}$Reset)")
        }
        if diff.removed.size > 20 then
          println(s"    $Gray... and ${diff.removed.size - 20} more$Reset")

    if diff.changed.nonEmpty then
      println(s"  ${Yellow}Changed: ${diff.changed.size}$Reset")
      if verbose then
        diff.changed.take(10).foreach { c =>
          val displayName = formatSymbolName(c.name, demangleMap)
          println(s"    $Yellow~ $displayName$Reset")
          c.changes.foreach(change => println(s"      $Gray$change$Reset"))
        }
        if diff.changed.size > 10 then
          println(s"    $Gray... and ${diff.changed.size - 10} more$Reset")

    println()

  private def printFunctionDiff(diff: FunctionDiff, verbose: Boolean, demangleMap: Map[String, String]): Unit =
    val summary = FunctionComparator.summary(diff)
    println(s"${Bold}Step 3: Function Body Comparison$Reset")
    println(s"  Total functions: ${summary.totalOld} (old) → ${summary.totalNew} (new)")
    println(s"  ${Green}Identical: ${summary.identical} (${f"${summary.identicalPercent}%.1f"}%)$Reset")

    if summary.modified > 0 then
      println(s"  ${Yellow}Modified: ${summary.modified}$Reset")
      if summary.modifiedWithControlFlowChanges > 0 then
        println(s"    ${Yellow}⚠ ${summary.modifiedWithControlFlowChanges} with control flow changes$Reset")
      if summary.modifiedWithCallChanges > 0 then
        println(s"    ${Yellow}⚠ ${summary.modifiedWithCallChanges} with call changes$Reset")

      if verbose then
        diff.modified.take(15).foreach { fcr =>
          fcr.status match
            case FunctionStatus.Modified(d) =>
              val oldCount = fcr.oldInstructionCount.getOrElse(0)
              val newCount = fcr.newInstructionCount.getOrElse(0)
              val cfFlag = if d.changedControlFlow then s"${Yellow}[CF]$Reset " else ""
              val callFlag = if d.changedCalls then s"${Yellow}[CALL]$Reset " else ""
              val displayName = formatSymbolName(fcr.name, demangleMap)
              println(s"    $Yellow~ $displayName$Reset $cfFlag$callFlag")
              println(s"      $Gray$oldCount → $newCount instructions, +${d.added.size}/-${d.removed.size}$Reset")
            case _ => ()
        }
        if diff.modified.size > 15 then
          println(s"    $Gray... and ${diff.modified.size - 15} more$Reset")

    if summary.added > 0 then
      println(s"  ${Green}Added: ${summary.added}$Reset")
      if verbose then
        diff.added.take(10).foreach { fcr =>
          val count = fcr.newInstructionCount.getOrElse(0)
          val displayName = formatSymbolName(fcr.name, demangleMap)
          println(s"    $Green+ $displayName$Reset ($Gray$count instructions$Reset)")
        }
        if diff.added.size > 10 then
          println(s"    $Gray... and ${diff.added.size - 10} more$Reset")

    if summary.removed > 0 then
      println(s"  ${Red}Removed: ${summary.removed}$Reset")
      if verbose then
        diff.removed.take(10).foreach { fcr =>
          val count = fcr.oldInstructionCount.getOrElse(0)
          val displayName = formatSymbolName(fcr.name, demangleMap)
          println(s"    $Red- $displayName$Reset ($Gray$count instructions$Reset)")
        }
        if diff.removed.size > 10 then
          println(s"    $Gray... and ${diff.removed.size - 10} more$Reset")

    println()

  private def printStringDiff(diff: StringDiff, verbose: Boolean): Unit =
    println(s"${Bold}Step 4: String Literal Comparison$Reset")
    println(s"  Total strings: ${diff.oldStrings.count} (old) → ${diff.newStrings.count} (new)")
    println(s"  Common: ${diff.common.size}")

    if diff.added.nonEmpty then
      println(s"  ${Green}Added: ${diff.added.size}$Reset")
      if verbose then
        // Filter out binary-looking strings and show meaningful ones
        val meaningfulAdded = diff.added.filter(isMeaningfulString).take(20)
        meaningfulAdded.foreach { s =>
          val display = truncateString(s, 60)
          println(s"    $Green+ \"$display\"$Reset")
        }
        val remaining = diff.added.size - meaningfulAdded.size
        if remaining > 0 then
          println(s"    $Gray... and $remaining more$Reset")

    if diff.removed.nonEmpty then
      println(s"  ${Red}Removed: ${diff.removed.size}$Reset")
      if verbose then
        val meaningfulRemoved = diff.removed.filter(isMeaningfulString).take(20)
        meaningfulRemoved.foreach { s =>
          val display = truncateString(s, 60)
          println(s"    $Red- \"$display\"$Reset")
        }
        val remaining = diff.removed.size - meaningfulRemoved.size
        if remaining > 0 then
          println(s"    $Gray... and $remaining more$Reset")

    println()

  /** Check if a string looks meaningful (not binary garbage) */
  private def isMeaningfulString(s: String): Boolean =
    s.length >= 3 &&
    s.length <= 200 &&
    s.forall(c => c.isLetterOrDigit || c.isWhitespace || ".,;:!?-_+=()[]{}<>/'\"\\@#$%^&*|~`".contains(c))

  /** Truncate string for display */
  private def truncateString(s: String, maxLen: Int): String =
    if s.length <= maxLen then s
    else s.take(maxLen - 3) + "..."

  /** Format a symbol name with demangled version if available */
  private def formatSymbolName(name: String, demangleMap: Map[String, String]): String =
    demangleMap.get(name) match
      case Some(demangled) => s"$name\n        $Gray($demangled)$Reset"
      case None            => name

  private def printSummary(result: ComparisonResult): Unit =
    println(s"$Bold$Cyan╔══════════════════════════════════════════════════════════════════╗$Reset")
    println(s"$Bold$Cyan║                           Summary                                ║$Reset")
    println(s"$Bold$Cyan╚══════════════════════════════════════════════════════════════════╝$Reset")

    val severity = result.severity
    val severityColor = severity match
      case ComparisonSeverity.Identical => Green
      case ComparisonSeverity.Low       => Green
      case ComparisonSeverity.Medium    => Yellow
      case ComparisonSeverity.High      => Red

    println()
    if result.isIdentical then
      println(s"  ${Green}${Bold}Result: BINARIES ARE IDENTICAL$Reset")
    else
      println(s"  ${severityColor}${Bold}Result: BINARIES DIFFER (Severity: $severity)$Reset")

      result.symbolDiff.foreach { sd =>
        val stats = SymbolComparator.stats(sd)
        println(s"  Symbols: ${stats.added} added, ${stats.removed} removed, ${stats.changed} changed")
      }

      result.functionDiff.foreach { fd =>
        val summary = FunctionComparator.summary(fd)
        println(s"  Functions: ${summary.added} added, ${summary.removed} removed, ${summary.modified} modified")
        println(s"  Function identity: ${f"${summary.identicalPercent}%.1f"}% unchanged")
      }

      result.stringDiff.foreach { sd =>
        if sd.hasChanges then
          println(s"  Strings: ${sd.added.size} added, ${sd.removed.size} removed")
      }

    println()

  private def formatSize(bytes: Long): String =
    val absBytes = math.abs(bytes)
    if absBytes < 1024 then s"$bytes B"
    else if absBytes < 1024 * 1024 then f"${bytes / 1024.0}%.1f KB"
    else if absBytes < 1024 * 1024 * 1024 then f"${bytes / (1024.0 * 1024)}%.1f MB"
    else f"${bytes / (1024.0 * 1024 * 1024)}%.1f GB"

  /** Generate JSON output for CI/automation */
  def toJson(result: ComparisonResult): String =
    val sb = new StringBuilder
    sb.append("{\n")
    sb.append(s"""  "identical": ${result.isIdentical},\n""")
    sb.append(s"""  "severity": "${result.severity}",\n""")

    // Bit equivalence
    sb.append(s"""  "bitEquivalence": {\n""")
    sb.append(s"""    "identical": ${result.bitEquivalence.identical},\n""")
    sb.append(s"""    "oldHash": "${result.bitEquivalence.oldHash}",\n""")
    sb.append(s"""    "newHash": "${result.bitEquivalence.newHash}",\n""")
    sb.append(s"""    "oldSize": ${result.bitEquivalence.oldSize},\n""")
    sb.append(s"""    "newSize": ${result.bitEquivalence.newSize}\n""")
    sb.append("  }")

    // Symbol diff
    result.symbolDiff.foreach { sd =>
      val stats = SymbolComparator.stats(sd)
      sb.append(",\n")
      sb.append(s"""  "symbols": {\n""")
      sb.append(s"""    "totalOld": ${stats.totalOld},\n""")
      sb.append(s"""    "totalNew": ${stats.totalNew},\n""")
      sb.append(s"""    "added": ${stats.added},\n""")
      sb.append(s"""    "removed": ${stats.removed},\n""")
      sb.append(s"""    "changed": ${stats.changed},\n""")
      sb.append(s"""    "unchanged": ${stats.unchanged}\n""")
      sb.append("  }")
    }

    // Function diff
    result.functionDiff.foreach { fd =>
      val summary = FunctionComparator.summary(fd)
      sb.append(",\n")
      sb.append(s"""  "functions": {\n""")
      sb.append(s"""    "totalOld": ${summary.totalOld},\n""")
      sb.append(s"""    "totalNew": ${summary.totalNew},\n""")
      sb.append(s"""    "identical": ${summary.identical},\n""")
      sb.append(s"""    "modified": ${summary.modified},\n""")
      sb.append(s"""    "added": ${summary.added},\n""")
      sb.append(s"""    "removed": ${summary.removed},\n""")
      sb.append(s"""    "identicalPercent": ${summary.identicalPercent}\n""")
      sb.append("  }")
    }

    // String diff
    result.stringDiff.foreach { sd =>
      sb.append(",\n")
      sb.append(s"""  "strings": {\n""")
      sb.append(s"""    "totalOld": ${sd.oldStrings.count},\n""")
      sb.append(s"""    "totalNew": ${sd.newStrings.count},\n""")
      sb.append(s"""    "added": ${sd.added.size},\n""")
      sb.append(s"""    "removed": ${sd.removed.size},\n""")
      sb.append(s"""    "common": ${sd.common.size}\n""")
      sb.append("  }")
    }

    sb.append("\n}")
    sb.toString

