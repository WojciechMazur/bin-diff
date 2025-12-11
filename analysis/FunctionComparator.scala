package analysis

import model.*

/** Compares function bodies between two binaries */
object FunctionComparator:

  /** Compare disassembled functions from two binaries
    * @param oldFunctions disassembled functions from the old binary
    * @param newFunctions disassembled functions from the new binary
    * @param focusPrefix optional prefix to filter functions (include only matching)
    * @param shouldIgnore predicate to exclude functions from comparison
    */
  def compare(
      oldFunctions: Map[String, DisassembledFunction],
      newFunctions: Map[String, DisassembledFunction],
      focusPrefix: Option[String] = None,
      shouldIgnore: String => Boolean = _ => false
  ): FunctionDiff =
    // Apply focus filter if specified
    val focusedOld = focusPrefix match
      case Some(prefix) => oldFunctions.filter(_._1.contains(prefix))
      case None         => oldFunctions

    val focusedNew = focusPrefix match
      case Some(prefix) => newFunctions.filter(_._1.contains(prefix))
      case None         => newFunctions

    // Apply ignore filter
    val filteredOld = focusedOld.filterNot((name, _) => shouldIgnore(name))
    val filteredNew = focusedNew.filterNot((name, _) => shouldIgnore(name))

    val oldNames = filteredOld.keySet
    val newNames = filteredNew.keySet

    val addedNames = newNames -- oldNames
    val removedNames = oldNames -- newNames
    val commonNames = oldNames.intersect(newNames)

    // Functions only in new
    val added = addedNames.toList.sorted.map { name =>
      val fn = filteredNew(name)
      FunctionComparisonResult(
        name = name,
        status = FunctionStatus.OnlyInNew,
        oldInstructionCount = None,
        newInstructionCount = Some(fn.instructions.size)
      )
    }

    // Functions only in old
    val removed = removedNames.toList.sorted.map { name =>
      val fn = filteredOld(name)
      FunctionComparisonResult(
        name = name,
        status = FunctionStatus.OnlyInOld,
        oldInstructionCount = Some(fn.instructions.size),
        newInstructionCount = None
      )
    }

    // Compare common functions
    val (identical, modified) = commonNames.toList.sorted.partitionMap { name =>
      val oldFn = filteredOld(name)
      val newFn = filteredNew(name)
      compareFunctionBodies(oldFn, newFn)
    }

    FunctionDiff(
      identical = identical,
      modified = modified,
      added = added,
      removed = removed
    )

  /** Compute hash of address-normalized instructions */
  private def normalizedBodyHash(fn: DisassembledFunction): String =
    val content = fn.instructions.map(i => FunctionDiffer.normalizeForComparison(i)).mkString("\n")
    java.security.MessageDigest
      .getInstance("SHA-256")
      .digest(content.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString

  /** Compare two function bodies and return Left for identical, Right for modified */
  private def compareFunctionBodies(
      oldFn: DisassembledFunction,
      newFn: DisassembledFunction
  ): Either[FunctionComparisonResult, FunctionComparisonResult] =
    // Use address-normalized instructions for comparison
    val oldNormalized = oldFn.instructions.map(i => FunctionDiffer.normalizeForComparison(i))
    val newNormalized = newFn.instructions.map(i => FunctionDiffer.normalizeForComparison(i))

    // Quick hash comparison first (using address-normalized hash)
    val oldHash = normalizedBodyHash(oldFn)
    val newHash = normalizedBodyHash(newFn)

    if oldHash == newHash then
      Left(
        FunctionComparisonResult(
          name = oldFn.name,
          status = FunctionStatus.Identical,
          oldInstructionCount = Some(oldFn.instructions.size),
          newInstructionCount = Some(newFn.instructions.size)
        )
      )
    else
      // Detailed diff using LCS-based algorithm
      val diff = computeInstructionDiff(oldFn.instructions, newFn.instructions)
      Right(
        FunctionComparisonResult(
          name = oldFn.name,
          status = FunctionStatus.Modified(diff),
          oldInstructionCount = Some(oldFn.instructions.size),
          newInstructionCount = Some(newFn.instructions.size)
        )
      )

  /** Compute diff between two instruction sequences using a simple LCS-based approach */
  private def computeInstructionDiff(
      oldInstructions: List[Instruction],
      newInstructions: List[Instruction]
  ): InstructionDiff =
    // Use address-normalized instructions for comparison
    val oldNormalized = oldInstructions.map(i => FunctionDiffer.normalizeForComparison(i))
    val newNormalized = newInstructions.map(i => FunctionDiffer.normalizeForComparison(i))

    // Find common elements using sets (simple approach)
    val oldSet = oldNormalized.toSet
    val newSet = newNormalized.toSet

    val commonCount = oldSet.intersect(newSet).size

    // Instructions only in old (removed)
    val removedNormalized = oldNormalized.filterNot(newSet.contains).toSet
    val removed = oldInstructions.filter(i => removedNormalized.contains(FunctionDiffer.normalizeForComparison(i)))

    // Instructions only in new (added)
    val addedNormalized = newNormalized.filterNot(oldSet.contains).toSet
    val added = newInstructions.filter(i => addedNormalized.contains(FunctionDiffer.normalizeForComparison(i)))

    // Check if control flow or calls changed
    val changedControlFlow = removed.exists(_.isControlFlow) || added.exists(_.isControlFlow)
    val changedCalls = removed.exists(_.isCall) || added.exists(_.isCall)

    InstructionDiff(
      added = added,
      removed = removed,
      commonCount = commonCount,
      changedControlFlow = changedControlFlow,
      changedCalls = changedCalls
    )

  /** Get a summary of function changes */
  def summary(diff: FunctionDiff): FunctionDiffSummary =
    val modifiedWithControlFlowChanges = diff.modified.count {
      case FunctionComparisonResult(_, FunctionStatus.Modified(d), _, _) => d.changedControlFlow
      case _ => false
    }

    val modifiedWithCallChanges = diff.modified.count {
      case FunctionComparisonResult(_, FunctionStatus.Modified(d), _, _) => d.changedCalls
      case _ => false
    }

    FunctionDiffSummary(
      totalOld = diff.totalOld,
      totalNew = diff.totalNew,
      identical = diff.identical.size,
      modified = diff.modified.size,
      added = diff.added.size,
      removed = diff.removed.size,
      identicalPercent = diff.identicalPercent,
      modifiedWithControlFlowChanges = modifiedWithControlFlowChanges,
      modifiedWithCallChanges = modifiedWithCallChanges
    )

/** Summary statistics for function comparison */
case class FunctionDiffSummary(
    totalOld: Int,
    totalNew: Int,
    identical: Int,
    modified: Int,
    added: Int,
    removed: Int,
    identicalPercent: Double,
    modifiedWithControlFlowChanges: Int,
    modifiedWithCallChanges: Int
)

