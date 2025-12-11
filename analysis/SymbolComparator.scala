package analysis

import model.*

/** Compares symbol tables between two binaries */
object SymbolComparator:

  /** Compare two lists of symbols and produce a diff
    * @param oldSymbols symbols from the old binary
    * @param newSymbols symbols from the new binary
    * @param focusPrefix optional prefix to filter symbols (include only matching)
    * @param shouldIgnore predicate to exclude symbols from comparison
    */
  def compare(
      oldSymbols: List[Symbol],
      newSymbols: List[Symbol],
      focusPrefix: Option[String] = None,
      shouldIgnore: String => Boolean = _ => false
  ): SymbolDiff =
    // Apply focus filter if specified
    val focusedOld = focusPrefix match
      case Some(prefix) => oldSymbols.filter(_.name.contains(prefix))
      case None         => oldSymbols

    val focusedNew = focusPrefix match
      case Some(prefix) => newSymbols.filter(_.name.contains(prefix))
      case None         => newSymbols

    // Apply ignore filter
    val filteredOld = focusedOld.filterNot(s => shouldIgnore(s.name))
    val filteredNew = focusedNew.filterNot(s => shouldIgnore(s.name))

    // Build maps for quick lookup
    val oldMap = filteredOld.groupBy(_.name).view.mapValues(_.head).toMap
    val newMap = filteredNew.groupBy(_.name).view.mapValues(_.head).toMap

    val oldNames = oldMap.keySet
    val newNames = newMap.keySet

    // Find added/removed symbols
    val addedNames = newNames -- oldNames
    val removedNames = oldNames -- newNames
    val commonNames = oldNames.intersect(newNames)

    val added = addedNames.toList.sorted.map(newMap)
    val removed = removedNames.toList.sorted.map(oldMap)

    // Compare common symbols for changes
    val (unchanged, changed) = commonNames.toList.sorted.partitionMap { name =>
      val oldSym = oldMap(name)
      val newSym = newMap(name)
      compareSymbols(oldSym, newSym) match
        case Nil     => Left(oldSym)  // unchanged
        case changes => Right(SymbolChange(name, oldSym, newSym, changes))
    }

    SymbolDiff(
      oldTotal = filteredOld.size,
      newTotal = filteredNew.size,
      added = added,
      removed = removed,
      changed = changed,
      unchanged = unchanged
    )

  /** Compare two symbols and return list of changes (empty if identical) */
  private def compareSymbols(old: Symbol, newSym: Symbol): List[String] =
    val changes = List.newBuilder[String]

    if old.kind != newSym.kind then
      changes += s"kind: ${old.kind} → ${newSym.kind}"

    if old.binding != newSym.binding then
      changes += s"binding: ${old.binding} → ${newSym.binding}"

    // Size comparison (if both have sizes)
    (old.size, newSym.size) match
      case (Some(oldSize), Some(newSize)) if oldSize != newSize =>
        changes += s"size: $oldSize → $newSize"
      case _ => // no change or no sizes available

    // Section comparison (if both have sections)
    (old.section, newSym.section) match
      case (Some(oldSec), Some(newSec)) if oldSec != newSec =>
        changes += s"section: $oldSec → $newSec"
      case _ => // no change or no sections available

    changes.result()

  /** Get statistics about the diff */
  def stats(diff: SymbolDiff): SymbolDiffStats =
    val functionChanges = diff.changed.count(_.oldSymbol.isFunction)
    val dataChanges = diff.changed.count(_.oldSymbol.isData)

    SymbolDiffStats(
      totalOld = diff.oldTotal,
      totalNew = diff.newTotal,
      added = diff.added.size,
      removed = diff.removed.size,
      changed = diff.changed.size,
      unchanged = diff.unchanged.size,
      addedFunctions = diff.added.count(_.isFunction),
      removedFunctions = diff.removed.count(_.isFunction),
      changedFunctions = functionChanges,
      addedData = diff.added.count(_.isData),
      removedData = diff.removed.count(_.isData),
      changedData = dataChanges
    )

/** Statistics about symbol changes */
case class SymbolDiffStats(
    totalOld: Int,
    totalNew: Int,
    added: Int,
    removed: Int,
    changed: Int,
    unchanged: Int,
    addedFunctions: Int,
    removedFunctions: Int,
    changedFunctions: Int,
    addedData: Int,
    removedData: Int,
    changedData: Int
):
  def unchangedPercent: Double =
    if totalOld == 0 then 100.0
    else (unchanged.toDouble / totalOld) * 100

