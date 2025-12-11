package analysis

import model.*
import process.*

/** Extracted strings from a binary */
case class ExtractedStrings(
    strings: List[String],
    count: Int
):
  def asSet: Set[String] = strings.toSet

/** Result of comparing strings between two binaries */
case class StringDiff(
    oldStrings: ExtractedStrings,
    newStrings: ExtractedStrings,
    added: List[String],
    removed: List[String],
    common: List[String]
):
  def hasChanges: Boolean = added.nonEmpty || removed.nonEmpty

/** Extracts and compares string literals from binaries */
class StringExtractor(runner: ProcessRunner):

  /** Extract printable strings from a binary using `strings` command */
  def extract(path: os.Path, minLength: Int = 4): Either[String, ExtractedStrings] =
    // Use strings command with minimum length filter
    val result = runner.run("strings", "-n", minLength.toString, path.toString)

    result match
      case ProcessResult.Success(stdout, _) =>
        val strings = stdout
          .linesIterator
          .map(_.trim)
          .filter(_.nonEmpty)
          .toList
        Right(ExtractedStrings(strings, strings.size))

      case ProcessResult.Failure(code, _, stderr) =>
        Left(s"strings command failed (exit $code): $stderr")

  /** Compare strings between two binaries */
  def compare(oldPath: os.Path, newPath: os.Path, minLength: Int = 4): Either[String, StringDiff] =
    for
      oldStrings <- extract(oldPath, minLength)
      newStrings <- extract(newPath, minLength)
    yield
      val oldSet = oldStrings.asSet
      val newSet = newStrings.asSet

      val added = (newSet -- oldSet).toList.sorted
      val removed = (oldSet -- newSet).toList.sorted
      val common = (oldSet.intersect(newSet)).toList.sorted

      StringDiff(
        oldStrings = oldStrings,
        newStrings = newStrings,
        added = added,
        removed = removed,
        common = common
      )

object StringExtractor:
  def apply(runner: ProcessRunner): StringExtractor = new StringExtractor(runner)

/** Section data from binary */
case class SectionData(
    name: String,
    size: Long,
    hash: String
)

/** Result of comparing sections between binaries */
case class SectionDiff(
    oldSections: List[SectionData],
    newSections: List[SectionData],
    added: List[SectionData],
    removed: List[SectionData],
    modified: List[(SectionData, SectionData)],  // (old, new) pairs with same name but different content
    unchanged: List[SectionData]
):
  def hasChanges: Boolean = added.nonEmpty || removed.nonEmpty || modified.nonEmpty

/** Extracts and compares binary sections */
class SectionExtractor(runner: ProcessRunner):

  /** Extract section information from a binary */
  def extractSections(path: os.Path): Either[String, List[SectionData]] =
    // Try objdump first, fall back to otool on macOS
    val result = runner.run("objdump", "-h", path.toString)

    result match
      case ProcessResult.Success(stdout, _) =>
        Right(parseObjdumpSections(stdout))

      case ProcessResult.Failure(_, _, _) =>
        // Try otool on macOS
        val otoolResult = runner.run("otool", "-l", path.toString)
        otoolResult match
          case ProcessResult.Success(stdout, _) =>
            Right(parseOtoolSections(stdout, path))
          case ProcessResult.Failure(code, _, stderr) =>
            Left(s"Failed to extract sections: $stderr")

  /** Parse objdump -h output */
  private def parseObjdumpSections(output: String): List[SectionData] =
    // Format: Idx Name          Size      VMA               LMA               File off  Algn
    val sectionRegex = """^\s*\d+\s+(\S+)\s+([0-9a-fA-F]+)\s+""".r

    output.linesIterator.flatMap { line =>
      sectionRegex.findFirstMatchIn(line).map { m =>
        val name = m.group(1)
        val size = java.lang.Long.parseUnsignedLong(m.group(2), 16)
        SectionData(name, size, "") // Hash would require reading actual content
      }
    }.toList

  /** Parse otool -l output for macOS */
  private def parseOtoolSections(output: String, path: os.Path): List[SectionData] =
    // Look for section entries
    val sections = scala.collection.mutable.ListBuffer.empty[SectionData]
    var currentSection: Option[String] = None
    var currentSize: Long = 0

    for line <- output.linesIterator do
      val trimmed = line.trim
      if trimmed.startsWith("sectname") then
        currentSection = Some(trimmed.split("\\s+").last)
      else if trimmed.startsWith("size") && currentSection.isDefined then
        val sizeStr = trimmed.split("\\s+").last
        currentSize = if sizeStr.startsWith("0x") then
          java.lang.Long.parseUnsignedLong(sizeStr.drop(2), 16)
        else
          sizeStr.toLong
        sections += SectionData(currentSection.get, currentSize, "")
        currentSection = None

    sections.toList

  /** Compare sections between two binaries */
  def compare(oldPath: os.Path, newPath: os.Path): Either[String, SectionDiff] =
    for
      oldSections <- extractSections(oldPath)
      newSections <- extractSections(newPath)
    yield
      val oldMap = oldSections.map(s => s.name -> s).toMap
      val newMap = newSections.map(s => s.name -> s).toMap

      val oldNames = oldMap.keySet
      val newNames = newMap.keySet

      val addedNames = newNames -- oldNames
      val removedNames = oldNames -- newNames
      val commonNames = oldNames.intersect(newNames)

      val added = addedNames.toList.sorted.map(newMap)
      val removed = removedNames.toList.sorted.map(oldMap)

      val (unchanged, modified) = commonNames.toList.sorted.partitionMap { name =>
        val oldSec = oldMap(name)
        val newSec = newMap(name)
        if oldSec.size == newSec.size then Left(oldSec)
        else Right((oldSec, newSec))
      }

      SectionDiff(
        oldSections = oldSections,
        newSections = newSections,
        added = added,
        removed = removed,
        modified = modified,
        unchanged = unchanged
      )

object SectionExtractor:
  def apply(runner: ProcessRunner): SectionExtractor = new SectionExtractor(runner)

