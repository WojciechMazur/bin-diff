//> using scala 3.8.0-RC3
//> using toolkit default
//> using dep com.github.scopt::scopt::4.1.1-M3
//> using files model/ process/ analysis/ report/

import model.*
import process.*
import analysis.*
import report.*
import scopt.OParser

/** Configuration for the binary diff tool */
case class Config(
    oldBinary: os.Path = os.pwd,
    newBinary: os.Path = os.pwd,
    verbose: Boolean = false,
    focusPrefix: Option[String] = None,
    ignoreFile: Option[os.Path] = None,
    diffPattern: Option[String] = None,  // Regex pattern for functions to diff
    normalizeAddresses: Boolean = true,
    usePltSymbols: Boolean = false      // Show PLT/stub symbol annotations
)

/** Ignore patterns loaded from an ignore file */
case class IgnorePatterns(patterns: List[String]):
  /** Check if a symbol name matches any ignore pattern */
  def matches(name: String): Boolean =
    patterns.exists(pattern => IgnorePatterns.matchesGlob(pattern, name))

  def isEmpty: Boolean = patterns.isEmpty

object IgnorePatterns:
  val empty: IgnorePatterns = IgnorePatterns(Nil)

  /** Load patterns from a file (one pattern per line, # for comments) */
  def fromFile(path: os.Path): Either[String, IgnorePatterns] =
    if !os.exists(path) then Left(s"Ignore file does not exist: $path")
    else if !os.isFile(path) then Left(s"Ignore file is not a regular file: $path")
    else
      val patterns = os.read.lines(path)
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#"))
        .toList
      Right(IgnorePatterns(patterns))

  /** Match a string against a glob pattern with * wildcard */
  def matchesGlob(pattern: String, str: String): Boolean =
    val regex = pattern
      .replace(".", "\\.")
      .replace("*", ".*")
      .replace("?", ".")
    str.matches(s"^$regex$$")

object Config:
  private val builder = OParser.builder[Config]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("bindiff"),
      head("bindiff", "1.0"),
      help("help").abbr("h").text("Show this help message"),
      opt[String]("old")
        .required()
        .valueName("<path>")
        .action((x, c) => c.copy(oldBinary = os.Path(x, os.pwd)))
        .text("Path to the old/reference binary"),
      opt[String]("new")
        .required()
        .valueName("<path>")
        .action((x, c) => c.copy(newBinary = os.Path(x, os.pwd)))
        .text("Path to the new binary to compare"),
      opt[Unit]("verbose")
        .abbr("v")
        .action((_, c) => c.copy(verbose = true))
        .text("Enable verbose output"),
      opt[String]("focus")
        .valueName("<prefix>")
        .action((x, c) => c.copy(focusPrefix = Some(x)))
        .text("Only show symbols matching this prefix"),
      opt[String]("ignore-file")
        .valueName("<path>")
        .action((x, c) => c.copy(ignoreFile = Some(os.Path(x, os.pwd))))
        .text("File with patterns to ignore (one glob pattern per line, e.g. GCC_except_table*)"),
      opt[String]("diff")
        .valueName("<regex>")
        .action((x, c) => c.copy(diffPattern = Some(x)))
        .text("Show instruction diff for functions matching regex (e.g. '.*' for all, 'main', 'log_.*')"),
      opt[Unit]("raw-addresses")
        .action((_, c) => c.copy(normalizeAddresses = false))
        .text("Show raw addresses in diff (don't normalize)"),
      opt[Boolean]("use-plt-symbols")
        .action((x, c) => c.copy(usePltSymbols = x))
        .text("Show PLT/stub symbol annotations in diffs (normally filtered as linker noise)")
    )

  def parse(args: Array[String]): Option[Config] =
    OParser.parse(parser, args, Config())

/** Validates that required system tools are available */
def validateSystemTools(runner: ProcessRunner): Either[String, Unit] =
  val requiredTools = List("nm", "objdump", "file", "shasum")
  val missing = requiredTools.filterNot: tool =>
    runner.run("which", tool).isSuccess

  if missing.isEmpty then Right(())
  else Left(s"Missing required system tools: ${missing.mkString(", ")}")

/** Validates binary files exist and have compatible architectures */
def validateBinaries(
    oldPath: os.Path,
    newPath: os.Path,
    runner: ProcessRunner
): Either[String, BinaryInfo] =
  def validateFile(path: os.Path, name: String): Either[String, Unit] =
    if !os.exists(path) then Left(s"$name binary does not exist: $path")
    else if !os.isFile(path) then Left(s"$name path is not a regular file: $path")
    else Right(())

  def getFileInfo(path: os.Path): Either[String, String] =
    runner.run("file", "-b", path.toString) match
      case ProcessResult.Success(stdout, _) => Right(stdout.trim)
      case ProcessResult.Failure(code, _, stderr) =>
        Left(s"Failed to get file info for $path: $stderr")

  for
    _ <- validateFile(oldPath, "Old")
    _ <- validateFile(newPath, "New")
    oldInfo <- getFileInfo(oldPath)
    newInfo <- getFileInfo(newPath)
    _ <- validateArchitectureMatch(oldInfo, newInfo, oldPath, newPath)
  yield BinaryInfo(oldPath, newPath, oldInfo, newInfo)

def validateArchitectureMatch(
    oldInfo: String,
    newInfo: String,
    oldPath: os.Path,
    newPath: os.Path
): Either[String, Unit] =
  // Extract architecture from file output (e.g., "Mach-O 64-bit executable arm64")
  def extractArch(info: String): String =
    if info.contains("arm64") then "arm64"
    else if info.contains("x86_64") || info.contains("x86-64") then "x86_64"
    else if info.contains("i386") || info.contains("i686") then "x86"
    else if info.contains("aarch64") then "aarch64"
    else "unknown"

  val oldArch = extractArch(oldInfo)
  val newArch = extractArch(newInfo)

  if oldArch == newArch then Right(())
  else
    Left(
      s"""Architecture mismatch:
         |  Old ($oldPath): $oldArch
         |  New ($newPath): $newArch
         |Cannot compare binaries with different architectures.""".stripMargin
    )

/** Main pipeline orchestrating all comparison steps */
def runComparison(
    config: Config,
    runner: ProcessRunner
): Either[String, ComparisonResult] =
  val binaryInfo = validateBinaries(config.oldBinary, config.newBinary, runner) match
    case Left(err) => return Left(err)
    case Right(bi) => bi

  // Load ignore patterns if specified
  val ignorePatterns = config.ignoreFile match
    case Some(path) =>
      IgnorePatterns.fromFile(path) match
        case Left(err)       => return Left(err)
        case Right(patterns) => patterns
    case None => IgnorePatterns.empty

  if config.verbose then
    println(s"Comparing binaries:")
    println(s"  Old: ${config.oldBinary}")
    println(s"  New: ${config.newBinary}")
    if !ignorePatterns.isEmpty then
      println(s"  Ignore patterns: ${ignorePatterns.patterns.size}")
    println()

  // Step 1: Bit equivalence check
  if config.verbose then println("Step 1: Checking bit equivalence...")
  val bitResult = BitEquivalence.check(config.oldBinary, config.newBinary)

  if bitResult.identical then
    if config.verbose then println(s"  Binaries are bit-identical (hash: ${bitResult.oldHash})")
    return Right(
      ComparisonResult(
        binaryInfo = binaryInfo,
        bitEquivalence = bitResult,
        symbolDiff = None,
        functionDiff = None
      )
    )

  if config.verbose then
    println(s"  Binaries differ at byte level")
    println(s"    Old hash: ${bitResult.oldHash}")
    println(s"    New hash: ${bitResult.newHash}")
    println()

  // Step 2: Symbol comparison
  if config.verbose then println("Step 2: Extracting and comparing symbols...")
  val symbolExtractor = SymbolExtractor(runner)

  val symbolDiff = for
    oldSymbols <- symbolExtractor.extract(config.oldBinary)
    newSymbols <- symbolExtractor.extract(config.newBinary)
  yield SymbolComparator.compare(oldSymbols, newSymbols, config.focusPrefix, ignorePatterns.matches)

  val symbolResult = symbolDiff match
    case Left(err)   => return Left(s"Symbol extraction failed: $err")
    case Right(diff) => diff

  if config.verbose then
    println(s"  Old binary: ${symbolResult.oldTotal} symbols")
    println(s"  New binary: ${symbolResult.newTotal} symbols")
    println(s"  Added: ${symbolResult.added.size}")
    println(s"  Removed: ${symbolResult.removed.size}")
    println(s"  Changed: ${symbolResult.changed.size}")
    println()

  // Step 3: Function body comparison
  if config.verbose then println("Step 3: Comparing function bodies...")
  val disassembler = Disassembler(runner)

  val functionDiff = for
    oldDisasm <- disassembler.disassemble(config.oldBinary)
    newDisasm <- disassembler.disassemble(config.newBinary)
  yield FunctionComparator.compare(oldDisasm, newDisasm, config.focusPrefix, ignorePatterns.matches)

  val functionResult = functionDiff match
    case Left(err)   => return Left(s"Function comparison failed: $err")
    case Right(diff) => diff

  if config.verbose then
    println(s"  Identical: ${functionResult.identical.size}")
    println(s"  Modified: ${functionResult.modified.size}")
    println(s"  Added: ${functionResult.added.size}")
    println(s"  Removed: ${functionResult.removed.size}")
    println()

  // Step 4: String literal comparison
  if config.verbose then println("Step 4: Comparing embedded strings...")
  val stringExtractor = StringExtractor(runner)

  val stringDiff = stringExtractor.compare(config.oldBinary, config.newBinary) match
    case Left(err) =>
      if config.verbose then println(s"  Warning: String extraction failed: $err")
      None
    case Right(diff) =>
      if config.verbose then
        println(s"  Old binary: ${diff.oldStrings.count} strings")
        println(s"  New binary: ${diff.newStrings.count} strings")
        println(s"  Added: ${diff.added.size}")
        println(s"  Removed: ${diff.removed.size}")
        println()
      Some(diff)

  Right(
    ComparisonResult(
      binaryInfo = binaryInfo,
      bitEquivalence = bitResult,
      symbolDiff = Some(symbolResult),
      functionDiff = Some(functionResult),
      stringDiff = stringDiff
    )
  )

/** Show detailed diff for functions matching a regex pattern */
def showDiff(
    config: Config,
    runner: ProcessRunner,
    pattern: String,
    ignorePatterns: IgnorePatterns
): Either[String, Unit] =
  val disassembler = Disassembler(runner)
  val regex = pattern.r

  for
    oldDisasm <- disassembler.disassemble(config.oldBinary)
    newDisasm <- disassembler.disassemble(config.newBinary)
  yield
    // Apply focus filter
    val focusedOld = config.focusPrefix match
      case Some(prefix) => oldDisasm.filter(_._1.contains(prefix))
      case None         => oldDisasm

    val focusedNew = config.focusPrefix match
      case Some(prefix) => newDisasm.filter(_._1.contains(prefix))
      case None         => newDisasm

    // Apply ignore filter
    val filteredOld = focusedOld.filterNot((name, _) => ignorePatterns.matches(name))
    val filteredNew = focusedNew.filterNot((name, _) => ignorePatterns.matches(name))

    // Find functions matching the pattern
    val allNames = filteredOld.keySet.union(filteredNew.keySet)
    val matchingNames = allNames.filter(name => regex.findFirstIn(name).isDefined).toList.sorted

    if matchingNames.isEmpty then
      System.err.println(s"${report.Colors.Yellow}No functions matching pattern '$pattern'${report.Colors.Reset}")
      System.err.println(s"\nAvailable functions:")
      filteredOld.keys.toList.sorted.take(20).foreach(n => System.err.println(s"  $n"))
      if filteredOld.size > 20 then System.err.println(s"  ... and ${filteredOld.size - 20} more")
      sys.exit(1)

    var hasChanges = false
    var diffCount = 0

    for name <- matchingNames do
      val oldFnOpt = filteredOld.get(name)
      val newFnOpt = filteredNew.get(name)

      (oldFnOpt, newFnOpt) match
        case (Some(oldFn), None) =>
          hasChanges = true
          diffCount += 1
          println(s"${report.Colors.Red}Function '${oldFn.name}' only exists in old binary.${report.Colors.Reset}")
          println(s"Instructions: ${oldFn.instructions.size}")
          println(s"${report.Colors.Gray}${"─" * 70}${report.Colors.Reset}\n")

        case (None, Some(newFn)) =>
          hasChanges = true
          diffCount += 1
          println(s"${report.Colors.Green}Function '${newFn.name}' only exists in new binary.${report.Colors.Reset}")
          println(s"Instructions: ${newFn.instructions.size}")
          println(s"${report.Colors.Gray}${"─" * 70}${report.Colors.Reset}\n")

        case (Some(oldFn), Some(newFn)) =>
          val diff = FunctionDiffer.diff(oldFn, newFn, config.usePltSymbols)
          if diff.stats.added > 0 || diff.stats.removed > 0 then
            hasChanges = true
            diffCount += 1
            val formatted = FunctionDiffer.formatUnifiedDiff(
              diff,
              contextLines = 3,
              normalizeAddresses = config.normalizeAddresses,
              usePltSymbols = config.usePltSymbols,
              runner = Some(runner)
            )
            println(formatted)
            println(s"${report.Colors.Gray}${"─" * 70}${report.Colors.Reset}\n")

        case (None, None) => () // shouldn't happen

    if diffCount == 0 then
      println(s"${report.Colors.Green}All ${matchingNames.size} matching functions are identical.${report.Colors.Reset}")
      sys.exit(0)
    else
      println(s"${report.Colors.Bold}Total: $diffCount modified out of ${matchingNames.size} matching functions${report.Colors.Reset}")
      sys.exit(1)

@main def bindiff(args: String*): Unit =
  val config = Config.parse(args.toArray) match
    case None      => sys.exit(1) // scopt already prints usage/error
    case Some(cfg) => cfg

  val runner = DefaultProcessRunner()

  // Validate system tools first
  validateSystemTools(runner) match
    case Left(err) =>
      System.err.println(s"Error: $err")
      sys.exit(1)
    case Right(_) =>
      if config.verbose then println("All required system tools are available.\n")

  // Load ignore patterns if specified
  val ignorePatterns = config.ignoreFile match
    case Some(path) =>
      IgnorePatterns.fromFile(path) match
        case Left(err) =>
          System.err.println(s"Error: $err")
          sys.exit(1)
        case Right(patterns) => patterns
    case None => IgnorePatterns.empty

  // Handle diff mode or run comparison
  config.diffPattern match
    case Some(pattern) =>
      showDiff(config, runner, pattern, ignorePatterns) match
        case Left(err) =>
          System.err.println(s"Error: $err")
          sys.exit(1)
        case Right(_) => ()

    case None =>
      // Run the comparison pipeline
      runComparison(config, runner) match
        case Left(err) =>
          System.err.println(s"Error: $err")
          sys.exit(1)
        case Right(result) =>
          Reporter.printReport(result, config.verbose)
          // Exit with code 0 if identical, 1 if different
          if result.bitEquivalence.identical then sys.exit(0)
          else sys.exit(1)

