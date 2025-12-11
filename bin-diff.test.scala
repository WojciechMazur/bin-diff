import model.*
import process.*
import analysis.*

class BinDiffIntegrationTest extends munit.FunSuite:

  val testProgramDir: os.Path = os.pwd / "test-program"
  val runner: ProcessRunner = DefaultProcessRunner()
  val bazelBinDir: os.Path = testProgramDir / "bazel-bin"
  val cmakeBuildDir: os.Path = testProgramDir / "cmake-build"

  // Shared library extension (platform-dependent)
  val sharedExt: String = if System.getProperty("os.name").toLowerCase.contains("mac") then "dylib" else "so"

  // Executables
  lazy val bazelBinary: os.Path = bazelBinDir / "demo"
  lazy val cmakeBinary: os.Path = cmakeBuildDir / "demo"

  // Static libraries (.a)
  lazy val bazelMathUtilsStatic: os.Path = bazelBinDir / "libmath_utils.a"
  lazy val bazelLoggerStatic: os.Path = bazelBinDir / "liblogger.a"
  lazy val bazelStringUtilsStatic: os.Path = bazelBinDir / "libstring_utils.a"
  lazy val cmakeMathUtilsStatic: os.Path = cmakeBuildDir / "libmath_utils.a"
  lazy val cmakeLoggerStatic: os.Path = cmakeBuildDir / "liblogger.a"
  lazy val cmakeStringUtilsStatic: os.Path = cmakeBuildDir / "libstring_utils.a"

  // Shared libraries (.so/.dylib)
  lazy val bazelMathUtilsShared: os.Path = bazelBinDir / s"libmath_utils_shared.$sharedExt"
  lazy val bazelLoggerShared: os.Path = bazelBinDir / s"liblogger_shared.$sharedExt"
  lazy val bazelStringUtilsShared: os.Path = bazelBinDir / s"libstring_utils_shared.$sharedExt"
  lazy val cmakeMathUtilsShared: os.Path = cmakeBuildDir / s"libmath_utils.$sharedExt"
  lazy val cmakeLoggerShared: os.Path = cmakeBuildDir / s"liblogger.$sharedExt"
  lazy val cmakeStringUtilsShared: os.Path = cmakeBuildDir / s"libstring_utils.$sharedExt"

  /** Helper to run comparison and handle the result with a custom assertion lambda */
  def withComparison(config: Config)(assertions: ComparisonResult => Unit): Unit =
    runComparison(config, runner) match
      case Left(err)         => fail(s"Comparison failed unexpectedly: $err")
      case Right(comparison) => assertions(comparison)

  override def beforeAll(): Unit =
    // Ensure test-program directory exists
    assert(os.exists(testProgramDir), s"test-program directory does not exist: $testProgramDir")

    // Build with Bazel (all targets)
    println("Building with Bazel...")
    val bazelResult = runner.runInDir("bazel", testProgramDir, "build", "//...")
    bazelResult match
      case ProcessResult.Failure(code, stdout, stderr) =>
        fail(s"Bazel build failed (exit code $code):\n$stdout\n$stderr")
      case ProcessResult.Success(_, _) =>
        println("  Bazel build succeeded")

    // Build with CMake
    println("Building with CMake...")

    // Clean and recreate cmake-build directory to avoid stale cache issues
    if os.exists(cmakeBuildDir) then
      os.remove.all(cmakeBuildDir)
    os.makeDir.all(cmakeBuildDir)

    // Run cmake configure
    val cmakeConfigResult = runner.runInDir("cmake", cmakeBuildDir, "..")
    cmakeConfigResult match
      case ProcessResult.Failure(code, stdout, stderr) =>
        fail(s"CMake configure failed (exit code $code):\n$stdout\n$stderr")
      case ProcessResult.Success(_, _) =>
        println("  CMake configure succeeded")

    // Run cmake build
    val cmakeBuildResult = runner.runInDir("cmake", cmakeBuildDir, "--build", ".")
    cmakeBuildResult match
      case ProcessResult.Failure(code, stdout, stderr) =>
        fail(s"CMake build failed (exit code $code):\n$stdout\n$stderr")
      case ProcessResult.Success(_, _) =>
        println("  CMake build succeeded")

    // Verify all outputs exist
    val requiredFiles = List(
      bazelBinary -> "Bazel demo",
      cmakeBinary -> "CMake demo",
      bazelMathUtilsStatic -> "Bazel libmath_utils.a",
      bazelLoggerStatic -> "Bazel liblogger.a",
      bazelStringUtilsStatic -> "Bazel libstring_utils.a",
      cmakeMathUtilsStatic -> "CMake libmath_utils.a",
      cmakeLoggerStatic -> "CMake liblogger.a",
      cmakeStringUtilsStatic -> "CMake libstring_utils.a",
      bazelMathUtilsShared -> s"Bazel libmath_utils_shared.$sharedExt",
      bazelLoggerShared -> s"Bazel liblogger_shared.$sharedExt",
      bazelStringUtilsShared -> s"Bazel libstring_utils_shared.$sharedExt",
      cmakeMathUtilsShared -> s"CMake libmath_utils.$sharedExt",
      cmakeLoggerShared -> s"CMake liblogger.$sharedExt",
      cmakeStringUtilsShared -> s"CMake libstring_utils.$sharedExt"
    )
    for (path, name) <- requiredFiles do
      assert(os.exists(path), s"$name does not exist: $path")
    println(s"  All ${requiredFiles.size} build outputs verified")

  test("comparing bazel binary to cmake binary should detect expected differences"):
    withComparison(Config(oldBinary = bazelBinary, newBinary = cmakeBinary)): result =>
      // Binaries built with different build systems should NOT be bit-identical
      assert(!result.bitEquivalence.identical, "Bazel and CMake binaries should NOT be bit-identical")

      // Function comparison: should have some modified functions
      val funcDiff = result.functionDiff.getOrElse(fail("Expected function diff"))
      assert(funcDiff.modified.nonEmpty, "Expected some modified functions between builds")

      // String comparison: "[INFO]" added (cmake), "[INFORMATION]" removed (bazel)
      // This is due to VERBOSE_LOGS=1 define in bazel that changes the log prefix
      val strDiff = result.stringDiff.getOrElse(fail("Expected string diff"))
      assert(strDiff.added.contains("[INFO]"), s"Expected '[INFO]' in added strings, got: ${strDiff.added}")
      assert(strDiff.removed.contains("[INFORMATION]"), s"Expected '[INFORMATION]' in removed strings, got: ${strDiff.removed}")

  test("comparing bazel binary to itself should pass"):
    withComparison(Config(oldBinary = bazelBinary, newBinary = bazelBinary)): result =>
      assert(
        result.bitEquivalence.identical,
        s"Bazel binary compared to itself should be bit-identical. " +
          s"Old hash: ${result.bitEquivalence.oldHash}, New hash: ${result.bitEquivalence.newHash}"
      )
      assert(result.isIdentical, "Bazel binary compared to itself should be identical")

  test("comparing cmake binary to itself should pass"):
    withComparison(Config(oldBinary = cmakeBinary, newBinary = cmakeBinary)): result =>
      assert(
        result.bitEquivalence.identical,
        s"CMake binary compared to itself should be bit-identical. " +
          s"Old hash: ${result.bitEquivalence.oldHash}, New hash: ${result.bitEquivalence.newHash}"
      )
      assert(result.isIdentical, "CMake binary compared to itself should be identical")

  test("comparison result severity for different binaries should not be Identical"):
    withComparison(Config(oldBinary = bazelBinary, newBinary = cmakeBinary)): result =>
      assert(
        result.severity != ComparisonSeverity.Identical,
        s"Severity should not be Identical for different binaries, got: ${result.severity}"
      )

  // Static library tests
  test("comparing bazel vs cmake static math_utils library should detect differences"):
    withComparison(Config(oldBinary = bazelMathUtilsStatic, newBinary = cmakeMathUtilsStatic)): result =>
      assert(!result.bitEquivalence.identical, "Static libraries from different build systems should differ")

  test("comparing bazel vs cmake static logger library should detect expected differences"):
    withComparison(Config(oldBinary = bazelLoggerStatic, newBinary = cmakeLoggerStatic)): result =>
      assert(!result.bitEquivalence.identical, "Static libraries from different build systems should differ")

      // String comparison: "[INFO]" vs "[INFORMATION]" (due to VERBOSE_LOGS define)
      val strDiff = result.stringDiff.getOrElse(fail("Expected string diff"))
      assert(strDiff.added.contains("[INFO]"), s"Expected '[INFO]' in added strings, got: ${strDiff.added}")
      assert(strDiff.removed.contains("[INFORMATION]"), s"Expected '[INFORMATION]' in removed strings, got: ${strDiff.removed}")

  test("static math_utils library compared to itself should be identical"):
    withComparison(Config(oldBinary = bazelMathUtilsStatic, newBinary = bazelMathUtilsStatic)): result =>
      assert(result.bitEquivalence.identical, "Static library compared to itself should be identical")

  test("static logger library compared to itself should be identical"):
    withComparison(Config(oldBinary = cmakeLoggerStatic, newBinary = cmakeLoggerStatic)): result =>
      assert(result.bitEquivalence.identical, "Static library compared to itself should be identical")

  // Shared library tests
  test("comparing bazel vs cmake shared math_utils library should detect differences"):
    withComparison(Config(oldBinary = bazelMathUtilsShared, newBinary = cmakeMathUtilsShared)): result =>
      assert(!result.bitEquivalence.identical, "Shared libraries from different build systems should differ")

  test("comparing bazel vs cmake shared logger library should detect expected differences"):
    withComparison(Config(oldBinary = bazelLoggerShared, newBinary = cmakeLoggerShared)): result =>
      assert(!result.bitEquivalence.identical, "Shared libraries from different build systems should differ")

      // Function: log_with_prefix should be modified due to VERBOSE_LOGS define difference
      val funcDiff = result.functionDiff.getOrElse(fail("Expected function diff"))
      assert(
        funcDiff.modified.exists(_.name.contains("log_with_prefix")),
        s"Expected modified function 'log_with_prefix', got: ${funcDiff.modified.map(_.name)}"
      )

      // String comparison: "[INFO]" vs "[INFORMATION]"
      val strDiff = result.stringDiff.getOrElse(fail("Expected string diff"))
      assert(strDiff.added.contains("[INFO]"), s"Expected '[INFO]' in added strings, got: ${strDiff.added}")
      assert(strDiff.removed.contains("[INFORMATION]"), s"Expected '[INFORMATION]' in removed strings, got: ${strDiff.removed}")

  test("shared math_utils library compared to itself should be identical"):
    withComparison(Config(oldBinary = bazelMathUtilsShared, newBinary = bazelMathUtilsShared)): result =>
      assert(result.bitEquivalence.identical, "Shared library compared to itself should be identical")

  test("shared logger library compared to itself should be identical"):
    withComparison(Config(oldBinary = cmakeLoggerShared, newBinary = cmakeLoggerShared)): result =>
      assert(result.bitEquivalence.identical, "Shared library compared to itself should be identical")

  // String utils library tests - same content between build systems (no defines that change behavior)
  // Excludes linker/archive metadata differences (object file names, timestamps, stubs)
  def isLinkerArtifact(name: String): Boolean = {
    name.startsWith("__") || name.endsWith(".o:") 
    // C++ standard library functions (Itanium ABI mangled names)
    || name.startsWith("_ZNSt") || name.startsWith("_ZSt") || name.startsWith("_ZN9__gnu_cxx")
    // PLT entries
    || name == ".plt" || name.startsWith(".plt.")
    // Compiler/linker initialization functions
    || name == "_init" || name == "deregister_tm_clones" || name == "frame_dummy" || name == "register_tm_clones"
    // Lambda functions (closures) generated by compiler
    // || name.startsWith("_ZZ")
  }

  /** Helper to show detailed diffs for modified functions */
  def showFunctionDiffs(
      oldBinary: os.Path,
      newBinary: os.Path,
      modifiedUserFuncs: List[model.FunctionComparisonResult]
  ): Unit = {
    if modifiedUserFuncs.nonEmpty && sys.env.get("DEBUG").exists(_.toBoolean) then
      val disassembler = Disassembler(runner)
      val List(oldDisasm, newDisasm) = List(oldBinary, newBinary).map: binary =>
        disassembler.disassemble(binary)
        .getOrElse(fail(s"Failed to disassemble binary: $binary"))
      println(s"\n${"=" * 80}")
      println(s"Found ${modifiedUserFuncs.size} modified user functions:")
      println(s"${"=" * 80}\n")
      
      for func <- modifiedUserFuncs do
        (oldDisasm.get(func.name), newDisasm.get(func.name)) match
          case (Some(oldFn), Some(newFn)) =>
            val diff = FunctionDiffer.diff(oldFn, newFn, usePltSymbols = false)
            val formatted = FunctionDiffer.formatUnifiedDiff(
              diff,
              contextLines = 5,
              normalizeAddresses = true,
              usePltSymbols = false
            )
            println(formatted)
            println(s"${"-" * 80}\n")
          case _ =>
            println(s"Warning: Could not find function ${func.name} in disassembly\n")
  }

  test("bazel vs cmake static string_utils should have same content"):
    withComparison(Config(oldBinary = bazelStringUtilsStatic, newBinary = cmakeStringUtilsStatic)): result =>
      val funcDiff = result.functionDiff.getOrElse(fail("Expected function diff"))
      val symbolDiff = result.symbolDiff.getOrElse(fail("Expected symbol diff"))

      // No modified function bodies
      val modifiedUserFuncs = funcDiff.modified.filterNot(f => isLinkerArtifact(f.name))
      showFunctionDiffs(bazelStringUtilsStatic, cmakeStringUtilsStatic, modifiedUserFuncs)
      assert(modifiedUserFuncs.isEmpty, s"Expected no modified functions, got: ${modifiedUserFuncs.map(_.name)}")

      // No added/removed user symbols (excluding object file names)
      val addedUserSymbols = symbolDiff.added.filterNot(s => isLinkerArtifact(s.name))
      val removedUserSymbols = symbolDiff.removed.filterNot(s => isLinkerArtifact(s.name))
      assert(addedUserSymbols.isEmpty, s"Expected no added user symbols, got: ${addedUserSymbols.map(_.name)}")
      assert(removedUserSymbols.isEmpty, s"Expected no removed user symbols, got: ${removedUserSymbols.map(_.name)}")

  test("bazel vs cmake shared string_utils should have same content"):
    withComparison(Config(oldBinary = bazelStringUtilsShared, newBinary = cmakeStringUtilsShared)): result =>
      val funcDiff = result.functionDiff.getOrElse(fail("Expected function diff"))
      val symbolDiff = result.symbolDiff.getOrElse(fail("Expected symbol diff"))

      // No modified user function bodies (linker stubs like __stubs may differ)
      val modifiedUserFuncs = funcDiff.modified.filterNot(f => isLinkerArtifact(f.name))
      showFunctionDiffs(bazelStringUtilsShared, cmakeStringUtilsShared, modifiedUserFuncs)
      assert(modifiedUserFuncs.isEmpty, s"Expected no modified user functions, got: ${modifiedUserFuncs.map(_.name)}")

      // No meaningful string differences (exclude archive/linker metadata)
      val strDiff = result.stringDiff.get
      assert(strDiff.added.isEmpty && strDiff.removed.isEmpty,
        s"Expected no meaningful string differences, got added: ${strDiff.added}, removed: ${strDiff.removed}")
