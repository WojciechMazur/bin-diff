import model.*
import process.*
import analysis.*

class BinDiffIntegrationTest extends munit.FunSuite:

  val testProgramDir: os.Path = os.pwd / "test-program"
  val runner: ProcessRunner = DefaultProcessRunner()

  // Build outputs
  lazy val bazelBinary: os.Path = testProgramDir / "bazel-bin" / "demo"
  lazy val cmakeBuildDir: os.Path = testProgramDir / "cmake-build"
  lazy val cmakeBinary: os.Path = cmakeBuildDir / "demo"

  /** Helper to run comparison and handle the result with a custom assertion lambda */
  def withComparison(config: Config)(assertions: ComparisonResult => Unit): Unit =
    runComparison(config, runner) match
      case Left(err)         => fail(s"Comparison failed unexpectedly: $err")
      case Right(comparison) => assertions(comparison)

  override def beforeAll(): Unit =
    // Ensure test-program directory exists
    assert(os.exists(testProgramDir), s"test-program directory does not exist: $testProgramDir")

    // Build with Bazel
    println("Building with Bazel...")
    val bazelResult = runner.runInDir("bazel", testProgramDir, "build", "//:demo")
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

    // Verify binaries exist
    assert(os.exists(bazelBinary), s"Bazel binary does not exist: $bazelBinary")
    assert(os.exists(cmakeBinary), s"CMake binary does not exist: $cmakeBinary")
    println(s"  Bazel binary: $bazelBinary")
    println(s"  CMake binary: $cmakeBinary")

  test("comparing bazel binary to cmake binary should detect differences"):
    withComparison(Config(oldBinary = bazelBinary, newBinary = cmakeBinary)): result =>
      // Binaries built with different build systems should NOT be bit-identical
      assert(!result.bitEquivalence.identical, "Bazel and CMake binaries should NOT be bit-identical")
      println(s"  Bazel hash: ${result.bitEquivalence.oldHash}")
      println(s"  CMake hash: ${result.bitEquivalence.newHash}")

      // There should be some differences detected
      val hasSymbolDiff = result.symbolDiff.exists(_.hasChanges)
      val hasFunctionDiff = result.functionDiff.exists: fd =>
        fd.modified.nonEmpty || fd.added.nonEmpty || fd.removed.nonEmpty

      println(s"  Symbol changes: $hasSymbolDiff")
      println(s"  Function changes: $hasFunctionDiff")

      // At minimum, the binaries should be detected as different
      assert(!result.isIdentical, "Comparison should detect that binaries are different")

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
